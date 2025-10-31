{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Main
Description : SimpleFin API Client
Copyright   : (c) John Wiegley, 2025
License     : MIT
Maintainer  : johnw@newartisans.com

This program interacts with the SimpleFin API to retrieve account and transaction data.

Workflow:
    1. Obtain a setup token (base64-encoded claim URL)
    2. Claim an access URL by POSTing to the claim URL
    3. Parse the access URL to extract credentials
    4. Fetch account data from the /accounts endpoint
    5. Display formatted account balances and transactions

Environment Variables:
    SIMPLEFIN_SETUP_TOKEN: Base64-encoded setup token (skips interactive prompt)
    SIMPLEFIN_ACCESS_URL: Pre-claimed access URL (skips claiming step)

Usage:
    simplefin-download
-}

module Main (main) where

import Control.Exception (Exception, catch, throwIO, try)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (..), Object, Value (..), withObject, (.:))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Scientific (Scientific)
import Data.Scientific qualified as Sci
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (utcToLocalZonedTime)
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Req (GET (..), JsonResponse, NoReqBody (..), POST (..), Req, Scheme (Https), Url, bsResponse, defaultHttpConfig, header, jsonResponse, req, responseBody, responseStatusCode, runReq, useHttpsURI, (/:))
import Network.HTTP.Req qualified as Req
import Network.HTTP.Types.Status (statusCode)
import Network.URI qualified as NetURI
import Text.URI qualified as ModernURI
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (BufferMode (..), hFlush, hPutStrLn, hSetBuffering, hSetEcho, stderr, stdin, stdout)
import Text.Printf (printf)

-- * Type Safety Wrappers

-- | Base64-encoded setup token
newtype SetupToken = SetupToken Text
  deriving stock (Show, Eq)

-- | URL for claiming access credentials
newtype ClaimUrl = ClaimUrl Text
  deriving stock (Show, Eq)

-- | Access URL with embedded credentials (https://user:pass@host/path)
newtype AccessUrl = AccessUrl Text
  deriving stock (Show, Eq)

-- | Authentication username
newtype Username = Username Text
  deriving stock (Show, Eq)

-- | Authentication password
newtype Password = Password Text
  deriving stock (Show, Eq)

-- | Base API URL without credentials
newtype BaseUrl = BaseUrl Text
  deriving stock (Show, Eq)

-- * Domain Types

-- | Account information with transactions
data Account = Account
  { accountName :: !Text
  , accountBalance :: !Scientific
  , accountBalanceDate :: !POSIXTime
  , accountTransactions :: ![Transaction]
  }
  deriving stock (Show, Eq, Generic)

-- | Individual transaction record
data Transaction = Transaction
  { transactionPosted :: !POSIXTime
  , transactionAmount :: !Scientific
  , transactionDescription :: !Text
  }
  deriving stock (Show, Eq, Generic)

-- | API response containing accounts
newtype AccountsResponse = AccountsResponse
  { accounts :: [Account]
  }
  deriving stock (Show, Eq, Generic)

-- * Error Types

-- | Errors that can occur during SimpleFin API operations
data SimplefinError
  = Base64DecodeError String
  | HttpError String
  | JsonDecodeError String
  | UrlParseError String
  | InvalidCredentials
  | MissingEnvironmentVariable String
  | EmptyResponse
  | RequestTimeout Int
  | ConnectionError String
  deriving stock (Show, Eq)

instance Exception SimplefinError

-- * JSON Instances

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \v ->
    Transaction
      <$> v .: "posted"
      <*> v .: "amount"
      <*> v .: "description"

instance FromJSON Account where
  parseJSON = withObject "Account" $ \v ->
    Account
      <$> v .: "name"
      <*> v .: "balance"
      <*> v .: "balance-date"
      <*> v .: "transactions"

instance FromJSON AccountsResponse where
  parseJSON = withObject "AccountsResponse" $ \v ->
    AccountsResponse <$> v .: "accounts"

-- * Core Functions

-- | Decode a base64-encoded setup token to retrieve the claim URL.
--
-- >>> decodeSetupToken (SetupToken "aHR0cHM6Ly9leGFtcGxlLmNvbS9jbGFpbQ==")
-- Right (ClaimUrl "https://example.com/claim")
decodeSetupToken :: SetupToken -> Either SimplefinError ClaimUrl
decodeSetupToken (SetupToken token) =
  case Base64.decode (TE.encodeUtf8 token) of
    Left err -> Left $ Base64DecodeError err
    Right decoded ->
      case TE.decodeUtf8' decoded of
        Left err -> Left $ Base64DecodeError (show err)
        Right url -> Right $ ClaimUrl url

-- | Claim an access URL by POSTing to the claim URL.
--
-- The claim endpoint returns an access URL with embedded credentials.
claimAccessUrl :: ClaimUrl -> IO (Either SimplefinError AccessUrl)
claimAccessUrl (ClaimUrl claimUrl) = runExceptT $ do
  case parseUrlForReq (T.unpack claimUrl) of
    Nothing -> throwError $ UrlParseError "Invalid claim URL"
    Just (urlScheme, _) -> do
      result <- liftIO $ try $ runReq defaultHttpConfig $ do
        response <- req POST urlScheme NoReqBody bsResponse $
          Req.responseTimeout 30000000 -- 30 seconds
        pure $ responseBody response

      case result of
        Left (err :: Req.HttpException) -> throwError $ httpExceptionToError err
        Right body
          | BS.null body -> throwError EmptyResponse
          | otherwise -> case TE.decodeUtf8' body of
              Left _ -> throwError $ JsonDecodeError "Response is not valid UTF-8"
              Right accessUrl -> pure $ AccessUrl (T.strip accessUrl)

-- | Parse access URL to extract credentials and base URL.
--
-- The access URL format is: https://username:password\@host/path
parseAccessUrl :: AccessUrl -> Either SimplefinError (Username, Password, BaseUrl)
parseAccessUrl (AccessUrl url) = do
  uri <- maybe (Left $ UrlParseError "Invalid URL format") Right $
    NetURI.parseURI (T.unpack url)

  auth <- maybe (Left InvalidCredentials) Right $ NetURI.uriAuthority uri

  let userInfo = NetURI.uriUserInfo auth
  (username, passwordWithColon) <- case break (== ':') userInfo of
    (u, ':' : p) -> Right (u, dropWhile (== '@') p)
    _ -> Left InvalidCredentials

  when (null username || null passwordWithColon) $
    Left InvalidCredentials

  let password = takeWhile (/= '@') passwordWithColon
      scheme = NetURI.uriScheme uri
      host = NetURI.uriRegName auth
      port = NetURI.uriPort auth
      baseUrl = scheme ++ "//" ++ host ++ port

  Right (Username (T.pack username), Password (T.pack password), BaseUrl (T.pack baseUrl))

-- | Fetch account data from the SimpleFin API.
fetchAccounts ::
  BaseUrl ->
  Username ->
  Password ->
  IO (Either SimplefinError AccountsResponse)
fetchAccounts (BaseUrl base) (Username user) (Password pass) = runExceptT $ do
  let fullUrl = T.unpack $ base <> "/accounts"

  case parseUrlForReq fullUrl of
    Nothing -> throwError $ UrlParseError "Invalid base URL"
    Just (urlScheme, _) -> do
      result <- liftIO $ try $ runReq defaultHttpConfig $ do
        response <- req GET (urlScheme /: "accounts") NoReqBody jsonResponse $
          Req.basicAuth (TE.encodeUtf8 user) (TE.encodeUtf8 pass)
            <> Req.responseTimeout 30000000 -- 30 seconds
        pure $ responseBody response

      case result of
        Left (err :: Req.HttpException) -> throwError $ httpExceptionToError err
        Right accountData -> pure accountData

-- | Convert POSIXTime to formatted local time string
formatPosixTime :: POSIXTime -> IO String
formatPosixTime posixTime = do
  let utcTime = posixSecondsToUTCTime posixTime
  zonedTime <- utcToLocalZonedTime utcTime
  pure $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" zonedTime

-- | Format and display account data with transactions
formatAccounts :: AccountsResponse -> IO ()
formatAccounts (AccountsResponse accts) = do
  if null accts
    then putStrLn "No accounts found."
    else mapM_ formatAccount accts

-- | Format a single account with its transactions
formatAccount :: Account -> IO ()
formatAccount Account {..} = do
  balanceDateStr <- formatPosixTime accountBalanceDate
  let balanceFormatted = formatMoney accountBalance
  putStrLn ""
  putStrLn $ balanceDateStr ++ " " ++ printf "%12s" balanceFormatted ++ " " ++ T.unpack accountName
  putStrLn $ replicate 80 '-'

  if null accountTransactions
    then putStrLn "  No transactions"
    else mapM_ formatTransaction accountTransactions

-- | Format a single transaction
formatTransaction :: Transaction -> IO ()
formatTransaction Transaction {..} = do
  postedStr <- formatPosixTime transactionPosted
  let amountFormatted = formatMoney transactionAmount
  putStrLn $ postedStr ++ " " ++ printf "%12s" amountFormatted ++ " " ++ T.unpack transactionDescription

-- | Format a Scientific number as money (2 decimal places)
formatMoney :: Scientific -> String
formatMoney sci =
  printf "%.2f" (Sci.toRealFloat sci :: Double)

-- | Get password input without echoing to terminal
getPasswordInput :: IO Text
getPasswordInput = do
  hSetBuffering stdout NoBuffering
  putStr "Setup Token: "
  hFlush stdout
  hSetEcho stdin False
  password <- TIO.getLine
  hSetEcho stdin True
  putStrLn "" -- Print newline after password input
  pure password

-- | Parse URL string into req-compatible scheme and path
parseUrlForReq :: String -> Maybe (Url 'Https, Text)
parseUrlForReq urlStr = do
  uri <- ModernURI.mkURI (T.pack urlStr)
  (url, _) <- useHttpsURI uri
  pure (url, "")

-- | Convert HttpException to SimplefinError with meaningful message
httpExceptionToError :: Req.HttpException -> SimplefinError
httpExceptionToError err =
  case err of
    Req.VanillaHttpException (HTTP.HttpExceptionRequest _ content) ->
      case content of
        HTTP.ResponseTimeout -> RequestTimeout 30
        HTTP.ConnectionTimeout -> RequestTimeout 30
        HTTP.ConnectionFailure _ -> ConnectionError "Connection failed"
        _ -> HttpError (show content)
    Req.JsonHttpException msg -> JsonDecodeError msg
    _ -> HttpError (show err)

-- * Main Program

-- | Main application logic using ExceptT for error handling
runSimplefin :: ExceptT SimplefinError IO ()
runSimplefin = do
  -- Check if access URL is already available
  maybeAccessUrl <- liftIO $ lookupEnv "SIMPLEFIN_ACCESS_URL"

  accessUrl <- case maybeAccessUrl of
    Just url -> do
      liftIO $ putStrLn "Using access URL from environment variable"
      pure $ AccessUrl (T.pack url)
    Nothing -> do
      -- Get setup token
      maybeSetupToken <- liftIO $ lookupEnv "SIMPLEFIN_SETUP_TOKEN"

      setupToken <- case maybeSetupToken of
        Just token -> pure $ SetupToken (T.pack token)
        Nothing -> do
          token <- liftIO getPasswordInput
          when (T.null token) $
            throwError $ MissingEnvironmentVariable "Setup token is required"
          pure $ SetupToken token

      -- Decode setup token and claim access URL
      liftIO $ putStrLn "Decoding setup token..."
      claimUrl <- case decodeSetupToken setupToken of
        Left err -> throwError err
        Right url -> pure url

      liftIO $ putStrLn "Claiming access URL..."
      result <- liftIO $ claimAccessUrl claimUrl
      case result of
        Left err -> throwError err
        Right url -> do
          liftIO $ putStrLn "Access URL claimed successfully"
          liftIO $ putStrLn ""
          liftIO $ putStrLn "Tip: Save this access URL to environment variable SIMPLEFIN_ACCESS_URL"
          liftIO $ putStrLn "     to skip the claiming step in future runs."
          liftIO $ putStrLn ""
          pure url

  -- Parse access URL
  (username, password, baseUrl) <- case parseAccessUrl accessUrl of
    Left err -> throwError err
    Right creds -> pure creds

  -- Fetch account data
  liftIO $ putStrLn "Fetching account data..."
  result <- liftIO $ fetchAccounts baseUrl username password
  accountData <- case result of
    Left err -> throwError err
    Right data_ -> pure data_

  -- Format and display
  liftIO $ formatAccounts accountData

-- | Main entry point with error handling
main :: IO ()
main = do
  result <- runExceptT runSimplefin
  case result of
    Left err -> do
      hPutStrLn stderr $ "Error: " <> showError err
      exitWith (ExitFailure 1)
    Right () -> exitSuccess
  `catch` \(_ :: ExitCode) -> exitSuccess -- Proper exit codes
  `catch` \(_ :: IOError) -> do
    -- Catch Ctrl-C and other interrupts
    hPutStrLn stderr "\n\nInterrupted by user"
    exitWith (ExitFailure 130)

-- | Convert SimplefinError to user-friendly message
showError :: SimplefinError -> String
showError = \case
  Base64DecodeError msg -> "Failed to decode setup token: " ++ msg
  HttpError msg -> "HTTP error: " ++ msg
  JsonDecodeError msg -> "Invalid JSON response: " ++ msg
  UrlParseError msg -> "URL parsing error: " ++ msg
  InvalidCredentials ->
    "Access URL missing credentials. Expected format: https://username:password@host/path"
  MissingEnvironmentVariable msg -> msg
  EmptyResponse -> "Received empty response from server"
  RequestTimeout seconds -> "Request timed out after " ++ show seconds ++ " seconds"
  ConnectionError msg -> "Connection error: " ++ msg
