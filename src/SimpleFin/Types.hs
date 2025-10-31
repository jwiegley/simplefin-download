{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : SimpleFin.Types
Description : Type definitions for SimpleFin API
Copyright   : (c) John Wiegley, 2025
License     : MIT
Maintainer  : johnw@newartisans.com

Core type definitions, newtypes, and error types for the SimpleFin API client.
-}
module SimpleFin.Types
  ( -- * Type Safety Wrappers
    SetupToken (..)
  , ClaimUrl (..)
  , AccessUrl (..)
  , Username (..)
  , Password (..)
  , BaseUrl (..)

    -- * Domain Types
  , Account (..)
  , Transaction (..)
  , AccountsResponse (..)

    -- * Error Types
  , SimplefinError (..)
  , showError
  ) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.=), withObject, withText, (.:))
import Data.Scientific (Scientific)
import Data.Scientific qualified as Sci
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

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

-- Helper to parse Scientific from string (SimpleFin API format)
parseScientificFromString :: Text -> Either String Scientific
parseScientificFromString t = case TR.signed TR.rational t of
  Right (sci, rest) | T.null rest -> Right $ Sci.fromFloatDigits (sci :: Double)
  _ -> Left $ "Invalid scientific number: " ++ T.unpack t

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \v -> do
    posted <- v .: "posted"
    amountText <- v .: "amount"
    amount <- case amountText of
      String txt -> case parseScientificFromString txt of
        Right sci -> pure sci
        Left err -> fail err
      Number sci -> pure sci
      _ -> fail "amount must be a string or number"
    desc <- v .: "description"
    pure $ Transaction posted amount desc

instance FromJSON Account where
  parseJSON = withObject "Account" $ \v -> do
    name <- v .: "name"
    balanceText <- v .: "balance"
    balance <- case balanceText of
      String txt -> case parseScientificFromString txt of
        Right sci -> pure sci
        Left err -> fail err
      Number sci -> pure sci
      _ -> fail "balance must be a string or number"
    balDate <- v .: "balance-date"
    txs <- v .: "transactions"
    pure $ Account name balance balDate txs

instance FromJSON AccountsResponse where
  parseJSON = withObject "AccountsResponse" $ \v ->
    AccountsResponse <$> v .: "accounts"

-- ToJSON instances for testing (matching SimpleFin API format)
instance ToJSON Transaction where
  toJSON (Transaction posted amount desc) =
    object [ "posted" .= posted
           , "amount" .= String (scientificToText amount)
           , "description" .= desc
           ]

instance ToJSON Account where
  toJSON (Account name balance balDate txs) =
    object [ "name" .= name
           , "balance" .= String (scientificToText balance)
           , "balance-date" .= balDate
           , "transactions" .= txs
           ]

instance ToJSON AccountsResponse where
  toJSON (AccountsResponse accts) =
    object [ "accounts" .= accts ]

-- Helper to convert Scientific to Text
scientificToText :: Scientific -> Text
scientificToText = T.pack . Sci.formatScientific Sci.Fixed Nothing

-- * Error Formatting

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
