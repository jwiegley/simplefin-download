{-# LANGUAGE DataKinds #-}

{- |
Module      : SimpleFin.API
Description : HTTP API client functions
Copyright   : (c) John Wiegley, 2025
License     : MIT
Maintainer  : johnw@newartisans.com

HTTP client functions for interacting with the SimpleFin API.
-}
module SimpleFin.API
  ( claimAccessUrl
  , fetchAccounts
  , parseUrlForReq
  , httpExceptionToError
  ) where

import Control.Exception (try)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Req
  ( GET (..)
  , NoReqBody (..)
  , POST (..)
  , Scheme (Https)
  , Url
  , bsResponse
  , defaultHttpConfig
  , jsonResponse
  , req
  , responseBody
  , runReq
  , useHttpsURI
  , (/:)
  )
import Network.HTTP.Req qualified as Req
import SimpleFin.Types
  ( AccessUrl (..)
  , AccountsResponse
  , BaseUrl (..)
  , ClaimUrl (..)
  , Password (..)
  , SimplefinError (..)
  , Username (..)
  )
import Text.URI qualified as ModernURI

-- | Claim an access URL by POSTing to the claim URL.
--
-- The claim endpoint returns an access URL with embedded credentials
-- that can be used for subsequent API requests.
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

-- | Fetch account data from the SimpleFin API.
--
-- Requires base URL and authentication credentials extracted from the access URL.
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
