{- |
Module      : SimpleFin.Auth
Description : Authentication and token handling
Copyright   : (c) John Wiegley, 2025
License     : MIT
Maintainer  : johnw@newartisans.com

Functions for decoding setup tokens and parsing access URLs with credentials.
-}
module SimpleFin.Auth
  ( decodeSetupToken
  , parseAccessUrl
  ) where

import Control.Monad (when)
import Data.ByteString.Base64 qualified as Base64
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.URI qualified as NetURI
import SimpleFin.Types
  ( AccessUrl (..)
  , BaseUrl (..)
  , ClaimUrl (..)
  , Password (..)
  , SetupToken (..)
  , SimplefinError (..)
  , Username (..)
  )

-- | Decode a base64-encoded setup token to retrieve the claim URL.
--
-- The setup token is a base64-encoded URL that points to the claim endpoint.
--
-- >>> decodeSetupToken (SetupToken "aHR0cHM6Ly9leGFtcGxlLmNvbS9jbGFpbQ==")
-- Right (ClaimUrl "https://example.com/claim")
decodeSetupToken :: SetupToken -> Either SimplefinError ClaimUrl
decodeSetupToken (SetupToken token)
  | T.null token = Left $ Base64DecodeError "Empty token"
  | otherwise =
      let -- Add padding if needed for lenient base64 decoding
          paddedToken = addBase64Padding token
       in case Base64.decode (TE.encodeUtf8 paddedToken) of
            Left err -> Left $ Base64DecodeError err
            Right decoded ->
              case TE.decodeUtf8' decoded of
                Left err -> Left $ Base64DecodeError (show err)
                Right url
                  | T.null url -> Left $ Base64DecodeError "Decoded URL is empty"
                  | otherwise -> Right $ ClaimUrl url

-- | Add padding to base64 string if needed
addBase64Padding :: Text -> Text
addBase64Padding txt =
  let len = T.length txt
      remainder = len `mod` 4
   in if remainder == 0
        then txt
        else txt <> T.replicate (4 - remainder) "="

-- | Parse access URL to extract credentials and base URL.
--
-- The access URL format is: https://username:password\@host/path
--
-- This function extracts the username, password, and reconstructs the base URL
-- without the credentials embedded.
--
-- >>> parseAccessUrl (AccessUrl "https://user:pass@example.com/api")
-- Right (Username "user", Password "pass", BaseUrl "https://example.com")
parseAccessUrl :: AccessUrl -> Either SimplefinError (Username, Password, BaseUrl)
parseAccessUrl (AccessUrl url) = do
  uri <- maybe (Left $ UrlParseError "Invalid URL format") Right $
    NetURI.parseURI (T.unpack url)

  auth <- maybe (Left InvalidCredentials) Right $ NetURI.uriAuthority uri

  let userInfo = NetURI.uriUserInfo auth

  -- Parse username and password from userInfo
  -- userInfo format is "username:password@" (note the trailing @)
  when (null userInfo || not ('@' `elem` userInfo)) $
    Left InvalidCredentials

  -- Remove trailing @ from userInfo
  let userInfoNoAt = reverse . dropWhile (== '@') . reverse $ userInfo

  (username, passwordWithColon) <- case break (== ':') userInfoNoAt of
    (u, ':' : p) -> Right (u, p)
    _ -> Left InvalidCredentials

  when (null username || null passwordWithColon) $
    Left InvalidCredentials

  let password = passwordWithColon
      scheme = NetURI.uriScheme uri
      host = NetURI.uriRegName auth
      port = NetURI.uriPort auth
      baseUrl = scheme ++ "//" ++ host ++ port

  Right (Username (T.pack username), Password (T.pack password), BaseUrl (T.pack baseUrl))
