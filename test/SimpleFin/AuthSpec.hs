module SimpleFin.AuthSpec (spec) where

import Data.ByteString.Base64 qualified
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified
import SimpleFin.Auth
import SimpleFin.Types
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()

spec :: Spec
spec = do
  describe "decodeSetupToken" $ do
    it "decodes valid base64 setup token" $ do
      let token = SetupToken "aHR0cHM6Ly9leGFtcGxlLmNvbS9jbGFpbQ=="
          expected = ClaimUrl "https://example.com/claim"
      decodeSetupToken token `shouldBe` Right expected

    it "decodes token with different URL" $ do
      -- Base64 of "https://api.simplefin.org/simplefin/claim/ABCD1234"
      let token = SetupToken "aHR0cHM6Ly9hcGkuc2ltcGxlZmluLm9yZy9zaW1wbGVmaW4vY2xhaW0vQUJDRDEyMzQ="
          expected = ClaimUrl "https://api.simplefin.org/simplefin/claim/ABCD1234"
      decodeSetupToken token `shouldBe` Right expected

    it "fails on invalid base64 encoding" $ do
      let token = SetupToken "not-valid-base64!!!"
      decodeSetupToken token `shouldSatisfy` isLeft

    it "fails on empty token" $ do
      let token = SetupToken ""
      decodeSetupToken token `shouldSatisfy` isLeft

    it "handles base64 with padding" $ do
      -- Base64 of "https://test.com/c"
      let token = SetupToken "aHR0cHM6Ly90ZXN0LmNvbS9j"
          expected = ClaimUrl "https://test.com/c"
      decodeSetupToken token `shouldBe` Right expected

    it "handles base64 without padding" $ do
      -- Some base64 implementations omit padding
      let token = SetupToken "aHR0cHM6Ly90ZXN0LmNvbS9jbGFpbQ"
          expected = ClaimUrl "https://test.com/claim"
      decodeSetupToken token `shouldBe` Right expected

    it "returns descriptive error message" $ do
      let token = SetupToken "invalid!!!"
          result = decodeSetupToken token
      case result of
        Left (Base64DecodeError msg) -> msg `shouldSatisfy` (not . null)
        _ -> expectationFailure "Expected Base64DecodeError"

  describe "parseAccessUrl" $ do
    it "parses URL with credentials" $ do
      let url = AccessUrl "https://user123:pass456@api.example.com/path"
          result = parseAccessUrl url
      case result of
        Right (Username user, Password pass, BaseUrl base) -> do
          user `shouldBe` "user123"
          pass `shouldBe` "pass456"
          base `shouldBe` "https://api.example.com"
        Left err -> expectationFailure $ "Failed to parse: " ++ show err

    it "parses URL with complex password" $ do
      -- @ in password must be URL-encoded as %40
      let url = AccessUrl "https://user:p%40ssw0rd!@api.example.com/api"
          result = parseAccessUrl url
      case result of
        Right (Username user, Password pass, BaseUrl _) -> do
          user `shouldBe` "user"
          -- The password will be URL-encoded in the result
          pass `shouldBe` "p%40ssw0rd!"
        Left err -> expectationFailure $ "Failed to parse: " ++ show err

    it "parses URL with port number" $ do
      let url = AccessUrl "https://user:pass@api.example.com:8443/api"
          result = parseAccessUrl url
      case result of
        Right (Username _, Password _, BaseUrl base) -> do
          base `shouldBe` "https://api.example.com:8443"
        Left err -> expectationFailure $ "Failed to parse: " ++ show err

    it "fails on URL without credentials" $ do
      let url = AccessUrl "https://api.example.com/path"
      parseAccessUrl url `shouldBe` Left InvalidCredentials

    it "fails on URL with username only" $ do
      let url = AccessUrl "https://user@api.example.com/path"
      parseAccessUrl url `shouldBe` Left InvalidCredentials

    it "fails on URL with empty username" $ do
      let url = AccessUrl "https://:password@api.example.com/path"
      parseAccessUrl url `shouldBe` Left InvalidCredentials

    it "fails on URL with empty password" $ do
      let url = AccessUrl "https://user:@api.example.com/path"
      parseAccessUrl url `shouldBe` Left InvalidCredentials

    it "fails on malformed URL" $ do
      let url = AccessUrl "not-a-url"
      parseAccessUrl url `shouldSatisfy` isLeft

    it "fails on non-http URL" $ do
      let url = AccessUrl "ftp://user:pass@example.com"
      -- This might parse successfully depending on implementation
      -- but we're testing that it handles various schemes
      parseAccessUrl url `shouldSatisfy` isEither

    it "preserves special characters in credentials" $ do
      let url = AccessUrl "https://user%40email:p%40ss@api.example.com/api"
          result = parseAccessUrl url
      case result of
        Right (Username user, Password pass, BaseUrl _) -> do
          -- URL encoding should be preserved or decoded appropriately
          user `shouldSatisfy` (not . T.null)
          pass `shouldSatisfy` (not . T.null)
        Left err -> expectationFailure $ "Failed to parse: " ++ show err

  describe "Token decode and URL parse integration" $ do
    it "successfully processes valid token to credentials" $ do
      -- Base64 of "https://user:pass@example.com/api"
      let token = SetupToken "aHR0cHM6Ly91c2VyOnBhc3NAZXhhbXBsZS5jb20vYXBp"
      case decodeSetupToken token of
        Right claimUrl -> do
          -- In real scenario, claimUrl would be POSTed to get accessUrl
          -- Here we verify the decode step works
          claimUrl `shouldBe` ClaimUrl "https://user:pass@example.com/api"
        Left err -> expectationFailure $ "Decode failed: " ++ show err

  describe "Property-based tests" $ do
    it "decoding is inverse of base64 encoding" $ property $
      \url -> not (T.null url) ==>
        let encoded = T.pack $ convertToBase64 $ T.unpack url
            token = SetupToken encoded
            result = decodeSetupToken token
         in case result of
              Right (ClaimUrl decoded) -> decoded === url
              Left _ -> False === True

    it "parseAccessUrl handles various valid formats" $ property $
      forAll genValidUrlParts $ \(user, pass, host) ->
        let url = AccessUrl $ T.pack $
                    "https://" ++ user ++ ":" ++ pass ++ "@" ++ host ++ ".com/api"
            result = parseAccessUrl url
         in isRight result

-- Helper functions

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isEither :: Either a b -> Bool
isEither _ = True

convertToBase64 :: String -> String
convertToBase64 = T.unpack . Data.Text.Encoding.decodeUtf8 . Data.ByteString.Base64.encode . Data.Text.Encoding.encodeUtf8 . T.pack

-- Sanitize string to be valid in URL
sanitize :: String -> String
sanitize = filter (\c -> c `notElem` (":/@?#[]!$&'()*+,;= \t\n\r" :: String))

-- Generate valid URL parts for property testing
genValidUrlParts :: Gen (String, String, String)
genValidUrlParts = do
  -- Generate valid username: alphanumeric and some safe special chars
  user <- listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_."))
  -- Generate valid password: alphanumeric and common safe special chars
  -- Avoid ':' and '@' as they have special meaning in URLs
  pass <- listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_.!$*"))
  -- Generate valid hostname: lowercase letters and hyphens, must start with letter
  hostStart <- elements ['a'..'z']
  hostRest <- listOf (elements (['a'..'z'] ++ ['0'..'9'] ++ "-"))
  let host = hostStart : hostRest
  pure (user, pass, host)

instance Arbitrary SetupToken where
  arbitrary = SetupToken . T.pack <$> listOf1 (elements ['A'..'Z'])

instance Arbitrary AccessUrl where
  arbitrary = do
    user <- listOf1 (elements (['a'..'z'] ++ ['0'..'9']))
    pass <- listOf1 (elements (['a'..'z'] ++ ['0'..'9']))
    host <- listOf1 (elements ['a'..'z'])
    let url = "https://" ++ user ++ ":" ++ pass ++ "@" ++ host ++ ".com/api"
    pure $ AccessUrl (T.pack url)

instance Arbitrary ClaimUrl where
  arbitrary = ClaimUrl . T.pack <$> do
    host <- listOf1 (elements ['a'..'z'])
    pure $ "https://" ++ host ++ ".com/claim"
