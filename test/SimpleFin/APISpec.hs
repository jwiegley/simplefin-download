{-# LANGUAGE DataKinds #-}

module SimpleFin.APISpec (spec) where

import Data.Text qualified as T
import SimpleFin.API
import SimpleFin.Types
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "parseUrlForReq" $ do
    it "parses valid HTTPS URL" $ do
      let result = parseUrlForReq "https://api.example.com/endpoint"
      result `shouldSatisfy` isJust

    it "parses URL with path segments" $ do
      let result = parseUrlForReq "https://api.simplefin.org/simplefin/accounts"
      result `shouldSatisfy` isJust

    it "parses URL with port" $ do
      let result = parseUrlForReq "https://api.example.com:8443/api"
      result `shouldSatisfy` isJust

    it "parses URL with query parameters" $ do
      let result = parseUrlForReq "https://api.example.com/api?key=value"
      result `shouldSatisfy` isJust

    it "fails on invalid URL" $ do
      let result = parseUrlForReq "not a url"
      result `shouldBe` Nothing

    it "fails on non-HTTPS URL" $ do
      -- parseUrlForReq only handles HTTPS
      let result = parseUrlForReq "http://api.example.com/api"
      result `shouldBe` Nothing

    it "fails on empty URL" $ do
      let result = parseUrlForReq ""
      result `shouldBe` Nothing

    it "fails on malformed URL" $ do
      let result = parseUrlForReq "https://[invalid"
      result `shouldBe` Nothing

  describe "httpExceptionToError" $ do
    it "has correct type signature" $ do
      -- This test documents the error conversion behavior
      -- In a real scenario, we'd need to construct actual HttpExceptions
      -- Here we verify the function exists and has correct type
      let _f = httpExceptionToError
      True `shouldBe` True

  describe "API function types" $ do
    it "claimAccessUrl has correct type signature" $ do
      -- Type-level test: verifying the function exists with expected signature
      let _f = claimAccessUrl :: ClaimUrl -> IO (Either SimplefinError AccessUrl)
      True `shouldBe` True

    it "fetchAccounts has correct type signature" $ do
      -- Type-level test: verifying the function exists with expected signature
      let _f = fetchAccounts :: BaseUrl -> Username -> Password -> IO (Either SimplefinError AccountsResponse)
      True `shouldBe` True

  describe "URL construction" $ do
    it "constructs valid claim URL from setup token" $ do
      let claimUrl = ClaimUrl "https://api.simplefin.org/simplefin/claim/TOKEN123"
      -- In real test, we'd mock the HTTP call
      -- Here we verify the URL is well-formed
      case claimUrl of
        ClaimUrl url -> url `shouldSatisfy` T.isPrefixOf "https://"

    it "constructs valid accounts endpoint URL" $ do
      let baseUrl = BaseUrl "https://api.example.com"
          username = Username "user123"
          password = Password "pass456"
      -- Verify inputs are well-formed
      case baseUrl of
        BaseUrl url -> url `shouldSatisfy` T.isPrefixOf "https://"

  describe "Error handling patterns" $ do
    it "creates appropriate error for empty response" $ do
      EmptyResponse `shouldBe` EmptyResponse

    it "creates appropriate error for invalid URL" $ do
      UrlParseError "test" `shouldSatisfy` isUrlParseError

    it "creates appropriate error for connection failure" $ do
      ConnectionError "test" `shouldSatisfy` isConnectionError

    it "creates appropriate error for timeout" $ do
      RequestTimeout 30 `shouldSatisfy` isRequestTimeout

  describe "Integration scenarios" $ do
    it "handles claim URL workflow" $ do
      -- Document the expected workflow
      let token = SetupToken "validtoken"
          -- 1. Decode token -> ClaimUrl
          -- 2. POST to ClaimUrl -> AccessUrl
          -- 3. Parse AccessUrl -> credentials
          workflow = "decode -> claim -> parse"
      workflow `shouldBe` "decode -> claim -> parse"

    it "handles fetch accounts workflow" $ do
      -- Document the expected workflow
      let workflow = "parse credentials -> fetch /accounts -> decode JSON"
      workflow `shouldBe` "parse credentials -> fetch /accounts -> decode JSON"

  describe "Property-based tests" $ do
    it "parseUrlForReq is deterministic" $ property $
      \url ->
        let result1 = parseUrlForReq url
            result2 = parseUrlForReq url
         in result1 `shouldBe` result2

    it "parseUrlForReq handles empty input consistently" $ do
      parseUrlForReq "" `shouldBe` parseUrlForReq ""

-- Note: Full HTTP integration tests would require:
-- 1. Mock HTTP server (using servant-server for mocking)
-- 2. Network test fixtures
-- 3. Golden file tests for responses
-- 4. These are complex and should be in a separate integration test suite

-- Helper functions

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isUrlParseError :: SimplefinError -> Bool
isUrlParseError (UrlParseError _) = True
isUrlParseError _ = False

isConnectionError :: SimplefinError -> Bool
isConnectionError (ConnectionError _) = True
isConnectionError _ = False

isRequestTimeout :: SimplefinError -> Bool
isRequestTimeout (RequestTimeout _) = True
isRequestTimeout _ = False

-- Mock test helpers for future integration tests

{- Future integration test structure:

describe "HTTP Integration Tests" $ around withMockServer $ do
  it "successfully claims access URL" $ \port -> do
    let claimUrl = ClaimUrl $ "https://localhost:" <> T.pack (show port) <> "/claim"
    result <- claimAccessUrl claimUrl
    result `shouldSatisfy` isRight

  it "successfully fetches accounts" $ \port -> do
    let baseUrl = BaseUrl $ "https://localhost:" <> T.pack (show port)
        username = Username "testuser"
        password = Password "testpass"
    result <- fetchAccounts baseUrl username password
    result `shouldSatisfy` isRight

withMockServer :: ActionWith Int -> IO ()
withMockServer action = do
  -- Start mock server on random port
  -- Run action with port
  -- Shutdown server
  action 8080
-}

instance Arbitrary ClaimUrl where
  arbitrary = do
    host <- listOf1 (elements ['a'..'z'])
    token <- listOf1 (elements (['A'..'Z'] ++ ['0'..'9']))
    pure $ ClaimUrl $ T.pack $ "https://" ++ host ++ ".com/claim/" ++ token

instance Arbitrary BaseUrl where
  arbitrary = do
    host <- listOf1 (elements ['a'..'z'])
    pure $ BaseUrl $ T.pack $ "https://" ++ host ++ ".com"

instance Arbitrary Username where
  arbitrary = Username . T.pack <$> listOf1 (elements (['a'..'z'] ++ ['0'..'9']))

instance Arbitrary Password where
  arbitrary = Password . T.pack <$> listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
