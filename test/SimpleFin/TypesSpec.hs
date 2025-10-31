{-# LANGUAGE QuasiQuotes #-}

module SimpleFin.TypesSpec (spec) where

import Data.Aeson (Result (Error, Success), decode, encode, eitherDecode, fromJSON)
import Data.Aeson.QQ (aesonQQ)
import Data.ByteString.Lazy qualified as BL
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import SimpleFin.Types
import Test.Hspec
import Test.QuickCheck hiding (Success)
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()

spec :: Spec
spec = do
  describe "SimplefinError" $ do
    it "shows user-friendly error messages" $ do
      showError (Base64DecodeError "invalid") `shouldBe` "Failed to decode setup token: invalid"
      showError InvalidCredentials `shouldContain` "Expected format"
      showError EmptyResponse `shouldBe` "Received empty response from server"
      showError (RequestTimeout 30) `shouldBe` "Request timed out after 30 seconds"

    it "distinguishes different error types" $ do
      Base64DecodeError "a" `shouldNotBe` HttpError "a"
      UrlParseError "x" `shouldNotBe` JsonDecodeError "x"

  describe "Transaction JSON parsing" $ do
    it "parses valid transaction JSON" $ do
      let json = [aesonQQ|
            {
              "posted": 1609459200,
              "amount": "123.45",
              "description": "Coffee Shop"
            }
          |]
      case fromJSON json of
        Success tx -> do
          transactionPosted tx `shouldBe` 1609459200
          transactionAmount tx `shouldBe` fromFloatDigits (123.45 :: Double)
          transactionDescription tx `shouldBe` "Coffee Shop"
        Error err -> expectationFailure $ "Failed to parse transaction: " ++ err

    it "parses transaction with zero posted time (pending)" $ do
      let json = [aesonQQ|
            {
              "posted": 0,
              "amount": "-50.00",
              "description": "Pending charge"
            }
          |]
      case fromJSON json of
        Success tx -> do
          transactionPosted tx `shouldBe` 0
          transactionAmount tx `shouldBe` fromFloatDigits (-50.00 :: Double)
        Error err -> expectationFailure $ "Failed to parse pending transaction: " ++ err

    it "parses negative amounts" $ do
      let json = [aesonQQ|
            {
              "posted": 1609459200,
              "amount": "-99.99",
              "description": "Expense"
            }
          |]
      case fromJSON json of
        Success (tx :: Transaction) ->
          transactionAmount tx `shouldBe` fromFloatDigits (-99.99 :: Double)
        Error err -> expectationFailure $ "Failed to parse: " ++ err

    it "fails on missing required fields" $ do
      let json = BL.fromStrict "{\"posted\": 123, \"amount\": \"10\"}"
      let result = eitherDecode json :: Either String Transaction
      result `shouldSatisfy` isLeft

    it "fails on invalid amount format" $ do
      let json = BL.fromStrict "{\"posted\": 123, \"amount\": \"invalid\", \"description\": \"test\"}"
      let result = eitherDecode json :: Either String Transaction
      result `shouldSatisfy` isLeft

  describe "Account JSON parsing" $ do
    it "parses account with transactions" $ do
      let json = [aesonQQ|
            {
              "name": "Checking Account",
              "balance": "1500.50",
              "balance-date": 1609459200,
              "transactions": [
                {
                  "posted": 1609459200,
                  "amount": "100.00",
                  "description": "Deposit"
                }
              ]
            }
          |]
      case fromJSON json of
        Success acc -> do
          accountName acc `shouldBe` "Checking Account"
          accountBalance acc `shouldBe` fromFloatDigits (1500.50 :: Double)
          accountBalanceDate acc `shouldBe` 1609459200
          length (accountTransactions acc) `shouldBe` 1
        Error err -> expectationFailure $ "Failed to parse account: " ++ err

    it "parses account with empty transactions" $ do
      let json = [aesonQQ|
            {
              "name": "Savings Account",
              "balance": "5000.00",
              "balance-date": 1609459200,
              "transactions": []
            }
          |]
      case fromJSON json of
        Success (acc :: Account) ->
          accountTransactions acc `shouldBe` []
        Error err -> expectationFailure $ "Failed to parse: " ++ err

    it "parses account with multiple transactions" $ do
      let json = [aesonQQ|
            {
              "name": "Credit Card",
              "balance": "-250.75",
              "balance-date": 1609459200,
              "transactions": [
                {"posted": 1609459200, "amount": "-50.00", "description": "Purchase 1"},
                {"posted": 1609459300, "amount": "-100.00", "description": "Purchase 2"},
                {"posted": 1609459400, "amount": "200.00", "description": "Payment"}
              ]
            }
          |]
      case fromJSON json of
        Success (acc :: Account) ->
          length (accountTransactions acc) `shouldBe` 3
        Error err -> expectationFailure $ "Failed to parse: " ++ err

    it "fails on missing required account fields" $ do
      let json = BL.fromStrict "{\"name\": \"Test\", \"balance\": \"100\"}"
      let result = eitherDecode json :: Either String Account
      result `shouldSatisfy` isLeft

  describe "AccountsResponse JSON parsing" $ do
    it "parses response with multiple accounts" $ do
      let json = [aesonQQ|
            {
              "accounts": [
                {
                  "name": "Account 1",
                  "balance": "100.00",
                  "balance-date": 1609459200,
                  "transactions": []
                },
                {
                  "name": "Account 2",
                  "balance": "200.00",
                  "balance-date": 1609459200,
                  "transactions": []
                }
              ]
            }
          |]
      case fromJSON json of
        Success (resp :: AccountsResponse) ->
          length (accounts resp) `shouldBe` 2
        Error err -> expectationFailure $ "Failed to parse: " ++ err

    it "parses response with empty accounts array" $ do
      let json = [aesonQQ|{"accounts": []}|]
      case fromJSON json of
        Success (resp :: AccountsResponse) ->
          accounts resp `shouldBe` []
        Error err -> expectationFailure $ "Failed to parse: " ++ err

    it "fails on missing accounts field" $ do
      let json = BL.fromStrict "{}"
      let result = eitherDecode json :: Either String AccountsResponse
      result `shouldSatisfy` isLeft

  describe "Property-based tests" $ do
    it "Transaction round-trips through JSON" $ property $
      \posted amount desc ->
        let tx = Transaction posted amount desc
            encoded = encode tx
            decoded = decode encoded
         in decoded `shouldBe` Just tx

    it "Account name is preserved through JSON encoding" $ property $
      \name balance date ->
        let acc = Account name balance date []
            encoded = encode acc
            decoded = decode encoded :: Maybe Account
         in fmap accountName decoded `shouldBe` Just name

-- Helper functions

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- QuickCheck instances

instance Arbitrary Scientific where
  arbitrary = fromFloatDigits <$> (arbitrary :: Gen Double)

instance Arbitrary Transaction where
  arbitrary = Transaction
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Account where
  arbitrary = Account
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
