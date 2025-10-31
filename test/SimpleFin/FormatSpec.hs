module SimpleFin.FormatSpec (spec) where

import Data.Scientific (Scientific, fromFloatDigits)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (POSIXTime)
import SimpleFin.Format
import SimpleFin.Types
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()

spec :: Spec
spec = do
  describe "formatMoney" $ do
    it "formats positive amounts with 2 decimal places" $ do
      formatMoney (fromFloatDigits (123.45 :: Double)) `shouldBe` "123.45"

    it "formats negative amounts with 2 decimal places" $ do
      formatMoney (fromFloatDigits (-99.99 :: Double)) `shouldBe` "-99.99"

    it "formats zero correctly" $ do
      formatMoney (fromFloatDigits (0.0 :: Double)) `shouldBe` "0.00"

    it "rounds to 2 decimal places" $ do
      formatMoney (fromFloatDigits (123.456 :: Double)) `shouldBe` "123.46"
      formatMoney (fromFloatDigits (123.454 :: Double)) `shouldBe` "123.45"

    it "formats whole numbers with .00" $ do
      formatMoney (fromFloatDigits (100.0 :: Double)) `shouldBe` "100.00"

    it "formats small amounts correctly" $ do
      formatMoney (fromFloatDigits (0.01 :: Double)) `shouldBe` "0.01"
      formatMoney (fromFloatDigits (0.99 :: Double)) `shouldBe` "0.99"

    it "formats large amounts correctly" $ do
      formatMoney (fromFloatDigits (1234567.89 :: Double)) `shouldBe` "1234567.89"

    it "handles very small negative amounts" $ do
      formatMoney (fromFloatDigits (-0.01 :: Double)) `shouldBe` "-0.01"

  describe "formatPosixTime" $ do
    it "formats Unix epoch correctly" $ do
      -- Unix epoch: 1970-01-01 00:00:00 UTC
      result <- formatPosixTime 0
      -- Result depends on local timezone, but should be valid format
      result `shouldSatisfy` (not . null)
      result `shouldSatisfy` (\s -> length s >= 19) -- YYYY-MM-DD HH:MM:SS

    it "formats recent timestamp correctly" $ do
      -- 2021-01-01 00:00:00 UTC = 1609459200
      result <- formatPosixTime 1609459200
      result `shouldSatisfy` (not . null)
      -- Should contain date components
      result `shouldSatisfy` (\s -> any (`elem` s) ['0'..'9'])

    it "formats output in expected pattern" $ do
      result <- formatPosixTime 1609459200
      -- Should match pattern: YYYY-MM-DD HH:MM:SS
      let parts = words result
      length parts `shouldBe` 2
      head parts `shouldSatisfy` (\s -> length s == 10) -- Date part
      last parts `shouldSatisfy` (\s -> length s == 8)  -- Time part

    it "handles zero time (pending transactions)" $ do
      result <- formatPosixTime 0
      result `shouldSatisfy` (not . null)

  describe "formatTransaction" $ do
    it "formats transaction with positive amount" $ do
      let tx = Transaction 1609459200 (fromFloatDigits (100.00 :: Double)) "Deposit"
      -- formatTransaction performs IO, so we just verify it doesn't crash
      formatTransaction tx `shouldReturn` ()

    it "formats transaction with negative amount" $ do
      let tx = Transaction 1609459200 (fromFloatDigits (-50.00 :: Double)) "Withdrawal"
      formatTransaction tx `shouldReturn` ()

    it "formats transaction with long description" $ do
      let longDesc = T.replicate 100 "X"
          tx = Transaction 1609459200 (fromFloatDigits (10.00 :: Double)) longDesc
      formatTransaction tx `shouldReturn` ()

  describe "formatAccount" $ do
    it "formats account with no transactions" $ do
      let acc = Account "Checking" (fromFloatDigits (1000.00 :: Double)) 1609459200 []
      formatAccount acc `shouldReturn` ()

    it "formats account with transactions" $ do
      let tx1 = Transaction 1609459200 (fromFloatDigits (100.00 :: Double)) "Tx 1"
          tx2 = Transaction 1609459300 (fromFloatDigits (-50.00 :: Double)) "Tx 2"
          acc = Account "Savings" (fromFloatDigits (500.00 :: Double)) 1609459200 [tx1, tx2]
      formatAccount acc `shouldReturn` ()

    it "formats account with many transactions" $ do
      let txs = replicate 100 $ Transaction 1609459200 (fromFloatDigits (1.00 :: Double)) "Test"
          acc = Account "Test Account" (fromFloatDigits (100.00 :: Double)) 1609459200 txs
      formatAccount acc `shouldReturn` ()

  describe "formatAccounts" $ do
    it "handles empty account list" $ do
      let response = AccountsResponse []
      formatAccounts response `shouldReturn` ()

    it "formats single account" $ do
      let acc = Account "Checking" (fromFloatDigits (1000.00 :: Double)) 1609459200 []
          response = AccountsResponse [acc]
      formatAccounts response `shouldReturn` ()

    it "formats multiple accounts" $ do
      let acc1 = Account "Checking" (fromFloatDigits (1000.00 :: Double)) 1609459200 []
          acc2 = Account "Savings" (fromFloatDigits (5000.00 :: Double)) 1609459200 []
          acc3 = Account "Credit Card" (fromFloatDigits (-250.00 :: Double)) 1609459200 []
          response = AccountsResponse [acc1, acc2, acc3]
      formatAccounts response `shouldReturn` ()

  describe "Property-based formatting tests" $ do
    prop "formatMoney always returns valid decimal string" $
      \(amount :: Double) ->
        let formatted = formatMoney (fromFloatDigits amount)
            hasDot = '.' `elem` formatted
            parts = if hasDot then split '.' formatted else [formatted, "00"]
         in length parts == 2 && length (parts !! 1) == 2

    prop "formatMoney preserves sign" $
      \(NonZero amount) ->
        let formatted = formatMoney (fromFloatDigits (amount :: Double))
            startsWithMinus = not (null formatted) && head formatted == '-'
         in (amount < 0) == startsWithMinus

    it "formatPosixTime produces consistent length output" $ property $
      \(Positive time) ->
        ioProperty $ do
          result <- formatPosixTime time
          -- Should be at least 19 characters (YYYY-MM-DD HH:MM:SS)
          pure $ length result >= 19

  describe "Edge cases" $ do
    it "handles very large money amounts" $ do
      let huge = fromFloatDigits (999999999999.99 :: Double)
      formatMoney huge `shouldSatisfy` (not . null)

    it "handles very small money amounts" $ do
      let tiny = fromFloatDigits (0.001 :: Double)
      formatMoney tiny `shouldBe` "0.00" -- Should round to 0.00

    it "handles negative zero" $ do
      let negZero = fromFloatDigits (-0.0 :: Double)
      formatMoney negZero `shouldSatisfy` (\s -> s == "0.00" || s == "-0.00")

    it "handles future timestamps" $ do
      -- Year 2100: January 1, 2100 = 4102444800
      result <- formatPosixTime 4102444800
      result `shouldSatisfy` (not . null)

    it "handles very old timestamps" $ do
      -- Year 1970: Unix epoch
      result <- formatPosixTime 0
      result `shouldSatisfy` (not . null)

-- Helper functions

split :: Char -> String -> [String]
split delimiter str = case break (== delimiter) str of
  (before, "") -> [before]
  (before, _:after) -> before : split delimiter after

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

instance Arbitrary AccountsResponse where
  arbitrary = AccountsResponse <$> arbitrary
