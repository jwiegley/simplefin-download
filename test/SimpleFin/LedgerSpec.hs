{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : SimpleFin.LedgerSpec
Description : Tests for SimpleFin to Ledger conversion
Copyright   : (c) John Wiegley, 2025
License     : MIT

Test suite for the SimpleFin.Ledger module, verifying correct conversion
of SimpleFin data structures to Ledger journal format.
-}
module SimpleFin.LedgerSpec (spec) where

import Amount (Amount)
import Control.Lens ((^.))
import Data.Aeson (decode)
import Data.Aeson.QQ (aesonQQ)
import Data.ByteString.Lazy qualified as BL
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock.POSIX (POSIXTime)
import Ledger
  ( Account (..)
  , PostingAmount (..)
  , actualDate
  , amount
  , account
  , payee
  , postings
  )
import SimpleFin.Ledger
  ( accountToLedger
  , categorizeLedgerAccount
  , posixTimeToDay
  , renderLedgerToText
  , scientificToAmount
  , transactionToLedger
  )
import SimpleFin.Types qualified as SF
import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  , shouldContain
  , shouldSatisfy
  )

spec :: Spec
spec = do
  describe "posixTimeToDay" $ do
    it "converts POSIXTime to Day correctly" $ do
      let posixTime = 1609459200 :: POSIXTime -- 2021-01-01 00:00:00 UTC
          expectedDay = fromGregorian 2021 1 1
      posixTimeToDay posixTime `shouldBe` expectedDay

    it "handles different dates correctly" $ do
      let posixTime = 1640995200 :: POSIXTime -- 2022-01-01 00:00:00 UTC
          expectedDay = fromGregorian 2022 1 1
      posixTimeToDay posixTime `shouldBe` expectedDay

  describe "scientificToAmount" $ do
    it "converts positive amounts correctly" $ do
      let sci = 123.45 :: Scientific
          expected = fromRational 123.45 :: Amount 2
      scientificToAmount sci `shouldBe` expected

    it "converts negative amounts correctly" $ do
      let sci = -45.00 :: Scientific
          expected = fromRational (-45.00) :: Amount 2
      scientificToAmount sci `shouldBe` expected

    it "handles zero correctly" $ do
      let sci = 0 :: Scientific
          expected = fromRational 0 :: Amount 2
      scientificToAmount sci `shouldBe` expected

    it "preserves decimal precision" $ do
      let sci = 99.99 :: Scientific
          expected = fromRational 99.99 :: Amount 2
      scientificToAmount sci `shouldBe` expected

  describe "categorizeLedgerAccount" $ do
    it "categorizes negative amounts as Expenses" $ do
      let account = categorizeLedgerAccount (-45.00)
      account `shouldBe` Other "Expenses:Uncategorized"

    it "categorizes positive amounts as Income" $ do
      let account = categorizeLedgerAccount 100.00
      account `shouldBe` Other "Income:Uncategorized"

    it "categorizes zero as Income" $ do
      let account = categorizeLedgerAccount 0
      account `shouldBe` Other "Income:Uncategorized"

  describe "accountToLedger" $ do
    it "creates Cash account from account name" $ do
      let account = accountToLedger "Checking"
      account `shouldBe` Cash "Checking"

    it "preserves account names" $ do
      let account = accountToLedger "Savings Account"
      account `shouldBe` Cash "Savings Account"

  describe "transactionToLedger" $ do
    it "converts expense transaction correctly" $ do
      let tx = SF.Transaction
            { SF.transactionPosted = 1609459200 -- 2021-01-01
            , SF.transactionAmount = -45.00
            , SF.transactionDescription = "Coffee Shop"
            }
          ledgerTx = transactionToLedger "Checking" tx

      -- Check payee
      ledgerTx ^. payee `shouldBe` "Coffee Shop"

      -- Check date
      ledgerTx ^. actualDate `shouldBe` fromGregorian 2021 1 1

      -- Check postings count
      length (ledgerTx ^. postings) `shouldBe` 2

      -- Check posting amounts are balanced (sum to zero)
      let amounts = map (\p -> case p ^. amount of
                                 DollarAmount amt -> amt
                                 _ -> 0) (ledgerTx ^. postings)
      sum amounts `shouldBe` 0

    it "converts income transaction correctly" $ do
      let tx = SF.Transaction
            { SF.transactionPosted = 1609459200
            , SF.transactionAmount = 1000.00
            , SF.transactionDescription = "Salary Deposit"
            }
          ledgerTx = transactionToLedger "Checking" tx

      ledgerTx ^. payee `shouldBe` "Salary Deposit"
      length (ledgerTx ^. postings) `shouldBe` 2

    it "uses correct account categorization" $ do
      let tx = SF.Transaction
            { SF.transactionPosted = 1609459200
            , SF.transactionAmount = -25.50
            , SF.transactionDescription = "Grocery Store"
            }
          ledgerTx = transactionToLedger "Savings" tx
          accounts = map (\p -> p ^. account) (ledgerTx ^. postings)

      -- Should have both expense and bank account
      accounts `shouldContain` [Other "Expenses:Uncategorized"]
      accounts `shouldContain` [Cash "Savings"]

  describe "renderLedgerToText" $ do
    it "renders empty account list" $ do
      let response = SF.AccountsResponse []
          output = renderLedgerToText response
      output `shouldBe` ""

    it "renders account with opening balance" $ do
      let account = SF.Account
            { SF.accountName = "Checking"
            , SF.accountBalance = 1000.00
            , SF.accountBalanceDate = 1609459200
            , SF.accountTransactions = []
            }
          response = SF.AccountsResponse [account]
          output = renderLedgerToText response

      -- Should contain opening balance transaction
      output `shouldSatisfy` T.isInfixOf "Opening Balance"
      output `shouldSatisfy` T.isInfixOf "2021-01-01"
      output `shouldSatisfy` T.isInfixOf "Checking"

    it "renders account with transactions" $ do
      let tx1 = SF.Transaction
            { SF.transactionPosted = 1609459200
            , SF.transactionAmount = -45.00
            , SF.transactionDescription = "Coffee Shop"
            }
          account = SF.Account
            { SF.accountName = "Checking"
            , SF.accountBalance = 0
            , SF.accountBalanceDate = 1609459200
            , SF.accountTransactions = [tx1]
            }
          response = SF.AccountsResponse [account]
          output = renderLedgerToText response

      -- Should contain transaction description
      output `shouldSatisfy` T.isInfixOf "Coffee Shop"
      output `shouldSatisfy` T.isInfixOf "Expenses:Uncategorized"
      output `shouldSatisfy` T.isInfixOf "Cash"

    it "renders multiple transactions" $ do
      let tx1 = SF.Transaction
            { SF.transactionPosted = 1609459200
            , SF.transactionAmount = -45.00
            , SF.transactionDescription = "Coffee"
            }
          tx2 = SF.Transaction
            { SF.transactionPosted = 1609545600 -- 2021-01-02
            , SF.transactionAmount = 1000.00
            , SF.transactionDescription = "Salary"
            }
          account = SF.Account
            { SF.accountName = "Checking"
            , SF.accountBalance = 0
            , SF.accountBalanceDate = 1609459200
            , SF.accountTransactions = [tx1, tx2]
            }
          response = SF.AccountsResponse [account]
          output = renderLedgerToText response

      output `shouldSatisfy` T.isInfixOf "Coffee"
      output `shouldSatisfy` T.isInfixOf "Salary"
      output `shouldSatisfy` T.isInfixOf "Income:Uncategorized"

    it "renders multiple accounts" $ do
      let account1 = SF.Account
            { SF.accountName = "Checking"
            , SF.accountBalance = 500.00
            , SF.accountBalanceDate = 1609459200
            , SF.accountTransactions = []
            }
          account2 = SF.Account
            { SF.accountName = "Savings"
            , SF.accountBalance = 2000.00
            , SF.accountBalanceDate = 1609459200
            , SF.accountTransactions = []
            }
          response = SF.AccountsResponse [account1, account2]
          output = renderLedgerToText response

      -- Should contain both account names
      output `shouldSatisfy` T.isInfixOf "Checking"
      output `shouldSatisfy` T.isInfixOf "Savings"

  describe "Ledger format validation" $ do
    it "produces valid Ledger transaction format" $ do
      let tx = SF.Transaction
            { SF.transactionPosted = 1609459200
            , SF.transactionAmount = -100.00
            , SF.transactionDescription = "Test Purchase"
            }
          account = SF.Account
            { SF.accountName = "Test Account"
            , SF.accountBalance = 0
            , SF.accountBalanceDate = 1609459200
            , SF.accountTransactions = [tx]
            }
          response = SF.AccountsResponse [account]
          output = renderLedgerToText response

      -- Basic format checks
      output `shouldSatisfy` T.isInfixOf "2021-01-01" -- Date
      output `shouldSatisfy` T.isInfixOf "*" -- Status marker
      output `shouldSatisfy` T.isInfixOf "Test Purchase" -- Payee
      output `shouldSatisfy` T.isInfixOf "$" -- Currency symbol

    it "balances debits and credits" $ do
      let tx = SF.Transaction
            { SF.transactionPosted = 1609459200
            , SF.transactionAmount = -75.25
            , SF.transactionDescription = "Balanced Transaction"
            }
          ledgerTx = transactionToLedger "Account" tx

      -- Extract amounts and verify they sum to zero
      let amounts = map (\p -> case p ^. amount of
                                 DollarAmount amt -> amt
                                 _ -> 0) (ledgerTx ^. postings)
      sum amounts `shouldBe` 0
