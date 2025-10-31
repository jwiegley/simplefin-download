{-# LANGUAGE DataKinds #-}

{- |
Module      : SimpleFin.Ledger
Description : Convert SimpleFin data to Ledger format
Copyright   : (c) John Wiegley, 2025
License     : MIT
Maintainer  : johnw@newartisans.com

Converts SimpleFin account and transaction data to Ledger journal format
using the trade-journal library.

The conversion follows these conventions:
  * SimpleFin accounts → Assets:Bank:AccountName
  * Negative amounts are expenses (withdrawals)
  * Positive amounts are income (deposits)
  * Each transaction creates two postings (account + balancing)
  * Initial balance transactions are created for non-zero balances
-}
module SimpleFin.Ledger
  ( -- * Rendering Functions
    renderLedger
  , renderLedgerToText

    -- * Conversion Functions
  , accountToLedger
  , transactionToLedger
  , scientificToAmount
  , posixTimeToDay

    -- * Account Categorization
  , categorizeLedgerAccount
  ) where

import Amount (Amount)
import Data.Map qualified as M
import Data.Scientific (Scientific)
import Data.Scientific qualified as Sci
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Calendar (Day)
import Data.Time.Clock (utctDay)
import GHC.TypeLits (KnownNat)
import Ledger
  ( Account (..)
  , Posting (..)
  , PostingAmount (..)
  , Transaction (..)
  , newPosting
  )
import Ledger.Render (renderTransaction)
import SimpleFin.Types qualified as SF

-- | Convert POSIXTime to Day for Ledger transactions
posixTimeToDay :: POSIXTime -> Day
posixTimeToDay = utctDay . posixSecondsToUTCTime

-- | Convert Scientific to Amount 2 (dollars with cents precision)
--
-- SimpleFin uses Scientific for amounts, Ledger uses Amount with type-level precision.
scientificToAmount :: Scientific -> Amount 2
scientificToAmount sci = fromRational (toRational sci)

-- | Categorize transactions into appropriate Ledger accounts
--
-- Heuristic rules:
--   * Negative amounts → Expenses:Uncategorized
--   * Positive amounts → Income:Uncategorized
--
-- This provides a basic categorization that users can refine manually.
categorizeLedgerAccount :: Scientific -> Account
categorizeLedgerAccount amount
  | amount < 0 = Other "Expenses:Uncategorized"
  | otherwise = Other "Income:Uncategorized"

-- | Convert SimpleFin Account name to Ledger Account hierarchy
--
-- Creates account paths like: Assets:Bank:AccountName
accountToLedger :: Text -> Account
accountToLedger accountName = Cash accountName

-- | Convert SimpleFin Transaction to Ledger Transaction
--
-- Creates a two-posting transaction:
--   1. The bank account (debit/credit)
--   2. The balancing account (expense/income)
--
-- Example:
--   SimpleFin: -$45.00 "Coffee Shop"
--   Ledger:
--     Expenses:Uncategorized         $45.00
--     Assets:Bank:Checking          -$45.00
transactionToLedger
  :: Text
  -- ^ Account name (e.g., "Checking")
  -> SF.Transaction
  -- ^ SimpleFin transaction
  -> Transaction () 2
transactionToLedger accountName SF.Transaction{..} =
  Transaction
    { _actualDate = posixTimeToDay transactionPosted
    , _effectiveDate = Nothing
    , _code = ""
    , _payee = transactionDescription
    , _postings =
        [ -- Balancing posting (Expense/Income)
          newPosting
            (categorizeLedgerAccount transactionAmount)
            False -- not virtual
            (DollarAmount $ scientificToAmount $ negate transactionAmount)
        , -- Bank account posting
          newPosting
            (accountToLedger accountName)
            False -- not virtual
            (DollarAmount $ scientificToAmount transactionAmount)
        ]
    , _xactMetadata = M.empty
    , _provenance = ()
    }

-- | Create an opening balance transaction
--
-- Used to set the initial balance for an account when it's non-zero.
createOpeningBalance
  :: Text
  -- ^ Account name
  -> POSIXTime
  -- ^ Balance date
  -> Scientific
  -- ^ Balance amount
  -> Transaction () 2
createOpeningBalance accountName balanceDate balance =
  Transaction
    { _actualDate = posixTimeToDay balanceDate
    , _effectiveDate = Nothing
    , _code = ""
    , _payee = "Opening Balance"
    , _postings =
        [ newPosting
            (accountToLedger accountName)
            False
            (DollarAmount $ scientificToAmount balance)
        , newPosting
            OpeningBalances
            False
            (DollarAmount $ scientificToAmount $ negate balance)
        ]
    , _xactMetadata = M.empty
    , _provenance = ()
    }

-- | Render SimpleFin AccountsResponse as Ledger journal
--
-- For each account:
--   1. Create opening balance transaction (if balance != 0)
--   2. Convert all transactions to Ledger format
--   3. Render using trade-journal's renderTransaction
renderLedger :: SF.AccountsResponse -> IO ()
renderLedger (SF.AccountsResponse accounts) = do
  TIO.putStrLn $ renderLedgerToText (SF.AccountsResponse accounts)

-- | Render SimpleFin AccountsResponse as Ledger text
--
-- Pure version that returns Text instead of printing.
renderLedgerToText :: SF.AccountsResponse -> Text
renderLedgerToText (SF.AccountsResponse accounts) =
  T.unlines $ concatMap convertAccount accounts
  where
    convertAccount :: SF.Account -> [Text]
    convertAccount SF.Account{..} =
      let accountNameShort = accountName

          -- Create opening balance if non-zero
          openingBalanceTxs =
            if accountBalance /= 0
              then renderTransaction
                     accountNameShort
                     (createOpeningBalance accountNameShort accountBalanceDate accountBalance)
                     ++ [""]
              else []

          -- Convert each SimpleFin transaction to Ledger transaction
          ledgerTxs =
            concatMap
              ( \tx ->
                  renderTransaction
                    accountNameShort
                    (transactionToLedger accountNameShort tx)
                    ++ [""]
              )
              accountTransactions
       in openingBalanceTxs ++ ledgerTxs
