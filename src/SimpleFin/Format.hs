{- |
Module      : SimpleFin.Format
Description : Output formatting functions
Copyright   : (c) John Wiegley, 2025
License     : MIT
Maintainer  : johnw@newartisans.com

Functions for formatting and displaying account and transaction data.
-}
module SimpleFin.Format
  ( formatAccounts
  , formatAccount
  , formatTransaction
  , formatMoney
  , formatPosixTime
  , getPasswordInput
  ) where

import Data.Scientific (Scientific)
import Data.Scientific qualified as Sci
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (utcToLocalZonedTime)
import SimpleFin.Types (Account (..), AccountsResponse (..), Transaction (..))
import System.IO (BufferMode (..), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import Text.Printf (printf)

-- | Convert POSIXTime to formatted local time string
--
-- Returns time in format: "YYYY-MM-DD HH:MM:SS"
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
--
-- Displays account header with balance and transaction list.
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
--
-- Displays transaction date, amount, and description.
formatTransaction :: Transaction -> IO ()
formatTransaction Transaction {..} = do
  postedStr <- formatPosixTime transactionPosted
  let amountFormatted = formatMoney transactionAmount
  putStrLn $ postedStr ++ " " ++ printf "%12s" amountFormatted ++ " " ++ T.unpack transactionDescription

-- | Format a Scientific number as money (2 decimal places)
--
-- >>> formatMoney 1234.567
-- "1234.57"
formatMoney :: Scientific -> String
formatMoney sci =
  printf "%.2f" (Sci.toRealFloat sci :: Double)

-- | Get password input without echoing to terminal
--
-- Used for securely reading the setup token from stdin.
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
