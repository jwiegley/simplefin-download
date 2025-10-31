{- |
Module      : Main
Description : SimpleFin API Client - Main Entry Point
Copyright   : (c) John Wiegley, 2025
License     : MIT
Maintainer  : johnw@newartisans.com

Command-line interface for the SimpleFin API client.

Workflow:
    1. Obtain a setup token (base64-encoded claim URL)
    2. Claim an access URL by POSTing to the claim URL
    3. Parse the access URL to extract credentials
    4. Fetch account data from the /accounts endpoint
    5. Display formatted account balances and transactions

Environment Variables:
    SIMPLEFIN_SETUP_TOKEN: Base64-encoded setup token (skips interactive prompt)
    SIMPLEFIN_ACCESS_URL: Pre-claimed access URL (skips claiming step)

Usage:
    simplefin-download [--ledger]

Options:
    --ledger    Output in Ledger journal format instead of default format
-}
module Main (main) where

import Control.Exception (catch)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Options.Applicative
  ( Parser
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , progDesc
  , switch
  , (<**>)
  )
import SimpleFin.API (claimAccessUrl, fetchAccounts)
import SimpleFin.Auth (decodeSetupToken, parseAccessUrl)
import SimpleFin.Format (formatAccounts, getPasswordInput)
import SimpleFin.Ledger (renderLedger)
import SimpleFin.Types
  ( AccessUrl (..)
  , SetupToken (..)
  , SimplefinError (..)
  , showError
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)

-- | Command-line options
data Options = Options
  { optLedger :: Bool
  -- ^ Output in Ledger format
  }

-- | Command-line parser
optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch
      ( long "ledger"
          <> help "Output in Ledger journal format"
      )

-- | Main application logic using ExceptT for error handling
runSimplefin :: Options -> ExceptT SimplefinError IO ()
runSimplefin opts = do
  -- Check if access URL is already available
  maybeAccessUrl <- liftIO $ lookupEnv "SIMPLEFIN_ACCESS_URL"

  accessUrl <- case maybeAccessUrl of
    Just url -> do
      liftIO $ putStrLn "Using access URL from environment variable"
      pure $ AccessUrl (T.pack url)
    Nothing -> do
      -- Get setup token
      maybeSetupToken <- liftIO $ lookupEnv "SIMPLEFIN_SETUP_TOKEN"

      setupToken <- case maybeSetupToken of
        Just token -> pure $ SetupToken (T.pack token)
        Nothing -> do
          token <- liftIO getPasswordInput
          when (T.null token) $
            throwError $ MissingEnvironmentVariable "Setup token is required"
          pure $ SetupToken token

      -- Decode setup token and claim access URL
      liftIO $ putStrLn "Decoding setup token..."
      claimUrl <- case decodeSetupToken setupToken of
        Left err -> throwError err
        Right url -> pure url

      liftIO $ putStrLn "Claiming access URL..."
      result <- liftIO $ claimAccessUrl claimUrl
      case result of
        Left err -> throwError err
        Right url -> do
          liftIO $ putStrLn "Access URL claimed successfully"
          liftIO $ putStrLn ""
          liftIO $ putStrLn "Tip: Save this access URL to environment variable SIMPLEFIN_ACCESS_URL"
          liftIO $ putStrLn "     to skip the claiming step in future runs."
          liftIO $ putStrLn ""
          pure url

  -- Parse access URL
  (username, password, baseUrl) <- case parseAccessUrl accessUrl of
    Left err -> throwError err
    Right creds -> pure creds

  -- Fetch account data
  liftIO $ putStrLn "Fetching account data..."
  result <- liftIO $ fetchAccounts baseUrl username password
  accountData <- case result of
    Left err -> throwError err
    Right data_ -> pure data_

  -- Format and display based on options
  liftIO $
    if optLedger opts
      then renderLedger accountData
      else formatAccounts accountData

-- | Main entry point with error handling
main :: IO ()
main = do
  opts <-
    execParser $
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "SimpleFin API client for retrieving account and transaction data"
            <> header "simplefin-download - SimpleFin API Client"
        )

  result <- runExceptT (runSimplefin opts)
  case result of
    Left err -> do
      hPutStrLn stderr $ "Error: " <> showError err
      exitWith (ExitFailure 1)
    Right () -> exitSuccess
  `catch` \(_ :: ExitCode) -> exitSuccess -- Proper exit codes
  `catch` \(_ :: IOError) -> do
    -- Catch Ctrl-C and other interrupts
    hPutStrLn stderr "\n\nInterrupted by user"
    exitWith (ExitFailure 130)
