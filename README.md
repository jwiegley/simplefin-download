# SimpleFin Download - Haskell Implementation

A modern Haskell implementation of the SimpleFin API client for retrieving account and transaction data.

## Features

- Type-safe API client with newtype wrappers for security
- Comprehensive error handling with custom error types
- Support for environment variables (`SIMPLEFIN_SETUP_TOKEN`, `SIMPLEFIN_ACCESS_URL`)
- Secure password input (no echo to terminal)
- HTTP Basic Authentication with timeout support
- JSON parsing with Aeson
- Local timezone-aware timestamp formatting
- Full feature parity with the Python version

## Prerequisites

- GHC 9.8 or later
- Cabal or Stack
- (Optionally) Nix for reproducible builds

## Building

### With Nix

```bash
nix-shell
cabal build
```

### With Cabal (standalone)

```bash
cabal update
cabal build
```

### With Stack

```bash
stack build
```

## Usage

### Interactive Mode

```bash
simplefin-download
# You'll be prompted for your setup token
```

### Using Environment Variables

```bash
# First time: claim access URL
export SIMPLEFIN_SETUP_TOKEN="your_base64_token_here"
simplefin-download

# Subsequent runs: use saved access URL
export SIMPLEFIN_ACCESS_URL="https://username:password@host/path"
simplefin-download
```

## Environment Variables

- `SIMPLEFIN_SETUP_TOKEN`: Base64-encoded setup token (for initial claim)
- `SIMPLEFIN_ACCESS_URL`: Pre-claimed access URL (skip claiming step)

## Output Format

```
Decoding setup token...
Claiming access URL...
Access URL claimed successfully

Tip: Save this access URL to environment variable SIMPLEFIN_ACCESS_URL
     to skip the claiming step in future runs.

Fetching account data...

2025-01-15 09:30:00     1234.56 Checking Account
--------------------------------------------------------------------------------
2025-01-15 09:00:00      -45.00 Coffee Shop
2025-01-14 15:30:00     2000.00 Paycheck
```

## Architecture

### Type Safety

The implementation uses newtype wrappers for type safety:

- `SetupToken`: Base64-encoded setup token
- `ClaimUrl`: URL for claiming access credentials
- `AccessUrl`: Access URL with embedded credentials
- `Username`, `Password`: Authentication credentials
- `BaseUrl`: Base API URL without credentials

### Error Handling

Custom error type `SimplefinError` covers all failure modes:

- Base64 decoding errors
- HTTP errors (with timeout, connection failures)
- JSON parsing errors
- URL parsing errors
- Invalid credentials
- Missing environment variables
- Empty responses

### JSON Parsing

Custom `FromJSON` instances handle the SimpleFin API schema:

- `Account`: Account name, balance, balance date, transactions
- `Transaction`: Posted time, amount, description
- `AccountsResponse`: Container for accounts array

### HTTP Client

Uses the `req` library for type-safe HTTP requests:

- POST for claiming access URL
- GET with Basic Auth for fetching accounts
- 30-second timeout on all requests
- Comprehensive error handling

## Development

### Running in Development

```bash
nix-shell
cabal run simplefin-download
```

### Code Structure

- `Main.hs`: Complete implementation
  - Type definitions (newtypes, domain types, errors)
  - JSON instances
  - Core functions (decode, claim, parse, fetch)
  - Formatting and display
  - Main program logic with ExceptT

## Comparison with Python Version

This Haskell implementation provides:

1. **Compile-time type safety**: Catches errors before runtime
2. **Explicit error handling**: All error paths are type-checked
3. **Pure functions**: Business logic separated from effects
4. **Referential transparency**: Easier to reason about and test
5. **Pattern matching**: Exhaustive case handling enforced by compiler

## License

MIT

## Author

John Wiegley <johnw@newartisans.com>
