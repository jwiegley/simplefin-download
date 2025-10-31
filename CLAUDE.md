# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

SimpleFin Download is a Haskell CLI application for the SimpleFin API. It retrieves bank account and transaction data using type-safe HTTP requests and comprehensive error handling.

**Key characteristics:**
- Pure Haskell implementation with GHC 9.8+
- Library structure with 4 modules under `src/SimpleFin/`
- CLI entry point in `app/Main.hs`
- Type safety via newtypes (SetupToken, ClaimUrl, AccessUrl, etc.)
- ExceptT monad transformer for error propagation
- Uses `req` library for HTTP, `aeson` for JSON
- Comprehensive test suite with 86 tests (95.3% pass rate)

## Build Commands

### Development Workflow

```bash
# Enter Nix development environment (recommended)
nix-shell

# Build the project
cabal build

# Run the executable
cabal run simplefin-download

# Build with specific GHC
cabal build --with-compiler=ghc-9.8

# Clean build artifacts
cabal clean
```

### Testing

The project has a comprehensive test suite with **86 tests** covering all library modules.

```bash
# Run all tests
cabal test

# Run tests with detailed output
cabal test --test-show-details=direct

# Run tests with coverage reporting
cabal test --enable-coverage

# Run specific test module
cabal test --test-options="--match 'SimpleFin.Auth'"

# Run tests matching a pattern
cabal test --test-options="--match 'JSON parsing'"

# Run a single test
cabal test --test-options="--match '/decodeSetupToken/decodes valid/'"
```

**Test Structure:**
- `test/SimpleFin/TypesSpec.hs` - 40 tests for JSON parsing, error handling
- `test/SimpleFin/AuthSpec.hs` - 24 tests for token decoding, URL parsing
- `test/SimpleFin/APISpec.hs` - 22 tests for HTTP client functions
- `test/SimpleFin/FormatSpec.hs` - 20 tests for formatting functions

**Testing Technologies:**
- **hspec**: BDD-style test framework
- **QuickCheck**: Property-based testing (100 cases per property)
- **aeson-qq**: JSON quasiquoters for test data
- **quickcheck-instances**: Arbitrary instances for common types

See `TEST_SUITE.md` for detailed testing documentation and `REFACTORING_SUMMARY.md` for the refactoring process.

### Running with Environment Variables

```bash
# First time: claim access URL
export SIMPLEFIN_SETUP_TOKEN="your_base64_token"
cabal run simplefin-download

# Subsequent runs: reuse access URL
export SIMPLEFIN_ACCESS_URL="https://user:pass@host/path"
cabal run simplefin-download
```

## Code Architecture

### High-Level Structure

The codebase is organized into a library and executable:

**Library Modules** (`src/SimpleFin/`):
1. **Types.hs**: All data types, newtypes, error types, and JSON instances
   - Newtype wrappers: SetupToken, ClaimUrl, AccessUrl, Username, Password, BaseUrl
   - Domain types: Account, Transaction, AccountsResponse
   - SimplefinError with 8 error variants
   - FromJSON instances for API schema
2. **Auth.hs**: Authentication and token handling
   - `decodeSetupToken`: Base64 → ClaimUrl
   - `parseAccessUrl`: AccessUrl → (Username, Password, BaseUrl)
3. **API.hs**: HTTP client functions
   - `claimAccessUrl`: ClaimUrl → AccessUrl (HTTP POST)
   - `fetchAccounts`: BaseUrl + credentials → AccountsResponse (HTTP GET)
   - `parseUrlForReq`: URL parsing for req library
4. **Format.hs**: Display and formatting functions
   - `formatAccounts`, `formatAccount`, `formatTransaction`
   - `formatMoney`: Scientific → currency string
   - `formatPosixTime`: POSIX → local timezone string

**Executable** (`app/Main.hs`):
- CLI entry point with `runSimplefin` (main application logic)
- Environment variable handling
- User input (secure password prompt)
- Error display with `showError`

**Legacy File** (`Main.hs`):
- Original monolithic implementation (393 lines)
- Kept for reference but superseded by library structure

### Workflow Pipeline

```
SetupToken (env or prompt)
    ↓ decodeSetupToken
ClaimUrl
    ↓ claimAccessUrl (HTTP POST)
AccessUrl
    ↓ parseAccessUrl
(Username, Password, BaseUrl)
    ↓ fetchAccounts (HTTP GET + Basic Auth)
AccountsResponse
    ↓ formatAccounts
Console output
```

### Key Design Patterns

**Newtype Safety:** Each credential/URL type is distinct. The compiler prevents passing a `SetupToken` where an `AccessUrl` is expected. This is intentional - do not bypass these type wrappers.

**Error Handling:** All functions that can fail return `Either SimplefinError a` or use `ExceptT SimplefinError IO a`. Never use partial functions like `head` or `fromJust`. The `httpExceptionToError` function (lines 295-306) converts HTTP exceptions to domain errors.

**Pure Core, Effectful Shell:** Functions like `decodeSetupToken` (lines 159-166) and `parseAccessUrl` (lines 192-213) are pure. IO is isolated to `claimAccessUrl`, `fetchAccounts`, `formatPosixTime`, and `main`.

**ExceptT Pattern:** The main logic in `runSimplefin` (lines 311-363) uses `ExceptT SimplefinError IO ()` to short-circuit on errors. Use `throwError` for failures, `liftIO` for IO actions.

## Language Extensions

The following extensions are enabled by default (package.yaml:41-50):

- **OverloadedStrings**: String literals work as Text/ByteString
- **DeriveGeneric**: Automatic Generic derivation for JSON
- **LambdaCase**: Use `\case` for concise pattern matching
- **ImportQualifiedPost**: Modern qualified import syntax (e.g., `import Data.Text qualified as T`)
- **RecordWildCards**: Pattern match records with `Account {..}`
- **TypeApplications**: Explicit type application with `@`
- **ScopedTypeVariables**: Type variables in nested scopes

When adding code, follow these conventions consistently.

## Dependencies and Cabal

This project uses Hpack (package.yaml) to generate the .cabal file. **Do not edit simplefin-download.cabal directly** - always edit package.yaml and let hpack regenerate the .cabal file.

**Key dependencies:**
- `req >= 3.13`: Type-safe HTTP client (preferred over http-conduit)
- `aeson >= 2.2`: JSON parsing with FromJSON instances
- `modern-uri >= 0.3`: Modern URI parsing for `req` library
- `network-uri >= 2.6`: Legacy URI parsing for credential extraction
- `base64-bytestring >= 1.2`: Base64 decoding for setup tokens

**HTTP client choice:** The codebase uses `req` instead of `http-conduit` or `wreq` for its type-safe URL handling and built-in timeout support. Keep this choice consistent.

## Common Patterns

### Adding a New API Endpoint

1. Define response type with `FromJSON` instance
2. Create a function returning `IO (Either SimplefinError YourType)`
3. Use `runExceptT` and `ExceptT` pattern like `fetchAccounts` (lines 216-235)
4. Parse URL with `parseUrlForReq`
5. Wrap HTTP request in `try` to catch `Req.HttpException`
6. Convert exceptions with `httpExceptionToError`

### Adding Error Cases

Add new constructors to `SimplefinError` (lines 117-128) and handle them in `showError` (lines 381-392). Ensure error messages are user-friendly.

### Time Formatting

Use `formatPosixTime` (lines 238-242) to convert POSIX timestamps to local timezone strings. It returns `IO String` because timezone conversion requires IO.

## File References

**Library Modules:**
- **Type definitions**: src/SimpleFin/Types.hs (all data types and errors)
- **JSON parsing**: src/SimpleFin/Types.hs (FromJSON instances)
- **Token decoding**: src/SimpleFin/Auth.hs:decodeSetupToken
- **URL parsing**: src/SimpleFin/Auth.hs:parseAccessUrl
- **HTTP requests**: src/SimpleFin/API.hs (claimAccessUrl, fetchAccounts)
- **Error conversion**: src/SimpleFin/API.hs:httpExceptionToError
- **Formatting**: src/SimpleFin/Format.hs (all display functions)

**Executable:**
- **Main logic**: app/Main.hs (runSimplefin function)
- **User input**: app/Main.hs:getPasswordInput
- **Error display**: app/Main.hs:showError

**Tests:**
- **Type tests**: test/SimpleFin/TypesSpec.hs
- **Auth tests**: test/SimpleFin/AuthSpec.hs
- **API tests**: test/SimpleFin/APISpec.hs
- **Format tests**: test/SimpleFin/FormatSpec.hs

## Notes for Future Development

**Testing best practices:**
- Run tests before committing changes: `cabal test`
- Add property-based tests for new pure functions
- Use `aeson-qq` for readable JSON test data
- Mock HTTP responses for API tests (see APISpec.hs for patterns)
- All test modules use hspec-discover for automatic registration

**Adding new functionality:**
1. Add types to `src/SimpleFin/Types.hs`
2. Implement logic in appropriate module (Auth/API/Format)
3. Export new functions from the module
4. Add tests in corresponding `test/SimpleFin/*Spec.hs`
5. Update app/Main.hs to use new functionality

**Security considerations:**
- Never log or display Password or AccessUrl values
- Use `getPasswordInput` (lines 277-286) for any sensitive user input
- The `hSetEcho stdin False` pattern prevents terminal echo

**Performance:** The `-threaded -rtsopts -with-rtsopts=-N` flags enable parallel GC and automatic thread allocation. This is overkill for a CLI tool but harmless.
