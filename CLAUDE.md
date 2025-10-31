# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

SimpleFin Download is a Haskell CLI application for the SimpleFin API. It's a single-file executable (Main.hs, 393 lines) that retrieves bank account and transaction data using type-safe HTTP requests and comprehensive error handling.

**Key characteristics:**
- Pure Haskell implementation with GHC 9.8+
- Single executable with no library components
- Type safety via newtypes (SetupToken, ClaimUrl, AccessUrl, etc.)
- ExceptT monad transformer for error propagation
- Uses `req` library for HTTP, `aeson` for JSON

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

**Current status:** No tests implemented yet. Main.hs is not structured as a library.

**Future test commands (when tests exist):**
```bash
cabal test
cabal test --enable-coverage
cabal test --test-show-details=direct
```

See `test/README.md` for planned test architecture. To enable testing, Main.hs needs to be refactored into library modules under `src/SimpleFin/`.

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

Main.hs is organized in clear sections (see module documentation in lines 6-28):

1. **Type Safety Layer** (lines 64-88): Newtype wrappers prevent mixing credentials
2. **Domain Types** (lines 90-113): Account, Transaction, AccountsResponse
3. **Error Types** (lines 115-130): SimplefinError with 8 error variants
4. **JSON Instances** (lines 132-151): FromJSON for API schema
5. **Core Functions** (lines 153-236): decode → claim → parse → fetch pipeline
6. **Formatting** (lines 237-274): Display logic with timezone conversion
7. **Utilities** (lines 276-306): Password input, URL parsing, error conversion
8. **Main Program** (lines 308-392): ExceptT-based control flow

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

- **Main logic**: Main.hs:311-363 (`runSimplefin` function)
- **HTTP requests**: Main.hs:172-187 (claim), Main.hs:227-235 (fetch)
- **Error conversion**: Main.hs:296-306 (`httpExceptionToError`)
- **JSON parsing**: Main.hs:134-151 (FromJSON instances)
- **URL parsing**: Main.hs:192-213 (credentials), Main.hs:289-293 (req URLs)

## Notes for Future Development

**Refactoring for testability:** To add tests, extract functions into library modules:
- `src/SimpleFin/Types.hs`: All data types and newtypes
- `src/SimpleFin/API.hs`: HTTP functions (claimAccessUrl, fetchAccounts)
- `src/SimpleFin/Auth.hs`: Token/URL handling (decode, parse)
- `src/SimpleFin/Format.hs`: Display functions
- `app/Main.hs`: Minimal CLI entry point

**Security considerations:**
- Never log or display Password or AccessUrl values
- Use `getPasswordInput` (lines 277-286) for any sensitive user input
- The `hSetEcho stdin False` pattern prevents terminal echo

**Performance:** The `-threaded -rtsopts -with-rtsopts=-N` flags enable parallel GC and automatic thread allocation. This is overkill for a CLI tool but harmless.
