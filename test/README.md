# SimpleFin Download Test Strategy

This document outlines the testing approach for the SimpleFin API client.

## Test Categories

### 1. Pure Function Tests (Unit Tests)

These functions can be tested in isolation without network or I/O:

- **Base64 decoding**: Test `decodeSetupToken` with valid and invalid tokens
  - Valid base64 strings
  - Invalid base64 strings
  - Empty strings
  - Edge cases

- **URL parsing**: Test `parseAccessUrl` with various URL formats
  - Valid URLs with credentials
  - URLs with missing credentials
  - Malformed URLs
  - URLs with special characters in credentials
  - URLs with different ports

- **Error conversion**: Test `httpExceptionToError` mapping
  - Different HTTP exception types
  - Timeout scenarios
  - Connection failures

### 2. Property-Based Tests (QuickCheck)

QuickCheck generators for:

- **URL round-tripping**: Generate valid access URLs, parse them, verify structure
- **Base64 encoding/decoding**: Verify decode . encode = id
- **Error types**: Ensure all error constructors are covered

### 3. Integration Tests (Requires Test Environment)

These require either:
- Mock HTTP server (using `wai`/`warp`)
- Test fixtures with recorded HTTP responses
- Actual SimpleFin test account (not recommended for CI)

Test scenarios:
- Full workflow: token claim → access URL → fetch accounts
- HTTP error handling (404, 500, timeout)
- JSON parsing edge cases
- Authentication failures

### 4. End-to-End Tests (Manual)

Manual testing with actual SimpleFin API:
- Setup token claiming
- Access URL reuse
- Environment variable support
- Error message clarity
- Output formatting

## Current Test Implementation

Currently, tests are not implemented because:

1. **Main.hs is not a library**: Functions are not exported for testing
2. **Network-dependent**: Most functions require HTTP requests
3. **No mock infrastructure**: Would need http-mock or similar

## Recommended Test Structure

To enable proper testing, restructure the project:

```
simplefin-download/
├── src/
│   ├── SimpleFin/
│   │   ├── Types.hs       # Data types
│   │   ├── API.hs         # HTTP client functions
│   │   ├── Auth.hs        # Token/URL handling
│   │   └── Format.hs      # Output formatting
├── app/
│   └── Main.hs             # CLI entry point
├── test/
│   ├── Spec.hs             # Test runner
│   ├── SimpleFin/
│   │   ├── TypesSpec.hs
│   │   ├── AuthSpec.hs
│   │   └── FormatSpec.hs
```

##  Property Examples

```haskell
-- Base64 round-trip
prop_base64_roundtrip :: Text -> Property
prop_base64_roundtrip text =
  let encoded = Base64.encodeBase64 (encodeUtf8 text)
      decoded = decodeSetupToken (SetupToken encoded)
  in decoded === Right (ClaimUrl text)

-- URL parsing
prop_url_parsing :: Property
prop_url_parsing = forAll genValidAccessUrl $ \url ->
  case parseAccessUrl url of
    Left err -> counterexample (show err) False
    Right (user, pass, base) ->
      not (T.null (unUsername user)) &&
      not (T.null (unPassword pass)) &&
      not (T.null (unBaseUrl base))
```

## Running Tests

Once tests are implemented:

```bash
# Run all tests
cabal test

# Run with coverage
cabal test --enable-coverage

# Run specific test suite
cabal test simplefin-download-test
```

## Future Work

1. Extract pure functions into testable modules
2. Add QuickCheck generators for all data types
3. Create mock HTTP server for integration tests
4. Add Golden tests for output formatting
5. Property tests for JSON round-tripping
6. Benchmark performance of URL parsing
