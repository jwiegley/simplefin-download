# SimpleFin to Ledger Integration

## Overview

The SimpleFin Download application now supports outputting bank account and transaction data in [Ledger](https://www.ledger-cli.org/) journal format, using the `trade-journal` library for type-safe Ledger data structures and rendering.

## Usage

### Command-Line Interface

```bash
# Default format (human-readable)
simplefin-download

# Ledger journal format
simplefin-download --ledger
```

### Environment Variables

Works with the same environment variables as before:
```bash
# Using setup token
export SIMPLEFIN_SETUP_TOKEN="base64_token"
simplefin-download --ledger

# Using saved access URL
export SIMPLEFIN_ACCESS_URL="https://user:pass@host/path"
simplefin-download --ledger
```

## Output Format

### Opening Balances

Non-zero account balances generate opening balance transactions:

```ledger
2021-01-01 * () "Opening Balance"
    Assets:Bank:Checking         $1000.00
    Equity:Opening Balances      -$1000.00
```

### Regular Transactions

SimpleFin transactions are converted to balanced double-entry Ledger transactions:

#### Expense (Negative Amount)
```ledger
2021-01-02 * () "Coffee Shop"
    Expenses:Uncategorized          $45.00
    Assets:Bank:Checking           -$45.00
```

#### Income (Positive Amount)
```ledger
2021-01-03 * () "Salary Deposit"
    Assets:Bank:Checking          $2000.00
    Income:Uncategorized          -$2000.00
```

## Implementation Details

### Module Structure

**`src/SimpleFin/Ledger.hs`**
- Conversion functions from SimpleFin types to Ledger types
- Integration with trade-journal library

### Type Conversions

| SimpleFin Type | Ledger Type | Notes |
|---------------|-------------|-------|
| `Scientific` | `Amount 2` | Dollar amounts with cents precision |
| `POSIXTime` | `Day` | Date-only for transactions |
| Account name | `Cash "name"` | Maps to Assets:Bank hierarchy |
| Transaction | `Transaction` | Creates balanced postings |

### Account Hierarchy

SimpleFin accounts are mapped to standard Ledger account hierarchy:

- Bank accounts → `Assets:Bank:AccountName`
- Expenses → `Expenses:Uncategorized`
- Income → `Income:Uncategorized`
- Opening balances → `Equity:Opening Balances`

### Double-Entry Accounting

Every SimpleFin transaction generates two Ledger postings that balance:

1. **Bank Account Posting**: Reflects the change in bank balance
2. **Category Posting**: Balancing entry to Expenses or Income

Example for a $45 expense:
- Debit: `Expenses:Uncategorized +$45.00`
- Credit: `Assets:Bank:Checking -$45.00`

## Dependencies

### Required Libraries

- **trade-journal** (local): Provides Ledger data types and rendering
- **simple-amount >= 0.2.0**: Phantom-typed numeric amounts
- **lens >= 5.0**: For data manipulation
- **containers >= 0.6**: For Map data structures

### Build Configuration

The project includes a `cabal.project` file that references the local trade-journal:

```cabal
packages: .
          ../trade-journal
```

On macOS with Homebrew, the `cabal.project.local` includes:

```cabal
package trade-journal
  extra-lib-dirs: /opt/homebrew/lib
  extra-include-dirs: /opt/homebrew/include
```

This is required for the `mpfr` library dependency used by `simple-amount`.

## Testing

The test suite includes comprehensive Ledger format tests:

### Test Coverage

- **Unit Tests**: Conversion functions (23 tests)
  - `scientificToAmount`: Numeric precision
  - `posixTimeToDay`: Date conversion
  - `categorizeLedgerAccount`: Expense/Income categorization
  - `accountToLedger`: Account hierarchy
  - `transactionToLedger`: Transaction conversion

- **Integration Tests**: Full Ledger output
  - Opening balance generation
  - Transaction formatting
  - Balanced double-entry validation
  - Empty account handling
  - Multiple account support

### Running Tests

```bash
# Run all tests
cabal test

# Run with detailed output
cabal test --test-show-details=direct

# Run only Ledger tests
cabal test --test-options="--match Ledger"
```

## Example Workflow

### Input: SimpleFin API Response

```json
{
  "accounts": [
    {
      "name": "Checking Account",
      "balance": "1500.45",
      "balance-date": 1640995200,
      "transactions": [
        {
          "posted": 1641081600,
          "amount": "-45.50",
          "description": "Coffee Shop"
        },
        {
          "posted": 1641168000,
          "amount": "2000.00",
          "description": "Paycheck"
        }
      ]
    }
  ]
}
```

### Output: Ledger Journal Format

```ledger
2022-01-01 * () "Opening Balance"
    Assets:Bank:Checking Account    $1500.45
    Equity:Opening Balances         -$1500.45

2022-01-02 * () "Coffee Shop"
    Expenses:Uncategorized            $45.50
    Assets:Bank:Checking Account     -$45.50

2022-01-03 * () "Paycheck"
    Assets:Bank:Checking Account    $2000.00
    Income:Uncategorized            -$2000.00
```

## Advanced Features

### Trade-Journal Capabilities

The integration uses trade-journal's powerful Ledger rendering system, which supports:

- **Commodity Lots**: For investment tracking (future enhancement)
- **Virtual Postings**: Parenthesized postings that don't affect balance
- **Metadata**: Transaction and posting-level metadata
- **Multi-currency**: Different currency support (USD default)

### Future Enhancements

Potential improvements leveraging trade-journal:

1. **Investment Accounts**: Use CommodityLot for stock/crypto holdings
2. **Category Mapping**: Intelligent transaction categorization
3. **Tax Tracking**: Capital gains/losses for investment accounts
4. **Wash Sale Rules**: For investment transactions
5. **Custom Metadata**: Transaction tags and notes

## Troubleshooting

### Build Issues

If you encounter linking errors with `mpfr`:

1. Install mpfr via Homebrew:
   ```bash
   brew install mpfr gmp
   ```

2. Add library paths to `cabal.project.local`:
   ```cabal
   package trade-journal
     extra-lib-dirs: /opt/homebrew/lib
     extra-include-dirs: /opt/homebrew/include
   ```

3. Clean and rebuild:
   ```bash
   cabal clean
   cabal build
   ```

### Output Validation

To validate the Ledger output:

```bash
# Save output to file
simplefin-download --ledger > accounts.ledger

# Validate with ledger CLI
ledger -f accounts.ledger balance
ledger -f accounts.ledger register
```

## Architecture

### Data Flow

```
SimpleFin API
    ↓
SimpleFin Types (Account, Transaction)
    ↓
SimpleFin.Ledger (conversion layer)
    ↓
Trade-Journal Types (Posting, Transaction)
    ↓
Ledger.Render (formatting)
    ↓
Ledger Journal Text Output
```

### Key Design Decisions

1. **Type Safety**: Uses phantom types for numeric precision
2. **Pure Functions**: All conversions are pure, testable functions
3. **Balanced Entries**: Enforces double-entry accounting
4. **Extensible**: Can easily add more sophisticated categorization

## Contributing

To extend the Ledger integration:

1. **Add New Account Types**: Modify `accountToLedger` in `SimpleFin.Ledger`
2. **Enhance Categorization**: Update `categorizeLedgerAccount` logic
3. **Add Metadata**: Use trade-journal's metadata support
4. **Custom Rendering**: Extend `renderLedger` function

## License

The integration maintains compatibility with both SimpleFin Download (MIT) and trade-journal (MIT) licenses.