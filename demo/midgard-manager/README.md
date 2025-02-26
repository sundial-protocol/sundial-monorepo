# Midgard Manager

Tools for managing Midgard MVP L2 node and generating test transactions.

## Structure

- **CLI** (`packages/cli`): Command-line interface for Midgard nodes
- **TX Generator** (`packages/tx-generator`): Transaction generator for testing

## Configuration

Midgard Manager uses a centralized configuration system with a single source of truth:

1. **Central Configuration**: Located at `config/settings.json`
2. **Command-line Arguments**: Can override specific settings per command
3. **Wallet Storage**: Located at `config/wallets/default.json`

The configuration can be modified either by directly editing the `config/settings.json` file or through the interactive CLI commands, which will update the file automatically.

### Configuration Structure

```json
{
  "node": {
    "endpoint": "http://localhost:3000"
  },
  "generator": {
    "enabled": true,
    "maxConcurrent": 10,
    "batchSize": 100,
    "intervalMs": 1000,
    "transactionType": "mixed",
    "oneToOneRatio": 70,
    "defaultWallet": "test"
  },
  "logging": {
    "level": "info",
    "format": "pretty"
  },
  "wallets": {
    "directory": "./config/wallets"
  }
}
```

### Configuration Fields

- **node**: Node connection settings

  - `endpoint`: The Midgard node endpoint URL

- **generator**: Transaction generator settings

  - `enabled`: Whether the generator is currently running
  - `maxConcurrent`: Maximum number of concurrent transactions
  - `batchSize`: Number of transactions per batch
  - `intervalMs`: Interval between batches in milliseconds
  - `transactionType`: Type of transactions to generate ("one-to-one", "multi-output", or "mixed")
  - `oneToOneRatio`: For mixed type, percentage of one-to-one transactions (0-100)
  - `defaultWallet`: Default wallet to use for signing transactions

- **logging**: Logging configuration

  - `level`: Log level (debug, info, warn, error)
  - `format`: Log format (pretty, json)

- **wallets**: Wallet management settings
  - `directory`: Directory to store wallet information

## Quick Start

```bash
# Install dependencies
cd demo/midgard-manager
pnpm install

# Build all packages
pnpm build

# Start the CLI in interactive mode
pnpm start interactive
```

## Available Tools

### 1. Interactive CLI

The main CLI provides a complete suite of tools for managing Midgard nodes:

```bash
# Start interactive mode
pnpm start interactive

# Show available commands
pnpm start --help
```

### 2. Transaction Generator

Generate test transactions in various patterns:

```bash
# Using the CLI
pnpm start generate-tx --interactive  # Interactively configure and start generator
pnpm start tx-status                 # Check generator status
pnpm start stop-tx                   # Stop the generator

# Direct generator usage
pnpm tx-generator --help
```

### 3. Wallet Management

Manage wallets for transaction signing:

```bash
pnpm start wallet add <NAME> --private-key <key>
pnpm start wallet list
pnpm start wallet details <NAME>
pnpm start wallet remove <NAME>
```

### 4. Node Operations

Configure and monitor the Midgard node:

```bash
pnpm start node-status
pnpm start configure-node --interactive
```

## Development

To add new features or modify existing ones:

1. Make changes in the relevant package
2. Run `pnpm build` to rebuild all packages
3. Test your changes using the CLI

For more detailed development information, see the README in each package directory.
