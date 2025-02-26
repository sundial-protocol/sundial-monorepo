# Midgard Manager

Tools for managing Midgard MVP L2 node and generating test transactions.

## Structure

- **CLI** (`packages/cli`): Command-line interface for Midgard nodes
- **TX Generator** (`packages/tx-generator`): Transaction generator for testing

## Quick Start

```bash
# Install dependencies
cd demo/midgard-manager
pnpm install

# Build all packages
pnpm build
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

A dedicated tool for generating test transactions. You can use it in two ways:

```bash
# 1. Using the dedicated tx-generator command
pnpm tx-generator --help                    # Show all commands
pnpm tx-generator start --test-wallet       # Start with test wallet
pnpm tx-generator status                    # Check generator status

# 2. Using the main CLI (alternative)
pnpm start tx generate --interactive        # Interactive mode
pnpm start tx status                       # Check status
```

#### Transaction Generator Options

```bash
# Start with custom configuration
pnpm tx-generator start \
  --type mixed \                # one-to-one, multi-output, or mixed
  --ratio 80 \                 # % of one-to-one transactions
  --batch-size 20 \            # transactions per batch
  --interval 10 \              # seconds between batches
  --concurrency 3 \            # parallel batches
  --endpoint http://localhost:3000 \
  --test-wallet               # or use --private-key

# Check current status
pnpm tx-generator status
```

For detailed documentation:
- Transaction Generator: [tx-generator README](packages/tx-generator/README.md)
- Interactive CLI: [cli README](packages/cli/README.md)

