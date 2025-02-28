# Midgard Transaction Generator

Generate test transactions for Midgard L2.

## Features

- Transaction types:
  - `one-to-one`: Simple transfers (1→1)
  - `multi-output`: Complex transactions (1→many)
  - `mixed`: Combination with configurable ratio
- CLI interface for easy usage
- Programmatic API for integration
- Real-time status monitoring
- Test wallet generation

## Usage

You can use the transaction generator in two ways:

### 1. Command Line Interface (CLI)

From the project root:

```bash
# Show available commands
pnpm tx-generator --help

# Show detailed options for start command
pnpm tx-generator start --help

# Start with test wallet (quickest way to begin)
pnpm tx-generator start --test-wallet

# Start with custom configuration
pnpm tx-generator start \
  --endpoint http://localhost:3000 \
  --type mixed \
  --ratio 80 \
  --batch-size 20 \
  --interval 10 \
  --concurrency 3 \
  --private-key your-private-key

# Monitor status
pnpm tx-generator status
```

#### Available Options

| Option                       | Description                                        | Default                    |
| ---------------------------- | -------------------------------------------------- | -------------------------- |
| `-e, --endpoint <url>`       | Node endpoint URL                                  | http://localhost:3000      |
| `-t, --type <type>`          | Transaction type (one-to-one, multi-output, mixed) | mixed                      |
| `-r, --ratio <number>`       | % of one-to-one txs in mixed mode                  | 70                         |
| `-b, --batch-size <number>`  | Transactions per batch                             | 10                         |
| `-i, --interval <seconds>`   | Time between batches                               | 5                          |
| `-c, --concurrency <number>` | Parallel transaction batches                       | 5                          |
| `--test-wallet`              | Generate a test wallet                             | false                      |
| `-k, --private-key <key>`    | Wallet private key                                 | required if no test wallet |

### 2. Programmatic API

Import and use in your TypeScript/JavaScript code:

```typescript
import { startGenerator, stopGenerator, getGeneratorStatus } from '@midgard-manager/tx-generator';

// Start generator
await startGenerator({
  nodeEndpoint: 'http://localhost:3000',
  walletPrivateKey: 'your-private-key',
  transactionType: 'mixed',
  oneToOneRatio: 70,
  batchSize: 10,
  interval: 5000, // milliseconds
  concurrency: 5,
});

// Check status
const status = getGeneratorStatus();

// Stop generator
await stopGenerator();
```

## Transaction Types

### One-to-One Transactions

Simple transactions with one input and one output.

### Multi-Output Transactions

Complex transactions with one input and multiple outputs.

### Mixed Transactions

A combination of one-to-one and multi-output transactions with a configurable ratio.

## Development

```bash
# Install dependencies
pnpm install

# Build the package
pnpm build

# Run tests
pnpm test
```

## Project Structure

```
midgard-tx-generator/
├── src/
│   ├── bin/           # CLI implementation
│   ├── lib/
│   │   ├── client/    # Node communication
│   │   ├── generators/# TX generators
│   │   └── scheduler/ # Periodic generation
│   └── utils/         # Utility functions
```

## Environment Variables

You can configure the generator using environment variables:

```bash
# Node settings
MIDGARD_NODE_URL=http://localhost:3000  # Your node URL
CARDANO_NETWORK=Preview                 # Network
TX_INTERVAL_MS=5000                     # Time between cycles

# Transaction settings
ONE_TO_ONE_TXS=10                      # Number of simple txs per cycle
COMPLEX_TXS=10                         # Number of complex tx pairs per cycle
```

## Output Format

Each generated transaction follows this structure:

```typescript
{
  cborHex: string; // CBOR-encoded transaction
  description: string;
  txId: string; // Transaction hash
  type: string;
}
```

## Telemetry

The generator provides basic telemetry:

- Success/failure rates
- Submission latency
- Total transactions
- Batch processing times

## Troubleshooting

1. **Command not found**

   ```bash
   # Make sure you've built the package
   pnpm build
   ```

2. **Permission denied**

   ```bash
   # Make the CLI executable
   chmod +x ./dist/bin/index.js
   ```

3. **Node connection failed**
   - Check if the node is running
   - Verify the endpoint URL
   - Check network connectivity
