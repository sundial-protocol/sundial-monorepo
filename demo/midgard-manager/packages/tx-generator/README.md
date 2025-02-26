# Midgard Transaction Generator

Generate test transactions for Midgard L2.

## Features

- Transaction types:
  - `one-to-one`: Simple transfers (1→1)
  - `multi-output`: Complex transactions (1→many)
  - `mixed`: Combination with configurable ratio

## Usage

```typescript
import {
  startGenerator,
  stopGenerator,
  getGeneratorStatus,
} from "@midgard-manager/tx-generator";

// Start generator
await startGenerator({
  nodeEndpoint: "http://localhost:8545",
  walletPrivateKey: "your-private-key",
  transactionType: "mixed",
  oneToOneRatio: 70,
  batchSize: 10,
  interval: 5000,
  concurrency: 5,
});

// Check status
const status = getGeneratorStatus();

// Stop generator
await stopGenerator();
```

## Options

| Option            | Description                              | Default                 |
| ----------------- | ---------------------------------------- | ----------------------- |
| `nodeEndpoint`    | Node URL                                 | `http://localhost:3000` |
| `transactionType` | `one-to-one`, `multi-output`, or `mixed` | `mixed`                 |
| `oneToOneRatio`   | % of one-to-one txs (0-100)              | 70                      |
| `batchSize`       | Transactions per batch                   | 10                      |
| `interval`        | Time between batches (ms)                | 5000                    |
| `concurrency`     | Parallel transaction batches             | 5                       |

## Install & Build

```bash
pnpm install
pnpm build
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
# Build
pnpm build
```

## Project Structure

```
midgard-tx-generator/
├── src/
│   ├── bin/
│   │   ├── index.ts     # Main
│   │   └── cli.ts       # CLI interface
│   ├── lib/
│   │   ├── client/      # Node communication
│   │   ├── generators/  # TX generators
│   │   └── scheduler/   # Periodic generation
│   └── utils/
```

## Quick Start

```bash
cd offchain/midgard-tx-generator
pnpm install
pnpm build
pnpm start
```

## Configuration

Configure by creating a `.env` file (copy from `.env.example`):

```bash
# Node settings
MIDGARD_NODE_URL=http://localhost:3000  # Your node URL
CARDANO_NETWORK=Preview                 # Network
TX_INTERVAL_MS=5000                     # Time between cycles

# Transaction settings
ONE_TO_ONE_TXS=10                       # Number of simple txs per cycle (default: 10)
COMPLEX_TXS=10                          # Number of complex tx pairs per cycle (default: 10)
```

You can also set these as environment variables when running the command:

```bash
ONE_TO_ONE_TXS=5 COMPLEX_TXS=3 pnpm start
```

## Default Behavior

With default settings, each cycle generates:

- 10 simple one-to-one transactions
- 20 complex transactions (from 10 COMPLEX_TXS):
  - 10 distribution transactions (1→20)
  - 10 collection transactions (20→1)
- Total: 30 transactions per cycle

## Node Interaction

1. If node is available:

   - Transactions are submitted directly
   - Stats are shown in console

2. If node is unavailable:
   - Transactions are saved to `generated-transactions/`
   - Files are named with timestamp and type

## Output Format

Each transaction follows this structure:

```typescript
{
  cborHex: string; // CBOR-encoded transaction
  description: string;
  txId: string; // Transaction hash
  type: string;
}
```

## Basic Telemetry

- Success/failure rates
- Submission latency
- Total transactions
