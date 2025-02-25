# Midgard Transaction Generator

A transaction generator to submit test transactions to MVP Midgard node over HTTP and simulate user activity.

## What it does

Generates two types of transactions in alternating cycles:

1. Simple one-to-one transactions (1 input → 1 output)
2. Complex multi-output transactions:
   - Distribution phase: splits into multiple outputs (1 → 20)
   - Collection phase: merges outputs back (20 → 1)

Each complex transaction creates 2 actual transactions (distribution + collection)

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
