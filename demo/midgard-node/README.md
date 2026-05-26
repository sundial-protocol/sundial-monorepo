# Midgard Node

Server application with GET and POST endpoints for interacting with Midgard.

## How to Run

### With Docker

Using Docker, you can run Midgard node on `localhost:3000` (or another port)
quite easily.

0. If you don't have Docker yet or want to update, follow this [GUIDE](https://docs.docker.com/engine/install/). After installation, do not forget to execute also the [POST-INSTALLATION STEPS](https://docs.docker.com/engine/install/linux-postinstall/#manage-docker-as-a-non-root-user) to avoid using sudo with Docker.

1. Run Docker daemon if it's not running already:

   ```sh
   sudo dockerd
   ```

2. Pack the `midgard-sdk` tarball (see [here](../midgard-sdk/README.md)).

3. Prepare your `.env` file. You can use `.env.example` as your starting point:

   ```sh
   cd ../midgard-node
   cp .env.example .env
   ```

   1. When running with `L1_PROVIDER=Kupmios`, you do not need to fill out
      `L1_BLOCKFROST_API_URL` and `L1_BLOCKFROST_KEY`. The remaining fields need
      to be filled out.

4. Install all the dependencies:

   ```sh
   pnpm install --frozen-lockfile
   ```

   1. If the install fails with an incorrect SHA, that most likely means
      `midgard-sdk` was updated recently, but `pnpm-lock.yaml` still expects the
      old hash. Update the SHA value inside the `pnpm-lock.yaml` file with the
      new one.
   2. Rerun `pnpm install --frozen-lockfile`. Now it should install correctly.

5. Build the midgard-node:

   ```sh
   pnpm build
   ```

6. Run the application stack:

   ```sh
   docker compose up -d

   # or this for development:
   docker compose -f docker-compose.dev.yaml up -d
   ```

Midgard node should be running on port `PORT` (from your `.env`).

You can view logs of the `node` service with `docker`:

```sh
docker compose logs -f node
```

If you made any changes to `midgard-node` and had an image running, restart it
with the 3 steps:

```sh
docker compose down -v
docker compose up -d --build
```

### Without Docker (No Monitoring)

For running the node itself, a running PostgreSQL server is also needed. The
fields you most likely want to modify in your `.env` file are:

```sh
POSTGRES_USER=postgres
POSTGRES_PASSWORD=postgres
POSTGRES_DB=midgard
POSTGRES_HOST=localhost
LEDGER_MPT_DB_PATH=midgard-ledger-mpt-db
MEMPOOL_MPT_DB_PATH=midgard-mempool-mpt-db
TX_QUEUE_CAPACITY=10000
TX_QUEUE_MAX_PENDING=20000
TX_QUEUE_DRAIN_BATCH_SIZE=250
TX_QUEUE_OFFER_TIMEOUT_MS=100
TX_QUEUE_CLAIM_IDLE_MS=30000
TX_QUEUE_CLAIM_BATCH_SIZE=100
TX_QUEUE_MAX_DELIVERY_ATTEMPTS=5
TX_QUEUE_DEAD_LETTER_STREAM=midgard:tx-submissions:dead-letter
NODE_ROLE=all
REDIS_URL=redis://localhost:6379
REDIS_STREAM_KEY=midgard:tx-submissions
REDIS_STREAM_CONSUMER_GROUP=midgard-tx-processors
REDIS_STREAM_CONSUMER_NAME=midgard-node-1
REDIS_STREAM_BLOCK_MS=1000
COMMITMENT_WORKER_TIMEOUT_MS=300000
SUBMIT_SIGNED_TX_TIMEOUT_MS=30000
COMMITMENT_WINDOW_WARN_TX_REQUESTS=50000
COMMITMENT_WINDOW_WARN_TOTAL_EVENTS=60000
COMMITMENT_WINDOW_WARN_TOTAL_BYTES=20000000
```

Role notes:

- `NODE_ROLE=api`: run HTTP API ingress only (`POST /submit` enqueues into Redis Streams).
- `NODE_ROLE=tx-processor`: run Redis consumer-group tx workers only.
- `NODE_ROLE=sequencer`: run block commitment/submission/merge/user-event sync only.
- `NODE_ROLE=all`: run all roles in one process (default).

With a properly setup database, the following set of commands should start the
most up to date `midgard-node`:

```sh
# Optional
nix develop

# Bundle the SDK
cd ../midgard-sdk
pnpm install
pnpm repack

# Go back to `midgard-node` and force reinstallation of the SDK (faster than
# `pnpm install --force`)
cd ../midgard-node
rm -rf node_module
pnpm install
pnpm listen
```

## Testing

### With Docker

```sh
docker compose run --rm node-tests
```

### Without Docker

```sh
cd midgard-node
pnpm test
```

## HTTP Operational Endpoints

Reset and state-queue diagnostics endpoints are useful before scalability or
replay runs.

- `GET /reset`
  - Starts a reset operation.
  - If a reset is already in progress, the node returns `409 Conflict`:
    `{"error":"Reset already in progress"}`.
  - This prevents concurrent resets from racing each other.
- `GET /stateQueue/root-unit-diagnostics`
  - Returns current root-unit health for the state queue.
  - Response shape:
    - `status`: `ok` when exactly one root unit exists, otherwise `invalid`.
    - `resetInProgress`: current in-memory reset lock state.
    - `stateQueueAddress`: address queried.
    - `rootUnit`: unit queried (`policyId + NODE_ASSET_NAME`).
    - `count`: number of matching UTxOs found.
    - `outRefs`: matching outrefs (`txHash#outputIndex`).
  - On provider/query failure, returns `503 Service Unavailable` with
    `status: "error"`.
- `GET /stateQueue/repair-root-units`
  - Runs a targeted repair that burns duplicate state-queue root units when
    `count > 1`.
  - This endpoint is intended for recovery when full `/reset` is too slow for
    deep historical state.
  - If another reset/repair is already in progress, returns `409 Conflict`:
    `{"error":"Reset already in progress"}`.
  - Successful repair sets root-unit count to `0`; run `GET /init` once after
    repair to mint a fresh single root unit.

Quick checks:

```sh
curl -i http://localhost:3000/reset
curl -s http://localhost:3000/stateQueue/root-unit-diagnostics | jq .
curl -i http://localhost:3000/stateQueue/repair-root-units
```

## Operational Metrics

When the node runs with monitoring enabled, Prometheus metrics include block
commitment and cold-start seeding signals.

Important commitment seeding metrics:

- `blocks_db_seed_attempts_total`
- `blocks_db_seed_success_total`
- `blocks_db_seed_failures_total`
- `blocks_db_seed_duration_seconds`
- `blocks_db_seed_traversal_hops_last`

Important tx ingress metrics:

- `tx_stream_depth`
- `tx_stream_pending`
- `tx_stream_consumer_lag`
- `tx_stream_ack_total`
- `tx_stream_fail_total`
- `tx_stream_retry_total`
- `tx_stream_dead_letter_total`

Important block submission reliability metrics:

- `submit_block_failures_total`
- `submit_block_sign_timeouts_total`
- `submit_block_submit_timeouts_total`
- `submit_block_sign_duration_seconds_sum`
- `submit_block_sign_duration_seconds_count`
- `submit_block_submit_duration_seconds_sum`
- `submit_block_submit_duration_seconds_count`
