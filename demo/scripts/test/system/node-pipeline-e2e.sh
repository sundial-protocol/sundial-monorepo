#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel)"
NODE_DIR="$ROOT_DIR/demo/midgard-node"
CORPUS_PATH="$ROOT_DIR/demo/midgard-manager/packages/scalability-harness/corpora/corpus-v1-net-preview-t-one-to-one-r-na-n-1500000.jsonl"

if [[ ! -f "$CORPUS_PATH" ]]; then
  echo "ERROR: corpus file not found at: $CORPUS_PATH" >&2
  exit 1
fi

BASE_ENV_FILE="$NODE_DIR/.env"
if [[ ! -f "$BASE_ENV_FILE" ]]; then
  BASE_ENV_FILE="$NODE_DIR/.env.example"
fi

RUNTIME_ENV_FILE="$(mktemp /tmp/midgard-node-pipeline-e2e-env-XXXXXX)"
cp "$BASE_ENV_FILE" "$RUNTIME_ENV_FILE"

upsert_env() {
  local key="$1"
  local value="$2"
  if grep -q "^${key}=" "$RUNTIME_ENV_FILE"; then
    sed -i "s|^${key}=.*|${key}=${value}|" "$RUNTIME_ENV_FILE"
  else
    echo "${key}=${value}" >> "$RUNTIME_ENV_FILE"
  fi
}

E2E_COMPOSE_PROJECT="sundial-test"
E2E_NODE_API_PORT=3010
E2E_NODE_PROM_PORT=9465
E2E_POSTGRES_PORT=5434
E2E_REDIS_PORT=6380

upsert_env NODE_ROLE all
upsert_env L1_PROVIDER Blockfrost
upsert_env L1_BLOCKFROST_API_URL "http://127.0.0.1:1"
upsert_env L1_BLOCKFROST_KEY "e2e-dummy-key"
upsert_env L1_OGMIOS_KEY "http://127.0.0.1:1"
upsert_env L1_KUPO_KEY "http://127.0.0.1:1"
# Use test-only seed phrases so the node can derive wallet addresses
upsert_env L1_OPERATOR_SEED_PHRASE "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art"
upsert_env L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art"
upsert_env L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art"
upsert_env REDIS_STREAM_KEY "midgard:tx-submissions:pipeline-e2e"
upsert_env REDIS_STREAM_CONSUMER_GROUP "midgard-tx-processors"
upsert_env REDIS_STREAM_CONSUMER_NAME "midgard-node-pipeline-e2e"
upsert_env TX_QUEUE_DEAD_LETTER_STREAM "midgard:tx-submissions:pipeline-e2e:dead-letter"
upsert_env TX_QUEUE_DRAIN_BATCH_SIZE 250
upsert_env REDIS_STREAM_BLOCK_MS 500
upsert_env LUCID_INIT_MAX_RETRIES 0
# Space out sequencer fibers far enough that they don't interfere during the short test window
upsert_env WAIT_BETWEEN_BLOCK_COMMITMENTS 600000
upsert_env WAIT_BETWEEN_BLOCK_SUBMISSIONS 600000
upsert_env WAIT_BETWEEN_USER_EVENT_FETCHES 600000
upsert_env WAIT_BETWEEN_MERGE_TXS 600000
upsert_env NODE_API_HOST_PORT "$E2E_NODE_API_PORT"
upsert_env NODE_PROM_HOST_PORT "$E2E_NODE_PROM_PORT"
upsert_env POSTGRES_HOST_PORT "$E2E_POSTGRES_PORT"
upsert_env REDIS_HOST_PORT "$E2E_REDIS_PORT"

cleanup() {
  set +e
  cd "$NODE_DIR" || exit 0
  docker compose -p "$E2E_COMPOSE_PROJECT" --env-file "$RUNTIME_ENV_FILE" -f docker-compose.yaml down -v --remove-orphans
  rm -f "$RUNTIME_ENV_FILE"
}
trap cleanup EXIT

cd "$NODE_DIR"
export NODE_ENV_FILE="$RUNTIME_ENV_FILE"
docker compose -p "$E2E_COMPOSE_PROJECT" --env-file "$RUNTIME_ENV_FILE" -f docker-compose.yaml up -d --build node postgres redis

echo "Waiting for node to become healthy..."
for _ in $(seq 1 90); do
  if curl -fsS "http://127.0.0.1:${E2E_NODE_API_PORT}/health/live" >/dev/null 2>&1; then
    break
  fi
  sleep 1
done

curl -fsS "http://127.0.0.1:${E2E_NODE_API_PORT}/health/live" >/dev/null

API_BASE_URL="http://127.0.0.1:${E2E_NODE_API_PORT}" \
REDIS_URL="redis://127.0.0.1:${E2E_REDIS_PORT}" \
REDIS_STREAM_KEY="midgard:tx-submissions:pipeline-e2e" \
TX_QUEUE_DEAD_LETTER_STREAM="midgard:tx-submissions:pipeline-e2e:dead-letter" \
REDIS_STREAM_CONSUMER_GROUP="midgard-tx-processors" \
CORPUS_PATH="$CORPUS_PATH" \
PIPELINE_TX_COUNT="${PIPELINE_TX_COUNT:-500}" \
pnpm exec vitest run --config vitest.e2e.config.ts tests/e2e/tx-ingress-pipeline.e2e.test.ts
