#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel)"
NODE_DIR="$ROOT_DIR/demo/midgard-node"
BASE_ENV_FILE="$NODE_DIR/.env"
if [[ ! -f "$BASE_ENV_FILE" ]]; then
  BASE_ENV_FILE="$NODE_DIR/.env.example"
fi

RUNTIME_ENV_FILE="$(mktemp /tmp/midgard-node-e2e-env-XXXXXX)"
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

upsert_env NODE_ROLE api
upsert_env L1_PROVIDER Blockfrost
upsert_env L1_BLOCKFROST_API_URL http://127.0.0.1:1
upsert_env L1_BLOCKFROST_KEY e2e-dummy-key
upsert_env L1_OGMIOS_KEY http://127.0.0.1:1
upsert_env L1_KUPO_KEY http://127.0.0.1:1
upsert_env REDIS_STREAM_KEY midgard:tx-submissions:e2e
upsert_env REDIS_STREAM_CONSUMER_GROUP midgard-tx-processors
upsert_env REDIS_STREAM_CONSUMER_NAME midgard-node-e2e
upsert_env TX_QUEUE_DEAD_LETTER_STREAM midgard:tx-submissions:e2e:dead-letter
upsert_env TX_QUEUE_CAPACITY 10000
upsert_env TX_QUEUE_MAX_PENDING 20000
upsert_env LUCID_INIT_MAX_RETRIES 0

cleanup() {
  set +e
  cd "$NODE_DIR" || exit 0
  docker compose --env-file "$RUNTIME_ENV_FILE" -f docker-compose.yaml down -v --remove-orphans
  rm -f "$RUNTIME_ENV_FILE"
}
trap cleanup EXIT

cd "$NODE_DIR"
export NODE_ENV_FILE="$RUNTIME_ENV_FILE"
docker compose --env-file "$RUNTIME_ENV_FILE" -f docker-compose.yaml up -d --build node postgres redis

for _ in $(seq 1 60); do
  if curl -fsS "http://127.0.0.1:3000/health/live" >/dev/null 2>&1; then
    break
  fi
  sleep 1
done

curl -fsS "http://127.0.0.1:3000/health/live" >/dev/null

API_BASE_URL="http://127.0.0.1:3000" \
REDIS_URL="redis://127.0.0.1:6379" \
REDIS_STREAM_KEY="midgard:tx-submissions:e2e" \
TX_QUEUE_DEAD_LETTER_STREAM="midgard:tx-submissions:e2e:dead-letter" \
REDIS_STREAM_CONSUMER_GROUP="midgard-tx-processors" \
pnpm exec vitest run tests/e2e/redis-stream-ingress.e2e.test.ts
