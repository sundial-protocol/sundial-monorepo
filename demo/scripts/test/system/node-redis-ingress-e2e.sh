#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel)"
NODE_DIR="$ROOT_DIR/demo/midgard-node"
BASE_ENV_FILE="$NODE_DIR/.env"
if [[ ! -f "$BASE_ENV_FILE" ]]; then
  BASE_ENV_FILE="$NODE_DIR/.env.example"
fi

CONTAINER_COMPOSE=()
RUNTIME_ENV_FILE="$(mktemp /tmp/midgard-node-e2e-env-XXXXXX)"
cp "$BASE_ENV_FILE" "$RUNTIME_ENV_FILE"

die() {
  echo "$*" >&2
  exit 1
}

in_place_sed() {
  local expr="$1"
  local file="$2"

  if sed --version >/dev/null 2>&1; then
    sed -i "$expr" "$file"
  else
    sed -i '' "$expr" "$file"
  fi
}

init_container_compose() {
  if command -v docker >/dev/null 2>&1 && docker compose version >/dev/null 2>&1; then
    CONTAINER_COMPOSE=(docker compose)
    return 0
  fi

  if command -v colima >/dev/null 2>&1; then
    if ! colima nerdctl info >/dev/null 2>&1; then
      die "Docker is not installed and Colima is not running. Start Colima and retry."
    fi
    if ! colima nerdctl compose version >/dev/null 2>&1; then
      die "Colima is running, but 'colima nerdctl compose' is unavailable."
    fi
    CONTAINER_COMPOSE=(colima nerdctl compose)
    return 0
  fi

  die "Neither docker compose nor colima was found. Install Docker, or install/start Colima and retry."
}

container_compose() {
  "${CONTAINER_COMPOSE[@]}" "$@"
}

is_port_in_use() {
  local port="$1"

  if command -v lsof >/dev/null 2>&1; then
    lsof -nP -iTCP:"$port" -sTCP:LISTEN >/dev/null 2>&1
    return $?
  fi

  nc -z 127.0.0.1 "$port" >/dev/null 2>&1
}

find_available_port() {
  local start_port="$1"
  local max_port=$((start_port + 100))
  local port="$start_port"

  while [ "$port" -le "$max_port" ]; do
    if ! is_port_in_use "$port"; then
      echo "$port"
      return 0
    fi
    port=$((port + 1))
  done

  die "Failed to find an available port in range ${start_port}-${max_port}"
}

upsert_env() {
  local key="$1"
  local value="$2"
  if grep -q "^${key}=" "$RUNTIME_ENV_FILE"; then
    in_place_sed "s|^${key}=.*|${key}=${value}|" "$RUNTIME_ENV_FILE"
  else
    echo "${key}=${value}" >> "$RUNTIME_ENV_FILE"
  fi
}

E2E_COMPOSE_PROJECT="sundial-test-$$"
E2E_NODE_API_PORT="$(find_available_port 3010)"
E2E_NODE_PROM_PORT="$(find_available_port 9465)"
E2E_POSTGRES_PORT="$(find_available_port 5434)"
E2E_REDIS_PORT="$(find_available_port 6380)"

init_container_compose

echo "Using e2e compose project: $E2E_COMPOSE_PROJECT"
echo "Using ports api=$E2E_NODE_API_PORT prom=$E2E_NODE_PROM_PORT postgres=$E2E_POSTGRES_PORT redis=$E2E_REDIS_PORT"

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
upsert_env LUCID_INIT_MAX_RETRIES 0
upsert_env NODE_API_HOST_PORT "$E2E_NODE_API_PORT"
upsert_env NODE_PROM_HOST_PORT "$E2E_NODE_PROM_PORT"
upsert_env POSTGRES_HOST_PORT "$E2E_POSTGRES_PORT"
upsert_env REDIS_HOST_PORT "$E2E_REDIS_PORT"
upsert_env POSTGRES_SHARED_BUFFERS 128MB
upsert_env POSTGRES_EFFECTIVE_CACHE_SIZE 512MB
upsert_env POSTGRES_WORK_MEM 4MB
upsert_env POSTGRES_MAINTENANCE_WORK_MEM 64MB
upsert_env POSTGRES_MAX_CONNECTIONS 50

cleanup() {
  set +e
  cd "$NODE_DIR" || exit 0
  container_compose -p "$E2E_COMPOSE_PROJECT" --env-file "$RUNTIME_ENV_FILE" -f docker-compose.yaml down -v --remove-orphans
  rm -f "$RUNTIME_ENV_FILE"
}
trap cleanup EXIT

cd "$NODE_DIR"
export NODE_ENV_FILE="$RUNTIME_ENV_FILE"
container_compose -p "$E2E_COMPOSE_PROJECT" --env-file "$RUNTIME_ENV_FILE" -f docker-compose.yaml up -d --build node postgres redis

for _ in $(seq 1 60); do
  if curl -fsS "http://127.0.0.1:${E2E_NODE_API_PORT}/health/live" >/dev/null 2>&1; then
    break
  fi
  sleep 1
done

curl -fsS "http://127.0.0.1:${E2E_NODE_API_PORT}/health/live" >/dev/null

API_BASE_URL="http://127.0.0.1:${E2E_NODE_API_PORT}" \
REDIS_URL="redis://127.0.0.1:${E2E_REDIS_PORT}" \
REDIS_STREAM_KEY="midgard:tx-submissions:e2e" \
TX_QUEUE_DEAD_LETTER_STREAM="midgard:tx-submissions:e2e:dead-letter" \
REDIS_STREAM_CONSUMER_GROUP="midgard-tx-processors" \
pnpm exec vitest run --config vitest.e2e.config.ts tests/e2e/redis-stream-ingress.e2e.test.ts
