#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "${SCRIPT_DIR}/../../../" && pwd)"
cd "${REPO_ROOT}"

PG_CONT=midgard-legacy-pg
NODE_ENV_FILE="midgard-node/.env"
BACKUP_ENV_FILE="midgard-node/.env.bak.legacy"
OGMIOS_MOCK_PORT=1337
OGMIOS_MOCK_HOST=127.0.0.1
OGMIOS_MOCK_PID=""

cleanup() {
  if [ -n "$OGMIOS_MOCK_PID" ]; then
    kill "$OGMIOS_MOCK_PID" >/dev/null 2>&1 || true
  fi
  docker rm -f "$PG_CONT" >/dev/null 2>&1 || true
  if [ -f "$BACKUP_ENV_FILE" ]; then
    mv "$BACKUP_ENV_FILE" "$NODE_ENV_FILE"
  else
    rm -f "$NODE_ENV_FILE"
  fi
}
trap cleanup EXIT

OGMIOS_MOCK_HOST="$OGMIOS_MOCK_HOST" OGMIOS_MOCK_PORT="$OGMIOS_MOCK_PORT" \
  node scripts/test/legacy/ogmios-mock-server.mjs >/dev/null 2>&1 &
OGMIOS_MOCK_PID="$!"

for _ in $(seq 1 30); do
  if nc -z "$OGMIOS_MOCK_HOST" "$OGMIOS_MOCK_PORT" >/dev/null 2>&1; then
    break
  fi
  sleep 1
done

if ! nc -z "$OGMIOS_MOCK_HOST" "$OGMIOS_MOCK_PORT" >/dev/null 2>&1; then
  echo "Failed to start Ogmios mock server on ${OGMIOS_MOCK_HOST}:${OGMIOS_MOCK_PORT}" >&2
  exit 1
fi

docker rm -f "$PG_CONT" >/dev/null 2>&1 || true
docker run -d --name "$PG_CONT" \
  -e POSTGRES_USER=postgres \
  -e POSTGRES_PASSWORD=postgres \
  -e POSTGRES_DB=midgard \
  -p 5432:5432 \
  postgres:15-alpine >/dev/null

until docker exec "$PG_CONT" pg_isready -U postgres -d midgard >/dev/null 2>&1; do
  sleep 1
done

rm -f "$BACKUP_ENV_FILE"
if [ -f "$NODE_ENV_FILE" ]; then
  cp "$NODE_ENV_FILE" "$BACKUP_ENV_FILE"
fi
cp midgard-node/.env.example "$NODE_ENV_FILE"
sed -i 's/^POSTGRES_HOST=.*/POSTGRES_HOST=localhost/' "$NODE_ENV_FILE"
sed -i 's|^L1_OGMIOS_KEY=.*|L1_OGMIOS_KEY=http://127.0.0.1:1337|' "$NODE_ENV_FILE"
sed -i 's|^L1_KUPO_KEY=.*|L1_KUPO_KEY=http://127.0.0.1:1442|' "$NODE_ENV_FILE"

set +e
npm run test:legacy:all
TEST_EXIT_CODE=$?
set -e

exit "$TEST_EXIT_CODE"
