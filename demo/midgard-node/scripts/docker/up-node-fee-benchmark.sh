#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel)"
NODE_DIR="$ROOT_DIR/demo/midgard-node"

BASE_ENV_FILE="${BASE_ENV_FILE:-$NODE_DIR/.env}"
if [[ ! -f "$BASE_ENV_FILE" ]]; then
  BASE_ENV_FILE="$NODE_DIR/.env.example"
fi

PROFILE_ENV_FILE="${PROFILE_ENV_FILE:-$NODE_DIR/.env.profile.fee-benchmark}"
if [[ ! -f "$PROFILE_ENV_FILE" ]]; then
  echo "ERROR: fee benchmark profile not found at: $PROFILE_ENV_FILE" >&2
  exit 1
fi

RUNTIME_ENV_FILE="$(mktemp /tmp/midgard-node-fee-benchmark-env-XXXXXX)"
cleanup() {
  rm -f "$RUNTIME_ENV_FILE"
}
trap cleanup EXIT

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

while IFS= read -r line || [[ -n "$line" ]]; do
  if [[ -z "$line" || "$line" == \#* ]]; then
    continue
  fi
  key="${line%%=*}"
  value="${line#*=}"
  upsert_env "$key" "$value"
done < "$PROFILE_ENV_FILE"

if [[ -n "${FEE_BENCHMARK_WAIT_BETWEEN_BLOCK_COMMITMENTS_MS:-}" ]]; then
  upsert_env \
    WAIT_BETWEEN_BLOCK_COMMITMENTS \
    "$FEE_BENCHMARK_WAIT_BETWEEN_BLOCK_COMMITMENTS_MS"
fi

echo "Using base env:    $BASE_ENV_FILE"
echo "Using fee profile: $PROFILE_ENV_FILE"
echo "Merged env file:   $RUNTIME_ENV_FILE"

cd "$NODE_DIR"
export NODE_ENV_FILE="$RUNTIME_ENV_FILE"

docker compose rm -sf node >/dev/null 2>&1 || true
docker compose --profile split up -d node-api node-tx-processor node-sequencer

echo
echo "Fee benchmark node profile is active."
echo "Current WAIT_BETWEEN_BLOCK_COMMITMENTS=$(grep '^WAIT_BETWEEN_BLOCK_COMMITMENTS=' "$RUNTIME_ENV_FILE" | cut -d= -f2-)"
