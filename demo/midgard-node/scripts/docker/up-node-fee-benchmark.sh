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

CONTAINER_COMPOSE=()
RUNTIME_ENV_FILE="$(mktemp /tmp/midgard-node-fee-benchmark-env-XXXXXX)"

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

cleanup() {
  rm -f "$RUNTIME_ENV_FILE"
}
trap cleanup EXIT

cp "$BASE_ENV_FILE" "$RUNTIME_ENV_FILE"

upsert_env() {
  local key="$1"
  local value="$2"
  if grep -q "^${key}=" "$RUNTIME_ENV_FILE"; then
    in_place_sed "s|^${key}=.*|${key}=${value}|" "$RUNTIME_ENV_FILE"
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

init_container_compose

cd "$NODE_DIR"
export NODE_ENV_FILE="$RUNTIME_ENV_FILE"

container_compose rm -sf node >/dev/null 2>&1 || true
container_compose --profile split up -d node-api node-tx-processor node-sequencer

echo
echo "Fee benchmark node profile is active."
echo "Current WAIT_BETWEEN_BLOCK_COMMITMENTS=$(grep '^WAIT_BETWEEN_BLOCK_COMMITMENTS=' "$RUNTIME_ENV_FILE" | cut -d= -f2-)"
