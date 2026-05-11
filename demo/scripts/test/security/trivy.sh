#!/usr/bin/env bash
set -euo pipefail

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly DEMO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"

run_scan() {
  local target="$1"

  if [[ ! -e "${target}" ]]; then
    echo "[security-trivy] skip missing target=${target}" >&2
    return 0
  fi

  echo "[security-trivy] scanning target=${target}" >&2
  trivy fs \
    --scanners vuln \
    --severity HIGH,CRITICAL \
    --ignore-unfixed \
    --include-dev-deps \
    --exit-code 1 \
    --no-progress \
    --skip-version-check \
    "${target}"
}

if [[ $# -gt 0 ]]; then
  for target in "$@"; do
    run_scan "${target}"
  done
  exit 0
fi

echo "[security-trivy] scanning canonical lockfiles for demo workspaces" >&2
echo "[security-trivy] root lockfile covers: midgard-node, midgard-sdk, midgard-ts" >&2
run_scan "${DEMO_ROOT}/pnpm-lock.yaml"
