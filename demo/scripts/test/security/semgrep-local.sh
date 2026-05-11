#!/usr/bin/env bash
set -euo pipefail

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly DEMO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
readonly DEFAULT_SEMGREP_TARGET="${DEMO_ROOT}"

if ! command -v semgrep >/dev/null 2>&1; then
  echo "[security-semgrep-local] semgrep CLI is required. Install semgrep and retry." >&2
  exit 1
fi

if [[ $# -eq 0 ]]; then
  set -- "${DEFAULT_SEMGREP_TARGET}"
fi

semgrep scan --config=auto --error "$@"
