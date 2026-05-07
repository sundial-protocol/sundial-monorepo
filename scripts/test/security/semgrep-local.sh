#!/usr/bin/env bash
set -euo pipefail

if ! command -v semgrep >/dev/null 2>&1; then
  echo "[security-semgrep-local] semgrep CLI is required. Install semgrep and retry." >&2
  exit 1
fi

semgrep scan --config=auto --error "$@"
