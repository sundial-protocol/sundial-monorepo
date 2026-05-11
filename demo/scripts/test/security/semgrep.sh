#!/usr/bin/env bash
set -euo pipefail

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly DEMO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
readonly DEFAULT_SEMGREP_TARGET="${DEMO_ROOT}"

if [[ $# -eq 0 ]]; then
  set -- "${DEFAULT_SEMGREP_TARGET}"
fi

if [[ "${GITHUB_ACTIONS:-}" == "true" ]]; then
  exec "${SCRIPT_DIR}/semgrep-ci.sh" "$@"
fi

exec "${SCRIPT_DIR}/semgrep-local.sh" "$@"
