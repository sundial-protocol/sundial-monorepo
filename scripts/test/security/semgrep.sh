#!/usr/bin/env bash
set -euo pipefail

if [[ "${GITHUB_ACTIONS:-}" == "true" ]]; then
  exec "$(dirname "${BASH_SOURCE[0]}")/semgrep-ci.sh" "$@"
fi

exec "$(dirname "${BASH_SOURCE[0]}")/semgrep-local.sh" "$@"
