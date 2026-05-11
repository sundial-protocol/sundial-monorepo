#!/usr/bin/env bash
set -euo pipefail

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly DEMO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
readonly DEFAULT_SEMGREP_TARGET="${DEMO_ROOT}"
readonly SEMGREP_CI_TIMEOUT_DEFAULT_SECONDS=900
readonly SEMGREP_CI_TIMEOUT_SECONDS="${SEMGREP_CI_TIMEOUT_SECONDS:-${SEMGREP_CI_TIMEOUT_DEFAULT_SECONDS}}"

if [[ -z "${SEMGREP_APP_TOKEN:-}" ]]; then
  echo "[security-semgrep-ci] missing SEMGREP_APP_TOKEN. Configure it in GitHub Actions repository secrets." >&2
  exit 1
fi

if ! [[ "${SEMGREP_CI_TIMEOUT_SECONDS}" =~ ^[0-9]+$ ]] || (( SEMGREP_CI_TIMEOUT_SECONDS <= 0 )); then
  echo "[security-semgrep-ci] invalid SEMGREP_CI_TIMEOUT_SECONDS='${SEMGREP_CI_TIMEOUT_SECONDS}'. Use a positive integer (seconds)." >&2
  exit 1
fi

if ! command -v semgrep >/dev/null 2>&1; then
  echo "[security-semgrep-ci] semgrep CLI is required in CI environment." >&2
  exit 1
fi

echo "[security-semgrep-ci] starting semgrep ci timeout_seconds=${SEMGREP_CI_TIMEOUT_SECONDS}" >&2

if [[ $# -eq 0 ]]; then
  set -- "${DEFAULT_SEMGREP_TARGET}"
fi

if ! command -v timeout >/dev/null 2>&1; then
  echo "[security-semgrep-ci] timeout command unavailable; running semgrep without outer timeout guard." >&2
  exec semgrep ci --disable-version-check --no-suppress-errors "$@"
fi

set +e
timeout --foreground "${SEMGREP_CI_TIMEOUT_SECONDS}s" semgrep ci --disable-version-check --no-suppress-errors "$@"
semgrep_exit_code=$?
set -e

if (( semgrep_exit_code == 124 )); then
  echo "[security-semgrep-ci] semgrep ci timed out after ${SEMGREP_CI_TIMEOUT_SECONDS}s. Review the scan logs above." >&2
fi

exit "${semgrep_exit_code}"
