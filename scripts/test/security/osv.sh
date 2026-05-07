#!/usr/bin/env bash
set -euo pipefail

readonly script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly repo_root="$(cd "${script_dir}/../../.." && pwd)"

readonly allowed_licenses="MIT,Apache-2.0,ISC,BSD-2-Clause,BSD-3-Clause,0BSD,BlueOak-1.0.0,CC0-1.0,MPL-2.0,Python-2.0,Unlicense"

is_remote_license_failure() {
  local output="$1"

  [[ "${output}" == *"rpc error: code = Unavailable desc = service unavailable"* ]] \
    || [[ "${output}" == *"max retries exceeded"* ]] \
    || [[ "${output}" == *"HTTP 502"* ]] \
    || [[ "${output}" == *"HTTP 503"* ]] \
    || [[ "${output}" == *"HTTP 504"* ]]
}

run_scan() {
  (
    cd "${repo_root}"
    "$@"
  )
}

if ! command -v osv-scanner >/dev/null 2>&1; then
  echo "[quality-osv] osv-scanner CLI is required. Install it and retry." >&2
  exit 1
fi

echo "[quality-osv] running vulnerability scan."
run_scan osv-scanner scan source \
  -r \
  --config osv-scanner.toml \
  .

echo "[quality-osv] running online license scan."
license_output_file="$(mktemp)"

if run_scan osv-scanner scan source \
  -r \
  --config osv-scanner.toml \
  --licenses="${allowed_licenses}" \
  . >"${license_output_file}" 2>&1; then
  cat "${license_output_file}"
  rm -f "${license_output_file}"
  exit 0
fi

license_exit_code=$?
license_output="$(cat "${license_output_file}")"
rm -f "${license_output_file}"

printf "%s\n" "${license_output}" >&2

if is_remote_license_failure "${license_output}"; then
  echo "[quality-osv] online license scan unavailable; vulnerability scan already passed." >&2
  echo "[quality-osv] treating license scan as warning-only for this remote/RPC failure." >&2
  exit 0
fi

exit "${license_exit_code}"