#!/usr/bin/env bash
set -euo pipefail

readonly script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly repo_root="$(cd "${script_dir}/../../.." && pwd)"
readonly codeql_root="${repo_root}/.tmp/codeql"
readonly query_pack="codeql/javascript-queries"
readonly analysis_category="javascript-typescript"
readonly high_security_threshold="4.0"
readonly parser_script="${script_dir}/codeql-threshold-check.mjs"

if [[ $# -ne 1 ]]; then
  echo "[security-codeql] usage: codeql.sh <project-directory>" >&2
  echo "[security-codeql] example: codeql.sh midgard-ts" >&2
  exit 1
fi

readonly project="${1}"
readonly project_dir="${repo_root}/${project}"
readonly source_root="${codeql_root}/source-${project}"
readonly database_root="${codeql_root}/database-${project}"
readonly sarif_output="${codeql_root}/results-${project}.sarif"

if [[ ! -d "${project_dir}" ]]; then
  echo "[security-codeql] project directory not found: ${project_dir}" >&2
  exit 1
fi

if [[ "${GITHUB_ACTIONS:-}" == "true" ]]; then
  echo "[security-codeql] skipped in GitHub Actions: local-only due to licensing constraints." >&2
  exit 0
fi

if ! command -v codeql >/dev/null 2>&1; then
  echo "[security-codeql] codeql CLI is required. Install CodeQL locally and retry." >&2
  exit 1
fi

if ! command -v git >/dev/null 2>&1; then
  echo "[security-codeql] git is required to prepare the filtered source snapshot." >&2
  exit 1
fi

if ! command -v rsync >/dev/null 2>&1; then
  echo "[security-codeql] rsync is required to prepare the filtered source snapshot." >&2
  exit 1
fi

bootstrap_query_pack() {
  if codeql resolve queries "${query_pack}" >/dev/null 2>&1; then
    if ! codeql pack download --force "${query_pack}" >/dev/null 2>&1; then
      echo "[security-codeql] continuing with installed query pack because refresh failed." >&2
    fi
    return 0
  fi

  echo "[security-codeql] bootstrapping query pack '${query_pack}'." >&2
  codeql pack download "${query_pack}"
}

refresh_source_snapshot() {
  rm -rf "${source_root}"
  mkdir -p "${source_root}"
  git -C "${project_dir}" ls-files -co --exclude-standard -z \
    | while IFS= read -r -d '' relative_path; do
        if [[ -e "${project_dir}/${relative_path}" ]]; then
          printf '%s\0' "${relative_path}"
        fi
      done \
    | rsync -a --delete --from0 --files-from=- "${project_dir}/" "${source_root}/"
}

refresh_database() {
  rm -rf "${database_root}"
  codeql database create "${database_root}" \
    --language=javascript \
    --build-mode=none \
    --source-root="${source_root}" \
    --overwrite
}

cleanup_probe_artifacts() {
  local tmp_root="${repo_root}/.tmp"

  if [[ ! -d "${tmp_root}" ]]; then
    return 0
  fi

  find "${tmp_root}" -mindepth 1 -maxdepth 1 -type d -name "codeql-probe-*" -exec rm -rf {} +
}

mkdir -p "${codeql_root}"
cleanup_probe_artifacts

bootstrap_query_pack
refresh_source_snapshot
refresh_database

codeql database analyze "${database_root}" \
  "${query_pack}" \
  --format=sarifv2.1.0 \
  --output="${sarif_output}" \
  --sarif-category="${analysis_category}" \
  --threads=0 \
  --no-download

node "${parser_script}" "${sarif_output}" "${high_security_threshold}"
