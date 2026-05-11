#!/usr/bin/env bash
set -euo pipefail

readonly script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly repo_root="$(cd "${script_dir}/../../.." && pwd)"
readonly codeql_root="${repo_root}/.tmp/codeql"
readonly source_root="${codeql_root}/source"
readonly database_root="${codeql_root}/database"
readonly query_pack="codeql/javascript-queries"
readonly sarif_output="${codeql_root}/results.sarif"
readonly analysis_category="javascript-typescript"
readonly high_security_threshold="4.0"

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
  git -C "${repo_root}" ls-files -co --exclude-standard -z \
    | while IFS= read -r -d '' relative_path; do
        if [[ -e "${repo_root}/${relative_path}" ]]; then
          printf '%s\0' "${relative_path}"
        fi
      done \
    | rsync -a --delete --from0 --files-from=- "${repo_root}/" "${source_root}/"
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

node --input-type=module -e '
  import fs from "node:fs";

  const sarifPath = process.argv[1];
  const highSecurityThreshold = Number(process.argv[2]);
  const sarif = JSON.parse(fs.readFileSync(sarifPath, "utf8"));
  const run = sarif.runs?.[0];
  const rules = [
    ...(run?.tool?.driver?.rules ?? []),
    ...((run?.tool?.extensions ?? []).flatMap((extension) => extension.rules ?? [])),
  ];
  const ruleMap = new Map(rules.map((rule) => [rule.id, rule]));
  const results = run?.results ?? [];

  const highOrHigherFindings = results.flatMap((result) => {
    const rule = ruleMap.get(result.ruleId) ?? {};
    const level = result.level ?? rule.defaultConfiguration?.level ?? "warning";
    const securitySeverity = Number.parseFloat(rule.properties?.["security-severity"] ?? "0");
    const uri = result.locations?.[0]?.physicalLocation?.artifactLocation?.uri ?? "unknown";

    if (level !== "error" && securitySeverity < highSecurityThreshold) {
      return [];
    }

    return [{
      level,
      ruleId: result.ruleId,
      securitySeverity,
      uri,
    }];
  });

  if (highOrHigherFindings.length > 0) {
    console.error(
      `[security-codeql] analysis failed with ${highOrHigherFindings.length} fintech-threshold result(s). Review ${sarifPath}.`
    );
    for (const finding of highOrHigherFindings.slice(0, 10)) {
      const securitySeverityText = Number.isFinite(finding.securitySeverity)
        ? finding.securitySeverity.toFixed(1)
        : "n/a";
      console.error(
        `[security-codeql] fintech_threshold ruleId=${finding.ruleId} level=${finding.level} security_severity=${securitySeverityText} location=${finding.uri}`
      );
    }
    process.exit(1);
  }

  console.log(
    `[security-codeql] analysis passed with ${results.length} total result(s) and 0 fintech-threshold result(s). Report: ${sarifPath}`
  );
' "${sarif_output}" "${high_security_threshold}"
