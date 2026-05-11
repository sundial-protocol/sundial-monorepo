import fs from "node:fs";

const sarifPath = process.argv[2];
const highSecurityThreshold = Number(process.argv[3]);

if (!sarifPath || !Number.isFinite(highSecurityThreshold)) {
  console.error(
    "[security-codeql] usage: node codeql-threshold-check.mjs <sarif-path> <high-security-threshold>",
  );
  process.exit(1);
}

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

  return [
    {
      level,
      ruleId: result.ruleId,
      securitySeverity,
      uri,
    },
  ];
});

if (highOrHigherFindings.length > 0) {
  console.error(
    `[security-codeql] analysis failed with ${highOrHigherFindings.length} fintech-threshold result(s). Review ${sarifPath}.`,
  );
  for (const finding of highOrHigherFindings.slice(0, 10)) {
    const securitySeverityText = Number.isFinite(finding.securitySeverity)
      ? finding.securitySeverity.toFixed(1)
      : "n/a";
    console.error(
      `[security-codeql] fintech_threshold ruleId=${finding.ruleId} level=${finding.level} security_severity=${securitySeverityText} location=${finding.uri}`,
    );
  }
  process.exit(1);
}

console.log(
  `[security-codeql] analysis passed with ${results.length} total result(s) and 0 fintech-threshold result(s). Report: ${sarifPath}`,
);
