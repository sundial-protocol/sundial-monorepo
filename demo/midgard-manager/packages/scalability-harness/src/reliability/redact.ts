// Redaction for the public-safe report variant. Strips identifiers that must
// not appear in a community-facing document (per
// internal-docs/scalability-stress-test-report.md section 14): wallet /
// stake / pool addresses, transaction and block hashes, raw CBOR, absolute
// paths, and internal endpoints.

import { sanitizePathLikeText } from '../path-sanitization.js';

const SCRUBBERS: { re: RegExp; replacement: string }[] = [
  { re: /addr_test1[0-9ac-hj-np-z]{20,}/gi, replacement: '<addr>' },
  { re: /addr1[0-9ac-hj-np-z]{20,}/gi, replacement: '<addr>' },
  { re: /stake_test1[0-9ac-hj-np-z]{20,}/gi, replacement: '<stake-addr>' },
  { re: /stake1[0-9ac-hj-np-z]{20,}/gi, replacement: '<stake-addr>' },
  { re: /pool1[0-9ac-hj-np-z]{20,}/gi, replacement: '<pool-id>' },
  // outref: 64-hex tx hash + #index
  { re: /\b[0-9a-f]{64}#\d+\b/gi, replacement: '<utxo-ref>' },
  // raw CBOR / very long hex blobs
  { re: /\b[0-9a-f]{120,}\b/gi, replacement: '<cbor>' },
  // tx / header / policy hashes
  { re: /\b[0-9a-f]{64}\b/gi, replacement: '<hash>' },
  { re: /\b[0-9a-f]{56}\b/gi, replacement: '<hash>' },
  // http(s) endpoints (keep the scheme word so sentences still read)
  { re: /\bhttps?:\/\/[^\s)"'`]+/gi, replacement: '<endpoint>' },
  // bare host:port for internal service discovery names
  {
    re: /\b[a-z0-9-]+\.[a-z0-9.-]*(?:local|internal|svc)[a-z0-9.-]*(?::\d+)?\b/gi,
    replacement: '<host>',
  },
];

export function redactText(input: string): string {
  let out = sanitizePathLikeText(input);
  for (const { re, replacement } of SCRUBBERS) {
    out = out.replace(re, replacement);
  }
  return out;
}

/** True when the text still contains something that looks sensitive. */
export function findLeaks(input: string): string[] {
  const leaks: string[] = [];
  for (const { re } of SCRUBBERS) {
    const m = input.match(new RegExp(re.source, re.flags));
    if (m) leaks.push(...m);
  }
  return leaks;
}
