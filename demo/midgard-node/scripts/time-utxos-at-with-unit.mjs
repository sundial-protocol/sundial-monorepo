#!/usr/bin/env node
/**
 * Compares the O(n) full-address scan against the O(1) unit-scoped lookup
 * introduced to fix S-001 / F-006.
 *
 * Step 1 — Run lucid.utxosAt(address) and measure elapsed time.
 * Step 2 — Extract a state-queue NFT unit from the result (requires
 *           STATE_QUEUE_POLICY_ID so we can identify the right asset).
 * Step 3 — Run lucid.utxosAtWithUnit(address, unit) for that unit and
 *           measure elapsed time.
 * Step 4 — Print a side-by-side comparison.
 *
 * Usage (from demo/midgard-node):
 *   STATE_QUEUE_POLICY_ID=<56-hex-char-policy-id> node scripts/time-utxos-at-with-unit.mjs
 *
 * Or override provider env vars inline:
 *   L1_PROVIDER=Blockfrost \
 *   L1_BLOCKFROST_API_URL=https://cardano-preprod.blockfrost.io/api/v0 \
 *   L1_BLOCKFROST_KEY=preprode0... \
 *   STATE_QUEUE_POLICY_ID=<policyId> \
 *   node scripts/time-utxos-at-with-unit.mjs
 *
 * STATE_QUEUE_UNIT can be supplied directly to skip Step 1 and only run
 * the fast path:
 *   STATE_QUEUE_UNIT=<policyId><assetName> node scripts/time-utxos-at-with-unit.mjs
 */

import { Lucid, Blockfrost, Kupmios } from "@lucid-evolution/lucid";

const ADDRESS =
  "addr_test1wzpzd7k0lkpkr3kh460ll6c88r4p5nhqehu3gjtc359a2ds9qpd7c";

const provider = process.env.L1_PROVIDER ?? "Blockfrost";
const blockfrostUrl =
  process.env.L1_BLOCKFROST_API_URL ??
  "https://cardano-preprod.blockfrost.io/api/v0";
const blockfrostKey =
  process.env.L1_BLOCKFROST_KEY ?? "preprode0IPdksOzu31iDChG9gAAznZVkDP3yh1";
const kupoUrl = process.env.L1_KUPO_KEY ?? "";
const ogmiosUrl = process.env.L1_OGMIOS_KEY ?? "";

const policyId =
  process.env.STATE_QUEUE_POLICY_ID ??
  "565e23bcb46916d008be00384a247c1f06e98ed8b51307605012ec2e";
const knownUnit = process.env.STATE_QUEUE_UNIT ?? null;

console.log(`Provider : ${provider}`);
console.log(`Address  : ${ADDRESS}`);
if (policyId) console.log(`Policy ID: ${policyId}`);
if (knownUnit) console.log(`Unit     : ${knownUnit}`);
console.log();

let lucidProvider;
if (provider === "Kupmios") {
  lucidProvider = new Kupmios(kupoUrl, ogmiosUrl);
} else {
  lucidProvider = new Blockfrost(blockfrostUrl, blockfrostKey);
}

const lucid = await Lucid(lucidProvider, "Preprod");

// ── Step 1: full address scan ─────────────────────────────────────────────────

let discoveredUnit = knownUnit;
let fullScanElapsed = null;
let fullScanCount = null;

if (!knownUnit) {
  console.log("── Step 1: full address scan (lucid.utxosAt) ────────────────");
  const t0 = performance.now();
  let allUtxos;
  try {
    allUtxos = await lucid.utxosAt(ADDRESS);
  } catch (e) {
    const elapsed = ((performance.now() - t0) / 1000).toFixed(2);
    console.error(`FAILED after ${elapsed}s`);
    console.error(e);
    process.exit(1);
  }
  fullScanElapsed = ((performance.now() - t0) / 1000).toFixed(2);
  fullScanCount = allUtxos.length;
  console.log(`  UTxOs returned : ${fullScanCount}`);
  console.log(`  Elapsed        : ${fullScanElapsed}s`);

  for (const utxo of allUtxos) {
    const unit = Object.keys(utxo.assets).find((k) =>
      k.startsWith(policyId),
    );
    if (unit) {
      discoveredUnit = unit;
      break;
    }
  }

  if (!discoveredUnit) {
    console.error(
      `\nNo UTxO with policy ID ${policyId} found at address. Check STATE_QUEUE_POLICY_ID.`,
    );
    process.exit(1);
  }
  console.log(`  Unit discovered: ${discoveredUnit}`);
  console.log();
}

// ── Step 2: unit-scoped lookup ────────────────────────────────────────────────

console.log(
  "── Step 2: unit-scoped lookup (lucid.utxosAtWithUnit) ───────────────",
);
const t1 = performance.now();
let unitUtxos;
try {
  unitUtxos = await lucid.utxosAtWithUnit(ADDRESS, discoveredUnit);
} catch (e) {
  const elapsed = ((performance.now() - t1) / 1000).toFixed(2);
  console.error(`FAILED after ${elapsed}s`);
  console.error(e);
  process.exit(1);
}
const unitElapsed = ((performance.now() - t1) / 1000).toFixed(2);
console.log(`  UTxOs returned : ${unitUtxos.length}`);
console.log(`  Elapsed        : ${unitElapsed}s`);
console.log();

// ── Summary ───────────────────────────────────────────────────────────────────

console.log("── Summary ──────────────────────────────────────────────────────");
if (fullScanElapsed !== null) {
  console.log(`  utxosAt (full scan, ${fullScanCount} UTxOs) : ${fullScanElapsed}s`);
}
console.log(`  utxosAtWithUnit (1 UTxO)             : ${unitElapsed}s`);
if (fullScanElapsed !== null) {
  const ratio = (parseFloat(fullScanElapsed) / parseFloat(unitElapsed)).toFixed(1);
  console.log(`  Speedup                              : ${ratio}×`);
}
