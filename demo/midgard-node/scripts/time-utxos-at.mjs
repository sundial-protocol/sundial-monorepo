#!/usr/bin/env node
/**
 * Measures how long lucid.utxosAt() takes to fetch all UTxOs at the state
 * queue address via the configured L1 provider.
 *
 * Usage (from demo/midgard-node):
 *   node scripts/time-utxos-at.mjs
 *
 * Or override env vars inline:
 *   L1_PROVIDER=Blockfrost \
 *   L1_BLOCKFROST_API_URL=https://cardano-preprod.blockfrost.io/api/v0 \
 *   L1_BLOCKFROST_KEY=preprode0... \
 *   node scripts/time-utxos-at.mjs
 */

import { Lucid, Blockfrost, Kupmios } from "@lucid-evolution/lucid";

const ADDRESS =
  "addr_test1wzpzd7k0lkpkr3kh460ll6c88r4p5nhqehu3gjtc359a2ds9qpd7c";

const provider = process.env.L1_PROVIDER ?? "Blockfrost";
const blockfrostUrl =
  process.env.L1_BLOCKFROST_API_URL ??
  "https://cardano-preprod.blockfrost.io/api/v0";
const blockfrostKey = process.env.L1_BLOCKFROST_KEY ?? "preprode0IPdksOzu31iDChG9gAAznZVkDP3yh1";
const kupoUrl = process.env.L1_KUPO_KEY ?? "";
const ogmiosUrl = process.env.L1_OGMIOS_KEY ?? "";

console.log(`Provider : ${provider}`);
console.log(`Address  : ${ADDRESS}`);
console.log();

let lucidProvider;
if (provider === "Kupmios") {
  lucidProvider = new Kupmios(kupoUrl, ogmiosUrl);
} else {
  lucidProvider = new Blockfrost(blockfrostUrl, blockfrostKey);
}

const lucid = await Lucid(lucidProvider, "Preprod");

console.log("Starting lucid.utxosAt() ...");
const t0 = performance.now();

let utxos;
try {
  utxos = await lucid.utxosAt(ADDRESS);
} catch (e) {
  const elapsed = ((performance.now() - t0) / 1000).toFixed(2);
  console.error(`FAILED after ${elapsed}s`);
  console.error(e);
  process.exit(1);
}

const elapsed = ((performance.now() - t0) / 1000).toFixed(2);
console.log(`Done.`);
console.log(`  UTxOs returned : ${utxos.length}`);
console.log(`  Elapsed        : ${elapsed}s`);
