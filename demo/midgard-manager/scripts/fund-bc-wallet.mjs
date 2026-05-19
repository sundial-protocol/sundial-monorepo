import { Blockfrost, Lucid } from "@lucid-evolution/lucid";
import { readFile } from "node:fs/promises";

const TOPUP_LOVELACE = 1_000_000_000n;
const WAIT_SECONDS_PER_PHASE = 120;
const PROGRESS_BAR_WIDTH = 30;
const LOVELACE_PER_ADA = 1_000_000n;
const ENV_FILE_URL = new URL("./.env", import.meta.url);
const REQUIRED_ENV_KEYS = ["BLOCKFROST_URL", "BLOCKFROST_KEY", "MAIN_SEED", "BC_SEED"];

function parseDotEnv(text) {
  const parsed = {};
  const lines = text.split(/\r?\n/);
  for (const line of lines) {
    const trimmed = line.trim();
    if (trimmed.length === 0 || trimmed.startsWith("#")) {
      continue;
    }
    const index = trimmed.indexOf("=");
    if (index <= 0) {
      continue;
    }
    const key = trimmed.slice(0, index).trim();
    let value = trimmed.slice(index + 1).trim();
    if (
      (value.startsWith("\"") && value.endsWith("\"")) ||
      (value.startsWith("'") && value.endsWith("'"))
    ) {
      value = value.slice(1, -1);
    }
    parsed[key] = value;
  }
  return parsed;
}

async function loadScriptEnv() {
  try {
    const content = await readFile(ENV_FILE_URL, "utf8");
    return parseDotEnv(content);
  } catch (error) {
    if (error instanceof Error && "code" in error && error.code === "ENOENT") {
      return {};
    }
    throw error;
  }
}

function requireEnvValue(mergedEnv, key) {
  const raw = mergedEnv[key];
  if (typeof raw !== "string" || raw.trim().length === 0) {
    throw new Error(
      `Missing required ${key}. Set it in process environment or ${ENV_FILE_URL.pathname}.`
    );
  }
  return raw.trim();
}

function formatAda(lovelace) {
  const whole = lovelace / LOVELACE_PER_ADA;
  const fractional = lovelace % LOVELACE_PER_ADA;
  return `${whole}.${fractional.toString().padStart(6, "0")}`;
}

function renderProgressLine(label, elapsedSeconds, totalSeconds) {
  const ratio = Math.min(1, elapsedSeconds / totalSeconds);
  const filled = Math.round(PROGRESS_BAR_WIDTH * ratio);
  const empty = Math.max(0, PROGRESS_BAR_WIDTH - filled);
  const mm = String(Math.floor((totalSeconds - elapsedSeconds) / 60)).padStart(2, "0");
  const ss = String((totalSeconds - elapsedSeconds) % 60).padStart(2, "0");
  return `${label} [${"#".repeat(filled)}${"-".repeat(empty)}] ${Math.round(ratio * 100)
    .toString()
    .padStart(3, " ")}% | remaining ${mm}:${ss}`;
}

async function sleep(ms) {
  await new Promise((resolve) => setTimeout(resolve, ms));
}

async function waitWithCountdown(label, seconds) {
  for (let elapsed = 0; elapsed <= seconds; elapsed += 1) {
    process.stdout.write(`\r${renderProgressLine(label, elapsed, seconds)}`);
    if (elapsed < seconds) {
      await sleep(1000);
    }
  }
  process.stdout.write("\n");
}

async function fetchCommitmentWalletState(lucid) {
  lucid.selectWallet.fromSeed(BC_SEED);
  const address = await lucid.wallet().address();
  const utxos = await lucid.wallet().getUtxos();
  const lovelaceBalance = utxos.reduce((sum, utxo) => sum + (utxo.assets.lovelace ?? 0n), 0n);
  return { address, lovelaceBalance };
}

async function printBalance(lucid, label) {
  const { address, lovelaceBalance } = await fetchCommitmentWalletState(lucid);
  console.log(`${label}`);
  console.log(`Block commitment wallet: ${address}`);
  console.log(`Balance: ${lovelaceBalance.toString()} lovelace (${formatAda(lovelaceBalance)} ADA)`);
  return lovelaceBalance;
}

async function runCheckMode(lucid) {
  await printBalance(lucid, "Commitment wallet balance check");
}

async function runTopupMode(lucid) {
  const { address: bcAddress } = await fetchCommitmentWalletState(lucid);
  const initialBalance = await printBalance(lucid, "Before top-up");

  lucid.selectWallet.fromSeed(MAIN_SEED);
  const tx = await lucid.newTx()
    .pay.ToAddress(bcAddress, {
      lovelace: TOPUP_LOVELACE,
    })
    .complete();
  const signed = await tx.sign.withWallet().complete();
  const txHash = await signed.submit();
  console.log(`Top-up tx submitted: ${txHash}`);
  console.log(`Submitted top-up amount: ${TOPUP_LOVELACE.toString()} lovelace (${formatAda(TOPUP_LOVELACE)} ADA)`);

  await waitWithCountdown("Waiting for balance sync (phase 1/2)", WAIT_SECONDS_PER_PHASE);
  const balanceAfterFirstWait = await printBalance(lucid, "After wait 1/2");
  if (balanceAfterFirstWait > initialBalance) {
    console.log("Balance increased after phase 1/2; skipping second wait.");
    return;
  }

  await waitWithCountdown("Waiting for balance sync (phase 2/2)", WAIT_SECONDS_PER_PHASE);
  await printBalance(lucid, "After wait 2/2 (final)");
}

const scriptEnv = await loadScriptEnv();
const mergedEnv = { ...scriptEnv, ...process.env };
for (const key of REQUIRED_ENV_KEYS) {
  requireEnvValue(mergedEnv, key);
}

const BLOCKFROST_URL = requireEnvValue(mergedEnv, "BLOCKFROST_URL");
const BLOCKFROST_KEY = requireEnvValue(mergedEnv, "BLOCKFROST_KEY");
const MAIN_SEED = requireEnvValue(mergedEnv, "MAIN_SEED");
const BC_SEED = requireEnvValue(mergedEnv, "BC_SEED");

const lucid = await Lucid(new Blockfrost(BLOCKFROST_URL, BLOCKFROST_KEY), "Preprod");
const isCheckMode = process.argv.includes("--check");

if (isCheckMode) {
  await runCheckMode(lucid);
} else {
  await runTopupMode(lucid);
}
