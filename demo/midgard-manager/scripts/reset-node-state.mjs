import { readFile } from "node:fs/promises";
import path from "node:path";
import { spawn } from "node:child_process";
import process from "node:process";

const MODES = new Set(["mempool", "submit-blocks", "postgres", "auto"]);
const DEFAULT_MODE = "auto";
const DEFAULT_DB_USER = "postgres";
const DEFAULT_DB_NAME = "midgard";
const DEFAULT_COMPOSE_FILE = "docker-compose.yaml";

const MEMPOOL_TABLES = ["mempool", "mempool_ledger"];
const SUBMIT_BLOCKS_TABLES = ["unsubmitted_blocks"];
const POSTGRES_RESET_TABLES = [
  "mempool",
  "mempool_ledger",
  "unsubmitted_blocks",
  "blocks_txs",
  "immutable",
  "latest_ledger",
  "confirmed_ledger",
  "address_history",
  "deposits_utxos",
  "transaction_order_utxos",
  "withdrawal_order_utxos",
];

function parseArgs(argv) {
  const args = { mode: DEFAULT_MODE };
  for (let i = 2; i < argv.length; i += 1) {
    const current = argv[i];
    if (current === "--mode") {
      const next = argv[i + 1];
      if (next === undefined) {
        throw new Error("Missing value for --mode");
      }
      i += 1;
      args.mode = next;
      continue;
    }
    if (current === "--help" || current === "-h") {
      args.help = true;
      continue;
    }
    throw new Error(`Unknown argument: ${current}`);
  }
  return args;
}

function parseDotEnv(text) {
  const out = {};
  const lines = text.split(/\r?\n/);
  for (const line of lines) {
    const trimmed = line.trim();
    if (trimmed.length === 0 || trimmed.startsWith("#")) {
      continue;
    }
    const eq = trimmed.indexOf("=");
    if (eq <= 0) {
      continue;
    }
    const key = trimmed.slice(0, eq).trim();
    let value = trimmed.slice(eq + 1).trim();
    if (
      (value.startsWith('"') && value.endsWith('"')) ||
      (value.startsWith("'") && value.endsWith("'"))
    ) {
      value = value.slice(1, -1);
    }
    out[key] = value;
  }
  return out;
}

async function readNodeEnv(midgardNodeDir) {
  const envPath = path.join(midgardNodeDir, ".env");
  try {
    return parseDotEnv(await readFile(envPath, "utf8"));
  } catch {
    const examplePath = path.join(midgardNodeDir, ".env.example");
    return parseDotEnv(await readFile(examplePath, "utf8"));
  }
}

function runCommand(command, args, options = {}) {
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, {
      stdio: "pipe",
      ...options,
    });

    let stdout = "";
    let stderr = "";

    child.stdout?.on("data", (chunk) => {
      const text = chunk.toString();
      stdout += text;
      process.stdout.write(text);
    });

    child.stderr?.on("data", (chunk) => {
      const text = chunk.toString();
      stderr += text;
      process.stderr.write(text);
    });

    child.on("error", reject);
    child.on("close", (code) => resolve({ code: code ?? 1, stdout, stderr }));
  });
}

function buildTruncateSql(tableNames) {
  return `TRUNCATE TABLE ${tableNames.join(", ")};`;
}

async function runPsqlTruncate({ composeFilePath, dbUser, dbName, tableNames, cwd }) {
  const sql = buildTruncateSql(tableNames);
  const args = [
    "compose",
    "-f",
    composeFilePath,
    "exec",
    "-T",
    "postgres",
    "psql",
    "-v",
    "ON_ERROR_STOP=1",
    "-U",
    dbUser,
    "-d",
    dbName,
    "-c",
    sql,
  ];
  return runCommand("docker", args, { cwd });
}

function printUsage() {
  console.log(`Usage: node scripts/reset-node-state.mjs [--mode <mempool|submit-blocks|postgres|auto>]

Modes:
  mempool   Truncate only mempool tables (mempool, mempool_ledger)
  submit-blocks  Truncate only pending block submission queue (unsubmitted_blocks)
  postgres  Truncate all runtime Postgres tables used by midgard-node
  auto      Try mempool cleanup first; if it fails, run full Postgres reset
`);
}

async function main() {
  const args = parseArgs(process.argv);
  if (args.help) {
    printUsage();
    return;
  }
  if (!MODES.has(args.mode)) {
    throw new Error(
      `Invalid --mode ${args.mode}. Expected one of: ${Array.from(MODES).join(", ")}`
    );
  }

  const managerDir = process.cwd();
  const midgardNodeDir = path.resolve(managerDir, "..", "midgard-node");
  const composeFilePath = path.join(midgardNodeDir, DEFAULT_COMPOSE_FILE);
  const nodeEnv = await readNodeEnv(midgardNodeDir);
  const dbUser = nodeEnv.POSTGRES_USER ?? DEFAULT_DB_USER;
  const dbName = nodeEnv.POSTGRES_DB ?? DEFAULT_DB_NAME;

  const runMempoolReset = () =>
    runPsqlTruncate({
      composeFilePath,
      dbUser,
      dbName,
      tableNames: MEMPOOL_TABLES,
      cwd: midgardNodeDir,
    });
  const runSubmitBlocksReset = () =>
    runPsqlTruncate({
      composeFilePath,
      dbUser,
      dbName,
      tableNames: SUBMIT_BLOCKS_TABLES,
      cwd: midgardNodeDir,
    });
  const runPostgresReset = () =>
    runPsqlTruncate({
      composeFilePath,
      dbUser,
      dbName,
      tableNames: POSTGRES_RESET_TABLES,
      cwd: midgardNodeDir,
    });

  if (args.mode === "mempool") {
    console.log("Attempting mempool-only cleanup...");
    const result = await runMempoolReset();
    if (result.code !== 0) {
      throw new Error("Mempool cleanup failed");
    }
    console.log("Mempool cleanup completed.");
    return;
  }

  if (args.mode === "submit-blocks") {
    console.log("Attempting pending submit-blocks cleanup...");
    const result = await runSubmitBlocksReset();
    if (result.code !== 0) {
      throw new Error("Submit-blocks cleanup failed");
    }
    console.log("Submit-blocks cleanup completed.");
    return;
  }

  if (args.mode === "postgres") {
    console.log("Attempting full Postgres runtime reset...");
    const result = await runPostgresReset();
    if (result.code !== 0) {
      throw new Error("Postgres reset failed");
    }
    console.log("Postgres reset completed.");
    return;
  }

  console.log("Attempting mempool-only cleanup...");
  const mempoolResult = await runMempoolReset();
  if (mempoolResult.code === 0) {
    console.log("Mempool cleanup completed.");
    return;
  }

  console.log("Mempool cleanup failed; attempting full Postgres runtime reset...");
  const pgResult = await runPostgresReset();
  if (pgResult.code !== 0) {
    throw new Error("Postgres reset fallback failed");
  }
  console.log("Postgres reset completed.");
}

main().catch((error) => {
  console.error(`reset-node-state failed: ${error instanceof Error ? error.message : String(error)}`);
  process.exit(1);
});
