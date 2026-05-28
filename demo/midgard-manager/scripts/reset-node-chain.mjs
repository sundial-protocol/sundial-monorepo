import { readFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const RESET_PATH = "/reset";
const DEFAULT_ENDPOINT = "http://localhost:3000";
const TIMEOUT_MS = 700_000;

async function readNodeEndpoint() {
  const settingsPath = path.resolve(__dirname, "../config/settings.json");
  try {
    const text = await readFile(settingsPath, "utf8");
    const settings = JSON.parse(text);
    return settings?.node?.endpoint ?? DEFAULT_ENDPOINT;
  } catch {
    return DEFAULT_ENDPOINT;
  }
}

async function main() {
  const endpoint = await readNodeEndpoint();
  const url = `${endpoint.replace(/\/$/, "")}${RESET_PATH}`;

  console.log(`reset-node-chain: calling ${url} ...`);

  let response;
  try {
    response = await fetch(url, {
      signal: AbortSignal.timeout(TIMEOUT_MS),
    });
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    console.error(`reset-node-chain: request failed — ${message}`);
    process.exit(1);
  }

  let body;
  try {
    body = await response.text();
  } catch {
    body = "(could not read response body)";
  }

  if (!response.ok) {
    console.error(
      `reset-node-chain: node returned HTTP ${response.status} ${response.statusText}`
    );
    if (body.trim().length > 0) {
      console.error(`reset-node-chain: response body — ${body.trim()}`);
    }
    process.exit(1);
  }

  console.log(`reset-node-chain: reset completed (HTTP ${response.status}).`);
  if (body.trim().length > 0) {
    console.log(`reset-node-chain: ${body.trim()}`);
  }
}

main().catch((err) => {
  console.error(
    `reset-node-chain: unexpected error — ${err instanceof Error ? err.message : String(err)}`
  );
  process.exit(1);
});
