import path from "node:path";
import { fileURLToPath } from "node:url";
import { defineConfig } from "vitest/config";

const demoRoot = path.resolve(fileURLToPath(new URL(".", import.meta.url)), "..");

export default defineConfig({
  resolve: {
    alias: {
      chalk: path.resolve(demoRoot, "midgard-node/tests/unit/stubs/chalk.stub.ts"),
      "@al-ft/midgard-sdk": path.resolve(
        demoRoot,
        "midgard-node/tests/unit/stubs/midgard-sdk.stub.ts",
      ),
      "@lucid-evolution/lucid": path.resolve(
        demoRoot,
        "midgard-node/tests/unit/stubs/lucid.stub.ts",
      ),
      "@/database/utils/ledger.js": path.resolve(
        demoRoot,
        "midgard-node/tests/unit/stubs/ledger.stub.ts",
      ),
      "@": path.resolve(demoRoot, "midgard-node/src"),
    },
  },
  test: {
    include: ["midgard-node/tests/unit/**/*.test.ts"],
    environment: "node",
    coverage: {
      provider: "v8",
      reporter: [["text", { maxCols: 160 }], "html", "json-summary"],
      reportsDirectory: "./coverage/node",
      include: ["midgard-node/src/**/*.ts"],
      exclude: [
        // Tests
        "midgard-node/tests/**",
        // CLI entry points and scripts — no unit-testable logic
        "midgard-node/src/index.ts",
        "midgard-node/src/genesis.ts",
        "midgard-node/src/reset.ts",
        "midgard-node/src/commands/**",
        // Barrel re-exports — no logic, skew function counts to 0%
        "midgard-node/src/services/index.ts",
        "midgard-node/src/transactions/index.ts",
        "midgard-node/src/transactions/state-queue/index.ts",
        "midgard-node/src/fibers/index.ts",
        // Effect service wiring — require live Postgres / L1 blockchain
        "midgard-node/src/services/database.ts",
        "midgard-node/src/services/lucid.ts",
        "midgard-node/src/services/always-succeeds.ts",
        // Fibers — long-running Effect loops, integration-test territory
        "midgard-node/src/fibers/block-commitment.ts",
        "midgard-node/src/fibers/block-submission.ts",
        "midgard-node/src/fibers/merge.ts",
        "midgard-node/src/fibers/monitor-mempool.ts",
        "midgard-node/src/fibers/sync-user-events.ts",
        "midgard-node/src/fibers/tx-queue-processor.ts",
        // Worker thread orchestrator — runs in worker_threads context
        "midgard-node/src/workers/block-commitment.ts",
        // SQL-level DB modules — use SqlClient directly, integration-test territory
        "midgard-node/src/database/blocks.ts",
        "midgard-node/src/database/blocksTxs.ts",
      ],
      thresholds: {
        lines: 65,
        statements: 65,
        branches: 65,
        functions: 65,
      },
    },
  },
});
