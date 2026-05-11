import path from "node:path";
import { fileURLToPath } from "node:url";
import { defineConfig } from "vitest/config";

// Root of the demo/ workspace (one level up from demo/vitest/).
const demoRoot = path.resolve(
  fileURLToPath(new URL(".", import.meta.url)),
  "..",
);

// Integration tests wire real internal components (repositories, SQL schema,
// MPT persistence) and replace only the true external L1/provider boundary.
//
// Key differences from node.unit.config.mts:
//   - NO alias for @/database/utils/ledger.js — real ledger SQL runs.
//   - NO alias for @/database/utils/tx.js    — real tx SQL runs.
//   - Lucid and SDK are still aliased to integration stubs that return
//     deterministic fixtures without contacting Cardano testnet/mainnet.
//   - Tests are expected to supply their own SqlClient layer (PGlite or a
//     disposable test database) via Effect layer composition inside each test.
//
// Required packages for full implementation (add to midgard-node devDependencies):
//   - @electric-sql/pglite       — in-process Postgres-compatible storage
//   - @effect/sql-pglite (or a custom wrapper bridging PGlite to SqlClient)
export default defineConfig({
  resolve: {
    alias: {
      // Silence chalk colour output in test logs.
      chalk: path.resolve(
        demoRoot,
        "midgard-node/tests/integration/stubs/chalk.stub.ts",
      ),
      // Stub the full Midgard SDK at the L1/provider boundary.
      "@al-ft/midgard-sdk": path.resolve(
        demoRoot,
        "midgard-node/tests/integration/stubs/midgard-sdk.stub.ts",
      ),
      // Stub Lucid / CML so no live Cardano node calls are made.
      "@lucid-evolution/lucid": path.resolve(
        demoRoot,
        "midgard-node/tests/integration/stubs/lucid.stub.ts",
      ),
      // Source alias — maps @/* imports to the midgard-node src tree.
      "@": path.resolve(demoRoot, "midgard-node/src"),
    },
  },
  test: {
    include: ["midgard-node/tests/integration/**/*.test.ts"],
    environment: "node",
    reporters: [["default", { summary: false }]],
    // Integration tests may open LevelDB or temporary directories; allow
    // sufficient time per file.
    testTimeout: 30_000,
  },
});
