import path from "node:path";
import { fileURLToPath } from "node:url";
import { defineConfig } from "vitest/config";

// Root of the demo/ workspace (one level up from demo/vitest/).
const demoRoot = path.resolve(
  fileURLToPath(new URL(".", import.meta.url)),
  "..",
);

// SDK integration tests wire real SDK exports, real Effect programs, real Lucid
// Data schemas, real CML serialization, real node repositories, and real MPT
// helpers.  They replace only the true external L1/provider/network boundary.
//
// Integration boundary (per testing/sdk-integration-test-plan.md):
//   Real:   @sdk/**  (midgard-sdk source), @node/** (midgard-node source),
//           Effect programs, Lucid Data schemas, CML serialization,
//           node repositories, schema init, MPT persistence.
//   Faked:  External Lucid/Cardano L1 edge (deterministic fake from harness).
//           HTTP provider (localhost fake server in HTTP boundary mode tests).
//           Transaction submission (fake submit recorder in harness).
//
// Required packages for full implementation (add to midgard-node devDependencies
// if not already present — midgard-node exec provides the vitest runtime):
//   @electric-sql/pglite       — in-process Postgres-compatible storage engine
//   @effect/sql-pglite         — Effect SQL adapter bridging PGlite to SqlClient
//
// How to run:
//   npm --prefix demo run test:integration:sdk
//   (resolves as: pnpm --dir midgard-node exec vitest run --root .. --config
//    vitest/sdk.integration.config.mts)
export default defineConfig({
  resolve: {
    alias: {
      // Silence chalk colour output in test logs.
      chalk: path.resolve(
        demoRoot,
        "midgard-node/tests/integration/stubs/chalk.stub.ts",
      ),
      // @sdk/** → midgard-sdk source tree.
      // Tests import real SDK exports; no stub is applied here.
      "@sdk": path.resolve(demoRoot, "midgard-sdk/src"),
      // Internal SDK alias used by source modules (e.g. "@/common.js").
      "@": path.resolve(demoRoot, "midgard-sdk/src"),
      // @node/** → midgard-node source tree.
      // Integration tests may reach into node repository layers directly.
      "@node": path.resolve(demoRoot, "midgard-node/src"),
      // @effect/vitest lives in midgard-node/node_modules (not the SDK's).
      // The test files live under midgard-sdk/ where it is not installed, so
      // the alias bridges the gap without requiring a separate npm install.
      "@effect/vitest": path.resolve(
        demoRoot,
        "midgard-node/node_modules/@effect/vitest",
      ),
    },
  },
  test: {
    include: ["midgard-sdk/tests/integration/**/*.test.ts"],
    setupFiles: ["vitest/sdk.unit.setup.mjs"],
    environment: "node",
    reporters: [["default", { summary: false }]],
    // Integration tests may open LevelDB or temporary directories; allow
    // sufficient time per file.
    testTimeout: 30_000,
  },
});
