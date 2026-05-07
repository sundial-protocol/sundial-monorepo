import path from "node:path";
import { fileURLToPath } from "node:url";
import { defineConfig } from "vitest/config";

const demoRoot = path.resolve(fileURLToPath(new URL(".", import.meta.url)), "..");

// midgard-ts integration tests exercise the pure-TypeScript codec end-to-end,
// including CML-backed Cardano round-trips.  No external network calls are made.
//
// How to run:
//   npm --prefix demo run test:integration:ts
export default defineConfig({
  resolve: {
    alias: {
      "@": path.resolve(demoRoot, "midgard-ts/src"),
    },
  },
  test: {
    include: ["midgard-ts/tests/**/*.test.ts"],
    environment: "node",
    globals: true,
    reporters: [["default", { summary: false }]],
    testTimeout: 30_000,
    coverage: {
      thresholds: {
        lines: 0,
        statements: 0,
        branches: 0,
        functions: 0,
      },
    },
  },
});
