import path from "node:path";
import { fileURLToPath } from "node:url";
import { defineConfig } from "vitest/config";

const demoRoot = path.resolve(fileURLToPath(new URL(".", import.meta.url)), "..");

export default defineConfig({
  resolve: {
    alias: [
      {
        find: "@",
        replacement: path.resolve(demoRoot, "midgard-sdk/src"),
      },
    ],
  },
  test: {
    include: ["midgard-sdk/tests/unit/**/*.test.ts"],
    setupFiles: ["vitest/sdk.unit.setup.mjs"],
    environment: "node",
    reporters: "verbose",
    coverage: {
      provider: "v8",
      reporter: [["text", { maxCols: 160 }], "html", "json-summary"],
      reportsDirectory: "./coverage/sdk",
      include: ["midgard-sdk/src/**/*.ts"],
      exclude: [
        "midgard-sdk/tests/**",
        // Pure on-chain schema definitions and empty stub implementations — no testable logic
        "midgard-sdk/src/fraud-proof/tokens.ts",
        "midgard-sdk/src/fraud-proof/computation-threads.ts",
      ],
      thresholds: {
        lines: 10,
        statements: 10,
        branches: 10,
        functions: 10,
      },
    },
  },
});
