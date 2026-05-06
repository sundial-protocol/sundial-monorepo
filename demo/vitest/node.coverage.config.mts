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
      exclude: ["midgard-node/tests/**"],
      thresholds: {
        lines: 10,
        statements: 10,
        branches: 10,
        functions: 10,
      },
    },
  },
});
