import path from "node:path";
import { fileURLToPath } from "node:url";
import { defineConfig } from "vitest/config";

const nodeRoot = path.resolve(fileURLToPath(new URL(".", import.meta.url)));

export default defineConfig({
  resolve: {
    alias: {
      chalk: path.resolve(nodeRoot, "tests/unit/stubs/chalk.stub.ts"),
      "@al-ft/midgard-sdk": path.resolve(
        nodeRoot,
        "tests/unit/stubs/midgard-sdk.stub.ts",
      ),
      "@lucid-evolution/lucid": path.resolve(
        nodeRoot,
        "tests/unit/stubs/lucid.stub.ts",
      ),
      "@/database/utils/ledger.js": path.resolve(
        nodeRoot,
        "tests/unit/stubs/ledger.stub.ts",
      ),
      "@": path.resolve(nodeRoot, "src"),
    },
  },
  test: {
    include: ["tests/unit/**/*.test.ts"],
    environment: "node",
    reporters: [["default", { summary: false }]],
  },
});
