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
  },
});
