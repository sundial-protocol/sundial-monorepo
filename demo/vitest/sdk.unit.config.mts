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
  },
});
