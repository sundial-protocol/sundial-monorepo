import { defineConfig } from "vitest/config";
import path from "path";

export default defineConfig({
  test: {
    pool: "forks",
    reporters: [["default", { summary: false }]],
    include: ["./tests/**/*.test.{js,mjs,cjs,ts,mts,cts,jsx,tsx}"],
    exclude: ["./tests/unit/**", "./tests/integration/**"],
    setupFiles: [path.resolve(__dirname, "../vitest/sdk.unit.setup.mjs")],
    testTimeout: 420_000,
    hookTimeout: 420_000,
    teardownTimeout: 420_000,
    bail: 3,
    environment: "node",
  },
  resolve: {
    alias: {
      "@al-ft/midgard-sdk": path.resolve(
        __dirname,
        "./tests/unit/stubs/midgard-sdk.stub.ts",
      ),
      "@": path.resolve(__dirname, "./src"),
    },
  },
  esbuild: {
    target: "es2020",
  },
});
