import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    pool: "forks",
    reporters: [["default", { summary: false }]],
    include: ["./tests/e2e/**/*.test.{js,mjs,cjs,ts,mts,cts,jsx,tsx}"],
    testTimeout: 420_000,
    hookTimeout: 420_000,
    teardownTimeout: 420_000,
    environment: "node",
  },
  esbuild: {
    target: "es2020",
  },
});
