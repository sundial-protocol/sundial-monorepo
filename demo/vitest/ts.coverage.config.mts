import path from "node:path";
import { fileURLToPath } from "node:url";
import { defineConfig } from "vitest/config";

const demoRoot = path.resolve(fileURLToPath(new URL(".", import.meta.url)), "..");

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
    coverage: {
      provider: "v8",
      reporter: [["text", { maxCols: 160 }], "html", "json-summary"],
      reportsDirectory: "./coverage/ts",
      include: ["midgard-ts/src/**/*.ts"],
      exclude: [
        "midgard-ts/tests/**",
        // Pure type definitions + RejectCodes const (enum pattern) — no executable logic
        "midgard-ts/src/validation/types.ts",
        // Barrel re-export only
        "midgard-ts/src/validation/index.ts",
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
