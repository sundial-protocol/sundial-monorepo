import path from "node:path";
import { fileURLToPath } from "node:url";
import { defineConfig } from "vitest/config";

const demoRoot = path.resolve(fileURLToPath(new URL(".", import.meta.url)), "..");

// midgard-ts unit tests exercise the pure-TypeScript codec helpers in isolation.
// No external network calls, no Docker, no CML provider.  All tests are
// deterministic and use in-memory fixtures.
//
// How to run:
//   npm --prefix demo run test:unit:ts
export default defineConfig({
  resolve: {
    alias: {
      "@": path.resolve(demoRoot, "midgard-ts/src"),
    },
  },
  test: {
    include: ["midgard-ts/tests/unit/**/*.test.ts"],
    environment: "node",
    globals: true,
    reporters: "verbose",
    testTimeout: 30_000,
  },
});
