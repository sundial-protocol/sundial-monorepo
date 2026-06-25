import { defineConfig } from "tsup";

export default defineConfig({
  entry: ["src/index.ts"],
  format: ["esm", "cjs"],
  shims: true,
  dts: true,
  clean: false,
  sourcemap: true,
  external: ["@dcspark/cardano-multiplatform-lib-nodejs"],
  outExtension({ format }) {
    return {
      js: format === "cjs" ? ".cjs" : ".js",
    };
  },
});
