import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const demoRoot = path.resolve(fileURLToPath(new URL(".", import.meta.url)), "..");
const wrapperDir = path.resolve(
  demoRoot,
  "midgard-sdk/node_modules/libsodium-wrappers-sumo/dist/modules-sumo-esm",
);
const wrapperFile = path.resolve(wrapperDir, "libsodium-sumo.mjs");
const sodiumFile = path.resolve(
  demoRoot,
  "midgard-sdk/node_modules/libsodium-sumo/dist/modules-sumo-esm/libsodium-sumo.mjs",
);

if (!fs.existsSync(wrapperFile) && fs.existsSync(sodiumFile)) {
  fs.mkdirSync(wrapperDir, { recursive: true });
  const relativeTarget = path.relative(wrapperDir, sodiumFile);
  try {
    fs.symlinkSync(relativeTarget, wrapperFile);
  } catch {
    fs.copyFileSync(sodiumFile, wrapperFile);
  }
}
