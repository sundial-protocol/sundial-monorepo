import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const demoRoot = path.resolve(fileURLToPath(new URL(".", import.meta.url)), "..");
const MODULES_DIR = "dist/modules-sumo-esm";
const WRAPPER_FILE_NAME = "libsodium-sumo.mjs";

const maybeRepairWrapperImport = (wrapperDir, sodiumFile) => {
  const wrapperFile = path.resolve(wrapperDir, WRAPPER_FILE_NAME);
  if (fs.existsSync(wrapperFile) || !fs.existsSync(sodiumFile)) {
    return;
  }

  fs.mkdirSync(wrapperDir, { recursive: true });
  const relativeTarget = path.relative(wrapperDir, sodiumFile);
  try {
    fs.symlinkSync(relativeTarget, wrapperFile);
  } catch {
    fs.copyFileSync(sodiumFile, wrapperFile);
  }
};

const staticCandidates = [
  {
    wrapperDir: path.resolve(
      demoRoot,
      `midgard-sdk/node_modules/libsodium-wrappers-sumo/${MODULES_DIR}`,
    ),
    sodiumFile: path.resolve(
      demoRoot,
      `midgard-sdk/node_modules/libsodium-sumo/${MODULES_DIR}/${WRAPPER_FILE_NAME}`,
    ),
  },
  {
    wrapperDir: path.resolve(
      demoRoot,
      `node_modules/libsodium-wrappers-sumo/${MODULES_DIR}`,
    ),
    sodiumFile: path.resolve(
      demoRoot,
      `node_modules/libsodium-sumo/${MODULES_DIR}/${WRAPPER_FILE_NAME}`,
    ),
  },
];

for (const candidate of staticCandidates) {
  maybeRepairWrapperImport(candidate.wrapperDir, candidate.sodiumFile);
}

const pnpmRoot = path.resolve(demoRoot, "node_modules/.pnpm");
if (fs.existsSync(pnpmRoot)) {
  const entries = fs.readdirSync(pnpmRoot, { withFileTypes: true });
  const sodiumByVersion = new Map();

  for (const entry of entries) {
    if (!entry.isDirectory() || !entry.name.startsWith("libsodium-sumo@")) {
      continue;
    }
    const version = entry.name.slice("libsodium-sumo@".length).split("_")[0];
    const sodiumFile = path.resolve(
      pnpmRoot,
      entry.name,
      `node_modules/libsodium-sumo/${MODULES_DIR}/${WRAPPER_FILE_NAME}`,
    );
    if (fs.existsSync(sodiumFile)) {
      sodiumByVersion.set(version, sodiumFile);
    }
  }

  for (const entry of entries) {
    if (
      !entry.isDirectory() ||
      !entry.name.startsWith("libsodium-wrappers-sumo@")
    ) {
      continue;
    }
    const version = entry.name
      .slice("libsodium-wrappers-sumo@".length)
      .split("_")[0];
    const sodiumFile =
      sodiumByVersion.get(version) ?? [...sodiumByVersion.values()][0];
    if (!sodiumFile) {
      continue;
    }
    const wrapperDir = path.resolve(
      pnpmRoot,
      entry.name,
      `node_modules/libsodium-wrappers-sumo/${MODULES_DIR}`,
    );
    maybeRepairWrapperImport(wrapperDir, sodiumFile);
  }
}
