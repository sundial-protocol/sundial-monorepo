import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { Effect, pipe } from "effect";
import * as S from "@effect/schema/Schema";
import { config } from "dotenv";
import { MidgardConfig } from "../types/config.js";
import { MidgardError } from "../utils/errors.js";

// Load environment variables
config();

// Default configuration based on environment variables
const defaultConfig: S.Schema.To<typeof MidgardConfig> = {
  nodeEndpoint: process.env.NODE_ENDPOINT || "http://localhost:3000",
  txGenerator: {
    enabled: process.env.TX_GENERATOR_ENABLED === "true",
    interval: parseInt(process.env.TX_GENERATOR_INTERVAL || "5000", 10),
  },
  settings: {
    debug: false,
    logLevel: "info",
  },
};

// Load configuration
export const loadConfig = Effect.try({
  try: () => {
    if (!existsSync("./config.json")) {
      writeFileSync("./config.json", JSON.stringify(defaultConfig, null, 2));
      return defaultConfig;
    }

    const configFile = readFileSync("./config.json", "utf-8");
    return pipe(JSON.parse(configFile), S.parseSync(MidgardConfig));
  },
  catch: (error) => {
    throw MidgardError.config(`Failed to load config: ${error}`);
  },
});

// Save configuration
export const saveConfig = (config: S.Schema.To<typeof MidgardConfig>) =>
  Effect.try({
    try: () => {
      writeFileSync("./config.json", JSON.stringify(config, null, 2));
      return config;
    },
    catch: (error) => {
      throw MidgardError.config(`Failed to save config: ${error}`);
    },
  });
