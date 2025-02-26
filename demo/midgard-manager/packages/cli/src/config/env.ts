import { Effect } from "effect";
import * as dotenv from "dotenv";
import { ConfigError, MidgardConfig, defaultConfig } from "./schema.js";
import * as fs from "node:fs/promises";
import * as path from "node:path";

// Load environment variables from .env file
const loadEnvFile = Effect.tryPromise({
  try: async () => {
    try {
      // Try to load .env file
      const envPath = path.resolve(process.cwd(), ".env");
      await fs.access(envPath);
      return dotenv.config({ path: envPath });
    } catch {
      // .env file doesn't exist, that's fine
      return undefined;
    }
  },
  catch: (error) => new ConfigError("Failed to load .env file", error),
});

// Get value from environment with fallback
const getEnvValue = (key: string, fallback: string): string => {
  return process.env[key] ?? fallback;
};

// Load configuration from environment
export const loadConfigFromEnv = Effect.gen(function* (_) {
  // Load .env file first
  yield* _(loadEnvFile);

  // Build configuration from environment variables with fallbacks
  const config: MidgardConfig = {
    node: {
      endpoint: getEnvValue(
        "MIDGARD_NODE_ENDPOINT",
        defaultConfig.node.endpoint
      ),
    },
    generator: {
      enabled:
        getEnvValue(
          "MIDGARD_GENERATOR_ENABLED",
          String(defaultConfig.generator.enabled)
        ).toLowerCase() === "true",
      maxConcurrent: Number(
        getEnvValue(
          "MIDGARD_MAX_CONCURRENT",
          String(defaultConfig.generator.maxConcurrent)
        )
      ),
      batchSize: Number(
        getEnvValue(
          "MIDGARD_BATCH_SIZE",
          String(defaultConfig.generator.batchSize)
        )
      ),
      intervalMs: Number(
        getEnvValue(
          "MIDGARD_INTERVAL_MS",
          String(defaultConfig.generator.intervalMs)
        )
      ),
    },
    logging: {
      level: getEnvValue(
        "MIDGARD_LOG_LEVEL",
        defaultConfig.logging.level
      ) as MidgardConfig["logging"]["level"],
      format: getEnvValue(
        "MIDGARD_LOG_FORMAT",
        defaultConfig.logging.format
      ) as MidgardConfig["logging"]["format"],
    },
  };

  return config;
});
