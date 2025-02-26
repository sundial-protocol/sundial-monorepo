import { Effect } from "effect";
import * as S from "@effect/schema/Schema";

// Schema for our configuration
const configSchema = S.Struct({
  // Node configuration
  node: S.Struct({
    endpoint: S.String.pipe(S.pattern(/^https?:\/\/.+/)),
  }),

  // Transaction generator configuration
  generator: S.Struct({
    enabled: S.Boolean,
    maxConcurrent: S.Number.pipe(S.positive(), S.int()),
    batchSize: S.Number.pipe(S.positive(), S.int()),
    intervalMs: S.Number.pipe(S.positive(), S.int()),
  }),

  // Logging configuration
  logging: S.Struct({
    level: S.Literal("debug", "info", "warn", "error"),
    format: S.Literal("json", "pretty"),
  }),
});

// Type for our configuration
export type MidgardConfig = S.Schema.Type<typeof configSchema>;

// Default configuration
export const defaultConfig: MidgardConfig = {
  node: {
    endpoint: "http://localhost:3000",
  },
  generator: {
    enabled: true,
    maxConcurrent: 10,
    batchSize: 100,
    intervalMs: 1000,
  },
  logging: {
    level: "info",
    format: "pretty",
  },
};

// Configuration errors
export class ConfigError {
  readonly _tag = "ConfigError";
  constructor(readonly message: string, readonly cause?: unknown) {}
}

// Load and validate configuration
export const loadConfig = Effect.gen(function* (_) {
  // Load config from environment
  const { loadConfigFromEnv } = yield* _(
    Effect.promise(() => import("./env.js"))
  );
  const config = yield* _(loadConfigFromEnv);

  // Validate with schema
  try {
    const result = S.decodeSync(configSchema)(config);
    return result;
  } catch (error) {
    return yield* _(
      Effect.fail(new ConfigError("Invalid configuration", error))
    );
  }
});
