import { Blockfrost, Kupmios, Lucid, Network } from "@lucid-evolution/lucid";
import { Config, Context, Effect, Layer, pipe } from "effect";
import pg from "pg";
import { InitDB } from "@/database/index.js";

const SUPPORTED_PROVIDERS = ["kupmios", "blockfrost"] as const;
type Provider = (typeof SUPPORTED_PROVIDERS)[number];

const isValidProvider = (provider: string): provider is Provider => {
  return SUPPORTED_PROVIDERS.includes(provider.toLowerCase() as Provider);
};

export type NodeConfigDep = {
  L1_PROVIDER: Provider;
  L1_BLOCKFROST_API_URL: string;
  L1_BLOCKFROST_KEY: string;
  L1_OGMIOS_KEY: string;
  L1_KUPO_KEY: string;
  L1_OPERATOR_SEED_PHRASE: string;
  NETWORK: Network;
  PORT: number;
  POLLING_INTERVAL: number;
  CONFIRMED_STATE_POLLING_INTERVAL: number;
  PROM_METRICS_PORT: number;
  OLTP_EXPORTER_URL: string;
  DB_CONN: pg.Pool;
};

export const makeUserFn = (nodeConfig: NodeConfigDep) =>
  Effect.gen(function* () {
    const user = yield* Effect.tryPromise(() => {
      switch (nodeConfig.L1_PROVIDER) {
        case "kupmios":
          return Lucid(
            new Kupmios(nodeConfig.L1_KUPO_KEY, nodeConfig.L1_OGMIOS_KEY),
            nodeConfig.NETWORK,
          );
        case "blockfrost":
          return Lucid(
            new Blockfrost(
              nodeConfig.L1_BLOCKFROST_API_URL,
              nodeConfig.L1_BLOCKFROST_KEY,
            ),
            nodeConfig.NETWORK,
          );
      }
    });
    user.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE);
    yield* pipe(
      Effect.promise(() => user.wallet().address()),
      Effect.flatMap((address) => Effect.log(`Wallet : ${address}`)),
    );
    yield* pipe(
      Effect.promise(() => user.wallet().getUtxos()),
      Effect.flatMap((utxos) =>
        Effect.log(`Total Wallet UTxOs: ${utxos.length}`),
      ),
    );
    return {
      user,
    };
  });

const makeUser = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  return yield* makeUserFn(nodeConfig);
}).pipe(Effect.orDie);

export class User extends Context.Tag("User")<
  User,
  Effect.Effect.Success<typeof makeUser>
>() {
  static readonly layer = Layer.effect(User, makeUser);
}

export const NETWORK: Network = "Preprod";

export const makeConfig = Effect.gen(function* () {
  const config = yield* Config.all([
    Config.string("L1_PROVIDER"),
    Config.string("L1_BLOCKFROST_API_URL"),
    Config.string("L1_BLOCKFROST_KEY"),
    Config.string("L1_OGMIOS_KEY"),
    Config.string("L1_KUPO_KEY"),
    Config.string("L1_OPERATOR_SEED_PHRASE"),
    Config.string("NETWORK"),
    Config.integer("PORT").pipe(Config.withDefault(3000)),
    Config.integer("POLLING_INTERVAL").pipe(Config.withDefault(10000)),
    Config.integer("CONFIRMED_STATE_POLLING_INTERVAL").pipe(
      Config.withDefault(60000),
    ),
    Config.integer("PROM_METRICS_PORT").pipe(Config.withDefault(9464)),
    Config.string("OLTP_EXPORTER_URL").pipe(
      Config.withDefault("http://0.0.0.0:4318/v1/traces"),
    ),
    Config.string("POSTGRES_HOST").pipe(Config.withDefault("postgres")), // service name
    Config.string("POSTGRES_USER").pipe(Config.withDefault("postgres")),
    Config.string("POSTGRES_PASSWORD").pipe(Config.withDefault("postgres")),
    Config.string("POSTGRES_DB").pipe(Config.withDefault("midgard")),
  ]);

  yield* Effect.logInfo("📚 Opening connection to db...");

  const pool = new pg.Pool({
    host: config[12],
    user: config[13],
    password: config[14],
    database: config[15],
    max: 20,
    idleTimeoutMillis: 30000,
    connectionTimeoutMillis: 2000,
  });

  yield* Effect.tryPromise({
    try: () => InitDB.initializeDb(pool),
    catch: (e) => new Error(`${e}`),
  });

  yield* Effect.logInfo("📚 Done");

  const provider = config[0].toLowerCase();
  if (!isValidProvider(provider)) {
    throw new Error(
      `Invalid L1_PROVIDER: ${provider}. Supported providers: ${SUPPORTED_PROVIDERS.join(", ")}`,
    );
  }
  return {
    L1_PROVIDER: provider,
    L1_BLOCKFROST_API_URL: config[1],
    L1_BLOCKFROST_KEY: config[2],
    L1_OGMIOS_KEY: config[3],
    L1_KUPO_KEY: config[4],
    L1_OPERATOR_SEED_PHRASE: config[5],
    NETWORK: config[6] as Network,
    PORT: config[7],
    POLLING_INTERVAL: config[8],
    CONFIRMED_STATE_POLLING_INTERVAL: config[9],
    PROM_METRICS_PORT: config[10],
    OLTP_EXPORTER_URL: config[11],
    DB_CONN: pool,
  };
}).pipe(Effect.orDie);

export class NodeConfig extends Context.Tag("NodeConfig")<
  NodeConfig,
  NodeConfigDep
>() {
  static readonly layer = Layer.effect(NodeConfig, makeConfig);
}
