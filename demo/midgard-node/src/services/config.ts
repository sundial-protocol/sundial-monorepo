import { Network, UTxO, walletFromSeed } from "@lucid-evolution/lucid";
import { Config, Context, Data, Effect, Layer, Option } from "effect";
import * as SDK from "@al-ft/midgard-sdk";

type Provider = "Kupmios" | "Blockfrost";
export type NodeRole = "all" | "api" | "tx-processor" | "sequencer";

type NodeConfigDep = {
  L1_PROVIDER: Provider;
  L1_BLOCKFROST_API_URL: string;
  L1_BLOCKFROST_KEY: string;
  L1_OGMIOS_KEY: string;
  L1_KUPO_KEY: string;
  L1_OPERATOR_SEED_PHRASE: string;
  L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT: string;
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: string;
  NETWORK: Network;
  PROTOCOL_PARAMETERS: SDK.ProtocolParameters;
  PORT: number;
  WAIT_BETWEEN_BLOCK_COMMITMENTS: number;
  WAIT_BETWEEN_BLOCK_SUBMISSIONS: number;
  WAIT_BETWEEN_USER_EVENT_FETCHES: number;
  WAIT_BETWEEN_MERGE_TXS: number;
  COMMITMENT_WORKER_TIMEOUT_MS: number;
  COMMITMENT_WINDOW_WARN_TX_REQUESTS: number;
  COMMITMENT_WINDOW_WARN_TOTAL_EVENTS: number;
  COMMITMENT_WINDOW_WARN_TOTAL_BYTES: number;
  TX_QUEUE_CAPACITY: number;
  TX_QUEUE_MAX_PENDING: number;
  TX_QUEUE_DRAIN_BATCH_SIZE: number;
  TX_QUEUE_OFFER_TIMEOUT_MS: number;
  TX_QUEUE_CLAIM_IDLE_MS: number;
  TX_QUEUE_CLAIM_BATCH_SIZE: number;
  TX_QUEUE_MAX_DELIVERY_ATTEMPTS: number;
  TX_QUEUE_DEAD_LETTER_STREAM: string;
  TX_PARSE_CONCURRENCY: number;
  NODE_ROLE: NodeRole;
  REDIS_URL: string;
  REDIS_STREAM_KEY: string;
  REDIS_STREAM_CONSUMER_GROUP: string;
  REDIS_STREAM_CONSUMER_NAME: string;
  REDIS_STREAM_BLOCK_MS: number;
  PROM_METRICS_PORT: number;
  OLTP_EXPORTER_URL: string;
  POSTGRES_USER: string;
  POSTGRES_PASSWORD: string;
  POSTGRES_DB: string;
  POSTGRES_HOST: string;
  LEDGER_MPT_DB_PATH: string;
  MEMPOOL_MPT_DB_PATH: string;
  LUCID_INIT_MAX_RETRIES: number;
  GENESIS_UTXOS: UTxO[];
};

const makeConfig = Effect.gen(function* () {
  const nodeEnv = yield* Config.string("NODE_ENV").pipe(
    Config.withDefault("development"),
  );
  const isProduction = nodeEnv === "production";
  const readDeploymentString = (name: string, localDefault: string) =>
    Effect.gen(function* () {
      const value = isProduction
        ? yield* Config.option(Config.string(name)).pipe(
            Effect.flatMap((maybeValue) =>
              Option.match(maybeValue, {
                onNone: () =>
                  Effect.fail(
                    new ConfigError({
                      message: `Missing required production config: ${name}`,
                      cause: undefined,
                      fieldsAndValues: [[name, "<missing>"]],
                    }),
                  ),
                onSome: Effect.succeed,
              }),
            ),
          )
        : yield* Config.string(name).pipe(Config.withDefault(localDefault));
      if (isProduction && value.trim() === "") {
        return yield* Effect.fail(
          new ConfigError({
            message: `Missing required production config: ${name}`,
            cause: undefined,
            fieldsAndValues: [[name, "<empty>"]],
          }),
        );
      }
      return value;
    });

  const provider = yield* Config.literal(
    "Kupmios",
    "Blockfrost",
  )("L1_PROVIDER");
  const blockfrostApiUrl = yield* Config.string("L1_BLOCKFROST_API_URL");
  const blockfrostKey = yield* Config.string("L1_BLOCKFROST_KEY");
  const ogmiosKey = yield* Config.string("L1_OGMIOS_KEY");
  const kupoKey = yield* Config.string("L1_KUPO_KEY");
  const operatorSeedPhrase = yield* Config.string("L1_OPERATOR_SEED_PHRASE");
  const operatorSeedPhraseForBlockCommitment = yield* Config.string(
    "L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT",
  );
  const operatorSeedPhraseForMergeTx = yield* Config.string(
    "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
  );
  const network = yield* Config.literal(
    "Mainnet",
    "Preprod",
    "Preview",
    "Custom",
  )("NETWORK");
  const port = yield* Config.integer("PORT").pipe(Config.withDefault(3000));
  const waitBetweenBlockCommitments = yield* Config.integer(
    "WAIT_BETWEEN_BLOCK_COMMITMENTS",
  ).pipe(Config.withDefault(1000));
  const waitBetweenBlockSubmissions = yield* Config.integer(
    "WAIT_BETWEEN_BLOCK_SUBMISSIONS",
  ).pipe(Config.withDefault(10000));
  const waitBetweenMergeTxs = yield* Config.integer(
    "WAIT_BETWEEN_MERGE_TXS",
  ).pipe(Config.withDefault(10000));
  const commitmentWorkerTimeoutMs = yield* Config.integer(
    "COMMITMENT_WORKER_TIMEOUT_MS",
  ).pipe(Config.withDefault(300_000));
  const commitmentWindowWarnTxRequests = yield* Config.integer(
    "COMMITMENT_WINDOW_WARN_TX_REQUESTS",
  ).pipe(Config.withDefault(50_000));
  const commitmentWindowWarnTotalEvents = yield* Config.integer(
    "COMMITMENT_WINDOW_WARN_TOTAL_EVENTS",
  ).pipe(Config.withDefault(60_000));
  const commitmentWindowWarnTotalBytes = yield* Config.integer(
    "COMMITMENT_WINDOW_WARN_TOTAL_BYTES",
  ).pipe(Config.withDefault(20_000_000));
  const txQueueCapacity = yield* Config.integer("TX_QUEUE_CAPACITY").pipe(
    Config.withDefault(10_000),
  );
  const txQueueMaxPending = yield* Config.integer("TX_QUEUE_MAX_PENDING").pipe(
    Config.withDefault(20_000),
  );
  const txQueueDrainBatchSize = yield* Config.integer(
    "TX_QUEUE_DRAIN_BATCH_SIZE",
  ).pipe(Config.withDefault(250));
  const txQueueOfferTimeoutMs = yield* Config.integer(
    "TX_QUEUE_OFFER_TIMEOUT_MS",
  ).pipe(Config.withDefault(100));
  const txQueueClaimIdleMs = yield* Config.integer(
    "TX_QUEUE_CLAIM_IDLE_MS",
  ).pipe(Config.withDefault(30_000));
  const txQueueClaimBatchSize = yield* Config.integer(
    "TX_QUEUE_CLAIM_BATCH_SIZE",
  ).pipe(Config.withDefault(100));
  const txQueueMaxDeliveryAttempts = yield* Config.integer(
    "TX_QUEUE_MAX_DELIVERY_ATTEMPTS",
  ).pipe(Config.withDefault(5));
  const txQueueDeadLetterStream = yield* Config.string(
    "TX_QUEUE_DEAD_LETTER_STREAM",
  ).pipe(Config.withDefault("midgard:tx-submissions:dead-letter"));
  const txParseConcurrency = yield* Config.integer("TX_PARSE_CONCURRENCY").pipe(
    Config.withDefault(4),
  );
  const nodeRole = yield* Config.literal(
    "all",
    "api",
    "tx-processor",
    "sequencer",
  )("NODE_ROLE").pipe(Config.withDefault("all"));
  const redisUrl = yield* readDeploymentString(
    "REDIS_URL",
    "redis://redis:6379",
  );
  const redisStreamKey = yield* Config.string("REDIS_STREAM_KEY").pipe(
    Config.withDefault("midgard:tx-submissions"),
  );
  const redisStreamConsumerGroup = yield* Config.string(
    "REDIS_STREAM_CONSUMER_GROUP",
  ).pipe(Config.withDefault("midgard-tx-processors"));
  const redisStreamConsumerName = yield* Config.string(
    "REDIS_STREAM_CONSUMER_NAME",
  ).pipe(Config.withDefault(`midgard-node-${process.pid}`));
  const redisStreamBlockMs = yield* Config.integer(
    "REDIS_STREAM_BLOCK_MS",
  ).pipe(Config.withDefault(1_000));
  const waitBetweenUserEventFetches = yield* Config.integer(
    "WAIT_BETWEEN_USER_EVENT_FETCHES",
  ).pipe(Config.withDefault(10000));
  const promMetricsPort = yield* Config.integer("PROM_METRICS_PORT").pipe(
    Config.withDefault(9464),
  );
  const oltpExporterUrl = yield* Config.string("OLTP_EXPORTER_URL").pipe(
    Config.withDefault("http://0.0.0.0:4318/v1/traces"),
  );
  const postgresHost = yield* readDeploymentString("POSTGRES_HOST", "postgres"); // service name
  const postgresPassword = yield* readDeploymentString(
    "POSTGRES_PASSWORD",
    "postgres",
  );
  const postgresDb = yield* readDeploymentString("POSTGRES_DB", "midgard");
  const postgresUser = yield* readDeploymentString("POSTGRES_USER", "postgres");
  const ledgerMptDbPath = yield* readDeploymentString(
    "LEDGER_MPT_DB_PATH",
    "midgard-ledger-mpt-db",
  );
  const mempoolMptDbPath = yield* readDeploymentString(
    "MEMPOOL_MPT_DB_PATH",
    "midgard-mempool-mpt-db",
  );
  // -1 means retry indefinitely (production default); 0 means try once only (E2E test mode)
  const lucidInitMaxRetries = yield* Config.integer(
    "LUCID_INIT_MAX_RETRIES",
  ).pipe(Config.withDefault(-1));
  const seedA = yield* Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_A");
  const seedB = yield* Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_B");
  const seedC = yield* Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_C");

  const genesisUtxos: UTxO[] = [
    {
      txHash:
        "bb217abaca60fc0ca68c1555eca6a96d2478547818ae76ce6836133f3cc546e0",
      outputIndex: 1,
      address: walletFromSeed(seedA, { network }).address,
      assets: {
        lovelace: BigInt("4027026465"),
        // "25561d09e55d60b64525b9cdb3cfbec23c94c0634320fec2eaddde584c616365436f696e33":
        //   BigInt("10000"),
      },
    },
    {
      txHash:
        "c7c0973c6bbf1a04a9f306da7814b4fa564db649bf48b0bd93c273bd03143547",
      outputIndex: 0,
      address: walletFromSeed(seedA, { network }).address,
      assets: {
        lovelace: BigInt("3289566"),
        // "5c677ba4dd295d9286e0e22786fea9ed735a6ae9c07e7a45ae4d95c84372696d696e616c50756e6b73204c6f6f74":
        //   BigInt("1"),
      },
    },
    {
      txHash:
        "d1a25b8e9c3b985d9d2f0a5f2e6ca7efa1c43b10f2c0b61f29e4a2cd8142b09e",
      outputIndex: 0,
      address: walletFromSeed(seedB, { network }).address,
      assets: {
        lovelace: BigInt("200"),
      },
    },
    {
      txHash:
        "ea0f3c47bf18b02e9deb4e3a1239d8b263d765c4f7a3d12a9f62e8775e8c6141",
      outputIndex: 1,
      address: walletFromSeed(seedB, { network }).address,
      assets: {
        lovelace: BigInt("1500"),
      },
    },
    {
      txHash:
        "f40b9f6a507af50aad4ccf6c15157b6d05c7affe23ec55cf4109cc2549c97a37",
      outputIndex: 2,
      address: walletFromSeed(seedB, { network }).address,
      assets: {
        lovelace: BigInt("125243"),
      },
    },
    {
      txHash:
        "8e32d18c07cba2b65577bc829a9875e2fc3cdb554d5b0abbb3d4e3a71a3e3e3d",
      outputIndex: 0,
      address: walletFromSeed(seedC, { network }).address,
      assets: {
        lovelace: BigInt("300"),
        // "25561d09e55d60b64525b9cdb3cfbec23c94c0634320fec2eaddde584c616365436f696e33":
        //   BigInt("15"),
      },
    },
  ];

  const assertPositiveInteger = (fieldName: string, value: number) =>
    value > 0
      ? Effect.void
      : Effect.fail(
          new ConfigError({
            message: `Config field must be a positive integer: ${fieldName}`,
            cause: undefined,
            fieldsAndValues: [[fieldName, String(value)]],
          }),
        );

  yield* assertPositiveInteger("TX_QUEUE_CAPACITY", txQueueCapacity);
  yield* assertPositiveInteger("TX_QUEUE_MAX_PENDING", txQueueMaxPending);
  yield* assertPositiveInteger(
    "TX_QUEUE_DRAIN_BATCH_SIZE",
    txQueueDrainBatchSize,
  );
  yield* assertPositiveInteger(
    "TX_QUEUE_OFFER_TIMEOUT_MS",
    txQueueOfferTimeoutMs,
  );
  yield* assertPositiveInteger("TX_QUEUE_CLAIM_IDLE_MS", txQueueClaimIdleMs);
  yield* assertPositiveInteger(
    "TX_QUEUE_CLAIM_BATCH_SIZE",
    txQueueClaimBatchSize,
  );
  yield* assertPositiveInteger(
    "TX_QUEUE_MAX_DELIVERY_ATTEMPTS",
    txQueueMaxDeliveryAttempts,
  );
  yield* assertPositiveInteger("TX_PARSE_CONCURRENCY", txParseConcurrency);
  yield* assertPositiveInteger("REDIS_STREAM_BLOCK_MS", redisStreamBlockMs);
  yield* assertPositiveInteger(
    "COMMITMENT_WINDOW_WARN_TX_REQUESTS",
    commitmentWindowWarnTxRequests,
  );
  yield* assertPositiveInteger(
    "COMMITMENT_WINDOW_WARN_TOTAL_EVENTS",
    commitmentWindowWarnTotalEvents,
  );
  yield* assertPositiveInteger(
    "COMMITMENT_WINDOW_WARN_TOTAL_BYTES",
    commitmentWindowWarnTotalBytes,
  );

  return {
    L1_PROVIDER: provider,
    L1_BLOCKFROST_API_URL: blockfrostApiUrl,
    L1_BLOCKFROST_KEY: blockfrostKey,
    L1_OGMIOS_KEY: ogmiosKey,
    L1_KUPO_KEY: kupoKey,
    L1_OPERATOR_SEED_PHRASE: operatorSeedPhrase,
    L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT:
      operatorSeedPhraseForBlockCommitment,
    L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: operatorSeedPhraseForMergeTx,
    NETWORK: network,
    PROTOCOL_PARAMETERS: SDK.getProtocolParameters(network),
    PORT: port,
    WAIT_BETWEEN_BLOCK_COMMITMENTS: waitBetweenBlockCommitments,
    WAIT_BETWEEN_BLOCK_SUBMISSIONS: waitBetweenBlockSubmissions,
    WAIT_BETWEEN_MERGE_TXS: waitBetweenMergeTxs,
    WAIT_BETWEEN_USER_EVENT_FETCHES: waitBetweenUserEventFetches,
    COMMITMENT_WORKER_TIMEOUT_MS: commitmentWorkerTimeoutMs,
    COMMITMENT_WINDOW_WARN_TX_REQUESTS: commitmentWindowWarnTxRequests,
    COMMITMENT_WINDOW_WARN_TOTAL_EVENTS: commitmentWindowWarnTotalEvents,
    COMMITMENT_WINDOW_WARN_TOTAL_BYTES: commitmentWindowWarnTotalBytes,
    TX_QUEUE_CAPACITY: txQueueCapacity,
    TX_QUEUE_MAX_PENDING: txQueueMaxPending,
    TX_QUEUE_DRAIN_BATCH_SIZE: txQueueDrainBatchSize,
    TX_QUEUE_OFFER_TIMEOUT_MS: txQueueOfferTimeoutMs,
    TX_QUEUE_CLAIM_IDLE_MS: txQueueClaimIdleMs,
    TX_QUEUE_CLAIM_BATCH_SIZE: txQueueClaimBatchSize,
    TX_QUEUE_MAX_DELIVERY_ATTEMPTS: txQueueMaxDeliveryAttempts,
    TX_QUEUE_DEAD_LETTER_STREAM: txQueueDeadLetterStream,
    TX_PARSE_CONCURRENCY: txParseConcurrency,
    NODE_ROLE: nodeRole,
    REDIS_URL: redisUrl,
    REDIS_STREAM_KEY: redisStreamKey,
    REDIS_STREAM_CONSUMER_GROUP: redisStreamConsumerGroup,
    REDIS_STREAM_CONSUMER_NAME: redisStreamConsumerName,
    REDIS_STREAM_BLOCK_MS: redisStreamBlockMs,
    PROM_METRICS_PORT: promMetricsPort,
    OLTP_EXPORTER_URL: oltpExporterUrl,
    POSTGRES_HOST: postgresHost,
    POSTGRES_PASSWORD: postgresPassword,
    POSTGRES_DB: postgresDb,
    POSTGRES_USER: postgresUser,
    LEDGER_MPT_DB_PATH: ledgerMptDbPath,
    MEMPOOL_MPT_DB_PATH: mempoolMptDbPath,
    LUCID_INIT_MAX_RETRIES: lucidInitMaxRetries,
    GENESIS_UTXOS: network === "Mainnet" ? [] : genesisUtxos,
  };
}).pipe(
  Effect.mapError((e) =>
    e instanceof ConfigError
      ? e
      : new ConfigError({
          message: "Error instantiating the config service.",
          cause: e,
          fieldsAndValues: [["<n/a>", "<n/a>"]],
        }),
  ),
);

export class NodeConfig extends Context.Tag("NodeConfig")<
  NodeConfig,
  NodeConfigDep
>() {
  static readonly layer = Layer.effect(NodeConfig, makeConfig);
}

export class ConfigError extends Data.TaggedError("ConfigError")<
  SDK.GenericErrorFields & {
    readonly fieldsAndValues: [string, string][];
  }
> {}
