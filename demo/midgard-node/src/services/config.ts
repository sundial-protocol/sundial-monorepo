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
  SUBMIT_SIGNED_TX_TIMEOUT_MS: number;
  SUBMIT_SIGN_TIMEOUT_RECOVERY_MAX_RETRIES: number;
  WAIT_BETWEEN_USER_EVENT_FETCHES: number;
  WAIT_BETWEEN_MERGE_TXS: number;
  COMMITMENT_WORKER_TIMEOUT_MS: number;
  COMMITMENT_MAX_UNSUBMITTED_BLOCK_BACKLOG: number;
  COMMITMENT_WINDOW_WARN_TX_REQUESTS: number;
  COMMITMENT_WINDOW_WARN_TOTAL_EVENTS: number;
  COMMITMENT_WINDOW_WARN_TOTAL_BYTES: number;
  COMMITMENT_MIN_TX_REQUESTS_PER_BLOCK: number;
  COMMITMENT_MAX_WAIT_MS: number;
  COMMITMENT_MAX_TX_REQUESTS_PER_BLOCK: number;
  TX_QUEUE_DRAIN_BATCH_SIZE: number;
  TX_QUEUE_CONSUMER_WORKER_COUNT: number;
  TX_QUEUE_PROCESSOR_INTERVAL_MS: number;
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
  FAUCET_ENABLED: boolean;
  FAUCET_SEED_PHRASE: string;
  FAUCET_API_KEY: string;
  FAUCET_ADDRESS: string;
  FAUCET_AMOUNT_LOVELACE: bigint;
  FAUCET_COOLDOWN_SECONDS: number;
  FAUCET_DAILY_IP_LIMIT: number;
  FAUCET_MIN_BALANCE_LOVELACE: bigint;
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
  ).pipe(Config.withDefault(500));
  const waitBetweenBlockSubmissions = yield* Config.integer(
    "WAIT_BETWEEN_BLOCK_SUBMISSIONS",
  ).pipe(Config.withDefault(1000));
  const submitSignedTxTimeoutMs = yield* Config.integer(
    "SUBMIT_SIGNED_TX_TIMEOUT_MS",
  ).pipe(Config.withDefault(30_000));
  const submitSignTimeoutRecoveryMaxRetries = yield* Config.integer(
    "SUBMIT_SIGN_TIMEOUT_RECOVERY_MAX_RETRIES",
  ).pipe(Config.withDefault(1));
  const waitBetweenMergeTxs = yield* Config.integer(
    "WAIT_BETWEEN_MERGE_TXS",
  ).pipe(Config.withDefault(10000));
  const commitmentWorkerTimeoutMs = yield* Config.integer(
    "COMMITMENT_WORKER_TIMEOUT_MS",
  ).pipe(Config.withDefault(300_000));
  const commitmentMaxUnsubmittedBlockBacklog = yield* Config.integer(
    "COMMITMENT_MAX_UNSUBMITTED_BLOCK_BACKLOG",
  ).pipe(Config.withDefault(0));
  const commitmentWindowWarnTxRequests = yield* Config.integer(
    "COMMITMENT_WINDOW_WARN_TX_REQUESTS",
  ).pipe(Config.withDefault(50_000));
  const commitmentWindowWarnTotalEvents = yield* Config.integer(
    "COMMITMENT_WINDOW_WARN_TOTAL_EVENTS",
  ).pipe(Config.withDefault(60_000));
  const commitmentWindowWarnTotalBytes = yield* Config.integer(
    "COMMITMENT_WINDOW_WARN_TOTAL_BYTES",
  ).pipe(Config.withDefault(20_000_000));
  const commitmentMinTxRequestsPerBlock = yield* Config.integer(
    "COMMITMENT_MIN_TX_REQUESTS_PER_BLOCK",
  ).pipe(Config.withDefault(1));
  const commitmentMaxWaitMs = yield* Config.integer(
    "COMMITMENT_MAX_WAIT_MS",
  ).pipe(Config.withDefault(0));
  const commitmentMaxTxRequestsPerBlock = yield* Config.integer(
    "COMMITMENT_MAX_TX_REQUESTS_PER_BLOCK",
  ).pipe(Config.withDefault(2_000));
  const txQueueDrainBatchSize = yield* Config.integer(
    "TX_QUEUE_DRAIN_BATCH_SIZE",
  ).pipe(Config.withDefault(100));
  const txQueueConsumerWorkerCount = yield* Config.integer(
    "TX_QUEUE_CONSUMER_WORKER_COUNT",
  ).pipe(Config.withDefault(1));
  const txQueueProcessorIntervalMs = yield* Config.integer(
    "TX_QUEUE_PROCESSOR_INTERVAL_MS",
  ).pipe(Config.withDefault(250));
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
    Config.withDefault(8),
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

  // ---------------------------------------------------------------------------
  // Faucet wallet
  //
  // A dedicated, genesis-funded wallet that the (forthcoming) faucet service
  // spends from to hand out L2 test ADA. The seed phrase and API key are
  // sensitive and must come from AWS Secrets Manager in production; they must
  // NOT reuse a genesis or operator seed. The remaining knobs stay
  // server-configurable so claim economics can be tuned without a redeploy.
  // ---------------------------------------------------------------------------
  const parseLovelace = (name: string, raw: string) =>
    Effect.try({
      try: () => BigInt(raw),
      catch: () =>
        new ConfigError({
          message: `Config field must be an integer number of lovelace: ${name}`,
          cause: undefined,
          fieldsAndValues: [[name, raw]],
        }),
    });

  const faucetEnabled = yield* Config.boolean("FAUCET_ENABLED").pipe(
    Config.withDefault(false),
  );
  const faucetSeedPhrase = yield* Config.string("FAUCET_SEED_PHRASE").pipe(
    Config.withDefault(""),
  );
  const faucetApiKey = yield* Config.string("FAUCET_API_KEY").pipe(
    Config.withDefault(""),
  );
  const faucetAmountLovelace = yield* Config.string(
    "FAUCET_AMOUNT_LOVELACE",
  ).pipe(
    Config.withDefault("100000000"),
    Effect.flatMap((raw) => parseLovelace("FAUCET_AMOUNT_LOVELACE", raw)),
  );
  const faucetCooldownSeconds = yield* Config.integer(
    "FAUCET_COOLDOWN_SECONDS",
  ).pipe(Config.withDefault(86_400));
  const faucetDailyIpLimit = yield* Config.integer(
    "FAUCET_DAILY_IP_LIMIT",
  ).pipe(Config.withDefault(5));
  const faucetMinBalanceLovelace = yield* Config.string(
    "FAUCET_MIN_BALANCE_LOVELACE",
  ).pipe(
    Config.withDefault("100000000"),
    Effect.flatMap((raw) => parseLovelace("FAUCET_MIN_BALANCE_LOVELACE", raw)),
  );
  const faucetGenesisAllocationLovelace = yield* Config.string(
    "FAUCET_GENESIS_ALLOCATION_LOVELACE",
  ).pipe(
    Config.withDefault("10000000000000"), // ~10,000,000 L2 test ADA
    Effect.flatMap((raw) =>
      parseLovelace("FAUCET_GENESIS_ALLOCATION_LOVELACE", raw),
    ),
  );

  if (faucetEnabled) {
    if (isProduction && faucetSeedPhrase.trim() === "") {
      yield* Effect.fail(
        new ConfigError({
          message:
            "FAUCET_SEED_PHRASE is required when FAUCET_ENABLED=true in production",
          cause: undefined,
          fieldsAndValues: [["FAUCET_SEED_PHRASE", "<missing>"]],
        }),
      );
    }
    if (isProduction && faucetApiKey.trim() === "") {
      yield* Effect.fail(
        new ConfigError({
          message:
            "FAUCET_API_KEY is required when FAUCET_ENABLED=true in production",
          cause: undefined,
          fieldsAndValues: [["FAUCET_API_KEY", "<missing>"]],
        }),
      );
    }
    if (
      faucetSeedPhrase.trim() !== "" &&
      (faucetSeedPhrase.trim() === seedA.trim() ||
        faucetSeedPhrase.trim() === seedB.trim() ||
        faucetSeedPhrase.trim() === seedC.trim() ||
        faucetSeedPhrase.trim() === operatorSeedPhrase.trim())
    ) {
      yield* Effect.fail(
        new ConfigError({
          message:
            "FAUCET_SEED_PHRASE must not reuse a genesis or operator seed phrase",
          cause: undefined,
          fieldsAndValues: [["FAUCET_SEED_PHRASE", "<reused>"]],
        }),
      );
    }
  }

  // The faucet is funded the same way as the other genesis wallets: a single
  // large UTxO at its own address, only on testnet and only when configured.
  const faucetAddress =
    faucetEnabled && faucetSeedPhrase.trim() !== ""
      ? walletFromSeed(faucetSeedPhrase, { network }).address
      : "";
  const faucetGenesisUtxos: UTxO[] =
    faucetEnabled && faucetAddress !== "" && network !== "Mainnet"
      ? [
          {
            txHash:
              "fa0ce700fa0ce700fa0ce700fa0ce700fa0ce700fa0ce700fa0ce700fa0ce700",
            outputIndex: 0,
            address: faucetAddress,
            assets: {
              lovelace: faucetGenesisAllocationLovelace,
            },
          },
        ]
      : [];

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
    ...faucetGenesisUtxos,
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

  const assertNonNegativeInteger = (fieldName: string, value: number) =>
    value >= 0
      ? Effect.void
      : Effect.fail(
          new ConfigError({
            message: `Config field must be a non-negative integer: ${fieldName}`,
            cause: undefined,
            fieldsAndValues: [[fieldName, String(value)]],
          }),
        );

  const assertPositiveBigInt = (fieldName: string, value: bigint) =>
    value > 0n
      ? Effect.void
      : Effect.fail(
          new ConfigError({
            message: `Config field must be a positive integer: ${fieldName}`,
            cause: undefined,
            fieldsAndValues: [[fieldName, String(value)]],
          }),
        );

  const assertNonNegativeBigInt = (fieldName: string, value: bigint) =>
    value >= 0n
      ? Effect.void
      : Effect.fail(
          new ConfigError({
            message: `Config field must be a non-negative integer: ${fieldName}`,
            cause: undefined,
            fieldsAndValues: [[fieldName, String(value)]],
          }),
        );

  if (faucetEnabled) {
    yield* assertPositiveBigInt("FAUCET_AMOUNT_LOVELACE", faucetAmountLovelace);
    yield* assertNonNegativeBigInt(
      "FAUCET_MIN_BALANCE_LOVELACE",
      faucetMinBalanceLovelace,
    );
    yield* assertPositiveInteger(
      "FAUCET_COOLDOWN_SECONDS",
      faucetCooldownSeconds,
    );
    yield* assertPositiveInteger("FAUCET_DAILY_IP_LIMIT", faucetDailyIpLimit);
  }

  yield* assertPositiveInteger(
    "TX_QUEUE_DRAIN_BATCH_SIZE",
    txQueueDrainBatchSize,
  );
  yield* assertPositiveInteger(
    "TX_QUEUE_CONSUMER_WORKER_COUNT",
    txQueueConsumerWorkerCount,
  );
  yield* assertPositiveInteger(
    "TX_QUEUE_PROCESSOR_INTERVAL_MS",
    txQueueProcessorIntervalMs,
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
  yield* assertPositiveInteger(
    "COMMITMENT_MIN_TX_REQUESTS_PER_BLOCK",
    commitmentMinTxRequestsPerBlock,
  );
  yield* assertNonNegativeInteger(
    "COMMITMENT_MAX_WAIT_MS",
    commitmentMaxWaitMs,
  );
  yield* assertPositiveInteger(
    "COMMITMENT_MAX_TX_REQUESTS_PER_BLOCK",
    commitmentMaxTxRequestsPerBlock,
  );
  if (commitmentMinTxRequestsPerBlock > commitmentMaxTxRequestsPerBlock) {
    yield* Effect.fail(
      new ConfigError({
        message:
          "COMMITMENT_MIN_TX_REQUESTS_PER_BLOCK must be less than or equal to COMMITMENT_MAX_TX_REQUESTS_PER_BLOCK",
        cause: undefined,
        fieldsAndValues: [
          [
            "COMMITMENT_MIN_TX_REQUESTS_PER_BLOCK",
            String(commitmentMinTxRequestsPerBlock),
          ],
          [
            "COMMITMENT_MAX_TX_REQUESTS_PER_BLOCK",
            String(commitmentMaxTxRequestsPerBlock),
          ],
        ],
      }),
    );
  }
  yield* assertNonNegativeInteger(
    "COMMITMENT_MAX_UNSUBMITTED_BLOCK_BACKLOG",
    commitmentMaxUnsubmittedBlockBacklog,
  );
  yield* assertPositiveInteger(
    "SUBMIT_SIGNED_TX_TIMEOUT_MS",
    submitSignedTxTimeoutMs,
  );
  yield* assertNonNegativeInteger(
    "SUBMIT_SIGN_TIMEOUT_RECOVERY_MAX_RETRIES",
    submitSignTimeoutRecoveryMaxRetries,
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
    SUBMIT_SIGNED_TX_TIMEOUT_MS: submitSignedTxTimeoutMs,
    SUBMIT_SIGN_TIMEOUT_RECOVERY_MAX_RETRIES:
      submitSignTimeoutRecoveryMaxRetries,
    WAIT_BETWEEN_MERGE_TXS: waitBetweenMergeTxs,
    WAIT_BETWEEN_USER_EVENT_FETCHES: waitBetweenUserEventFetches,
    COMMITMENT_WORKER_TIMEOUT_MS: commitmentWorkerTimeoutMs,
    COMMITMENT_MAX_UNSUBMITTED_BLOCK_BACKLOG:
      commitmentMaxUnsubmittedBlockBacklog,
    COMMITMENT_WINDOW_WARN_TX_REQUESTS: commitmentWindowWarnTxRequests,
    COMMITMENT_WINDOW_WARN_TOTAL_EVENTS: commitmentWindowWarnTotalEvents,
    COMMITMENT_WINDOW_WARN_TOTAL_BYTES: commitmentWindowWarnTotalBytes,
    COMMITMENT_MIN_TX_REQUESTS_PER_BLOCK: commitmentMinTxRequestsPerBlock,
    COMMITMENT_MAX_WAIT_MS: commitmentMaxWaitMs,
    COMMITMENT_MAX_TX_REQUESTS_PER_BLOCK: commitmentMaxTxRequestsPerBlock,
    TX_QUEUE_DRAIN_BATCH_SIZE: txQueueDrainBatchSize,
    TX_QUEUE_CONSUMER_WORKER_COUNT: txQueueConsumerWorkerCount,
    TX_QUEUE_PROCESSOR_INTERVAL_MS: txQueueProcessorIntervalMs,
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
    FAUCET_ENABLED: faucetEnabled,
    FAUCET_SEED_PHRASE: faucetSeedPhrase,
    FAUCET_API_KEY: faucetApiKey,
    FAUCET_ADDRESS: faucetAddress,
    FAUCET_AMOUNT_LOVELACE: faucetAmountLovelace,
    FAUCET_COOLDOWN_SECONDS: faucetCooldownSeconds,
    FAUCET_DAILY_IP_LIMIT: faucetDailyIpLimit,
    FAUCET_MIN_BALANCE_LOVELACE: faucetMinBalanceLovelace,
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
