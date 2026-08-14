import { Effect, Option, Schedule } from "effect";
import { ConfigError, NodeConfig } from "./config.js";
import * as LE from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";

interface LucidApis {
  mainApi: LE.LucidEvolution;
  blockCommitmentApi: LE.LucidEvolution;
  mergeApi: LE.LucidEvolution;
}

const makeLucidInstance = (
  nodeConfig: NodeConfig["Type"],
): Effect.Effect<LE.LucidEvolution, ConfigError> =>
  Effect.tryPromise({
    try: () => {
      switch (nodeConfig.L1_PROVIDER) {
        case "Kupmios":
          return LE.Lucid(
            new LE.Kupmios(nodeConfig.L1_KUPO_KEY, nodeConfig.L1_OGMIOS_KEY),
            nodeConfig.NETWORK,
          );
        case "Blockfrost":
          return LE.Lucid(
            new LE.Blockfrost(
              nodeConfig.L1_BLOCKFROST_API_URL,
              nodeConfig.L1_BLOCKFROST_KEY,
            ),
            nodeConfig.NETWORK,
          );
      }
    },
    catch: (e) =>
      new ConfigError({
        message: `An error occurred on lucid initialization`,
        cause: e,
        fieldsAndValues: [
          ["L1_PROVIDER", nodeConfig.L1_PROVIDER],
          ["NETWORK", nodeConfig.NETWORK],
        ],
      }),
  }).pipe(Effect.tapError(Effect.logInfo));

const buildPinnedLucidApis = (
  nodeConfig: NodeConfig["Type"],
): Effect.Effect<LucidApis, ConfigError> =>
  Effect.gen(function* () {
    const mainApi = yield* makeLucidInstance(nodeConfig);
    const blockCommitmentApi = yield* makeLucidInstance(nodeConfig);
    const mergeApi = yield* makeLucidInstance(nodeConfig);

    yield* Effect.sync(() => {
      mainApi.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE);
      blockCommitmentApi.selectWallet.fromSeed(
        nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT,
      );
      mergeApi.selectWallet.fromSeed(
        nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX,
      );
    });

    return { mainApi, blockCommitmentApi, mergeApi };
  });

const makeL1ProviderCheckReady = (
  mainApi: LE.LucidEvolution,
): Effect.Effect<void, SDK.LucidError> => {
  const provider = mainApi.config().provider;
  return provider === undefined
    ? Effect.fail(
        new SDK.LucidError({
          message: "L1 provider readiness check failed: no provider configured",
          cause: undefined,
        }),
      )
    : Effect.tryPromise({
        try: () => provider.getProtocolParameters(),
        catch: (e) =>
          new SDK.LucidError({
            message: "L1 provider readiness check failed",
            cause: e,
          }),
      }).pipe(Effect.asVoid);
};

const makeDegradedCheckReady: Effect.Effect<void, SDK.LucidError> = Effect.fail(
  new SDK.LucidError({
    message: "Lucid not initialized (degraded mode); L1 provider unavailable",
    cause: undefined,
  }),
);

// Retries indefinitely (LUCID_INIT_MAX_RETRIES=-1) but with capped, jittered
// exponential backoff rather than a fixed 1s interval, so a slow-to-warm-up
// or rate-limited L1 provider (e.g. Blockfrost) isn't hammered forever.
export const lucidInitRetrySchedule = Schedule.exponential("1 second").pipe(
  Schedule.union(Schedule.spaced("30 seconds")),
  Schedule.jittered,
);

const makeLucid: Effect.Effect<
  {
    // Backward-compatible alias to the main operator wallet API.
    api: LE.LucidEvolution;
    mainApi: LE.LucidEvolution;
    blockCommitmentApi: LE.LucidEvolution;
    mergeApi: LE.LucidEvolution;
    reinitializeMergeApi: Effect.Effect<void, ConfigError>;
    // Deprecated compatibility methods; wallet switching is no longer used.
    switchToOperatorsMainWallet: Effect.Effect<void>;
    switchToOperatorsBlockCommitmentWallet: Effect.Effect<void>;
    switchToOperatorsMergingWallet: Effect.Effect<void>;
    // Probes L1 connectivity by querying the underlying provider; used by
    // the /health/ready endpoint so degraded/unreachable L1 fails readiness.
    checkReady: Effect.Effect<void, SDK.LucidError>;
  },
  ConfigError,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;

  // NODE_ROLE=tx-processor only does Redis consumer-group processing and has
  // no L1 dependency, so it never attempts (or retries) Lucid init.
  //
  // Otherwise, LUCID_INIT_MAX_RETRIES=-1 (default): retry indefinitely, with
  // capped/jittered exponential backoff, until the L1 provider is reachable.
  // LUCID_INIT_MAX_RETRIES=0: try once; if it fails, run in degraded mode (no
  // L1 connectivity). Use 0 in E2E test environments that don't need L1.
  const lucidApisOption: Option.Option<LucidApis> =
    nodeConfig.NODE_ROLE === "tx-processor"
      ? Option.none()
      : nodeConfig.LUCID_INIT_MAX_RETRIES < 0
        ? yield* buildPinnedLucidApis(nodeConfig)
            .pipe(Effect.retry(lucidInitRetrySchedule))
            .pipe(Effect.map(Option.some))
        : yield* Effect.option(buildPinnedLucidApis(nodeConfig));

  if (Option.isNone(lucidApisOption)) {
    if (nodeConfig.NODE_ROLE === "tx-processor") {
      yield* Effect.logInfo(
        `Lucid initialization skipped; NODE_ROLE=tx-processor has no L1 dependency.`,
      );
    } else {
      yield* Effect.logWarning(
        `Lucid initialization failed; node running in degraded mode (no L1 connectivity). NODE_ROLE=${nodeConfig.NODE_ROLE}`,
      );
    }
    // Proxy crashes with a descriptive message if L1 methods are actually called,
    // which should not happen for api/tx-processor roles.
    const degradedApi = new Proxy({} as LE.LucidEvolution, {
      get: (_target, prop) => {
        throw new Error(
          `Lucid not initialized (degraded mode); cannot access '${String(prop)}'. Check LUCID_INIT_MAX_RETRIES and L1 provider config.`,
        );
      },
    });
    const degradedWalletSwitch = Effect.logWarning(
      `Lucid not initialized (degraded mode); wallet switch skipped. NODE_ROLE=${nodeConfig.NODE_ROLE}`,
    );
    return {
      api: degradedApi,
      mainApi: degradedApi,
      blockCommitmentApi: degradedApi,
      mergeApi: degradedApi,
      reinitializeMergeApi: degradedWalletSwitch,
      switchToOperatorsMainWallet: degradedWalletSwitch,
      switchToOperatorsBlockCommitmentWallet: degradedWalletSwitch,
      switchToOperatorsMergingWallet: degradedWalletSwitch,
      checkReady: makeDegradedCheckReady,
    };
  }

  const lucidApis = lucidApisOption.value;
  let mergeApi = lucidApis.mergeApi;
  const reinitializeMergeApi: Effect.Effect<void, ConfigError> = Effect.gen(
    function* () {
      const refreshedMergeApi = yield* makeLucidInstance(nodeConfig);
      yield* Effect.sync(() => {
        refreshedMergeApi.selectWallet.fromSeed(
          nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX,
        );
        mergeApi = refreshedMergeApi;
      });
    },
  );
  const noOpWalletSwitch = Effect.void;
  return {
    api: lucidApis.mainApi,
    mainApi: lucidApis.mainApi,
    blockCommitmentApi: lucidApis.blockCommitmentApi,
    get mergeApi() {
      return mergeApi;
    },
    reinitializeMergeApi,
    switchToOperatorsMainWallet: noOpWalletSwitch,
    switchToOperatorsBlockCommitmentWallet: noOpWalletSwitch,
    switchToOperatorsMergingWallet: noOpWalletSwitch,
    checkReady: makeL1ProviderCheckReady(lucidApis.mainApi),
  };
});

export class Lucid extends Effect.Service<Lucid>()("Lucid", {
  effect: makeLucid,
  dependencies: [NodeConfig.layer],
}) {}

export const makeL1ProviderCheckReadyForTesting = makeL1ProviderCheckReady;
