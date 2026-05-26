import { DatabaseError } from "@/database/utils/common.js";
import { Lucid, AlwaysSucceedsContract, Globals } from "@/services/index.js";
import { StateQueueTx } from "@/transactions/index.js";
import { TxSignError, TxSubmitError } from "@/transactions/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, pipe, Ref, Schedule } from "effect";
import { Database } from "@/services/index.js";
export const mergeAction: Effect.Effect<
  void,
  | SDK.CmlDeserializationError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LucidError
  | SDK.MissingDatumError
  | SDK.StateQueueError
  | SDK.UnauthenticUtxoError
  | DatabaseError
  | TxSubmitError
  | TxSignError,
  Lucid | AlwaysSucceedsContract | Database | Globals
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const resetInProgress = yield* Ref.get(globals.RESET_IN_PROGRESS);
  if (resetInProgress) {
    return;
  }

  const lucid = yield* Lucid;
  const mergeApi = lucid.mergeApi;
  const { stateQueue: stateQueueAuthValidator } = yield* AlwaysSucceedsContract;

  const fetchConfig: SDK.StateQueueFetchConfig = {
    stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
    stateQueuePolicyId: stateQueueAuthValidator.policyId,
  };
  yield* StateQueueTx.buildAndSubmitMergeTx(
    mergeApi,
    fetchConfig,
    stateQueueAuthValidator.spendingScript,
    stateQueueAuthValidator.mintingScript,
  );
});

// possible issues:
// 1. tx-generator: large batch size & high concurrency
// 2. after initing node, can't commit the block
export const mergeFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Lucid | AlwaysSucceedsContract | Database | Globals
> =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🟠 Merge fiber started.");
      // Initialize merge metrics so dashboards show a baseline series.
      yield* StateQueueTx.initializeMergeMetrics;
      const action = mergeAction.pipe(
        Effect.withSpan("merge-confirmed-state-fiber"),
        Effect.tapErrorCause((cause) =>
          Effect.gen(function* () {
            yield* StateQueueTx.incrementMergeFailure;
            yield* Effect.logWarning(cause);
          }),
        ),
        Effect.catchAllCause(() => Effect.void),
      );
      yield* Effect.repeat(action, schedule);
    }),
  );
