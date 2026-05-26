import * as SDK from "@al-ft/midgard-sdk";
import { Assets, Data, TxBuilder, toUnit } from "@lucid-evolution/lucid";
import {
  AlwaysSucceedsContract,
  Database,
  Globals,
  Lucid,
  NodeConfig,
  TxIngressQueue,
} from "@/services/index.js";
import { Effect, Ref } from "effect";
import {
  TxConfirmError,
  handleSignSubmit,
  TxSubmitError,
  TxSignError,
} from "@/transactions/utils.js";
import {
  AddressHistoryDB,
  BlocksTxsDB,
  ConfirmedLedgerDB,
  ImmutableDB,
  LatestLedgerDB,
  MempoolDB,
  MempoolLedgerDB,
  BlocksDB,
} from "@/database/index.js";
import { deleteLedgerMpt, deleteMempoolMpt } from "@/workers/utils/mpt.js";
import { DatabaseError } from "@/database/utils/common.js";
import { FileSystemError } from "@/utils.js";

const BATCH_SIZE = 150;
const RESET_SPEND_AND_BURN_TIMEOUT_MS = 600_000;
const REPAIR_ROOT_UNITS_TIMEOUT_MS = 900_000;

/**
 * This function can only be used once per `AuthenticatedValidator` per tx.
 */
const spendAndBurnBeaconUTxOs = (
  tx: TxBuilder,
  authVal: SDK.AuthenticatedValidator,
  utxos: SDK.BeaconUTxO[],
): TxBuilder => {
  const assetsToBurn: Assets = {};
  utxos.map((u) => {
    const assetUnit = toUnit(u.policyId, u.assetName);
    if (assetsToBurn[assetUnit] !== undefined) {
      assetsToBurn[assetUnit] -= 1n;
    } else {
      assetsToBurn[assetUnit] = -1n;
    }
    tx.collectFrom([u.utxo], Data.void());
  });
  tx.mintAssets(assetsToBurn, Data.void())
    .attach.Script(authVal.spendingScript)
    .attach.Script(authVal.mintingScript);
  return tx;
};

type InternalAccumulator = {
  tx: TxBuilder;
  count: number;
};

const spendAndBurntAllUTxOs: Effect.Effect<
  void,
  SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError,
  Lucid | AlwaysSucceedsContract
> = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const midgardValidators = yield* AlwaysSucceedsContract;
  const allAuthVals = [
    midgardValidators.deposit,
    ...SDK.getInitializedValidatorsFromMidgardValidators(midgardValidators),
  ];
  const submitIfFull = (acc: InternalAccumulator) =>
    Effect.gen(function* () {
      // Reject transactions that are not filled.
      if (acc.count < BATCH_SIZE) {
        return acc;
      }
      const completedTx = yield* acc.tx.completeProgram();
      yield* lucid.switchToOperatorsMainWallet;
      yield* handleSignSubmit(lucid.api, completedTx);
      const resetAcc: InternalAccumulator = {
        tx: lucid.api.newTx(),
        count: 0,
      };
      return resetAcc;
    });

  const initialAccumulator: InternalAccumulator = {
    tx: lucid.api.newTx(),
    count: 0,
  };
  const finalAcc = yield* Effect.reduce(
    allAuthVals,
    initialAccumulator,
    (initialAcc, authVal) =>
      Effect.gen(function* () {
        const authValUTxOs = yield* SDK.utxosAtByNFTPolicyId(
          lucid.api,
          authVal.spendingScriptAddress,
          authVal.policyId,
        );
        const { acc } = yield* Effect.iterate(
          { acc: initialAcc, offset: 0 },
          {
            while: ({ offset }) => offset < authValUTxOs.length,
            body: ({ acc, offset }) =>
              Effect.gen(function* () {
                const txAcc = yield* submitIfFull(acc);
                const capacity = BATCH_SIZE - txAcc.count;
                const utxosToSpend = authValUTxOs.slice(
                  offset,
                  offset + capacity,
                );
                // For this tx segment, apply this validator exactly once.
                const nextAcc: InternalAccumulator = {
                  tx: spendAndBurnBeaconUTxOs(txAcc.tx, authVal, utxosToSpend),
                  count: txAcc.count + utxosToSpend.length,
                };
                return {
                  acc: nextAcc,
                  offset: offset + utxosToSpend.length,
                };
              }),
          },
        );
        return acc;
      }),
  );

  if (finalAcc.count > 0) {
    const completedLastTx = yield* finalAcc.tx.completeProgram();
    yield* lucid.switchToOperatorsMainWallet;
    yield* handleSignSubmit(lucid.api, completedLastTx);
  }
}).pipe(
  Effect.mapError((e) =>
    e._tag === "TxBuilderError" || e._tag === "RunTimeError"
      ? new SDK.LucidError({ message: "", cause: e })
      : e,
  ),
);

export const resetDatabases: Effect.Effect<
  void,
  DatabaseError | FileSystemError,
  NodeConfig | Database
> = Effect.all(
  [
    MempoolDB.clear,
    MempoolLedgerDB.clear,
    BlocksDB.clear,
    BlocksTxsDB.clear,
    ImmutableDB.clear,
    LatestLedgerDB.clear,
    ConfirmedLedgerDB.clear,
    AddressHistoryDB.clear,
    deleteMempoolMpt,
    deleteLedgerMpt,
  ],
  { discard: true },
);

export const program: Effect.Effect<
  void,
  | SDK.LucidError
  | TxSubmitError
  | TxSignError
  | TxConfirmError
  | DatabaseError
  | FileSystemError,
  | Lucid
  | NodeConfig
  | AlwaysSucceedsContract
  | Globals
  | Database
  | TxIngressQueue
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const txIngressQueue = yield* TxIngressQueue;
  const clearResetInProgress = Ref.set(globals.RESET_IN_PROGRESS, false).pipe(
    Effect.catchAllCause((cause) =>
      Effect.logWarning(
        `🚧 Failed to clear RESET_IN_PROGRESS after reset attempt. cause=${String(cause)}`,
      ),
    ),
  );

  yield* Ref.set(globals.RESET_IN_PROGRESS, true);

  const spendAndBurnWithTimeout = spendAndBurntAllUTxOs.pipe(
    Effect.timeoutFail({
      duration: `${RESET_SPEND_AND_BURN_TIMEOUT_MS} millis`,
      onTimeout: () =>
        new SDK.LucidError({
          message: `Timed out after ${RESET_SPEND_AND_BURN_TIMEOUT_MS}ms while collecting and burning reset UTxOs`,
          cause: "Timed out waiting for L1/provider response during reset",
        }),
    }),
    Effect.tapError((e) =>
      Effect.logWarning(
        `🚧 Reset spend-and-burn step failed; aborting reset without clearing local state. cause=${e instanceof Error ? e.message : String(e)}`,
      ),
    ),
  );

  yield* Effect.gen(function* () {
    yield* Effect.logInfo(`🚧 Reset started.`);
    // Reset is fail-closed: only clear local state after on-chain spend-and-burn succeeds.
    yield* spendAndBurnWithTimeout;
    yield* resetDatabases;
    yield* txIngressQueue.clear.pipe(
      Effect.catchTag("TxIngressQueueError", (e) =>
        Effect.logWarning(
          `🚧 Failed to clear tx ingress streams during reset; continuing. cause=${e.message}`,
        ),
      ),
    );

    yield* Effect.logInfo(`🚧 Resetting global variables...`);
    yield* Ref.set(globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH, Date.now());
    yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
    yield* Effect.logInfo(`🚧 Reset completed.`);
  }).pipe(Effect.ensuring(clearResetInProgress));
});

export const repairStateQueueRootUnitsProgram: Effect.Effect<
  void,
  SDK.LucidError | TxSubmitError | TxSignError | TxConfirmError,
  Lucid | AlwaysSucceedsContract | Globals
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const clearResetInProgress = Ref.set(globals.RESET_IN_PROGRESS, false).pipe(
    Effect.catchAllCause((cause) =>
      Effect.logWarning(
        `🚧 Failed to clear RESET_IN_PROGRESS after root-unit repair attempt. cause=${String(cause)}`,
      ),
    ),
  );

  yield* Ref.set(globals.RESET_IN_PROGRESS, true);

  yield* Effect.gen(function* () {
    const lucid = yield* Lucid;
    const validators = yield* AlwaysSucceedsContract;
    const stateQueueValidator = validators.stateQueue;
    const stateQueueAddress = stateQueueValidator.spendingScriptAddress;
    const rootUnit = stateQueueValidator.policyId + SDK.NODE_ASSET_NAME;

    const rootUtxos = yield* Effect.tryPromise({
      try: () => lucid.api.utxosAtWithUnit(stateQueueAddress, rootUnit),
      catch: (e) =>
        new SDK.LucidError({
          message: `Failed to fetch state-queue root-unit UTxOs at ${stateQueueAddress}`,
          cause: e,
        }),
    });
    const outRefs = rootUtxos.map((u) => `${u.txHash}#${u.outputIndex}`);

    if (rootUtxos.length <= 1) {
      yield* Effect.logInfo(
        `🚧 Root-unit repair skipped; state queue root-unit count is ${rootUtxos.length} at ${stateQueueAddress}.`,
      );
      return;
    }

    yield* Effect.logWarning(
      `🚧 Root-unit repair burning ${rootUtxos.length} duplicate root-unit UTxO(s). root_unit=${rootUnit} outrefs=${outRefs.join(",")}`,
    );

    const assetsToBurn: Assets = { [rootUnit]: -BigInt(rootUtxos.length) };
    const completedTx = yield* lucid.api
      .newTx()
      .collectFrom(rootUtxos, Data.void())
      .mintAssets(assetsToBurn, Data.void())
      .attach.Script(stateQueueValidator.spendingScript)
      .attach.Script(stateQueueValidator.mintingScript)
      .completeProgram()
      .pipe(
        Effect.mapError(
          (e) =>
            new SDK.LucidError({
              message: "Failed to build root-unit repair transaction",
              cause: e,
            }),
        ),
      );

    yield* lucid.switchToOperatorsMainWallet;
    yield* Effect.logInfo(`🚧 Root-unit repair submitting transaction...`);
    yield* handleSignSubmit(lucid.api, completedTx);
    yield* Effect.logInfo(`🚧 Root-unit repair transaction confirmed.`);

    const remaining = yield* Effect.tryPromise({
      try: () => lucid.api.utxosAtWithUnit(stateQueueAddress, rootUnit),
      catch: (e) =>
        new SDK.LucidError({
          message: `Failed to verify root-unit repair at ${stateQueueAddress}`,
          cause: e,
        }),
    });

    if (remaining.length !== 0) {
      const remainingRefs = remaining.map(
        (u) => `${u.txHash}#${u.outputIndex}`,
      );
      return yield* Effect.fail(
        new SDK.LucidError({
          message: `Root-unit repair incomplete: expected 0 root-unit UTxOs, got ${remaining.length}`,
          cause: `remaining_outrefs=${remainingRefs.join(",")}`,
        }),
      );
    }

    yield* Effect.logInfo(
      `🚧 Root-unit repair completed; state queue root-unit count is now 0.`,
    );
  })
    .pipe(
      Effect.timeoutFail({
        duration: `${REPAIR_ROOT_UNITS_TIMEOUT_MS} millis`,
        onTimeout: () =>
          new SDK.LucidError({
            message: `Timed out after ${REPAIR_ROOT_UNITS_TIMEOUT_MS}ms while repairing state-queue root units`,
            cause:
              "Timed out waiting for L1/provider response during root-unit repair",
          }),
      }),
    )
    .pipe(Effect.ensuring(clearResetInProgress));
});
