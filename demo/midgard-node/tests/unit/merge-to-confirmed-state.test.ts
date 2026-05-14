import { describe, expect, beforeEach, vi } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer, Ref } from "effect";
import type { LucidEvolution, Script, UTxO } from "@lucid-evolution/lucid";
import type * as SDK from "@al-ft/midgard-sdk";

const sdkMocks = vi.hoisted(() => ({
  fetchConfirmedStateAndItsLinkByUnitProgram: vi.fn(),
}));

vi.mock("@al-ft/midgard-sdk", () => {
  class LucidError extends Error {
    constructor(fields: { message: string; cause?: unknown }) {
      super(fields.message);
      this.name = "LucidError";
    }
  }

  return {
    LucidError,
    fetchConfirmedStateAndItsLinkByUnitProgram:
      sdkMocks.fetchConfirmedStateAndItsLinkByUnitProgram,
  };
});

import { Globals } from "@/services/globals.js";
import {
  buildAndSubmitMergeTx,
  initializeMergeMetrics,
  incrementMergeFailure,
} from "@/transactions/state-queue/merge-to-confirmed-state.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

const failUnusedLucidCall = () => {
  throw new Error("Unexpected Lucid method call in merge unit test");
};

const failUnusedLucidCallAsync = async () => {
  throw new Error("Unexpected Lucid async method call in merge unit test");
};

const makeLucid = () => {
  const utxosAt = vi.fn<LucidEvolution["utxosAt"]>(() => Promise.resolve([]));
  return {
    config: failUnusedLucidCall,
    wallet: failUnusedLucidCall,
    overrideUTxOs: failUnusedLucidCall,
    switchProvider: failUnusedLucidCallAsync,
    newTx: failUnusedLucidCall,
    fromTx: failUnusedLucidCall,
    selectWallet: {
      fromSeed: failUnusedLucidCall,
      fromPrivateKey: failUnusedLucidCall,
      fromAPI: failUnusedLucidCall,
      fromAddress: failUnusedLucidCall,
    },
    currentSlot: failUnusedLucidCall,
    unixTimeToSlot: failUnusedLucidCall,
    utxosAt,
    utxosAtWithUnit: failUnusedLucidCallAsync,
    utxoByUnit: failUnusedLucidCallAsync,
    utxosByOutRef: failUnusedLucidCallAsync,
    delegationAt: failUnusedLucidCallAsync,
    awaitTx: failUnusedLucidCallAsync,
    datumOf: failUnusedLucidCallAsync,
    metadataOf: failUnusedLucidCallAsync,
  } satisfies LucidEvolution;
};

const buildWithLucid = (lucid: LucidEvolution) =>
  buildAndSubmitMergeTx(lucid, fetchConfig, spendScript, mintScript);

const fetchConfig: SDK.StateQueueFetchConfig = {
  stateQueueAddress: "addr_test1statequeue",
  stateQueuePolicyId: "policy-id",
};

const spendScript: Script = {
  type: "PlutusV3",
  script: "spend",
};

const mintScript: Script = {
  type: "PlutusV3",
  script: "mint",
};

const testLayer = Layer.mergeAll(Globals.Default, createMockSqlHarness().layer);

const makeStateQueueUtxo = (index: number): UTxO => ({
  txHash: index.toString(16).padStart(64, "0"),
  outputIndex: index,
  address: fetchConfig.stateQueueAddress,
  assets: {},
});

describe("merge-to-confirmed-state", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it.effect(
    "initializes merge metrics without requiring runtime services",
    () => initializeMergeMetrics,
  );

  it.effect("increments merge failure metric", () => incrementMergeFailure);

  it.effect(
    "skips merge work when the local queue is below the merge minimum",
    () =>
      Effect.gen(function* () {
        const lucid = makeLucid();
        const globals = yield* Globals;
        yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
        yield* Ref.set(
          globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
          Date.now(),
        );

        yield* buildWithLucid(lucid);

        expect(lucid.utxosAt).not.toHaveBeenCalled();
      }).pipe(Effect.provide(testLayer)),
  );

  it.effect("skips merge work while reset is in progress", () =>
    Effect.gen(function* () {
      const lucid = makeLucid();
      const globals = yield* Globals;
      yield* Ref.set(globals.BLOCKS_IN_QUEUE, 8);
      yield* Ref.set(globals.RESET_IN_PROGRESS, true);
      yield* Ref.set(
        globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
        Date.now(),
      );

      yield* buildWithLucid(lucid);

      expect(lucid.utxosAt).not.toHaveBeenCalled();
    }).pipe(Effect.provide(testLayer)),
  );

  it.effect(
    "refreshes stale queue length and records an empty queue when there is no linked block",
    () =>
      Effect.gen(function* () {
        const lucid = makeLucid();
        lucid.utxosAt.mockResolvedValue(
          Array.from({ length: 10 }, (_value, index) =>
            makeStateQueueUtxo(index),
          ),
        );
        sdkMocks.fetchConfirmedStateAndItsLinkByUnitProgram.mockReturnValue(
          Effect.succeed({
            confirmed: { utxo: { txHash: "confirmed", outputIndex: 0 } },
            link: null,
          }),
        );
        const globals = yield* Globals;
        yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
        yield* Ref.set(globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH, 0);

        yield* buildWithLucid(lucid);

        const queueLength = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
        expect(lucid.utxosAt).toHaveBeenCalledWith(
          fetchConfig.stateQueueAddress,
        );
        expect(
          sdkMocks.fetchConfirmedStateAndItsLinkByUnitProgram,
        ).toHaveBeenCalledWith(lucid, fetchConfig);
        expect(queueLength).toBe(0);
      }).pipe(Effect.provide(testLayer)),
  );
});
