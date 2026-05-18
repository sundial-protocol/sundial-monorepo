import { beforeEach, describe, expect, vi } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer } from "effect";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

const sdkMocks = vi.hoisted(() => ({
  updateLatestBlocksDatumAndGetTheNewHeaderProgram: vi.fn(),
  hashBlockHeader: vi.fn(),
  incompleteCommitBlockHeaderTxProgram: vi.fn(),
}));

const dbCommonMocks = vi.hoisted(() => ({
  deserializeUTxOsFromStorage: vi.fn(),
  serializeUTxOsForStorage: vi.fn(),
}));

const blocksDbMocks = vi.hoisted(() => ({
  getAppendedStateQueueUTxOFromEntry: vi.fn(),
}));

vi.mock("@al-ft/midgard-sdk", () => {
  class LucidError extends Error {
    constructor(fields: { message: string; cause?: unknown }) {
      super(fields.message);
      this.name = "LucidError";
      if (fields.cause !== undefined) {
        (this as Error & { cause?: unknown }).cause = fields.cause;
      }
    }
  }

  return {
    LucidError,
    updateLatestBlocksDatumAndGetTheNewHeaderProgram:
      sdkMocks.updateLatestBlocksDatumAndGetTheNewHeaderProgram,
    hashBlockHeader: sdkMocks.hashBlockHeader,
    incompleteCommitBlockHeaderTxProgram:
      sdkMocks.incompleteCommitBlockHeaderTxProgram,
  };
});

vi.mock("@/database/utils/common.js", () => {
  class DatabaseError extends Error {}

  return {
    DatabaseError,
    deserializeUTxOsFromStorage: dbCommonMocks.deserializeUTxOsFromStorage,
    serializeUTxOsForStorage: dbCommonMocks.serializeUTxOsForStorage,
  };
});

vi.mock("@/database/index.js", () => ({
  Tx: {
    Columns: {
      TX_ID: "tx_id",
      TX: "tx",
    },
  },
  UserEvents: {
    Columns: {
      ID: "event_id",
      INFO: "event_info",
    },
  },
  Ledger: {
    Columns: {
      OUTREF: "outref",
      OUTPUT: "output",
      TIMESTAMPTZ: "time_stamp_tz",
    },
  },
  WithdrawalsDB: {
    entryToOutRef: vi.fn(),
  },
  DepositsDB: {
    entryToLedgerEntry: vi.fn(),
  },
  MempoolLedgerDB: {
    insert: vi.fn(),
    clearUTxOs: vi.fn(),
  },
  BlocksDB: {
    Columns: {
      HEADER_HASH: "header_hash",
      EVENT_START_TIME: "event_start_time",
      EVENT_END_TIME: "event_end_time",
      NEW_WALLET_UTXOS: "new_wallet_utxos",
      PRODUCED_UTXOS: "produced_utxos",
      L1_CBOR: "l1_cbor",
      STATUS: "status",
    },
    Status: {
      UNSUBMITTED: "UNSUBMITTED",
    },
    getAppendedStateQueueUTxOFromEntry:
      blocksDbMocks.getAppendedStateQueueUTxOFromEntry,
  },
}));

import { AlwaysSucceedsContract, Lucid } from "@/services/index.js";
import { BlocksDB } from "@/database/index.js";
import { buildNewBlockEntry } from "@/workers/utils/block-commitment.js";

const makeUtxo = (seed: string): UTxO => ({
  txHash: seed.padEnd(64, "0").slice(0, 64),
  outputIndex: 0,
  address: "addr_test1qpt3xyr5xq0dummyaddressvalue",
  assets: {
    lovelace: 10_000_000n,
  },
});

const makeEntry = (): Parameters<typeof buildNewBlockEntry>[0] =>
  ({
    [BlocksDB.Columns.NEW_WALLET_UTXOS]: Buffer.from([0x01]),
    [BlocksDB.Columns.EVENT_END_TIME]: new Date("2026-01-01T00:00:00.000Z"),
  }) as Parameters<typeof buildNewBlockEntry>[0];

const makeStats = (): BlocksDB.Stats => ({
  deposits_count: 1,
  tx_orders_count: 1,
  tx_requests_count: 1,
  withdrawals_count: 0,
  total_events_size: 10,
});

const makeSignedBuilder = () => ({
  sign: {
    withWallet: () => ({
      completeProgram: () =>
        Effect.succeed({
          toTransaction: () => ({
            to_cbor_bytes: () => new Uint8Array([0xca, 0xfe]),
          }),
        }),
    }),
  },
  toHash: () => "tx-hash",
});

const makeLucidFixture = (freshWalletUTxOs: readonly UTxO[]) => {
  const overrideUTxOs = vi.fn((_utxos: readonly UTxO[]) => undefined);
  const getUtxos = vi.fn(() => Promise.resolve([...freshWalletUTxOs]));
  const lucidApi: Pick<LucidEvolution, "overrideUTxOs" | "wallet"> = {
    overrideUTxOs,
    wallet: () => ({
      overrideUTxOs: vi.fn(),
      address: vi.fn(),
      rewardAddress: vi.fn(),
      getUtxos,
      getUtxosCore: vi.fn(),
      getDelegation: vi.fn(),
      signTx: vi.fn(),
      signMessage: vi.fn(),
      submitTx: vi.fn(),
    }),
  };
  return {
    lucidApi,
    overrideUTxOs,
    getUtxos,
  };
};

const makeRuntimeLayer = (
  lucidApi: Pick<LucidEvolution, "overrideUTxOs" | "wallet">,
) =>
  Layer.mergeAll(
    Layer.succeed(AlwaysSucceedsContract, {
      stateQueue: {
        spendingScript: {
          type: "PlutusV3",
          script: "spend-script",
        },
        spendingScriptAddress: "addr_test_state_queue",
        policyId: "policy-id",
        mintingScript: {
          type: "PlutusV3",
          script: "mint-script",
        },
      },
    } as never),
    Layer.succeed(Lucid, {
      _tag: "Lucid",
      api: lucidApi as LucidEvolution,
      switchToOperatorsMainWallet: Effect.void,
      switchToOperatorsBlockCommitmentWallet: Effect.void,
      switchToOperatorsMergingWallet: Effect.void,
    }),
  );

const setupPrerequisites = (staleWalletUtxos: readonly UTxO[]) => {
  dbCommonMocks.deserializeUTxOsFromStorage.mockReturnValue(
    Effect.succeed(staleWalletUtxos),
  );
  blocksDbMocks.getAppendedStateQueueUTxOFromEntry.mockReturnValue(
    Effect.succeed({
      utxo: makeUtxo("e"),
      datum: {
        next: {
          Key: {
            key: "f".repeat(56),
          },
        },
      },
    }),
  );
  sdkMocks.updateLatestBlocksDatumAndGetTheNewHeaderProgram.mockReturnValue(
    Effect.succeed({
      nodeDatum: { next: { Key: { key: "1".repeat(56) } } },
      header: {
        version: 1n,
      },
    }),
  );
  sdkMocks.hashBlockHeader.mockReturnValue(Effect.succeed("aa"));
};

describe("buildNewBlockEntry wallet topup retry", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it.effect("retries with refreshed wallet UTxOs after chain failure", () =>
    Effect.gen(function* () {
      const staleWalletUtxos: readonly UTxO[] = [makeUtxo("a")];
      const freshWalletUtxosFromProvider: UTxO[] = [makeUtxo("b")];
      const returnedWalletUtxos: readonly UTxO[] = [makeUtxo("c")];
      const producedUtxos: readonly UTxO[] = [makeUtxo("d")];

      const { lucidApi, overrideUTxOs, getUtxos } = makeLucidFixture(
        freshWalletUtxosFromProvider,
      );
      setupPrerequisites(staleWalletUtxos);
      dbCommonMocks.serializeUTxOsForStorage.mockReturnValue(
        Effect.succeed(Buffer.from([0x42])),
      );
      const signBuilder = makeSignedBuilder();

      const firstBuilder = {
        chainProgram: vi.fn(() => Effect.fail(new Error("collateral missing"))),
      };
      const secondBuilder = {
        chainProgram: vi.fn(() =>
          Effect.succeed([returnedWalletUtxos, producedUtxos, signBuilder]),
        ),
      };

      sdkMocks.incompleteCommitBlockHeaderTxProgram
        .mockReturnValueOnce(Effect.succeed(firstBuilder))
        .mockReturnValueOnce(Effect.succeed(secondBuilder));

      yield* buildNewBlockEntry(
        makeEntry(),
        "utxo-root",
        "tx-root",
        "deposit-root",
        "withdrawal-root",
        new Date("2026-01-02T00:00:00.000Z"),
        makeStats(),
      ).pipe(Effect.provide(makeRuntimeLayer(lucidApi)));

      expect(
        sdkMocks.incompleteCommitBlockHeaderTxProgram,
      ).toHaveBeenCalledTimes(2);
      expect(firstBuilder.chainProgram).toHaveBeenCalledTimes(1);
      expect(secondBuilder.chainProgram).toHaveBeenCalledTimes(1);
      expect(getUtxos).toHaveBeenCalledTimes(1);
      expect(overrideUTxOs).toHaveBeenCalledWith(staleWalletUtxos);
      expect(overrideUTxOs).toHaveBeenLastCalledWith(
        freshWalletUtxosFromProvider,
      );
    }),
  );

  it.effect("persists returned wallet and produced UTxOs", () =>
    Effect.gen(function* () {
      const staleWalletUtxos: readonly UTxO[] = [makeUtxo("a")];
      const freshWalletUtxosFromProvider: UTxO[] = [makeUtxo("b")];
      const returnedWalletUtxos: readonly UTxO[] = [makeUtxo("c")];
      const producedUTxOs: readonly UTxO[] = [makeUtxo("d")];
      const serializedWallet = Buffer.from([0xa1]);
      const serializedProduced = Buffer.from([0xb2]);

      const { lucidApi, getUtxos } = makeLucidFixture(
        freshWalletUtxosFromProvider,
      );
      setupPrerequisites(staleWalletUtxos);
      dbCommonMocks.serializeUTxOsForStorage.mockImplementation(
        (utxos: readonly UTxO[]) => {
          if (utxos === returnedWalletUtxos) {
            return Effect.succeed(serializedWallet);
          }
          if (utxos === producedUTxOs) {
            return Effect.succeed(serializedProduced);
          }
          return Effect.fail(
            new Error("Unexpected UTxO set for serialization"),
          );
        },
      );

      const txBuilder = {
        chainProgram: vi.fn(() =>
          Effect.succeed([
            returnedWalletUtxos,
            producedUTxOs,
            makeSignedBuilder(),
          ]),
        ),
      };
      sdkMocks.incompleteCommitBlockHeaderTxProgram.mockReturnValue(
        Effect.succeed(txBuilder),
      );

      const result = yield* buildNewBlockEntry(
        makeEntry(),
        "utxo-root",
        "tx-root",
        "deposit-root",
        "withdrawal-root",
        new Date("2026-01-02T00:00:00.000Z"),
        makeStats(),
      ).pipe(Effect.provide(makeRuntimeLayer(lucidApi)));

      expect(getUtxos).not.toHaveBeenCalled();
      expect(result[BlocksDB.Columns.NEW_WALLET_UTXOS]).toEqual(
        serializedWallet,
      );
      expect(result[BlocksDB.Columns.PRODUCED_UTXOS]).toEqual(
        serializedProduced,
      );
      expect(result[BlocksDB.Columns.STATUS]).toBe(BlocksDB.Status.UNSUBMITTED);
      expect(txBuilder.chainProgram).toHaveBeenCalledTimes(1);
      expect(
        sdkMocks.incompleteCommitBlockHeaderTxProgram,
      ).toHaveBeenCalledTimes(1);
    }),
  );
});
