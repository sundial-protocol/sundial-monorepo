import { describe, expect, vi, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer, Option } from "effect";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import { Lucid } from "@/services/lucid.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";

const retrieveLatestEntryFn = vi.hoisted(() => vi.fn());
const upsertFn = vi.hoisted(() => vi.fn());
const serializeUTxOsForStorageFn = vi.hoisted(() => vi.fn());
const fetchConfirmedStateAndItsLinkByUnitProgramFn = vi.hoisted(() => vi.fn());
const fetchUnsortedStateQueueUTxOsProgramFn = vi.hoisted(() => vi.fn());
const headerHashFromStateQueueUTxOFn = vi.hoisted(() => vi.fn());

vi.mock("@/database/index.js", () => ({
  BlocksDB: {
    get retrieveLatestEntry() {
      return retrieveLatestEntryFn();
    },
    upsert: (...args: unknown[]) => upsertFn(...args),
    Columns: {
      HEADER_HASH: "header_hash",
      EVENT_START_TIME: "event_start_time",
      EVENT_END_TIME: "event_end_time",
      NEW_WALLET_UTXOS: "new_wallet_utxos",
      PRODUCED_UTXOS: "produced_utxos",
      L1_CBOR: "l1_cbor",
      STATUS: "status",
      DEPOSITS_COUNT: "deposits_count",
      TX_REQUESTS_COUNT: "tx_requests_count",
      TX_ORDERS_COUNT: "tx_orders_count",
      WITHDRAWALS_COUNT: "withdrawals_count",
      TOTAL_EVENTS_SIZE: "total_events_size",
    },
    Status: {
      SUBMITTED: 1,
    },
  },
}));

vi.mock("@/database/utils/common.js", () => ({
  serializeUTxOsForStorage: (...args: unknown[]) =>
    serializeUTxOsForStorageFn(...args),
}));

vi.mock("@al-ft/midgard-sdk", () => ({
  NODE_ASSET_NAME: "4e6f6465",
  LucidError: class LucidError extends Error {
    _tag = "LucidError";
    constructor(fields: { message: string; cause?: unknown }) {
      super(fields.message);
    }
  },
  fetchConfirmedStateAndItsLinkByUnitProgram: (...args: unknown[]) =>
    fetchConfirmedStateAndItsLinkByUnitProgramFn(...args),
  fetchUnsortedStateQueueUTxOsProgram: (...args: unknown[]) =>
    fetchUnsortedStateQueueUTxOsProgramFn(...args),
  headerHashFromStateQueueUTxO: (...args: unknown[]) =>
    headerHashFromStateQueueUTxOFn(...args),
}));

import { ensureBlocksDBSeededFromChain } from "@/fibers/seed-blocks-db-from-chain.js";

const sqlHarness = createMockSqlHarness();

const fakeLucidLayer = Layer.succeed(
  Lucid,
  Lucid.of({
    _tag: "Lucid",
    api: {
      wallet: () => ({
        getUtxos: async () => {
          throw new Error(
            "main Lucid API should not be used for chain seeding",
          );
        },
      }),
    } as never,
    mainApi: {
      wallet: () => ({
        getUtxos: async () => [],
      }),
    } as never,
    blockCommitmentApi: {
      wallet: () => ({
        getUtxos: async () => [],
      }),
    } as never,
    mergeApi: {
      wallet: () => ({
        getUtxos: async () => {
          throw new Error(
            "merge Lucid API should not be used for chain seeding",
          );
        },
      }),
    } as never,
    reinitializeMergeApi: Effect.void,
    switchToOperatorsMainWallet: Effect.void,
    switchToOperatorsBlockCommitmentWallet: Effect.void,
    switchToOperatorsMergingWallet: Effect.void,
  }),
);

const fakeAlwaysSucceedsLayer = Layer.succeed(AlwaysSucceedsContract, {
  stateQueue: {
    spendingScriptAddress: "addr_test1qstatequeue",
    policyId: "aa".repeat(28),
  },
} as never);

const baseLayer = Layer.mergeAll(
  sqlHarness.layer,
  makeTestNodeConfigLayer(),
  fakeLucidLayer,
  fakeAlwaysSucceedsLayer,
);

const rootNode = {
  utxo: { txHash: "11".repeat(32), outputIndex: 0 },
  datum: {
    key: "Empty",
    next: { Key: { key: "bb".repeat(28) } },
  },
  assetName: "4e6f6465",
};

const firstBlockNode = {
  utxo: { txHash: "22".repeat(32), outputIndex: 0 },
  datum: {
    key: { Key: { key: "bb".repeat(28) } },
    next: { Key: { key: "cc".repeat(28) } },
  },
  assetName: `4e6f6465${"bb".repeat(28)}`,
};

const tailBlockNode = {
  utxo: { txHash: "33".repeat(32), outputIndex: 0 },
  datum: {
    key: { Key: { key: "cc".repeat(28) } },
    next: "Empty",
  },
  assetName: `4e6f6465${"cc".repeat(28)}`,
};

describe("ensureBlocksDBSeededFromChain", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    sqlHarness.reset();
    retrieveLatestEntryFn.mockReturnValue(Effect.succeed(Option.none()));
    upsertFn.mockReturnValue(Effect.succeed(undefined));
    serializeUTxOsForStorageFn.mockReturnValue(
      Effect.succeed(Buffer.from("00", "hex")),
    );
    headerHashFromStateQueueUTxOFn.mockReturnValue(
      Effect.succeed("ab".repeat(28)),
    );
    fetchConfirmedStateAndItsLinkByUnitProgramFn.mockReturnValue(
      Effect.succeed({ confirmed: rootNode, link: firstBlockNode }),
    );
    fetchUnsortedStateQueueUTxOsProgramFn.mockReturnValue(
      Effect.succeed([rootNode, firstBlockNode, tailBlockNode]),
    );
  });

  it.effect(
    "returns already-seeded when BlocksDB already has a latest entry",
    () =>
      Effect.gen(function* () {
        retrieveLatestEntryFn.mockReturnValue(Effect.succeed(Option.some({})));
        const result = yield* ensureBlocksDBSeededFromChain.pipe(
          Effect.provide(baseLayer),
        );
        expect(result).toBe("already-seeded");
        expect(upsertFn).not.toHaveBeenCalled();
        expect(
          fetchConfirmedStateAndItsLinkByUnitProgramFn,
        ).not.toHaveBeenCalled();
      }),
  );

  it.effect(
    "traverses queue via unit lookups and seeds BlocksDB when empty",
    () =>
      Effect.gen(function* () {
        const result = yield* ensureBlocksDBSeededFromChain.pipe(
          Effect.provide(baseLayer),
        );

        expect(result).toBe("seeded");
        expect(
          fetchConfirmedStateAndItsLinkByUnitProgramFn,
        ).toHaveBeenCalledTimes(1);
        expect(fetchUnsortedStateQueueUTxOsProgramFn).toHaveBeenCalledTimes(1);
        expect(upsertFn).toHaveBeenCalledTimes(1);
      }),
  );

  it.effect("returns retry-later when traversal fails", () =>
    Effect.gen(function* () {
      fetchConfirmedStateAndItsLinkByUnitProgramFn.mockReturnValue(
        Effect.fail(new Error("provider unavailable")),
      );

      const result = yield* ensureBlocksDBSeededFromChain.pipe(
        Effect.provide(baseLayer),
      );
      expect(result).toBe("retry-later");
      expect(upsertFn).not.toHaveBeenCalled();
    }),
  );
});
