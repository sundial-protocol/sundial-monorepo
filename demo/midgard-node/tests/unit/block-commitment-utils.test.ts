import { describe, expect, vi, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer } from "effect";
import * as Ledger from "@/database/utils/ledger.js";
import * as Tx from "@/database/utils/tx.js";
import * as UserEvents from "@/database/utils/user-events.js";
import {
  applyDepositsToLedger,
  applyTxOrdersToLedger,
  applyTxRequestsToLedger,
  applyWithdrawalsToLedger,
} from "@/workers/utils/block-commitment.js";
import type { MidgardMpt } from "@/workers/utils/mpt.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import {
  COMMON_ADDRESSES,
  makeFakeTrie,
  makeLedgerEntry,
  makeTxEntryNoTimeStamp,
  makeUserEventEntry,
} from "./harness/fixtures.js";

const breakDownTxMock = vi.hoisted(() => vi.fn());
const withdrawalsEntryToOutRefMock = vi.hoisted(() => vi.fn());
const depositsEntryToLedgerEntryMock = vi.hoisted(() => vi.fn());
const midgardMptCreateMock = vi.hoisted(() => vi.fn());

vi.mock("@/utils.js", () => ({
  breakDownTx: breakDownTxMock,
}));

vi.mock("@/workers/utils/mpt.js", () => ({
  MptError: class MptError extends Error {},
  MidgardMpt: {
    create: midgardMptCreateMock,
  },
}));

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
      TX_ID: "tx_id",
      OUTREF: "outref",
      OUTPUT: "output",
      ADDRESS: "address",
      TIMESTAMPTZ: "time_stamp_tz",
    },
  },
  WithdrawalsDB: {
    entryToOutRef: withdrawalsEntryToOutRefMock,
  },
  DepositsDB: {
    entryToLedgerEntry: depositsEntryToLedgerEntryMock,
  },
  BlocksDB: {},
}));

beforeEach(() => {
  vi.clearAllMocks();
});

it.effect("applyWithdrawalsToLedger applies all withdrawal events", () =>
  Effect.gen(function* () {
    const ledgerTrie = makeFakeTrie("ledger-root");
    const withdrawalsTrie = makeFakeTrie("withdrawals-root");
    midgardMptCreateMock.mockReturnValue(
      Effect.succeed(withdrawalsTrie as unknown),
    );

    const outRefA = Buffer.from([0xaa]);
    const outRefB = Buffer.from([0xbb]);
    withdrawalsEntryToOutRefMock
      .mockReturnValueOnce(Effect.succeed(outRefA))
      .mockReturnValueOnce(Effect.succeed(outRefB));

    const withdrawalEntries: UserEvents.Entry[] = [
      makeUserEventEntry(0x01, {
        event_info: Buffer.from([0x10, 0x11]),
      }),
      makeUserEventEntry(0x02, {
        event_info: Buffer.from([0x12, 0x13, 0x14]),
      }),
    ];

    const result = yield* applyWithdrawalsToLedger(
      ledgerTrie as unknown as MidgardMpt,
      withdrawalEntries,
    );

    expect(result.withdrawnOutRefs).toEqual([outRefA, outRefB]);
    expect(result.withdrawalsRoot).toBe("withdrawals-root");
    expect(result.sizeOfWithdrawals).toBe(5);
    expect(ledgerTrie.batch).toHaveBeenCalledOnce();
    expect(withdrawalsTrie.batch).toHaveBeenCalledOnce();
  }),
);

it.effect("applyTxOrdersToLedger applies tx orders to the ledger trie", () =>
  Effect.gen(function* () {
    const ledgerTrie = makeFakeTrie("ledger-root");
    const txsTrie = makeFakeTrie("txs-root");
    midgardMptCreateMock.mockReturnValue(Effect.succeed(txsTrie as unknown));

    const spentA = Buffer.from([0xa1]);
    const spentB = Buffer.from([0xb1]);
    const producedA = makeLedgerEntry(0xaa, {
      outref: Buffer.from([0xa2]),
      output: Buffer.from([0xa3]),
      address: COMMON_ADDRESSES.produced,
    });
    const producedB = makeLedgerEntry(0xbb, {
      outref: Buffer.from([0xb2]),
      output: Buffer.from([0xb3]),
      address: COMMON_ADDRESSES.produced,
    });

    breakDownTxMock
      .mockReturnValueOnce(
        Effect.succeed({
          spent: [spentA],
          produced: [producedA],
        }),
      )
      .mockReturnValueOnce(
        Effect.succeed({
          spent: [spentB],
          produced: [producedB],
        }),
      );

    const txOrders: UserEvents.Entry[] = [
      makeUserEventEntry(0x11, { event_info: Buffer.from([0x21, 0x22]) }),
      makeUserEventEntry(0x12, {
        event_info: Buffer.from([0x23, 0x24, 0x25]),
      }),
    ];

    const result = yield* applyTxOrdersToLedger(
      ledgerTrie as unknown as MidgardMpt,
      txOrders,
    );

    expect(result.txOrdersHashes).toEqual([
      txOrders[0][UserEvents.Columns.ID],
      txOrders[1][UserEvents.Columns.ID],
    ]);
    expect(result.spentByTxOrders).toEqual([spentA, spentB]);
    expect(result.producedByTxOrders).toEqual([producedA, producedB]);
    expect(result.sizeOfTxOrders).toBe(5);
    expect(ledgerTrie.batch).toHaveBeenCalledOnce();
    expect(result.txsTrie).toBe(txsTrie);
  }),
);

it.effect("applyTxRequestsToLedger applies mempool tx requests", () =>
  Effect.gen(function* () {
    const ledgerTrie = makeFakeTrie("ledger-root");
    const txsTrie = makeFakeTrie("txs-root");

    breakDownTxMock
      .mockReturnValueOnce(
        Effect.succeed({
          spent: [Buffer.from([0xc1])],
          produced: [
            {
              ...makeLedgerEntry(0xc2, {
                outref: Buffer.from([0xc3]),
                output: Buffer.from([0xc4]),
                address: COMMON_ADDRESSES.produced,
              }),
            },
          ],
        }),
      )
      .mockReturnValueOnce(
        Effect.succeed({
          spent: [Buffer.from([0xd1])],
          produced: [
            {
              ...makeLedgerEntry(0xd2, {
                outref: Buffer.from([0xd3]),
                output: Buffer.from([0xd4]),
                address: COMMON_ADDRESSES.produced,
              }),
            },
          ],
        }),
      );

    const mempoolTxs: Tx.Entry[] = [
      makeTxEntryNoTimeStamp(0x31, { tx: Buffer.from([0x41, 0x42]) }),
      makeTxEntryNoTimeStamp(0x32, { tx: Buffer.from([0x43, 0x44, 0x45]) }),
    ];

    const result = yield* applyTxRequestsToLedger(
      ledgerTrie as unknown as MidgardMpt,
      txsTrie as unknown as MidgardMpt,
      mempoolTxs,
    );

    expect(result.txRequestsHashes).toEqual([
      mempoolTxs[0][Tx.Columns.TX_ID],
      mempoolTxs[1][Tx.Columns.TX_ID],
    ]);
    expect(result.txsRoot).toBe("txs-root");
    expect(result.sizeOfTxRequests).toBe(5);
    expect(txsTrie.batch).toHaveBeenCalledOnce();
    expect(ledgerTrie.batch).toHaveBeenCalledOnce();
  }),
);

it.effect("applyDepositsToLedger applies deposits and returns new root", () => {
  const stubLayer = Layer.mergeAll(
    Layer.succeed(AlwaysSucceedsContract, null as any),
    makeTestNodeConfigLayer(),
  );
  return Effect.gen(function* () {
    const ledgerTrie = makeFakeTrie("ledger-root");
    const depositsTrie = makeFakeTrie("deposits-root");
    midgardMptCreateMock.mockReturnValue(
      Effect.succeed(depositsTrie as unknown),
    );

    const entryA = {
      ...makeLedgerEntry(0xe1, {
        outref: Buffer.from([0xe2]),
        output: Buffer.from([0xe3]),
        address: COMMON_ADDRESSES.produced,
      }),
      [Ledger.Columns.TIMESTAMPTZ]: new Date("2025-01-01T00:00:00.000Z"),
    };
    const entryB = {
      ...makeLedgerEntry(0xf1, {
        outref: Buffer.from([0xf2]),
        output: Buffer.from([0xf3]),
        address: COMMON_ADDRESSES.produced,
      }),
      [Ledger.Columns.TIMESTAMPTZ]: new Date("2025-01-01T00:00:01.000Z"),
    };

    depositsEntryToLedgerEntryMock
      .mockReturnValueOnce(Effect.succeed(entryA))
      .mockReturnValueOnce(Effect.succeed(entryB));

    const deposits: UserEvents.Entry[] = [
      makeUserEventEntry(0x51, { event_info: Buffer.from([0x61, 0x62]) }),
      makeUserEventEntry(0x52, {
        event_info: Buffer.from([0x63, 0x64, 0x65]),
      }),
    ];

    const result = yield* applyDepositsToLedger(
      ledgerTrie as unknown as MidgardMpt,
      deposits,
    );

    expect(result.depositLedgerEntries).toEqual([entryA, entryB]);
    expect(result.depositsRoot).toBe("deposits-root");
    expect(result.sizeOfDeposits).toBe(5);
    expect(ledgerTrie.batch).toHaveBeenCalledOnce();
    expect(depositsTrie.batch).toHaveBeenCalledOnce();
  }).pipe(Effect.provide(stubLayer));
});
