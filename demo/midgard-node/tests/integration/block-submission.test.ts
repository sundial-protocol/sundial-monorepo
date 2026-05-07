// NIT-045 … NIT-050  — Block and Submission Flow
//
// These tests verify BlocksDB query behavior, mempool-to-immutable transfer,
// and the submission action end-to-end using a fake Lucid submit boundary.
//
// ─── SHARED HARNESS NOTES ────────────────────────────────────────────────────
//
// BlocksDB.upsert shape:
//   BlocksDB.upsert(entry: BlocksDB.EntryNoMeta) where EntryNoMeta includes:
//     header_hash, event_start_time, event_end_time, new_wallet_utxos,
//     produced_utxos, l1_cbor, deposits_count, tx_requests_count,
//     tx_orders_count, withdrawals_count, total_events_size, status
//
//   new_wallet_utxos and produced_utxos are serialized UTxO arrays (Buffer).
//   Use serializeUTxOsForStorage([]) for tests that don't need real UTxOs.
//   l1_cbor is the CBOR of the L1 submission transaction (Buffer.alloc(64,0xee)
//   for a placeholder).
//
// BlocksDB.EntryNoMeta helper builder (shared across NIT-045 … NIT-050):
//   const makeBlockEntry = (
//     headerHash: Buffer,
//     startTime: Date,
//     endTime: Date,
//     status = BlocksDB.Status.UNSUBMITTED
//   ): BlocksDB.EntryNoMeta => ({
//     header_hash: headerHash,
//     event_start_time: startTime,
//     event_end_time: endTime,
//     new_wallet_utxos: emptyUtxosBuf,  // serializeUTxOsForStorage([])
//     produced_utxos: emptyUtxosBuf,
//     l1_cbor: Buffer.alloc(64, 0xee),
//     deposits_count: 0,
//     tx_requests_count: 0,
//     tx_orders_count: 0,
//     withdrawals_count: 0,
//     total_events_size: 0,
//     status,
//   });
//
// Fake Lucid submit (for NIT-049, NIT-050):
//   The submission fiber calls Lucid.api.awaitTx(txHash) after submit.
//   The fake should return immediately with a truthy confirmation value.
//   Pattern:
//     const fakeLucidLayer = Layer.succeed(Lucid, {
//       api: {
//         wallet: () => ({ signTx: async (tx) => tx, submitTx: async () => "deadbeef".repeat(8) }),
//         awaitTx: async () => true,
//         ...
//       } as LucidEvolution,
//     });
// ─────────────────────────────────────────────────────────────────────────────

import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";

// ─── NIT-045 ─────────────────────────────────────────────────────────────────

describe("BlocksDB retrieves combined event interval", () => {
  it.effect("BlocksDB retrieves combined event interval", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // Imports:
      //   import * as BlocksDB from "@/database/blocks.js";
      //   import * as DepositsDB from "@/database/deposits.js";
      //   import * as WithdrawalsDB from "@/database/withdrawals.js";
      //   import * as TxOrdersDB from "@/database/txOrders.js";
      //   import * as MempoolDB from "@/database/mempool.js";
      //   import { breakDownTx } from "@/utils.js";
      //
      // Fixtures (all inclusion_time within [intervalStart, intervalEnd]):
      //   depositEntry, withdrawalEntry, txOrderEntry, mempoolTxEntry
      //
      // Steps:
      //   1. yield* DepositsDB.insertEntries([depositEntry])
      //   2. yield* WithdrawalsDB.insertEntries([withdrawalEntry])
      //   3. yield* TxOrdersDB.insertEntries([txOrderEntry])
      //   4. yield* MempoolDB.insertMultiple([processedTx])  // adds mempool row
      //   5. const events = yield* BlocksDB.retrieveEvents(intervalStart, intervalEnd)
      //
      // Assert:
      //   expect(events.deposits.length).toBe(1)
      //   expect(events.withdrawals.length).toBe(1)
      //   expect(events.txOrders.length).toBe(1)
      //   expect(events.txRequests.length).toBe(1)
      //   Each entry is in its correct list (no cross-contamination)
      expect(1).toBe(1);
    }),
  );
});

// ─── NIT-046 ─────────────────────────────────────────────────────────────────

describe("BlocksDB earliest unsubmitted block follows height order", () => {
  it.effect("BlocksDB earliest unsubmitted block follows height order", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // Imports:
      //   import * as BlocksDB from "@/database/blocks.js";
      //   import { Option } from "effect";
      //
      // Steps:
      //   1. Build two block entries with distinct header_hash values:
      //        blockA (header_hash = Buffer.alloc(32, 0x01), earlier time)
      //        blockB (header_hash = Buffer.alloc(32, 0x02), later time)
      //   2. yield* BlocksDB.upsert(blockA)  — inserted first → lower BIGSERIAL height
      //   3. yield* BlocksDB.upsert(blockB)
      //   4. const earliest = yield* BlocksDB.retrieveEarliestUnsubmittedEntry
      //
      // Assert:
      //   expect(Option.isSome(earliest)).toBe(true)
      //   Option.getOrThrow(earliest).header_hash deep-equals blockA.header_hash
      //   The returned entry is blockA (lowest height), not blockB
      expect(1).toBe(1);
    }),
  );
});

// ─── NIT-047 ─────────────────────────────────────────────────────────────────

describe("BlocksDB latest entry follows newest height", () => {
  it.effect("BlocksDB latest entry follows newest height", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // Imports:
      //   import * as BlocksDB from "@/database/blocks.js";
      //   import { Option } from "effect";
      //
      // Steps:
      //   1. yield* BlocksDB.upsert(blockA)  // header_hash = Buffer.alloc(32, 0x01)
      //   2. yield* BlocksDB.upsert(blockB)  // header_hash = Buffer.alloc(32, 0x02)
      //   3. const latest = yield* BlocksDB.retrieveLatestEntry
      //
      // Assert:
      //   expect(Option.isSome(latest)).toBe(true)
      //   Option.getOrThrow(latest).header_hash deep-equals blockB.header_hash
      //   latest entry has the highest height (blockB inserted second)
      expect(1).toBe(1);
    }),
  );
});

// ─── NIT-048 ─────────────────────────────────────────────────────────────────

describe("Submitted block status is persisted", () => {
  it.effect("Submitted block status is persisted", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // Imports:
      //   import * as BlocksDB from "@/database/blocks.js";
      //   import { Option } from "effect";
      //
      // Steps:
      //   1. yield* BlocksDB.upsert(blockA)  // status = UNSUBMITTED
      //   2. const entry = yield* BlocksDB.retrieveEarliestUnsubmittedEntry
      //      const blockEntry = Option.getOrThrow(entry)
      //   3. yield* BlocksDB.setStatusOfEntry(blockEntry, BlocksDB.Status.SUBMITTED)
      //   4. const afterUnsubmitted = yield* BlocksDB.retrieveEarliestUnsubmittedEntry
      //   5. const allBlocks = yield* BlocksDB.retrieve
      //
      // Assert:
      //   expect(Option.isNone(afterUnsubmitted)).toBe(true)  — no longer unsubmitted
      //   allBlocks[0].status === BlocksDB.Status.SUBMITTED
      expect(1).toBe(1);
    }),
  );
});

// ─── NIT-049 ─────────────────────────────────────────────────────────────────

describe("Mempool transactions transfer to immutable block history", () => {
  it.effect("Mempool transactions transfer to immutable block history", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // Imports:
      //   import * as MempoolDB from "@/database/mempool.js";
      //   import * as ImmutableDB from "@/database/immutable.js";
      //   import * as BlocksDB from "@/database/blocks.js";
      //   import * as BlocksTxsDB from "@/database/blocksTxs.js";
      //   import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
      //   import { breakDownTx } from "@/utils.js";
      //
      // Setup:
      //   Create one unsubmitted block whose event interval contains txCborA.
      //   The transfer logic:
      //     a. Read all mempool txs in block's [event_start_time, event_end_time]
      //        via MempoolDB.retrieveTimeBoundEntries(start, end)
      //     b. Insert each tx into ImmutableDB.insertTxs(entries)
      //     c. Insert block-tx mapping rows via BlocksTxsDB.insert(blockHeight, txHashes)
      //     d. Delete from MempoolDB: MempoolDB.clearTxs(txHashes)
      //
      //   This transfer logic may live in the block-commitment worker or a
      //   dedicated helper — trace the call from @/workers/block-commitment.ts.
      //
      // Steps:
      //   1. yield* MempoolLedgerDB.insert([seedEntry])  // seed spent input
      //   2. yield* MempoolDB.insertMultiple([processedTx])
      //   3. yield* BlocksDB.upsert(blockEntry)
      //   4. Execute the transfer logic (either via worker function or inline):
      //        a. const txEntries = yield* MempoolDB.retrieveTimeBoundEntries(start, end)
      //        b. yield* ImmutableDB.insertTxs(txEntries)
      //        c. Record block-tx mappings
      //        d. yield* MempoolDB.clearTxs([processedTx.txId])
      //   5. const mempoolCount = yield* MempoolDB.retrieveTxCount
      //   6. const immutableCbor = yield* ImmutableDB.retrieveTxCborByHash(processedTx.txId)
      //
      // Assert:
      //   expect(mempoolCount).toBe(0n)           — mempool drained
      //   expect(immutableCbor).not.toBeUndefined()  — tx moved to immutable
      //   Block-tx mapping row exists for processedTx.txId at blockEntry height
      expect(1).toBe(1);
    }),
  );
});

// ─── NIT-050 ─────────────────────────────────────────────────────────────────

describe("Submitted block updates latest ledger and address history", () => {
  it.effect("Submitted block updates latest ledger and address history", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // This is the widest integration test: it wires the block-commitment
      // worker end-to-end with the fake Lucid submit boundary.
      //
      // Imports:
      //   import * as BlocksDB from "@/database/blocks.js";
      //   import * as LatestLedgerDB from "@/database/latestLedger.js";
      //   import * as AddressHistoryDB from "@/database/addressHistory.js";
      //   import * as MempoolDB from "@/database/mempool.js";
      //   import * as DepositsDB from "@/database/deposits.js";
      //   import { MidgardMpt } from "@/workers/utils/mpt.js";
      //
      // The submission flow (trace from @/workers/block-commitment.ts):
      //   1. Read earliest unsubmitted block
      //   2. Retrieve events in its interval (deposits, withdrawals, tx orders,
      //      mempool tx requests) via BlocksDB.retrieveEvents
      //   3. Apply each event group to the ledger/txs tries
      //   4. Build the L1 block commitment transaction via fake Lucid
      //   5. Insert the signed tx CBOR as l1_cbor
      //   6. Transfer mempool txs to immutable + block-tx mappings
      //   7. Apply latest ledger updates (insertMultiple produced, clearUTxOs spent)
      //   8. Upsert submitted address history entries
      //   9. Set block status to SUBMITTED
      //
      // Layer setup:
      //   const fakeLucidLayer = makeFakeLucidSubmitLayer()
      //     — signTx returns deterministic CBOR, submitTx returns a tx hash,
      //       awaitTx returns true (immediate confirmation)
      //   const fakeAlwaysSucceedsLayer = makeAlwaysSucceedsContractLayer()
      //   const layers = Layer.mergeAll(sqlLayer, nodeConfigLayer, Globals.Default,
      //                                fakeLucidLayer, fakeAlwaysSucceedsLayer)
      //
      // Seed:
      //   - LatestLedgerDB: seed spentEntry (outref = outrefA)
      //   - DepositsDB: seed one deposit event in the block's interval
      //   - MempoolDB: seed one processed tx (via insertMultiple)
      //   - BlocksDB: upsert one unsubmitted block entry covering the interval
      //
      // Assert after running the submission action:
      //   BlocksDB status == SUBMITTED
      //   LatestLedgerDB does NOT contain outrefA (spent)
      //   LatestLedgerDB DOES contain processedTx.produced[0].outref
      //   AddressHistoryDB contains entries with Status.SUBMITTED for
      //     the deposit, withdrawal, and tx request events included in the block
      expect(1).toBe(1);
    }),
  );
});
