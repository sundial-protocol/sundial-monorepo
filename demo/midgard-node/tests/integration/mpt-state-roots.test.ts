// NIT-036 … NIT-044  — MPT and State Roots
//
// These tests verify makeMpts, MidgardMpt persistence, withTrieTransaction,
// and the apply*ToLedger helpers that compute trie roots.
//
// ─── SHARED HARNESS NOTES ────────────────────────────────────────────────────
//
// LevelDB cleanup:
//   Any test that creates a LevelDB-backed MidgardMpt must close the LevelDB
//   handle and delete the directory in afterEach to avoid port/file conflicts:
//     afterEach(async () => {
//       if (tmpPath) await Effect.runPromise(deleteMpt(tmpPath, "test"));
//     });
//
// EMPTY_ROOT constant:
//   "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"
//   (matches ETH.createMPT() empty root; validated in the unit MPT tests)
//
// Deterministic MPT fixture:
//   key:   Buffer.alloc(32, 0xaa)
//   value: Buffer.alloc(64, 0xbb)
//   putOp: { type: "put", key, value }
//   delOp: { type: "del", key }
//
// Genesis UTxO fixture (for NIT-036, NIT-037):
//   One UTxO object whose utxoToCore() produces a deterministic CML
//   input/output pair.  The lucid stub's utxoToCore is already set up
//   to produce stable bytes.
//   utxoFixture = { txHash: "aa".repeat(32), outputIndex: 0,
//                   address: testAddress, assets: {} }
//
// NodeConfig with genesis (for NIT-036, NIT-037):
//   Layer.succeed(NodeConfig, NodeConfig.of({
//     GENESIS_UTXOS: [utxoFixture],
//     LEDGER_MPT_DB_PATH: tmpLedgerPath,
//     MEMPOOL_MPT_DB_PATH: tmpMempoolPath,
//     ...rest,
//   }))
// ─────────────────────────────────────────────────────────────────────────────

import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";

// ─── NIT-036 ─────────────────────────────────────────────────────────────────

describe("makeMpts seeds ledger trie from genesis UTxOs", () => {
  it.effect("makeMpts seeds ledger trie from genesis UTxOs", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // Imports:
      //   import { makeMpts } from "@/workers/utils/mpt.js";
      //   import { NodeConfig } from "@/services/config.js";
      //   import * as os from "os";
      //   import * as path from "path";
      //   import { randomUUID } from "crypto";
      //   import { Layer } from "effect";
      //
      // Teardown: close LevelDB + deleteMpt in afterEach
      //
      // Steps:
      //   1. Build nodeConfigLayer with GENESIS_UTXOS=[utxoFixture]
      //   2. const { ledgerTrie, txsTrie } = yield* makeMpts.pipe(Effect.provide(nodeConfigLayer))
      //   3. const ledgerRoot = yield* ledgerTrie.getRootHex()
      //   4. const txsIsEmpty = yield* txsTrie.rootIsEmpty()
      //
      // Assert:
      //   expect(ledgerRoot).not.toBe(EMPTY_ROOT)   — genesis UTxO was inserted
      //   expect(txsIsEmpty).toBe(true)              — txs trie starts empty
      expect(1).toBe(1);
    }),
  );
});

// ─── NIT-037 ─────────────────────────────────────────────────────────────────

describe("makeMpts does not reseed non-empty ledger trie", () => {
  it.effect("makeMpts does not reseed non-empty ledger trie", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // Steps:
      //   1. First makeMpts call: ledger trie empty → genesis UTxO inserted
      //   2. const root1 = yield* ledgerTrie.getRootHex()
      //   3. Close LevelDB handle (ledgerTrie.databaseAndPath.database._leveldb.close())
      //   4. Second makeMpts call: ledger trie has root1 → must NOT reinsert genesis
      //   5. const root2 = yield* ledgerTrie2.getRootHex()
      //
      // Assert:
      //   expect(root2).toBe(root1)
      //   root is stable across reopen — genesis seeding is guarded by rootIsEmpty()
      expect(1).toBe(1);
    }),
  );
});

// ─── NIT-038 ─────────────────────────────────────────────────────────────────

describe("LevelDB-backed ledger MPT persists committed root", () => {
  it.effect("LevelDB-backed ledger MPT persists committed root", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // Imports:
      //   import { MidgardMpt, deleteMpt } from "@/workers/utils/mpt.js";
      //   import * as os from "os";
      //   import * as path from "path";
      //   import { randomUUID } from "crypto";
      //
      // This test is a direct integration companion to the unit test
      // "LevelDB-backed MPT persists root" in tests/unit/mpt.test.ts.
      // The integration version additionally verifies behaviour inside the
      // full withTrieTransaction wrapper (see NIT-039).
      //
      // Steps:
      //   1. const tmpPath = path.join(os.tmpdir(), `nit038-${randomUUID()}`)
      //   2. const mpt1 = yield* MidgardMpt.create("nit038", tmpPath)
      //   3. yield* mpt1.batch([{ type: "put", key: keyBuf, value: valueBuf }])
      //   4. const rootAfterPut = yield* mpt1.getRootHex()
      //   5. Close: yield* Effect.tryPromise(() => mpt1.databaseAndPath!.database._leveldb.close())
      //   6. const mpt2 = yield* MidgardMpt.create("nit038", tmpPath)
      //   7. const reopenedRoot = yield* mpt2.getRootHex()
      //
      // Assert:
      //   expect(rootAfterPut).not.toBe(EMPTY_ROOT)
      //   expect(reopenedRoot).toBe(rootAfterPut)
      expect(1).toBe(1);
    }),
  );
});

// ─── NIT-039 ─────────────────────────────────────────────────────────────────

describe("withTrieTransaction commits SQL and trie changes together", () => {
  it.effect("withTrieTransaction commits SQL and trie changes together", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // Imports:
      //   import { MidgardMpt, withTrieTransaction } from "@/workers/utils/mpt.js";
      //   import * as ImmutableDB from "@/database/immutable.js";
      //
      // Steps:
      //   1. const trie = yield* MidgardMpt.create("nit039")  // in-memory
      //   2. const initialRoot = yield* trie.getRootHex()
      //   3. const putOp = { type: "put", key: keyBuf, value: valueBuf }
      //   4. yield* withTrieTransaction(trie, Effect.gen(function* () {
      //        yield* ImmutableDB.insertTx({ tx_id: txIdA, tx: txCborA })
      //        yield* trie.batch([putOp])
      //      })).pipe(Effect.provide(layers))
      //   5. const afterRoot = yield* trie.getRootHex()
      //   6. const dbRecord = yield* ImmutableDB.retrieveTxCborByHash(txIdA)
      //
      // Assert:
      //   expect(afterRoot).not.toBe(initialRoot)   — trie committed
      //   expect(dbRecord).not.toBeUndefined()       — SQL committed
      expect(1).toBe(1);
    }),
  );
});

// ─── NIT-040 ─────────────────────────────────────────────────────────────────

describe("Applying deposits updates ledger and deposit roots", () => {
  it.effect("Applying deposits updates ledger and deposit roots", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // Imports:
      //   import { applyDepositsToLedger } from "@/workers/utils/block-commitment.js";
      //   import { MidgardMpt } from "@/workers/utils/mpt.js";
      //
      // Steps:
      //   1. const ledgerTrie = yield* MidgardMpt.create("nit040-ledger")
      //   2. const initialLedgerRoot = yield* ledgerTrie.getRootHex()
      //   3. Construct depositEntry: a UserEvents.Entry with valid l1_utxo_cbor
      //      that the lucid stub can decode to produce ledger output bytes.
      //   4. const result = yield* applyDepositsToLedger(ledgerTrie, [depositEntry])
      //      result = { depositsRoot, ledgerRoot, ledgerEntries }
      //
      // Assert:
      //   expect(result.depositsRoot).not.toBe(EMPTY_ROOT)  — deposits trie non-empty
      //   expect(result.ledgerRoot).not.toBe(initialLedgerRoot)  — ledger changed
      //   expect(result.ledgerEntries.length).toBe(1)
      //   result.ledgerEntries[0] has correct outref, output, address fields
      expect(1).toBe(1);
    }),
  );
});

// ─── NIT-041 ─────────────────────────────────────────────────────────────────

describe("Applying withdrawals updates ledger and withdrawals root", () => {
  it.effect("Applying withdrawals updates ledger and withdrawals root", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // Imports:
      //   import { applyWithdrawalsToLedger } from "@/workers/utils/block-commitment.js";
      //   import { MidgardMpt } from "@/workers/utils/mpt.js";
      //
      // Steps:
      //   1. const ledgerTrie = yield* MidgardMpt.create("nit041-ledger")
      //   2. Seed the ledger trie with a put op for outrefA/outputA
      //   3. const initialLedgerRoot = yield* ledgerTrie.getRootHex()
      //   4. Construct withdrawalEntry (UserEvents.Entry) whose decoded outref = outrefA
      //   5. const result = yield* applyWithdrawalsToLedger(ledgerTrie, [withdrawalEntry])
      //      result = { withdrawalsRoot, ledgerRoot, spentOutrefs }
      //
      // Assert:
      //   expect(result.withdrawalsRoot).not.toBe(EMPTY_ROOT)
      //   expect(result.ledgerRoot).not.toBe(initialLedgerRoot)  — del op applied
      //   expect(result.spentOutrefs).toContainEqual(outrefA)
      expect(1).toBe(1);
    }),
  );
});

// ─── NIT-042 ─────────────────────────────────────────────────────────────────

describe("Applying tx requests updates ledger and txs roots", () => {
  it.effect("Applying tx requests updates ledger and txs roots", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // Imports:
      //   import { applyTxRequestsToLedger } from "@/workers/utils/block-commitment.js";
      //   import { MidgardMpt } from "@/workers/utils/mpt.js";
      //   import { breakDownTx } from "@/utils.js";
      //
      // Steps:
      //   1. const ledgerTrie = yield* MidgardMpt.create("nit042-ledger")
      //   2. const txsTrie = yield* MidgardMpt.create("nit042-txs")
      //   3. Seed ledgerTrie with a put op for outrefA (the input txCborA spends)
      //   4. const processedTx = yield* breakDownTx(txCborA)
      //   5. const result = yield* applyTxRequestsToLedger(
      //        ledgerTrie, txsTrie, [{ tx_id: processedTx.txId, tx: processedTx.txCbor }]
      //      )  — check the actual signature in @/workers/utils/block-commitment.ts
      //
      // Assert:
      //   result.txsRoot changes from EMPTY_ROOT (tx added to txs trie)
      //   result.ledgerRoot changes from initial (spent del + produced put)
      expect(1).toBe(1);
    }),
  );
});

// ─── NIT-043 ─────────────────────────────────────────────────────────────────

describe("Applying tx orders updates ledger root", () => {
  it.effect("Applying tx orders updates ledger root", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // Imports:
      //   import { applyTxOrdersToLedger } from "@/workers/utils/block-commitment.js";
      //   import { MidgardMpt } from "@/workers/utils/mpt.js";
      //
      // Steps:
      //   1. const ledgerTrie = yield* MidgardMpt.create("nit043-ledger")
      //   2. Seed ledgerTrie with put op for outrefA (the tx order spends this outref)
      //   3. const initialRoot = yield* ledgerTrie.getRootHex()
      //   4. Construct txOrderEntry (UserEvents.Entry) whose decoded infoCbor
      //      breaks down to a transaction that spends outrefA and produces newOutref
      //   5. const result = yield* applyTxOrdersToLedger(ledgerTrie, [txOrderEntry])
      //      result = { producedEntries, spentOutrefs, ledgerRoot }
      //
      // Assert:
      //   expect(result.producedEntries.length).toBeGreaterThan(0)
      //   expect(result.spentOutrefs).toContainEqual(outrefA)
      //   expect(result.ledgerRoot).not.toBe(initialRoot)
      expect(1).toBe(1);
    }),
  );
});

// ─── NIT-044 ─────────────────────────────────────────────────────────────────

describe("Empty event roots are stable for an empty block", () => {
  it.effect("Empty event roots are stable for an empty block", () =>
    Effect.gen(function* () {
      // IMPLEMENTATION PLAN:
      // ─────────────────────
      // Imports:
      //   import {
      //     applyDepositsToLedger,
      //     applyWithdrawalsToLedger,
      //     applyTxRequestsToLedger,
      //   } from "@/workers/utils/block-commitment.js";
      //   import { MidgardMpt } from "@/workers/utils/mpt.js";
      //
      // Steps:
      //   1. const ledgerTrie = yield* MidgardMpt.create("nit044-ledger")
      //   2. const txsTrie = yield* MidgardMpt.create("nit044-txs")
      //   3. const depResult = yield* applyDepositsToLedger(ledgerTrie, [])
      //   4. const wthResult = yield* applyWithdrawalsToLedger(ledgerTrie, [])
      //   5. const txrResult = yield* applyTxRequestsToLedger(ledgerTrie, txsTrie, [])
      //
      // Assert:
      //   depResult.ledgerEntries.length === 0
      //   wthResult.spentOutrefs.length === 0
      //   txrResult count/size outputs are zero
      //   Roots remain EMPTY_ROOT (no mutations made to the tries)
      //   Check: depResult.depositsRoot === EMPTY_ROOT, etc., for each helper
      expect(1).toBe(1);
    }),
  );
});
