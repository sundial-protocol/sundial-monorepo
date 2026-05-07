// NIT-036 … NIT-044  — MPT and State Roots

import { describe, expect, afterEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer } from "effect";
import * as os from "node:os";
import * as path from "node:path";
import { randomUUID } from "node:crypto";

import { makeTestSqlLayer } from "./harness/pglite-sql-layer.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import * as DBInitialization from "@/database/init.js";
import * as ImmutableDB from "@/database/immutable.js";
import {
  MidgardMpt,
  makeMpts,
  deleteMpt,
  withTrieTransaction,
} from "@/workers/utils/mpt.js";
import {
  applyDepositsToLedger,
  applyWithdrawalsToLedger,
  applyTxRequestsToLedger,
  applyTxOrdersToLedger,
} from "@/workers/utils/block-commitment.js";
import * as Tx from "@/database/utils/tx.js";
import * as UserEvents from "@/database/utils/user-events.js";
import { breakDownTx } from "@/utils.js";

const EMPTY_ROOT =
  "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421";

// Deterministic MPT fixture key/value.
const keyBuf = Buffer.alloc(32, 0xaa);
const valueBuf = Buffer.alloc(64, 0xbb);

const txCborA = Buffer.alloc(64, 0xbb);
const txIdA = Buffer.alloc(32, 0xaa);

const makeBaseLayers = () =>
  Layer.mergeAll(makeTestSqlLayer(), makeTestNodeConfigLayer());

// ─── NIT-036 ─────────────────────────────────────────────────────────────────

describe("makeMpts seeds ledger trie from genesis UTxOs", () => {
  let tmpLedgerPath: string | undefined;
  let tmpMempoolPath: string | undefined;

  afterEach(async () => {
    if (tmpLedgerPath)
      await Effect.runPromise(deleteMpt(tmpLedgerPath, "ledger"));
    if (tmpMempoolPath)
      await Effect.runPromise(deleteMpt(tmpMempoolPath, "mempool"));
    tmpLedgerPath = undefined;
    tmpMempoolPath = undefined;
  });

  it.effect("makeMpts seeds ledger trie from genesis UTxOs", () =>
    Effect.gen(function* () {
      tmpLedgerPath = path.join(os.tmpdir(), `nit036-ledger-${randomUUID()}`);
      tmpMempoolPath = path.join(os.tmpdir(), `nit036-mempool-${randomUUID()}`);

      const genesisUtxo = {
        txHash: "aa".repeat(32),
        outputIndex: 0,
        address:
          "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58",
        assets: {},
      };
      const nodeConfigLayer = makeTestNodeConfigLayer({
        ledgerMptPath: tmpLedgerPath,
        mempoolMptPath: tmpMempoolPath,
        genesisUtxos: [genesisUtxo],
      });

      const { ledgerTrie, txsTrie } = yield* makeMpts.pipe(
        Effect.provide(nodeConfigLayer),
      );

      const ledgerRoot = yield* ledgerTrie.getRootHex();
      const txsIsEmpty = yield* txsTrie.rootIsEmpty();

      expect(ledgerRoot).not.toBe(EMPTY_ROOT);
      expect(txsIsEmpty).toBe(true);

      yield* Effect.tryPromise(() =>
        ledgerTrie.databaseAndPath!.database._leveldb.close(),
      );
      yield* Effect.tryPromise(() =>
        txsTrie.databaseAndPath!.database._leveldb.close(),
      );
    }),
  );
});

// ─── NIT-037 ─────────────────────────────────────────────────────────────────

describe("makeMpts does not reseed non-empty ledger trie", () => {
  let tmpLedgerPath: string | undefined;
  let tmpMempoolPath: string | undefined;

  afterEach(async () => {
    if (tmpLedgerPath)
      await Effect.runPromise(deleteMpt(tmpLedgerPath, "ledger"));
    if (tmpMempoolPath)
      await Effect.runPromise(deleteMpt(tmpMempoolPath, "mempool"));
    tmpLedgerPath = undefined;
    tmpMempoolPath = undefined;
  });

  it.effect("makeMpts does not reseed non-empty ledger trie", () =>
    Effect.gen(function* () {
      tmpLedgerPath = path.join(os.tmpdir(), `nit037-ledger-${randomUUID()}`);
      tmpMempoolPath = path.join(os.tmpdir(), `nit037-mempool-${randomUUID()}`);

      const genesisUtxo = {
        txHash: "aa".repeat(32),
        outputIndex: 0,
        address:
          "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58",
        assets: {},
      };
      const nodeConfigLayer = makeTestNodeConfigLayer({
        ledgerMptPath: tmpLedgerPath,
        mempoolMptPath: tmpMempoolPath,
        genesisUtxos: [genesisUtxo],
      });

      // First open: empty → seeds genesis.
      const { ledgerTrie: trie1, txsTrie: txs1 } = yield* makeMpts.pipe(
        Effect.provide(nodeConfigLayer),
      );
      const root1 = yield* trie1.getRootHex();
      expect(root1).not.toBe(EMPTY_ROOT);

      yield* Effect.tryPromise(() =>
        trie1.databaseAndPath!.database._leveldb.close(),
      );
      yield* Effect.tryPromise(() =>
        txs1.databaseAndPath!.database._leveldb.close(),
      );

      // Second open: non-empty → must NOT reseed.
      const { ledgerTrie: trie2, txsTrie: txs2 } = yield* makeMpts.pipe(
        Effect.provide(nodeConfigLayer),
      );
      const root2 = yield* trie2.getRootHex();
      expect(root2).toBe(root1);

      yield* Effect.tryPromise(() =>
        trie2.databaseAndPath!.database._leveldb.close(),
      );
      yield* Effect.tryPromise(() =>
        txs2.databaseAndPath!.database._leveldb.close(),
      );
    }),
  );
});

// ─── NIT-038 ─────────────────────────────────────────────────────────────────

describe("LevelDB-backed ledger MPT persists committed root", () => {
  let tmpPath: string | undefined;

  afterEach(async () => {
    if (tmpPath) await Effect.runPromise(deleteMpt(tmpPath, "nit038"));
    tmpPath = undefined;
  });

  it.effect("LevelDB-backed ledger MPT persists committed root", () =>
    Effect.gen(function* () {
      tmpPath = path.join(os.tmpdir(), `nit038-${randomUUID()}`);

      const mpt1 = yield* MidgardMpt.create("nit038", tmpPath);
      yield* mpt1.batch([{ type: "put", key: keyBuf, value: valueBuf }]);
      const rootAfterPut = yield* mpt1.getRootHex();
      expect(rootAfterPut).not.toBe(EMPTY_ROOT);

      yield* Effect.tryPromise(() =>
        mpt1.databaseAndPath!.database._leveldb.close(),
      );

      const mpt2 = yield* MidgardMpt.create("nit038", tmpPath);
      const reopenedRoot = yield* mpt2.getRootHex();
      expect(reopenedRoot).toBe(rootAfterPut);

      yield* Effect.tryPromise(() =>
        mpt2.databaseAndPath!.database._leveldb.close(),
      );
    }),
  );
});

// ─── NIT-039 ─────────────────────────────────────────────────────────────────

describe("withTrieTransaction commits SQL and trie changes together", () => {
  it.effect("withTrieTransaction commits SQL and trie changes together", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      const trie = yield* MidgardMpt.create("nit039");
      const initialRoot = yield* trie.getRootHex();

      yield* withTrieTransaction(
        trie,
        Effect.gen(function* () {
          yield* ImmutableDB.insertTx({ tx_id: txIdA, tx: txCborA });
          yield* trie.batch([{ type: "put", key: keyBuf, value: valueBuf }]);
        }),
      );

      const afterRoot = yield* trie.getRootHex();
      const dbRecord = yield* ImmutableDB.retrieveTxCborByHash(txIdA);

      expect(afterRoot).not.toBe(initialRoot);
      expect(dbRecord).not.toBeUndefined();
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-040 ─────────────────────────────────────────────────────────────────

describe("Applying deposits updates ledger and deposit roots", () => {
  it.effect("Applying deposits updates ledger and deposit roots", () => {
    const stubAlwaysSucceeds = Layer.succeed(
      AlwaysSucceedsContract,
      null as any,
    );
    const layers = Layer.mergeAll(makeBaseLayers(), stubAlwaysSucceeds);
    return Effect.gen(function* () {
      const ledgerTrie = yield* MidgardMpt.create("nit040-ledger");
      const initialLedgerRoot = yield* ledgerTrie.getRootHex();

      // Empty list: no changes — roots stay at EMPTY_ROOT.
      const result = yield* applyDepositsToLedger(ledgerTrie, []).pipe(
        Effect.provide(layers),
      );

      expect(result.depositLedgerEntries.length).toBe(0);
      expect(result.depositsRoot).toBe(EMPTY_ROOT);
      expect(yield* ledgerTrie.getRootHex()).toBe(initialLedgerRoot);
    });
  });
});

// ─── NIT-041 ─────────────────────────────────────────────────────────────────

describe("Applying withdrawals updates ledger and withdrawals root", () => {
  it.effect("Applying withdrawals updates ledger and withdrawals root", () => {
    return Effect.gen(function* () {
      const ledgerTrie = yield* MidgardMpt.create("nit041-ledger");
      const initialLedgerRoot = yield* ledgerTrie.getRootHex();

      // Empty list: no changes.
      const result = yield* applyWithdrawalsToLedger(ledgerTrie, []);

      expect(result.withdrawnOutRefs.length).toBe(0);
      expect(result.withdrawalsRoot).toBe(EMPTY_ROOT);
      expect(yield* ledgerTrie.getRootHex()).toBe(initialLedgerRoot);
    });
  });
});

// ─── NIT-042 ─────────────────────────────────────────────────────────────────

describe("Applying tx requests updates ledger and txs roots", () => {
  it.effect("Applying tx requests updates ledger and txs roots", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      const ledgerTrie = yield* MidgardMpt.create("nit042-ledger");
      const txsTrie = yield* MidgardMpt.create("nit042-txs");

      // Seed ledger with the outref that txCborA spends.
      const inputCborBytes = Buffer.from([0x82, 0x01, 0x02]);
      yield* ledgerTrie.batch([
        { type: "put", key: inputCborBytes, value: Buffer.alloc(16, 0xcc) },
      ]);
      const initialLedgerRoot = yield* ledgerTrie.getRootHex();

      const processedTx = yield* breakDownTx(txCborA);
      const mempoolEntry: Tx.EntryNoTimeStamp = {
        [Tx.Columns.TX_ID]: processedTx.txId,
        [Tx.Columns.TX]: processedTx.txCbor,
      };

      const result = yield* applyTxRequestsToLedger(ledgerTrie, txsTrie, [
        mempoolEntry,
      ]);

      expect(result.txRequestsHashes.length).toBe(1);
      expect(result.txsRoot).not.toBe(EMPTY_ROOT);
      expect(yield* ledgerTrie.getRootHex()).not.toBe(initialLedgerRoot);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-043 ─────────────────────────────────────────────────────────────────

describe("Applying tx orders updates ledger root", () => {
  it.effect("Applying tx orders updates ledger root", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      const ledgerTrie = yield* MidgardMpt.create("nit043-ledger");

      // Seed ledger with the outref that txCborA spends.
      const inputCborBytes = Buffer.from([0x82, 0x01, 0x02]);
      yield* ledgerTrie.batch([
        { type: "put", key: inputCborBytes, value: Buffer.alloc(16, 0xcc) },
      ]);
      const initialLedgerRoot = yield* ledgerTrie.getRootHex();

      // A tx order's INFO is the raw tx CBOR; its ID is the tx hash.
      const txOrderEntry: UserEvents.Entry = {
        [UserEvents.Columns.ID]: txIdA,
        [UserEvents.Columns.INFO]: txCborA,
        [UserEvents.Columns.ASSET_NAME]: "aa".repeat(10),
        [UserEvents.Columns.L1_UTXO_CBOR]: Buffer.alloc(64, 0x00),
        [UserEvents.Columns.INCLUSION_TIME]: new Date(),
      };

      const result = yield* applyTxOrdersToLedger(ledgerTrie, [txOrderEntry]);

      expect(result.producedByTxOrders.length).toBeGreaterThan(0);
      expect(result.spentByTxOrders.length).toBeGreaterThan(0);
      expect(yield* ledgerTrie.getRootHex()).not.toBe(initialLedgerRoot);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-044 ─────────────────────────────────────────────────────────────────

describe("Empty event roots are stable for an empty block", () => {
  it.effect("Empty event roots are stable for an empty block", () => {
    const stubAlwaysSucceeds = Layer.succeed(
      AlwaysSucceedsContract,
      null as any,
    );
    const layers = Layer.mergeAll(makeBaseLayers(), stubAlwaysSucceeds);
    return Effect.gen(function* () {
      const ledgerTrie = yield* MidgardMpt.create("nit044-ledger");
      const txsTrie = yield* MidgardMpt.create("nit044-txs");

      const depResult = yield* applyDepositsToLedger(ledgerTrie, []).pipe(
        Effect.provide(layers),
      );
      const wthResult = yield* applyWithdrawalsToLedger(ledgerTrie, []);
      const txrResult = yield* applyTxRequestsToLedger(ledgerTrie, txsTrie, []);

      expect(depResult.depositLedgerEntries.length).toBe(0);
      expect(depResult.depositsRoot).toBe(EMPTY_ROOT);
      expect(wthResult.withdrawnOutRefs.length).toBe(0);
      expect(wthResult.withdrawalsRoot).toBe(EMPTY_ROOT);
      expect(txrResult.txRequestsHashes.length).toBe(0);
      expect(txrResult.txsRoot).toBe(EMPTY_ROOT);
    });
  });
});
