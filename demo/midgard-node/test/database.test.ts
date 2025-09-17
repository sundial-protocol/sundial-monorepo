import { describe, expect, beforeAll } from "vitest";
import { toHex } from "@lucid-evolution/lucid";
import dotenv from "dotenv";
dotenv.config({ path: ".env" });
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import { Database } from "../src/services/database.js";
import { NodeConfig, User } from "../src/config.js";
import { initializeDb } from "../src/database/init.js";
import {
  // Block
  BlocksDB,

  // Address history
  AddressHistoryDB,

  // Tx
  ImmutableDB,
  ProcessedMempoolDB,
  MempoolDB,

  // Ledger
  LatestLedgerDB,
  MempoolLedgerDB,
  ConfirmedLedgerDB,

  // Utils
  TxUtils,
  LedgerUtils,
} from "../src/database/index.js";
import { ProcessedTx } from "../src/utils.js";

const provideLayers = <A, E, R>(eff: Effect.Effect<A, E, R>) =>
  eff.pipe(
    Effect.provide(Database.layer),
    Effect.provide(User.layer),
    Effect.provide(NodeConfig.layer),
  );

const flushAll = Effect.gen(function* () {
  yield* Effect.all(
    [
      MempoolLedgerDB.clear,
      LatestLedgerDB.clear,
      ConfirmedLedgerDB.clear,
      BlocksDB.clear,
      ImmutableDB.clear,
      MempoolDB.clear,
      AddressHistoryDB.clear,
      ProcessedMempoolDB.clear,
    ],
    { discard: true },
  );
});

const randomBytes = (n: number) =>
  Buffer.from(Array.from({ length: n }, () => Math.floor(Math.random() * 256)));

beforeAll(async () => {
  await Effect.runPromise(
    provideLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        // Ensure a clean schema: drop tables (and thus indexes) if they exist
        yield* sql`
          DROP SCHEMA public CASCADE;
          CREATE SCHEMA public;`;
        yield* initializeDb();
        yield* flushAll;
      }),
    ),
  );
});

describe("Database: initialization and basic operations", () => {
  it.effect("initialize and flush", (_) =>
    provideLayers(
      Effect.gen(function* () {
        yield* flushAll;
        // Smoke select to ensure connection works
        const sql = yield* SqlClient.SqlClient;
        const now = yield* sql<Date>`SELECT NOW()`;
        expect(now.length).toBeGreaterThan(0);
      }),
    ),
  );
});

// TODO: clearBlock, clearAll
describe("BlocksDB", () => {
  it.effect("insert, retrieve all, retrieve by header, retrieve by tx", (_) =>
    provideLayers(
      Effect.gen(function* () {
        yield* flushAll;

        // insert with some txs
        yield* BlocksDB.insert(blockHeader1, [tx1, tx2]);

        // retrieve tx hashes by header
        const txs = yield* BlocksDB.retrieveTxHashesByHeaderHash(blockHeader1);
        const txsHex = txs.map((row) => toHex(row));
        expect(new Set(txsHex)).toStrictEqual(new Set([toHex(tx1), toHex(tx2)]));

        // retrieve header by tx hash
        const retrievedHeader = yield* BlocksDB.retrieveHeaderHashByTxHash(tx1);
        expect(toHex(retrievedHeader)).toEqual(toHex(blockHeader1));

        // retrieve all
        const all = yield* BlocksDB.retrieve();
        expect(
          new Set(
            all.map((a) => ({ [BlocksDB.Columns.HEADER_HASH]: a[BlocksDB.Columns.HEADER_HASH], [BlocksDB.Columns.TX_ID]: a[BlocksDB.Columns.TX_ID] })),
          ),
        ).toStrictEqual(
          new Set([
            { [BlocksDB.Columns.HEADER_HASH]: blockHeader1, [BlocksDB.Columns.TX_ID]: tx1 },
            { [BlocksDB.Columns.HEADER_HASH]: blockHeader1, [BlocksDB.Columns.TX_ID]: tx2 },
          ]),
        );
      }),
    ),
  );
});

// TODO: insert multiple, clearTxs, clearAll
describe("MempoolDB", () => {
  it.effect("insert, retrieve all, retrieve cbor by hash, retrieve cbor by hashes, retrieve count", (_) =>
    provideLayers(
      Effect.gen(function* () {
        yield* flushAll;

        const pTxId = randomBytes(32);
        const pTx1 = randomBytes(64);
        const pSpent = randomBytes(32);
        const processedTx1: ProcessedTx = {
          txId: pTxId,
          txCbor: pTx1,
          spent: [pSpent],
          produced: [ledgerEntry1],
        };

        yield* MempoolDB.insert(processedTx1);

        const gotOne = yield* MempoolDB.retrieveTxCborByHash(pTxId);
        expect(toHex(gotOne)).toEqual(toHex(pTx1));

        const gotMany = yield* MempoolDB.retrieveTxCborsByHashes([pTxId]);
        expect(gotMany.map((r) => toHex(r))).toStrictEqual([toHex(pTx1)]);

        const gotAll = yield* MempoolDB.retrieve();
        expect(
          gotAll.map(e => removeTimestampFromTxEntry(e)),
        ).toStrictEqual([
          {
            [TxUtils.Columns.TX_ID]: pTxId,
            [TxUtils.Columns.TX]: pTx1,
          },
        ]);

        const gotCount: number = yield* MempoolDB.retrieveTxCount;
        expect(gotCount).toEqual(1);
      }),
    ),
  );
});

// TODO: insertTxs, clearAll
describe("ProcessedMempoolDB", () => {
  it.effect("insert tx, retrieve all, retrieve cbor by hash, retrieve cbor by hashes", (_) =>
    provideLayers(
      Effect.gen(function* () {
        yield* flushAll;

        yield* ProcessedMempoolDB.insertTx(txEntry1);

        const gotOne = yield* ProcessedMempoolDB.retrieveTxCborByHash(txId1);
        expect(toHex(gotOne)).toEqual(toHex(tx1));

        const gotMany = yield* ProcessedMempoolDB.retrieveTxCborsByHashes([txId1]);
        expect(gotMany.map((r) => toHex(r))).toStrictEqual([toHex(tx1)]);

        const gotAll = yield* ProcessedMempoolDB.retrieve;
        expect(
          gotAll.map(e => removeTimestampFromTxEntry(e)),
        ).toStrictEqual([
          {
            [TxUtils.Columns.TX_ID]: txId1,
            [TxUtils.Columns.TX]: tx1,
          },
        ]);
      }),
    ),
  );
});

// TODO: insertTxs, clearAll
describe("ImmutableDB", () => {
  it.effect("insert, retrieve all, retrieve cbor by hash, retrieve cbor by hashes", (_) =>
    provideLayers(
      Effect.gen(function* () {
        yield* flushAll;

        yield* ImmutableDB.insertTx(txEntry1);

        const gotOne = yield* ImmutableDB.retrieveTxCborByHash(txId1);
        expect(toHex(gotOne)).toEqual(toHex(tx1));

        const gotMany = yield* ImmutableDB.retrieveTxCborsByHashes([txId1]);
        expect(gotMany.map((r) => toHex(r))).toStrictEqual([toHex(tx1)]);

        const gotAll: TxUtils.EntryWithTimeStamp[] =
          yield* ImmutableDB.retrieve;
        expect(
          gotAll.map((e : TxUtils.EntryWithTimeStamp) => removeTimestampFromTxEntry(e))
        ).toStrictEqual([{
          [TxUtils.Columns.TX_ID]: txId1,
          [TxUtils.Columns.TX]: tx1,
        }]);
      }),
    ),
  );
});

// TODO: clearUTxOs, clearAll
describe("LatestLedgerDB", () => {
  it.effect("insert multiple, retrieve", () =>
    provideLayers(
      Effect.gen(function* () {
        yield* flushAll;

        // insert multiple
        yield* LatestLedgerDB.insertMultiple([ledgerEntry1]);

        // retrieve all
        const all = yield* LatestLedgerDB.retrieve;
        expect(
          new Set(all.map((e) => removeTimestampFromLedgerEntry(e)))
        ).toStrictEqual(
          new Set([ledgerEntry1])
        )})
    ),
  );
});

// TODO: clearUTxOs, clearAll
describe("MempoolLedgerDB", () => {
  it.effect("insert, retrieve by address, retrieve all", () =>
    provideLayers(
      Effect.gen(function* () {
        yield* flushAll;

        // insert
        yield* MempoolLedgerDB.insert([ledgerEntry1]);

        // retrieve by address
        const atAddress = yield* MempoolLedgerDB.retrieveByAddress(address1);
        expect(
          new Set(atAddress.map((e) => removeTimestampFromLedgerEntry(e)))
        ).toStrictEqual(
          new Set([ledgerEntry1])
        )

        // retrieve all
        const all = yield* MempoolLedgerDB.retrieve;
        expect(
          new Set(all.map((e) => removeTimestampFromLedgerEntry(e)))
        ).toStrictEqual(
          new Set([ledgerEntry1])
        )})

    ),
  );
});

// TODO: clearUTXOs, clearAll
describe("ConfirmedLedgerDB", () => {
  it.effect("insert multiple, retrieve", () =>
    provideLayers(
      Effect.gen(function* () {
        yield* flushAll;

        // insert
        yield* ConfirmedLedgerDB.insertMultiple([ledgerEntry1]);

        // retrieve all
        const all = yield* ConfirmedLedgerDB.retrieve;
        expect(
          new Set(all.map(e => removeTimestampFromLedgerEntry(e)))
        ).toStrictEqual(
          new Set([ledgerEntry1])
        )
      })
    ),
  );
});

describe("AddressHistoryDB", () => {
  it.effect("insert, retrieve", () =>
    provideLayers(
      Effect.gen(function* () {
        yield* flushAll;

        const pTxId = randomBytes(32);
        const pTx1 = randomBytes(64);
        const pSpent = randomBytes(32);
        const processedTx1: ProcessedTx = {
          txId: pTxId,
          txCbor: pTx1,
          spent: [pSpent],
          produced: [ledgerEntry1],
        };
        const ahEntry1: AddressHistoryDB.Entry = {
          [LedgerUtils.Columns.TX_ID]: pTxId,
          [LedgerUtils.Columns.ADDRESS]: address1,
        };

        // via mempool
        yield* MempoolDB.insert(processedTx1);
        yield* AddressHistoryDB.insertEntries([ahEntry1])
        const expectedViaMempool = yield* AddressHistoryDB.retrieve(address1);
        expect(
          expectedViaMempool.map(t => toHex(t))
        ).toStrictEqual(
          [toHex(pTx1)]
        )
        
        yield* flushAll;

        // via immutable
        const txEntry1: TxUtils.Entry = {
          [TxUtils.Columns.TX_ID]: pTxId,
          [TxUtils.Columns.TX]: pTx1,
        }
        yield* ImmutableDB.insertTx(txEntry1);
        yield* AddressHistoryDB.insertEntries([ahEntry1])
        const expectedViaImmutable = yield* AddressHistoryDB.retrieve(address1);
        expect(
          expectedViaImmutable.map(t => toHex(t))
        ).toStrictEqual(
          [toHex(pTx1)]
        )

      })
    ),
  );
});


const blockHeader1 = randomBytes(32);

const txId1 = randomBytes(32);

const tx1 = randomBytes(64);
const tx2 = randomBytes(64);

const outref1 = randomBytes(36);

const output1 = randomBytes(80);

const address1 = "addr_test1vz9xsqexampleaddress";

const txEntry1: TxUtils.Entry = {
  [TxUtils.Columns.TX_ID]: txId1,
  [TxUtils.Columns.TX]: tx1,
}

const removeTimestampFromTxEntry = (e: TxUtils.EntryWithTimeStamp): TxUtils.EntryNoTimeStamp => {
  return {
    [TxUtils.Columns.TX_ID]: e[TxUtils.Columns.TX_ID],
    [TxUtils.Columns.TX]: e[TxUtils.Columns.TX],
  };
}

const ledgerEntry1: LedgerUtils.Entry = {
  [LedgerUtils.Columns.TX_ID]: txId1,
  [LedgerUtils.Columns.OUTREF]: outref1,
  [LedgerUtils.Columns.OUTPUT]: output1,
  [LedgerUtils.Columns.ADDRESS]: address1,
};

const removeTimestampFromLedgerEntry = (e: LedgerUtils.EntryWithTimeStamp): LedgerUtils.EntryNoTimeStamp => {
  return {
    [LedgerUtils.Columns.TX_ID]: e[LedgerUtils.Columns.TX_ID],
    [LedgerUtils.Columns.OUTREF]: e[LedgerUtils.Columns.OUTREF],
    [LedgerUtils.Columns.OUTPUT]: e[LedgerUtils.Columns.OUTPUT],
    [LedgerUtils.Columns.ADDRESS]: e[LedgerUtils.Columns.ADDRESS],
  };
}




















// describe("Tx tables: mempool & immutable (via Tx utils)", () => {
//   it.effect("insert/retrieve single and multiple, delete and clear", (_) =>
//     provideLayers(
//       Effect.gen(function* () {
//         yield* flushAll;

//         const tx1 = { [TxUtils.Columns.TX_ID]: randomBytes(32), [TxUtils.Columns.TX]: randomBytes(64) } as TxUtils.Entry;
//         const tx2 = { [TxUtils.Columns.TX_ID]: randomBytes(32), [TxUtils.Columns.TX]: randomBytes(64) } as TxUtils.Entry;

//         // mempool (avoid TxUtils; use MempoolDB API)
//         const addr = "addr_test1vz9xsqexampleaddress";
//         const processed1 = makeProcessedFromTx(tx1, addr);
//         const processed2 = makeProcessedFromTx(tx2, addr);
//         yield* MempoolDB.insert(processed1);
//         yield* MempoolDB.insertMultiple([processed2]);

//         const got1 = yield* MempoolDB.retrieveTxCborByHash(tx1.tx_id);
//         const got1Hex = toHex(got1);
//         expect(got1Hex).toEqual(toHex(tx1.tx));

//         const gotBoth = yield* MempoolDB.retrieveTxCborsByHashes([tx1.tx_id, tx2.tx_id]);
//         const gotBothHex = gotBoth.map((row: any) => toHex(row[TxUtils.Columns.TX] as Buffer));
//         expect(new Set(gotBothHex)).toEqual(new Set([toHex(tx1.tx), toHex(tx2.tx)]));

//         yield* MempoolDB.clearTxs([tx1.tx_id, tx2.tx_id]);
//         const afterClear = yield* MempoolDB.retrieve();
//         expect(afterClear.length).toEqual(0);

//         // immutable
//         yield* ImmutableDB.insertTxs([tx1, tx2]);
//         const allImm = yield* ImmutableDB.retrieve;
//         expect(allImm.length).toEqual(2);
//         const immHex = new Set(allImm.map((r: any) => toHex(r.tx as Buffer)));
//         expect(immHex).toEqual(new Set([toHex(tx1.tx), toHex(tx2.tx)]));
//         yield* ImmutableDB.clear;
//         const afterImmClear = yield* ImmutableDB.retrieve;
//         expect(afterImmClear.length).toEqual(0);

//         // mempool retrieve should include proper rows when inserted
//         yield* MempoolDB.insertMultiple([processed1]);
//         const mempoolRows = yield* MempoolDB.retrieve();
//         const mempoolHex = new Set(mempoolRows.map((r: any) => toHex(r.tx as Buffer)));
//         expect(mempoolHex).toEqual(new Set([toHex(tx1.tx)]));

//         // // processed_mempool mirrors tx table selects
//         // yield* ProcessedMempoolDB.insertTxs([tx1, tx2]);
//         // const pmAll = yield* ProcessedMempoolDB.retrieve;
//         // expect(pmAll.length).toEqual(2);
//         // const pmHex = new Set(pmAll.map((r: any) => toHex(r.tx as Buffer)));
//         // expect(pmHex).toEqual(new Set([toHex(tx1.tx), toHex(tx2.tx)]));
//         // const pm1 = yield* ProcessedMempoolDB.retrieveTxCborByHash(tx1.tx_id);
//         // expect(Buffer.isBuffer(pm1)).toBeTruthy();
//         // expect(toHex(pm1)).toEqual(toHex(tx1.tx));
//         // const pmBoth = yield* ProcessedMempoolDB.retrieveTxCborsByHashes([tx1.tx_id, tx2.tx_id]);
//         // const pmBothHex = pmBoth.map((row: any) => toHex(row[TxUtils.Columns.TX] as Buffer));
//         // expect(new Set(pmBothHex)).toEqual(new Set([toHex(tx1.tx), toHex(tx2.tx)]));

//         // // mempool count
//         // const count = yield* MempoolDB.retrieveTxCount;
//         // const countNum = Number(count);
//         // expect(Number.isFinite(countNum)).toBeTruthy();
//         // expect(countNum).toBeGreaterThanOrEqual(2);
//       })
//     )
//   );
// });

// describe("Ledger tables: mempool/latest/confirmed", () => {
//   it.effect("insert entries, retrieve, filter by address, delete UTxOs", (_) =>
//     provideLayers(
//       Effect.gen(function* () {
//         yield* flushAll;

//         const addr = "addr_test1vz9xsqexampleaddress";
//         const entryA: LedgerUtils.Entry = {
//           [LedgerUtils.Columns.TX_ID]: randomBytes(32),
//           [LedgerUtils.Columns.OUTREF]: randomBytes(36),
//           [LedgerUtils.Columns.OUTPUT]: randomBytes(80),
//           [LedgerUtils.Columns.ADDRESS]: addr,
//         };
//         const entryB: LedgerUtils.Entry = {
//           [LedgerUtils.Columns.TX_ID]: randomBytes(32),
//           [LedgerUtils.Columns.OUTREF]: randomBytes(36),
//           [LedgerUtils.Columns.OUTPUT]: randomBytes(80),
//           [LedgerUtils.Columns.ADDRESS]: addr,
//         };

//         // mempool_ledger
//         yield* MempoolLedgerDB.insert([entryA, entryB]);
//         const all = yield* MempoolLedgerDB.retrieve;
//         expect(all.length).toEqual(2);
//         const allHex = new Set(all.map((r: any) => toHex(r.outref as Buffer)));
//         expect(allHex).toEqual(new Set([toHex(entryA.outref), toHex(entryB.outref)]));
//         const byAddr = yield* MempoolLedgerDB.retrieveByAddress(addr);
//         expect(byAddr.length).toEqual(2);
//         const byAddrOutrefs = new Set((byAddr as any[]).map((r) => toHex(r.outref as Buffer)));
//         expect(byAddrOutrefs).toEqual(new Set([toHex(entryA.outref), toHex(entryB.outref)]));
//         yield* MempoolLedgerDB.clearUTxOs([entryA.outref]);
//         const afterDel = yield* MempoolLedgerDB.retrieve;
//         expect(afterDel.length).toEqual(1);

//         // latest_ledger
//         yield* LatestLedgerDB.insertMultiple([entryA]);
//         const latest = yield* LatestLedgerDB.retrieve;
//         expect(latest.length).toEqual(1);
//         const latestHex = new Set(latest.map((r: any) => toHex(r.outref as Buffer)));
//         expect(latestHex).toEqual(new Set([toHex(entryA.outref)]));
//         yield* LatestLedgerDB.clearUTxOs([entryA.outref]);
//         const latestAfter = yield* LatestLedgerDB.retrieve;
//         expect(latestAfter.length).toEqual(0);

//         // confirmed_ledger
//         yield* ConfirmedLedgerDB.insertMultiple([entryB]);
//         const confirmed = yield* ConfirmedLedgerDB.retrieve;
//         expect(confirmed.length).toEqual(1);
//         const confirmedHex = new Set(confirmed.map((r: any) => toHex(r.outref as Buffer)));
//         expect(confirmedHex).toEqual(new Set([toHex(entryB.outref)]));
//         yield* ConfirmedLedgerDB.clearUTxOs([entryB.outref]);
//         const confirmedAfter = yield* ConfirmedLedgerDB.retrieve;
//         expect(confirmedAfter.length).toEqual(0);
//       })
//     )
//   );
// });

// describe("AddressHistory", () => {
//   it.effect("join mempool/immutable txs by address", (_) =>
//     provideLayers(
//       Effect.gen(function* () {
//         yield* flushAll;

//         // const addr = "addr_test1vz9xsqexampleaddress";
//         // const txId = randomBytes(32);
//         // const txCbor = randomBytes(100);

//         // // Put a tx in mempool and immutable
//         // yield* TxUtils.insertEntry(MempoolDB.tableName, { [TxUtils.Columns.TX_ID]: txId, [TxUtils.Columns.TX]: txCbor });
//         // yield* TxUtils.insertEntry(ImmutableDB.tableName, { [TxUtils.Columns.TX_ID]: txId, [TxUtils.Columns.TX]: txCbor });

//         // // Insert address history entry linking the same txId
//         // // AddressHistory.Entry expects keys from Ledger Columns
//         // const ahEntry = {
//         //   [LedgerUtils.Columns.TX_ID]: txId,
//         //   [LedgerUtils.Columns.ADDRESS]: addr,
//         // } as unknown as AddressHistoryDB.Entry;
//         // yield* AddressHistoryDB.insertEntries([ahEntry]);

//         // const results = yield* AddressHistoryDB.retrieve(addr);
//         // const resultsHex = results.map((b: Buffer) => toHex(b));
//         // // Expect the inserted tx cbor to be present at least once
//         // const occurrences = resultsHex.filter((h) => h === toHex(txCbor)).length;
//         // expect(occurrences).toBeGreaterThanOrEqual(1);

//         // // Clean up
//         // yield* AddressHistoryDB.delTxHash(txId);
//         // const after = yield* AddressHistoryDB.retrieve(addr);
//         // // After deletion, the join should no longer return this tx
//         // expect(after.find((b) => b.equals(txCbor))).toBeUndefined();
//       })
//     )
//   );
// });
