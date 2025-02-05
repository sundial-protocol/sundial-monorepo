import { describe, it, expect, beforeAll, afterAll } from "vitest";
import sqlite3 from "sqlite3";
import { changeLatestBlock, initializeDb } from "../src/database.js";
import { CML, LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { logAbort } from "../src/utils.js";
import * as blocks from "../src/database/blocks.js";
import * as mempool from "../src/database/mempool.js";
import * as immutable from "../src/database/immutable.js";
import * as confirmedLedger from "../src/database/confirmedLedger.js";
import { utxoFromRow, UtxoRow, utxoToRow } from "../src/database/utils.js";
import { AchiveBlockRow, ArchiveTxRow } from "../src/database/immutable.js";

describe("database", () => {
  let db: sqlite3.Database;
  const dbFilePath = ":memory:";
  const lucid = new MockLucid();

  beforeAll(async () => {
    db = await initializeDb(dbFilePath);
  });

  afterAll(async () => {
    db.close();
  });

  const tx1 =
    "84a300d9010281825820000000000000000000000000000000000000000000000000000000000000000000019582581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d603b493979352930f3eccbd47ae0e12ef6dcea89326211cd80f87cf85a1b00000001dcd95d400200a100d901028182582015c6708ee2da48b2c46c5ab3e001ff636fa0b0e2a48b6676dffc8e36fe02c5985840599c8a11e9888ab60c10f78c708b55f6b542c6d70fa57775b26efe519dc9459604f2e1633601e2b9e448181e694278f3a89d8fea462dcfd772a6a9248e1f730df5f6";
  const tx1Hash = lucid.fromTx(tx1).toHash();

  const tx2 =
    "84a300d90102818258207351d824f3565c936f87d67c7352e0d34f13a51e6427bfad2a86e49bbae8a2dc14019582581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60f2d5d0b7f8198330ba9410ffecdf45dcc67e774849bfbe8115a8cfcf1b0000000165a6d6800200a100d9010281825820f80e638fcfe3a3d60a785202efa9ff231b804db638896eb4a63bf6e0acceda405840567525cdfd1ee65e702c28901adbc16915a46f140dd91669c160b9b9095691205a26f7f95411ece1f2a9cd19901a6525a449256d4232eadf061db137967f0a09f5f6";
  const tx2Hash = lucid.fromTx(tx2).toHash();

  const block1Hash = "aaaaaaaaaaaaaaaaaa";
  const block2Hash = "bbbbbbbbbbbbbbbbbb";

  it("should store tx hashes in blocks", async () => {
    await blocks.insert(db, block1Hash, [tx1Hash]);
    const result1 = await blocks.retrieve(db);
    expect(result1.map((o) => Object.values(o))).toStrictEqual([
      [block1Hash, tx1Hash],
    ]);

    await blocks.insert(db, block2Hash, [tx2Hash]);
    const result2 = await blocks.retrieve(db);
    expect(result2.map((o) => Object.values(o))).toStrictEqual([
      [block1Hash, tx1Hash],
      [block2Hash, tx2Hash],
    ]);
  });

  it("clears blocks", async () => {
    await blocks.clear(db);
    const result = await blocks.retrieve(db);
    expect(result).toStrictEqual([]);
  });

  it("should store transaction in the mempool", async () => {
    await blocks.insert(db, block1Hash, [tx1Hash]);
    await blocks.insert(db, block2Hash, [tx2Hash]);
    await mempool.insert(db, tx1Hash, tx1);
    const result1 = await mempool.retrieve(db);
    expect(result1.map((o) => Object.values(o))).toStrictEqual([[tx1Hash, tx1]]);

    await mempool.insert(db, tx2Hash, tx2);
    const result2 = await mempool.retrieve(db);
    expect(result2.map((o) => Object.values(o))).toStrictEqual(
      [ [tx1Hash, tx1]
      , [tx2Hash, tx2]
      ]
    );
  });

  it("clears the mempool", async () => {
    const initialRows = await mempool.retrieve(db);
    expect(initialRows.length).toBe(2);
    await mempool.clear(db);
    const result = await mempool.retrieve(db);
    expect(result.length).toBe(0);
  });

  // it("should store a block hash alongside with it's transactions in the archive", async () => {
  //   await immutable.insert(db, block1Hash, [expectedRow1, expectedRow2]);
  //   const result = await immutable.retrieve(db);
  //   expect(result).toStrictEqual([
  //     { block_hash: block1Hash, ...expectedRow1 },
  //     { block_hash: block1Hash, ...expectedRow2 },
  //   ]);
  // });

  // it("clears the archive", async () => {
  //   const initialRows = await immutable.retrieve(db);
  //   expect(initialRows.length).toBe(2);
  //   await immutable.clear(db);
  //   const resultBlocks = await new Promise<AchiveBlockRow[]>(
  //     (resolve, reject) => {
  //       db.all("SELECT * FROM archive_block", (err, rows: AchiveBlockRow[]) => {
  //         if (err) {
  //           logAbort(`Error retrieving archive: ${err.message}`);
  //           reject(err);
  //         }
  //         resolve(rows);
  //       });
  //     }
  //   );
  //   const resultTxs = await new Promise<ArchiveTxRow[]>((resolve, reject) => {
  //     db.all("SELECT * FROM archive_tx", (err, rows: ArchiveTxRow[]) => {
  //       if (err) {
  //         logAbort(`Error retrieving archive: ${err.message}`);
  //         reject(err);
  //       }
  //       resolve(rows);
  //     });
  //   });
  //   expect(resultBlocks.length).toBe(0);
  //   expect(resultTxs.length).toBe(0);
  // });

  // const utxo1: UTxO = {
  //   txHash: tx1Hash,
  //   outputIndex: 0,
  //   address: "aaaa",
  //   assets: { lovelace: BigInt(25) },
  //   datum:
  //     "e100c1a248cb3e9eb91d1534b176a410312a283100345de6d7f3b7b55ea7b067b4b46a43dca4f674b0682b06ed9f",
  //   datumHash:
  //     "9eead0de42833bbd51866cbafe5d29b8448fa1b9e7430a7af7a7f0e7e9913a07",
  //   scriptRef: null,
  // };
  // const utxo2: UTxO = {
  //   txHash: tx2Hash,
  //   outputIndex: 0,
  //   address: "bbbb",
  //   assets: { abcd: BigInt(12) },
  //   datum: null,
  //   datumHash: null,
  //   scriptRef: {
  //     type: "PlutusV3",
  //     script:
  //       "6e461fe947e14c4a53b905d0aa92f08bff98fb94129f6ed26877fcf1c8a4495192c0b379fb06aa3b",
  //   },
  // };

  // it("fromRow . toRow == id", async () => {
  //   expect(utxoFromRow(utxoToRow(utxo1))).toStrictEqual(utxo1);
  //   expect(utxoFromRow(utxoToRow(utxo2))).toStrictEqual(utxo2);
  // });

  // it("adds block's utxos to confirmed state", async () => {
  //   await confirmedLedger.insert(db, block1Hash, [utxo1]);
  //   const result1 = await confirmedLedger.retrieve(db);
  //   expect(result1).toStrictEqual([{ block1Hash: block1Hash, ...utxo1 }]);

  //   await confirmedLedger.insert(db, anotherblock1Hash, [utxo2]);
  //   const result2 = await confirmedLedger.retrieve(db);
  //   expect(result2).toStrictEqual([
  //     { block1Hash: block1Hash, ...utxo1 },
  //     { block1Hash: anotherblock1Hash, ...utxo2 },
  //   ]);
  // });

  // it("clears the confirmed state", async () => {
  //   confirmedLedger.clear(db);
  //   const result = await confirmedLedger.retrieve(db);
  //   expect(result).toStrictEqual([]);
  // });

  // it("updates latest block utxos", async () => {
  //   await changeLatestBlock(db, block1Hash, [utxo1]);
  //   const result1 = await retrieveLatestBlock(db);
  //   expect(result1).toStrictEqual([{ block1Hash: block1Hash, ...utxo1 }]);

  //   await changeLatestBlock(db, anotherblock1Hash, [utxo2, utxo1]);
  //   const result2 = await retrieveLatestBlock(db);
  //   expect(result2).toStrictEqual([
  //     { block1Hash: anotherblock1Hash, ...utxo2 },
  //     { block1Hash: anotherblock1Hash, ...utxo1 },
  //   ]);
  // });
});

// export const retrieveLatestBlock = async (
//   db: sqlite3.Database
// ): Promise<({ block1Hash: string } & UTxO)[]> => {
//   return retrieveblock1HashWithUtxosFromTable(db, "latest_block_utxo");
// };

// const retrieveblock1HashWithUtxosFromTable = async (
//   db: sqlite3.Database,
//   tableName: string
// ): Promise<({ block1Hash: string } & UTxO)[]> => {
//   const query = `SELECT * FROM ${tableName}`;
//   return new Promise((resolve, reject) => {
//     db.all(query, (err, rows: ({ block_hash: string } & UtxoRow)[]) => {
//       if (err) {
//         logAbort(`Error retrieving block hash with utxos from table ${tableName}
//           : ${err.message}`);
//         return reject(err);
//       }
//       const result = rows.map(({ block_hash, ...utxoRow }) => {
//         return { block1Hash: block_hash, ...utxoFromRow(utxoRow) };
//       });
//       resolve(result);
//     });
//   });
// };

class MockLucid {
  fromTx(tx: string) {
    const tx_body = CML.Transaction.from_cbor_hex(tx).body();
    return {
      toHash: () => CML.hash_transaction(tx_body).to_hex(),
    };
  }
}
