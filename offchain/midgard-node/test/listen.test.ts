import { describe, it, expect, beforeAll, afterAll } from "vitest";
import sqlite3 from "sqlite3";
import {
  addBlockUtxosToConfirmedState,
  addToArchive,
  ArchiveTxRow,
  changeLatestBlock,
  clearArchive,
  clearConfirmedState,
  clearMempool,
  initializeDb,
  retrieveMempool,
  storeTx,
} from "../src/commands/listen.js";
import {
  Assets,
  CML,
  LucidEvolution,
  Record,
  UTxO,
} from "@lucid-evolution/lucid";
import { logAbort } from "../src/utils.js";
import { changeCase } from "change-object-case";

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
  const expectedRow1: ArchiveTxRow = {
    tx_cbor: tx1,
    tx_hash: tx1Hash,
  };
  const tx2 =
    "84a300d90102818258207351d824f3565c936f87d67c7352e0d34f13a51e6427bfad2a86e49bbae8a2dc14019582581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60902644a3189c282d9a54e4aeac15f03b16bef0fc17dd8aa6f9bc8b8f1a05f5b9f082581d60f2d5d0b7f8198330ba9410ffecdf45dcc67e774849bfbe8115a8cfcf1b0000000165a6d6800200a100d9010281825820f80e638fcfe3a3d60a785202efa9ff231b804db638896eb4a63bf6e0acceda405840567525cdfd1ee65e702c28901adbc16915a46f140dd91669c160b9b9095691205a26f7f95411ece1f2a9cd19901a6525a449256d4232eadf061db137967f0a09f5f6";
  const tx2Hash = lucid.fromTx(tx2).toHash();
  const expectedRow2: ArchiveTxRow = {
    tx_cbor: tx2,
    tx_hash: tx2Hash,
  };
  const blockHash = lucid.fromTx(tx1).toHash();

  it("should store transaction in the mempool", async () => {
    await storeTx(lucid as unknown as LucidEvolution, db, tx1);
    const result = await retrieveMempool(db);
    expect(result).toStrictEqual([expectedRow1]);
  });

  it("clears the mempool", async () => {
    const initialRows = await retrieveMempool(db);
    expect(initialRows.length).toBe(1);
    await clearMempool(db);
    const result = await retrieveMempool(db);
    expect(result.length).toBe(0);
  });


  it("should store a block hash alongside with it's transactions in the archive", async () => {
    await addToArchive(db, blockHash, [expectedRow1, expectedRow2]);
    const result = await retrieveArchive(db);
    expect(result).toStrictEqual([
      { block_hash: blockHash, ...expectedRow1 },
      { block_hash: blockHash, ...expectedRow2 },
    ]);
  });

  it("clears the archive", async () => {
    const initialRows = await retrieveArchive(db);
    expect(initialRows.length).toBe(2);
    await clearArchive(db);
    const resultBlocks = await new Promise<AchiveBlockRow[]>(
      (resolve, reject) => {
        db.all("SELECT * FROM archive_block", (err, rows: AchiveBlockRow[]) => {
          if (err) {
            logAbort(`Error retrieving archive: ${err.message}`);
            reject(err);
          }
          resolve(rows);
        });
      }
    );
    const resultTxs = await new Promise<ArchiveTxRow[]>((resolve, reject) => {
      db.all("SELECT * FROM archive_tx", (err, rows: ArchiveTxRow[]) => {
        if (err) {
          logAbort(`Error retrieving archive: ${err.message}`);
          reject(err);
        }
        resolve(rows);
      });
    });
    expect(resultBlocks.length).toBe(0);
    expect(resultTxs.length).toBe(0);
  });

  const utxo1: UTxO = {
    txHash: tx1Hash,
    outputIndex: 0,
    address: "aaaa",
    assets: { lovelace: BigInt(25) },
    datum: "e100c1a248cb3e9eb91d1534b176a410312a283100345de6d7f3b7b55ea7b067b4b46a43dca4f674b0682b06ed9f",
    datumHash: "9eead0de42833bbd51866cbafe5d29b8448fa1b9e7430a7af7a7f0e7e9913a07",
    scriptRef: null,
  };
  const utxo2: UTxO = {
    txHash: tx2Hash,
    outputIndex: 0,
    address: "bbbb",
    assets: { abcd: BigInt(12) },
    datum: null,
    datumHash: null,
    scriptRef: {type: "PlutusV3", script: "6e461fe947e14c4a53b905d0aa92f08bff98fb94129f6ed26877fcf1c8a4495192c0b379fb06aa3b"},
  };
  const anotherBlockHash = "aaaa22221111";

  it("adds block's utxos to confirmed state", async () => {
    await addBlockUtxosToConfirmedState(db, blockHash, [utxo1]);
    const result1 = await retrieveConfirmedState(db);
    expect(result1).toStrictEqual([{ blockHash: blockHash, ...utxo1 }]);

    await addBlockUtxosToConfirmedState(db, anotherBlockHash, [utxo2]);
    const result2 = await retrieveConfirmedState(db);
    expect(result2).toStrictEqual([
      { blockHash: blockHash, ...utxo1 },
      { blockHash: anotherBlockHash, ...utxo2 },
    ]);
  });

  it("clears the confirmed state", async () => {
    clearConfirmedState(db);
    const result = await retrieveConfirmedState(db);
    expect(result).toStrictEqual([]);
  });

  it("updates latest block utxos", async () => {
    await changeLatestBlock(db, blockHash, [utxo1]);
    const result1 = await retrieveLatestBlock(db);
    expect(result1).toStrictEqual([{ blockHash: blockHash, ...utxo1 }]);

    await changeLatestBlock(db, anotherBlockHash, [utxo2, utxo1]);
    const result2 = await retrieveLatestBlock(db);
    expect(result2).toStrictEqual([
      { blockHash: anotherBlockHash, ...utxo2 },
      { blockHash: anotherBlockHash, ...utxo1 },
    ]);
  });
});

interface AchiveBlockRow {
  block_hash: string;
  tx_hash: string;
}
interface ArchiveRow {
  block_hash: string;
  tx_hash: string;
  tx_cbor: string;
}

const retrieveArchive = async (db: sqlite3.Database) => {
  const query = `
    SELECT *
    FROM archive_block
    JOIN archive_tx ON archive_block.tx_hash = archive_tx.tx_hash;`;
  return new Promise<ArchiveRow[]>((resolve, reject) => {
    db.all(query, (err, rows: ArchiveRow[]) => {
      if (err) {
        logAbort(`Error retrieving archive: ${err.message}`);
        reject(err);
      }
      resolve(rows);
    });
  });
};

interface ConfirmedStateRow {
  tx_hash: string;
  output_index: number;
  address: string;
  assets: string;
  datum_hash: string;
  datum: string;
  script_ref: string;
}

const retrieveConfirmedState = async (db: sqlite3.Database) => {
  const query = `SELECT * FROM confirmed_state_utxo`;
  return new Promise<[ConfirmedStateRow]>((resolve, reject) => {
    db.all(query, (err, rows: [{ blockHash: string } & LastBlockUtxoRow]) => {
      if (err) {
        logAbort(`Error retrieving latest block utxo: ${err.message}`);
        return reject(err);
      }
      const resultSnakeCase = rows.map((row) => ({
        ...row,
        assets: JSON.parse(row.assets, (_, v) => {
          try {
            return BigInt(v);
          } catch {
            return v;
          }
        }),
      }));
      const result = require("change-object-case").toCamel(resultSnakeCase);
      resolve(result);
    });
  });
};

interface LastBlockUtxoRow {
  tx_hash: string;
  output_index: number;
  address: string;
  assets: string;
  datum_hash: string;
  datum: string;
  script_ref: string;
}

export const retrieveLatestBlock = async (
  db: sqlite3.Database
): Promise<[{ blockHash: string } & UTxO]> => {
  const query = `SELECT * FROM latest_block_utxo`;
  return new Promise((resolve, reject) => {
    db.all(query, (err, rows: [{ blockHash: string } & LastBlockUtxoRow]) => {
      if (err) {
        logAbort(`Error retrieving latest block utxo: ${err.message}`);
        return reject(err);
      }
      const resultSnakeCase = rows.map((row) => ({
        ...row,
        assets: JSON.parse(row.assets, (_, v) => {
          try {
            return BigInt(v);
          } catch {
            return v;
          }
        }),
      }));
      const result = require("change-object-case").toCamel(resultSnakeCase);
      resolve(result);
    });
  });
};
class MockLucid {
  fromTx(tx: string) {
    const tx_body = CML.Transaction.from_cbor_hex(tx).body();
    return {
      toHash: () => CML.hash_transaction(tx_body).to_hex(),
    };
  }
}
