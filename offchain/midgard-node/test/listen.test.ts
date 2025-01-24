import { describe, it, expect, beforeAll, afterAll } from "vitest";
import sqlite3 from "sqlite3";
import { clearMempool, initializeDb, storeTx } from "../src/commands/listen.js";
import { CML, LucidEvolution } from "@lucid-evolution/lucid";

describe("mempool database", () => {
  let db: sqlite3.Database;
  const dbFilePath = ":memory:";
  const lucid = new MockLucid();

  beforeAll(async () => {
    db = await initializeDb(dbFilePath);
  });

  afterAll(() => {
    db.close();
  });

  const tx =
    "84a300d9010281825820000000000000000000000000000000000000000000000000000000000000000000019582581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d605ceb8240667acbd2cb93d8efe342c0b9fa51f04678e3d5cddec94a881a05f5b9f082581d603b493979352930f3eccbd47ae0e12ef6dcea89326211cd80f87cf85a1b00000001dcd95d400200a100d901028182582015c6708ee2da48b2c46c5ab3e001ff636fa0b0e2a48b6676dffc8e36fe02c5985840599c8a11e9888ab60c10f78c708b55f6b542c6d70fa57775b26efe519dc9459604f2e1633601e2b9e448181e694278f3a89d8fea462dcfd772a6a9248e1f730df5f6";

  it("should store a valid transaction", async () => {
    storeTx(lucid as unknown as LucidEvolution, db, tx);

    db.get(
      "SELECT * FROM mempool WHERE tx_hash = ?",
      [lucid.fromTx(tx).toHash()],
      (err, row: MempoolRow) => {
        expect(err).toBeNull();
        expect(row.tx_cbor).toBe(tx);
      }
    );
  });

  it("clears the mempool", async () => {
    const initialRows = await new Promise<MempoolRow[]>((resolve, reject) => {
      db.all("SELECT * FROM mempool", (err, rows: MempoolRow[]) => {
        if (err) reject(err);
        resolve(rows);
      });
    });
    expect(initialRows.length).toBe(1);
    await clearMempool(db);
    const clearedRows = await new Promise<MempoolRow[]>((resolve, reject) => {
      db.all("SELECT * FROM mempool", (err, rows: MempoolRow[]) => {
        if (err) reject(err);
        resolve(rows);
      });
    });
    expect(clearedRows.length).toBe(0);
  });
});

class MockLucid {
  fromTx(tx: string) {
    const tx_body = CML.Transaction.from_cbor_hex(tx).body();
    return {
      toHash: () => CML.hash_transaction(tx_body).to_hex(),
    };
  }
}

interface MempoolRow {
  tx_hash: string;
  tx_cbor: string;
}
