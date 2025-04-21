import { NodeConfig } from "@/config.js";
import { LatestLedgerCloneDB, LatestLedgerDB } from "@/database/index.js";
import { BatchDBOp, DB } from "@ethereumjs/util";
import { Effect, pipe } from "effect";
import pg from "pg";

export class PostgresDB<TKey extends Uint8Array, TValue extends Uint8Array>
  implements DB<TKey, TValue>
{
  _pool: pg.Pool | undefined;
  _database: Effect.Effect<pg.Pool, Error, NodeConfig>;

  constructor() {
    this._pool = undefined;
    this._database = Effect.gen(function* () {
      const nodeConfig = yield* NodeConfig;
      return nodeConfig.DB_CONN;
    });
  }

  async open() {
    const pool = await Effect.runPromise(
      pipe(this._database, Effect.provide(NodeConfig.layer))
    );
    this._pool = pool;
    await pool.query(`
INSERT INTO ${LatestLedgerCloneDB.tableName}
SELECT * FROM ${LatestLedgerDB.tableName}`);
  }

  async get(key: TKey): Promise<TValue | undefined> {
    if (!this._pool) {
      throw new Error("Database not open");
    } else {
      const query =
        `SELECT tx_out_cbor FROM ${LatestLedgerCloneDB.tableName} WHERE tx_in_cbor = $1`;
      try {
        const result = await this._pool.query(query, [key]);
        if (result.rows.length > 0) {
          return result.rows[0].tx_cbor;
        } else {
          return undefined;
        }
      } catch (err) {
        throw err;
      }
    }
  }

  async put(key: TKey, val: TValue): Promise<void> {
    if (!this._pool) {
      throw new Error("Database not open");
    } else {
      const query =
        `INSERT INTO ${LatestLedgerCloneDB.tableName} (tx_in_cbor, tx_out_cbor) VALUES ($1, $2)`;
      await this._pool.query(query, [key, val]);
    }
  }

  async del(key: TKey): Promise<void> {
    if (!this._pool) {
      throw new Error("Database not open");
    } else {
      const query = `DELETE FROM ${LatestLedgerCloneDB.tableName} WHERE tx_in_cbor = $1`;
      await this._pool.query(query, [key]);
    }
  }

  async batch(opStack: BatchDBOp<TKey, TValue>[]): Promise<void> {
    if (!this._pool) {
      throw new Error("Database not open");
    } else {
      try {
        await this._pool.query("BEGIN");
        for (const op of opStack) {
          if (op.type === "del") {
            await this.del(op.key);
          }

          if (op.type === "put") {
            await this.put(op.key, op.value);
          }
        }
        await this._pool.query("COMMIT");
      } catch (err) {
        await this._pool.query("ROLLBACK");
        throw err;
      }
    }
  }

  shallowCopy(): DB<TKey, TValue> {
    return new PostgresDB();
  }
}
