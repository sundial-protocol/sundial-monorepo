import { NodeConfig } from "@/config.js";
import { UtilsDB } from "@/database/index.js";
import { BatchDBOp, DB } from "@ethereumjs/util";
import { Effect, pipe } from "effect";
import pg from "pg";
import { fromHex } from "@lucid-evolution/lucid";

export class PostgresDB<TKey extends string, TValue extends Uint8Array>
  implements DB<TKey, TValue>
{
  _pool: pg.Pool | undefined;
  _database: Effect.Effect<pg.Pool, Error, NodeConfig>;
  _tableName: string;
  _referenceTableName: string | undefined;

  constructor(tableName: string, referenceTableName?: string, pool?: pg.Pool) {
    this._tableName = tableName;
    this._referenceTableName = referenceTableName;
    this._pool = pool;
    this._database = Effect.gen(function* () {
      const nodeConfig = yield* NodeConfig;
      return nodeConfig.DB_CONN;
    });
  }

  async open(copyFromReference?: true) {
    if (!this._pool) {
      const pool = await Effect.runPromise(
        pipe(this._database, Effect.provide(NodeConfig.layer)),
      );
      this._pool = pool;
      await pool.query(`
CREATE TABLE IF NOT EXISTS ${this._tableName} (
  key BYTEA NOT NULL,
  value BYTEA NOT NULL,
  PRIMARY KEY (key)
);`);
      await this.clear();
    }
    if (this._referenceTableName && copyFromReference) {
      try {
        await this._pool.query(`
INSERT INTO ${this._tableName}
SELECT * FROM ${this._referenceTableName}`);
      } catch (e) {
        throw e;
      }
    }
  }

  async conclude() {
    if (this._pool) {
      await this.transferToReference();
      await this._pool.end();
    }
  }

  async get(key: TKey): Promise<TValue | undefined> {
    if (!this._pool) {
      throw new Error("Database not open");
    } else {
      const query = `SELECT value FROM ${this._tableName} WHERE key = $1`;
      try {
        const result = await this._pool.query(query, [fromHex(key)]);
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
      const query = `INSERT INTO ${this._tableName} (key, value) VALUES ($1, $2)`;
      await this._pool.query(query, [fromHex(key), val]);
    }
  }

  async del(key: TKey): Promise<void> {
    if (!this._pool) {
      throw new Error("Database not open");
    } else {
      const query = `DELETE FROM ${this._tableName} WHERE key = $1`;
      await this._pool.query(query, [fromHex(key)]);
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

  async getAll(): Promise<{ key: Uint8Array; value: Uint8Array }[]> {
    if (!this._pool) {
      throw new Error("Database not open");
    } else {
      const result = await this._pool.query(`SELECT * FROM ${this._tableName}`);
      return result.rows.map((row) => ({
        key: row.key,
        value: row.value,
      }));
    }
  }

  async getAllFromReference(): Promise<
    { key: Uint8Array; value: Uint8Array }[]
  > {
    if (!this._pool) {
      throw new Error("Database not open");
    } else if (this._referenceTableName) {
      const result = await this._pool.query(
        `SELECT * FROM ${this._referenceTableName}`,
      );
      return result.rows.map((row) => ({
        key: row.key,
        value: row.value,
      }));
    } else {
      throw new Error("No reference tables set");
    }
  }

  async clear(): Promise<void> {
    if (!this._pool) {
      throw new Error("Database not open");
    } else {
      await UtilsDB.clearTable(this._pool, this._tableName);
    }
  }

  async clearReference(): Promise<void> {
    if (!this._pool) {
      throw new Error("Database not open");
    } else if (this._referenceTableName) {
      await UtilsDB.clearTable(this._pool, this._referenceTableName);
    } else {
      throw new Error("No reference tables set");
    }
  }

  async transferToReference(): Promise<void> {
    if (!this._pool) {
      throw new Error("Database not open");
    } else if (this._referenceTableName) {
      try {
        await this._pool.query(`BEGIN`);
        await this.clearReference();
        await this._pool.query(`
INSERT INTO ${this._referenceTableName}
SELECT * FROM ${this._tableName}`);
        await this._pool.query(`COMMIT`);
      } catch (e) {
        await this._pool.query(`ROLLBACK`);
        throw e;
      }
    } else {
      throw new Error("No reference tables set");
    }
  }

  shallowCopy(): DB<TKey, TValue> {
    return new PostgresDB(
      this._tableName,
      this._referenceTableName,
      this._pool,
    );
  }
}
