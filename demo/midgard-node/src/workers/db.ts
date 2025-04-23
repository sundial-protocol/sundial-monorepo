import { NodeConfig } from "@/config.js";
import { UtilsDB } from "@/database/index.js";
import { BatchDBOp, DB } from "@ethereumjs/util";
import { Effect, pipe } from "effect";
import pg from "pg";
import { fromHex, toHex } from "@lucid-evolution/lucid";

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

  open = async (copyFromReference?: true) => {
    if (!this._pool) {
      const pool = await Effect.runPromise(
        pipe(this._database, Effect.provide(NodeConfig.layer)),
      );
      this._pool = pool;
    }
    await this._pool.query(UtilsDB.mkKeyValueCreateQuery(this._tableName));
    if (this._referenceTableName && copyFromReference) {
      try {
        await this._pool.query(`BEGIN`);
        await this.clear();
        await this._pool.query(`
INSERT INTO ${this._tableName}
SELECT * FROM ${this._referenceTableName}`);
        await this._pool.query(`COMMIT`);
      } catch (e) {
        await this._pool.query(`ROLLBACK`);
        throw e;
      }
    } else {
      try {
        await this.clear();
      } catch(e) {
        throw e;
      }
    }
  }

  conclude = async () => {
    if (this._pool) {
      await this.transferToReference();
      await this._pool.end();
    }
  }

  get = async (key: TKey): Promise<TValue | undefined> => {
    if (!this._pool) {
      throw new Error("Database not open");
    } else {
      const query = `SELECT value FROM ${this._tableName} WHERE key = $1`;
      try {
        const result = await this._pool.query(query, [fromHex(key)]);
        if (result.rows.length > 0) {
          return result.rows[0].value;
        } else {
          return undefined;
        }
      } catch (err) {
        throw err;
      }
    }
  }

  put = async (key: TKey, val: TValue): Promise<void> => {
    if (!this._pool) {
      throw new Error("Database not open");
    } else {
      const query = `INSERT INTO ${this._tableName} (key, value) VALUES ($1, $2) ON CONFLICT (key) DO UPDATE SET value = $2`;
      await this._pool.query(query, [fromHex(key), val]);
    }
  }

  del = async (key: TKey): Promise<void> => {
    if (!this._pool) {
      throw new Error("Database not open");
    } else {
      const query = `DELETE FROM ${this._tableName} WHERE key = $1`;
      await this._pool.query(query, [fromHex(key)]);
    }
  }

  batch = async (opStack: BatchDBOp<TKey, TValue>[]): Promise<void> => {
    if (!this._pool) {
      throw new Error("Database not open");
    } else {
      try {
        // await this._pool.query("BEGIN");
        for (const op of opStack) {
          if (op.type === "del") {
            await this.del(op.key);
          }

          if (op.type === "put") {
            await this.put(op.key, op.value);
          }
        }
        // await this._pool.query("COMMIT");
      } catch (err) {
        // await this._pool.query("ROLLBACK");
        throw err;
      }
    }
  }

  getAll = async (): Promise<{ key: Uint8Array; value: Uint8Array }[]> => {
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

  getAllFromReference = async (): Promise<
    { key: Uint8Array; value: Uint8Array }[]
  > => {
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

  clear = async (): Promise<void> => {
    if (!this._pool) {
      throw new Error("Database not open");
    } else {
      await UtilsDB.clearTable(this._pool, this._tableName);
    }
  }

  clearReference = async (): Promise<void> => {
    if (!this._pool) {
      throw new Error("Database not open");
    } else if (this._referenceTableName) {
      await UtilsDB.clearTable(this._pool, this._referenceTableName);
    } else {
      throw new Error("No reference tables set");
    }
  }

  transferToReference = async (): Promise<void> => {
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

  shallowCopy = (): DB<TKey, TValue> => {
    return new PostgresDB(
      this._tableName,
      this._referenceTableName,
      this._pool,
    );
  }
}
