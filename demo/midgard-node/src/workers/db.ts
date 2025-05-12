import { SqlClient } from "@effect/sql";
import { BatchDBOp, bytesToHex, DB, hexToBytes } from "@ethereumjs/util";
import { Effect, Layer } from "effect";
import { CheckpointDB, hexToKeybytes } from "@ethereumjs/mpt";
import { LRUCache } from "lru-cache"
import { UtilsDB } from "@/database/index.js";

export class PostgresCheckpointDB
  extends CheckpointDB
  implements DB<Uint8Array, Uint8Array>
  {

  cache: LRUCache<string, Uint8Array>
  _transactionDepth = 0
  _roots: Uint8Array[] = []
  // _clientLayer: Layer.Layer<SqlClient.SqlClient, never, never>
  _client: SqlClient.SqlClient;
  _tableName: string;
  _referenceTableName: string | undefined;

  constructor(
    // clientLayer: Layer.Layer<SqlClient.SqlClient>,
    client: SqlClient.SqlClient,
    tableName: string,
    referenceTableName?: string,
    options: { cacheSize?: number } = {},
  ) {
    super({
      db: {
        get: async (key: string) => this.get(Buffer.from(key, "hex")),
        put: async (key: string, value: Uint8Array) => this.put(Buffer.from(key, "hex"), value),
        del: async (key: string) => this.del(Buffer.from(key, "hex")),
        batch: async (ops) => this.batch(convertOps(ops))
      } as DB<string, Uint8Array>
    });
     // TODO: tune this value
    this.cache = new LRUCache({ max: options.cacheSize ?? 100 });
    this._client = client;
    // this._clientLayer = clientLayer;

    this._tableName = tableName;
    this._referenceTableName = referenceTableName;
  }

  openEffect = (copyFromReference?: true) => {
    return Effect.gen(function* (this: PostgresCheckpointDB) {
      const sql = yield* SqlClient.SqlClient;
      yield* UtilsDB.mkKeyValueCreateQuery(this._tableName);
      if (this._referenceTableName && copyFromReference) {
        const refTableName = this._referenceTableName;
        sql.withTransaction(
          Effect.gen(function* (this: PostgresCheckpointDB) {
            yield* UtilsDB.clearTable(this._tableName);
            yield* sql`INSERT INTO ${sql(this._tableName)} SELECT * FROM ${sql(refTableName)}`
          })
        )
      } else {
        yield* this.clear();
      }
    })
  }

  async open() {
    return await this.openEffect().pipe(
      Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      // Effect.scoped,
      Effect.runPromise,
    )
  }


  getEffect = (key: Uint8Array) => {
    return Effect.gen(function* (this: PostgresCheckpointDB) {
      const keyHex = bytesToHex(key)
      if (this.cache.has(keyHex)) return this.cache.get(keyHex)

      for (let i = this.checkpoints.length - 1; i >= 0; i--) {
        const value = this.checkpoints[i].keyValueMap.get(keyHex)
        if (value !== undefined) return value
      }
      const sql = yield* SqlClient.SqlClient;
      // yield* sql.reserve;
      const rows = yield* sql<{ value: Uint8Array }>`
        SELECT value FROM ${sql(this._tableName)}
        WHERE key = ${Buffer.from(key)}`
      return rows[0]?.value
    })
  }

  async get(key: Uint8Array): Promise<Uint8Array | undefined> {
    return await this.getEffect(key).pipe(
      Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      // Effect.scoped,
      Effect.runPromise,
    )
  }

  getAllEffect = () => {
    return Effect.gen(function* (this: PostgresCheckpointDB) {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ key: Uint8Array, value: Uint8Array }>`
        SELECT * FROM ${sql(this._tableName)}`
        return rows.map((row) => ({
                key: Buffer.from(row.key),
                value: Buffer.from(row.value),
              }));
    })
  }

  getAllFromReferenceEffect = () => {
    return Effect.gen(function* (this: PostgresCheckpointDB) {
      if (!this._referenceTableName) {throw new Error("No reference tables set")}
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ key: Uint8Array, value: Uint8Array }>
      `SELECT * FROM ${sql(this._referenceTableName)}`;
      return rows.map((row) => ({
        key: Buffer.from(row.key),
        value: Buffer.from(row.value),
      }));
    })
  }

  putEffect = (key: Uint8Array, value: Uint8Array) => {
    return Effect.gen(function* (this: PostgresCheckpointDB) {
      const keyHex = bytesToHex(key);
      this.cache.set(keyHex, value);

      if (this.checkpoints.length > 0) {
        this.checkpoints[this.checkpoints.length - 1].keyValueMap.set(keyHex, value)
      } else {
        const sql = yield* SqlClient.SqlClient;
        // yield* sql.reserve;
        const rowsToInsert = { key: key, value: value };
        yield* sql`
          INSERT INTO ${sql(this._tableName)} ${sql.insert(rowsToInsert)}
          ON CONFLICT (key) DO UPDATE SET value = EXCLUDED.value
          `
      }
    })
  }

  async put(key: Uint8Array, value: Uint8Array): Promise<void> {
    await this.putEffect(key, value).pipe(
      Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      Effect.runPromise,
    )
  }

  delEffect = (key: Uint8Array) => {
    return Effect.gen(function* (this: PostgresCheckpointDB) {
      const keyHex = bytesToHex(key)
      this.cache.set(keyHex, undefined)
      if (this.checkpoints.length > 0) {
        this.checkpoints[this.checkpoints.length - 1].keyValueMap.set(keyHex, undefined)
      } else {
        const sql = yield* SqlClient.SqlClient
        // yield* sql.reserve
        yield* sql`DELETE FROM ${sql(this._tableName)} WHERE key = ${key}`
      }
    })
  }

  async del(key: Uint8Array): Promise<void> {
    await this.delEffect(key).pipe(
      Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      Effect.runPromise
    )
  }

  clear = () => {
    return Effect.gen(function* (this: PostgresCheckpointDB) {
      UtilsDB.clearTable(this._tableName)
    })
  }

  clearReference = () => {
    return Effect.gen(function* (this: PostgresCheckpointDB) {
      if (!this._referenceTableName) {throw new Error("No reference tables set")}
      UtilsDB.clearTable(this._tableName)
    })
  }

  transferToReference = () => {
    return Effect.gen(function* (this: PostgresCheckpointDB) {
      if (!this._referenceTableName) {throw new Error("No reference tables set")}
      const sql = yield* SqlClient.SqlClient;
      const tableName = this._tableName;
      const refTableName = this._referenceTableName;
      yield* sql.withTransaction(
        Effect.gen(function* (this: PostgresCheckpointDB) {
          yield* this.clearReference();
          yield* sql`INSERT INTO ${sql(refTableName)} SELECT * FROM ${tableName}`
        })
      )
    })
  }

  async batch(opStack: BatchDBOp[]): Promise<void> {
    const program = Effect.gen(function* (this: PostgresCheckpointDB) {
      const sql = yield* SqlClient.SqlClient
      yield* sql.withTransaction(
        Effect.gen(function* (this: PostgresCheckpointDB) {
          for (const op of opStack) {
            if (op.type === "put") {
              yield* this.putEffect(op.key, op.value)
            } else {
              yield* this.delEffect(op.key)
            }
          }
        })
      )
    })
  return program.pipe(
    Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
    Effect.scoped,
    Effect.runPromise
  )
  }

  checkpoint(root: Uint8Array): void {
    super.checkpoint(root)
    this._roots.push(root)

    Effect.runFork(
      Effect.gen(function* (this: PostgresCheckpointDB) {
        const sql = yield* SqlClient.SqlClient
        // First checkpoint = start transaction
        if (this._transactionDepth === 0) {
          yield* sql`BEGIN`
        }
        // Nested checkpoint = create savepoint
        else {
          yield* sql`SAVEPOINT root_${bytesToHex(root)}`
        }
        this._transactionDepth++
      }).pipe(
        Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      )
    )
  }

  async commit(): Promise<void> {
    if (this._roots.length === 0) return
    const root = this._roots.pop()!
    super.commit()

    await Effect.runPromise(
      Effect.gen(function* (this: PostgresCheckpointDB) {
        const sql = yield* SqlClient.SqlClient
        this._transactionDepth--

        // Final commit = persist to database
        if (this._transactionDepth === 0) {
          yield* sql`COMMIT`
        }
        // Nested commit = release savepoint
        else {
          yield* sql`RELEASE SAVEPOINT root_${bytesToHex(root)}`
        }
      }).pipe(
        Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      )
    )
  }

  async revert(): Promise<Uint8Array> {
    if (this._roots.length === 0) {
      throw new Error("No checkpoints to revert")
    }
    const root = this._roots.pop()!
    super.revert()

    await Effect.runPromise(
      Effect.gen(function* (this: PostgresCheckpointDB) {
        const sql = yield* SqlClient.SqlClient
        this._transactionDepth--

        // Full rollback if root checkpoint
        if (this._transactionDepth === 0) {
          yield* sql`ROLLBACK`
        }
        else {
          yield* sql`ROLLBACK TO SAVEPOINT root_${bytesToHex(root)}`
        }
      }).pipe(
        Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      )
    )
    return root
  }

  async conclude() {this.transferToReference()}

  shallowCopy(): PostgresCheckpointDB {
    return new PostgresCheckpointDB(
      this._client,
      this._tableName,
      this._referenceTableName,
      { cacheSize: this.cache.max }
    )
  }
}

const convertOps = (
  ops: BatchDBOp<string, Uint8Array>[]
): BatchDBOp<Uint8Array, Uint8Array>[] => {
  return ops.map((op) => {
    const base = {
      opts: {
        ...op.opts,
      }
    }
    return op.type === 'put'
      ? {
          type: 'put',
          key: Buffer.from(op.key, "hex"),
          value: op.value,
          ...base
        }
      : {
          type: 'del',
          key:  Buffer.from(op.key, "hex"),
          ...base
        }
  })
}