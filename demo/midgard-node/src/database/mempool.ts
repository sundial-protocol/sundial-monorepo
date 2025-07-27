import { Database } from "@/services/database.js";
import {
  clearTable,
  insertKeyValue,
  delMultiple,
  retrieveValues,
  retrieveNumberOfEntries,
  createInputsTable,
  createKeyValueTable,
  ProcessedTx,
  InputsColumns,
  LedgerColumns,
  KVColumns,
  mapSqlError,
  insertSpentInput,
} from "./utils.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import * as ImmutableDB from "./immutable.js";
import * as BlocksDB from "./blocks.js";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { CML, fromHex } from "@lucid-evolution/lucid";

export const tableName = "mempool";

export const inputsTableName = "mempool_spent_inputs";

export const outputsTableName = MempoolLedgerDB.tableName;

export const init: Effect.Effect<void, Error, Database> = Effect.gen(
  function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* createKeyValueTable(tableName);
        yield* createInputsTable(inputsTableName, tableName);
        // Defining a PostgreSQL trigger to fire whenever a row from the spent
        // inputs table is deleted.
        //
        // The trigger simply archives the deleted row in the "spent inputs"
        // table associated with the Blocks DB.
        //
        // TODO: Syntax used for `OLD.` might be incorrect.
        yield* sql`CREATE OR REPLACE FUNCTION insert_inputs_into_blocks_on_delete()
        RETURNS trigger AS $$
        BEGIN
          INSERT INTO ${sql(BlocksDB.inputsTableName)}(${sql(InputsColumns.OUTREF)}, ${sql(InputsColumns.SPENDING_TX)})
          VALUES (OLD.${sql(InputsColumns.OUTREF)}, OLD.${sql(InputsColumns.SPENDING_TX)});
          RETURN OLD;
        END;
        $$ LANGUAGE plpgsql;
      `;
        yield* sql`CREATE TRIGGER trigger_insert_inputs_into_blocks_on_delete
        AFTER DELETE ON ${sql(inputsTableName)}
        FOR EACH ROW
        EXECUTE PROCEDURE insert_inputs_into_blocks_on_delete();
      `;
        // Defining another trigger to be attached to the mempool itself, for
        // archiving transactions included in a committed block.
        //
        // TODO: The session variable must also come from a TS variable.
        //
        // TODO: Syntax used for `OLD.` might be incorrect.
        yield* sql`CREATE OR REPLACE FUNCTION archive_mempool()
        RETURNS trigger AS $$
        BEGIN
          INSERT INTO ${sql(BlocksDB.tableName)}(${sql(BlocksDB.Columns.HEADER_HASH)}, ${sql(BlocksDB.Columns.TX_HASH)})
          VALUES (current_setting('block_commitment.deleting_block', TRUE), OLD.${sql(KVColumns.KEY)});
          INSERT INTO ${sql(ImmutableDB.tableName)}(${sql(KVColumns.KEY)}, ${sql(KVColumns.VALUE)})
          VALUES (OLD.${sql(KVColumns.KEY)}, OLD.${sql(KVColumns.VALUE)});
          RETURN OLD;
        END;
        $$ LANGUAGE plpgsql;
      `;
        yield* sql`CREATE TRIGGER trigger_archive_mempool
        AFTER DELETE ON ${sql(tableName)}
        FOR EACH ROW
        EXECUTE PROCEDURE archive_mempool();
      `;
      }),
    );
  },
);

export const insert = (
  txString: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const txCbor = fromHex(txString);
    const deserializedTx = CML.Transaction.from_cbor_bytes(txCbor);
    const txBody = deserializedTx.body();
    const txHash = CML.hash_transaction(txBody);
    const txHashBytes = Buffer.from(txHash.to_raw_bytes());
    const inputs = txBody.inputs();
    const inputsCount = inputs.len();
    // Insert the tx itself in `MempoolDB`.
    yield* insertKeyValue(tableName, {
      key: txHashBytes,
      value: Buffer.from(txCbor),
    });
    // Insert spent inputs int `inputsTableName`.
    for (let i = 0; i < inputsCount; i++) {
      yield* insertSpentInput(inputsTableName, {
        spent_outref: Buffer.from(inputs.get(i).to_cbor_bytes()),
        spending_tx_hash: txHashBytes,
      });
    }
    const outputs = txBody.outputs();
    const outputsCount = outputs.len();
    // Insert produced UTxOs in `MempoolLedgerDB`.
    for (let i = 0; i < outputsCount; i++) {
      const output = outputs.get(i);
      yield* MempoolLedgerDB.insert({
        tx_id: txHashBytes,
        outref: Buffer.from(
          CML.TransactionInput.new(txHash, BigInt(i)).to_cbor_bytes(),
        ),
        output: Buffer.from(output.to_cbor_bytes()),
        address: output.address().to_bech32(),
      });
    }
  });

/** Given a txHash, retrieves all associated values with a single SQL query.
 */
export const retrieveByHash = (
  txHash: Buffer,
): Effect.Effect<ProcessedTx, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<ProcessedTx>`
      SELECT 
        m.${sql(KVColumns.KEY)} as txHash, 
        ARRAY(SELECT outref FROM ${sql(inputsTableName)} WHERE ${sql(InputsColumns.SPENDING_TX)} = txHash) AS inputs,
        ARRAY(SELECT output FROM ${sql(outputsTableName)} WHERE ${sql(LedgerColumns.TX_ID)} = txHash) AS outputs
      FROM ${sql(tableName)} m 
      WHERE txHash = ${txHash}`;

    if (rows.length === 0) {
      yield* Effect.fail(new Error(`Transaction not found from ${tableName}`));
    }

    return rows[0];
  });

export const retrieveTxCborsByHashes = (txHashes: Buffer[]) =>
  retrieveValues(tableName, txHashes);

export const retrieve = (): Effect.Effect<
  readonly ProcessedTx[],
  Error,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<ProcessedTx>`
    SELECT 
      m.${sql(KVColumns.KEY)} as txHash, 
      ARRAY(SELECT outref FROM ${sql(inputsTableName)} WHERE ${sql(InputsColumns.SPENDING_TX)} = txHash) AS inputs,
      ARRAY(SELECT output FROM ${sql(outputsTableName)} WHERE ${sql(LedgerColumns.TX_ID)} = txHash) AS outputs
    FROM ${sql(tableName)} m`;
    return rows;
  }).pipe(
    Effect.withLogSpan(
      `retrieve utxos ${tableName}, ${inputsTableName} and ${outputsTableName}`,
    ),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieval failure: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieveTxCount = () => retrieveNumberOfEntries(tableName);

/** Deletes all the given transactions from `MempoolDB`, which also cascades
 * into deletion of the associated "inputs" table (spent inputs).
 *
 * Two triggers are fired as well:
 * - First, the one attached to the inputs table, which is simply an "archiving"
 *   mechanism that transfers the rows to the inputs table associated with the
 *   blocks db
 * - Second, the trigger attached to the mempool itself, which takes care of
 *   inserting deleted transactions to the blocks table (which is why we need
 *   to first set a session variable)
 */
// TODO: Session variable name is a magic string. It should come from a variable
//       like other table names, columns, etc.
export const clearTxs = (
  txHashes: Buffer[],
  committedBlockHeaderHash: Buffer,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`SET block_commitment.deleting_block = ${committedBlockHeaderHash};`;
        yield* delMultiple(tableName, txHashes);
      }),
    );
  });

export const clear = () => clearTable(tableName);
