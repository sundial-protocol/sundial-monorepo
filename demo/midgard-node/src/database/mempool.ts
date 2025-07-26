import { Database } from "@/services/database.js";
import {
  clearTable,
  insertKeyValue,
  delMultiple,
  retrieveValues,
  retrieveKeyValues,
  retrieveNumberOfEntries,
  createInputsTable,
  createKeyValueTable,
  ProcessedTx,
  inputsTablePrimaryKeyLabel,
  inputsTableForeignKeyLabel,
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
        // The trigger simply archives the deleted row in the "spent inputs" table
        // associated with the Blocks DB.
        yield* sql`CREATE OR REPLACE FUNCTION insert_inputs_into_blocks_on_delete()
        RETURNS trigger AS $$
        BEGIN
          INSERT INTO ${sql(BlocksDB.inputsTableName)}(${inputsTablePrimaryKeyLabel}, ${inputsTableForeignKeyLabel})
          VALUES (OLD.${inputsTablePrimaryKeyLabel}, OLD.${inputsTableForeignKeyLabel});
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
        // archiving transactions included in a commited block.
        yield* sql`CREATE OR REPLACE FUNCTION archive_mempool()
        RETURNS trigger AS $$
        BEGIN
          INSERT INTO ${sql(BlocksDB.tableName)}(headerHash, txHash)
          VALUES (current_setting('block_commitment.deleting_block', TRUE), OLD.key);
          INSERT INTO ${sql(ImmutableDB.tableName)}(key, value)
          VALUES (OLD.key, OLD.value);
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
    yield* insertKeyValue(tableName, txHashBytes, Buffer.from(txCbor));
    for (let i = 0; i < inputsCount; i++) {
      yield* insertKeyValue(
        inputsTableName,
        Buffer.from(inputs.get(i).to_cbor_bytes()),
        txHashBytes,
        "outref",
        "spendingTxHash",
      );
    }
    const outputs = txBody.outputs();
    const outputsCount = outputs.len();
    for (let i = 0; i < outputsCount; i++) {
      const output = outputs.get(i);
      yield* MempoolLedgerDB.insert({
        txId: txHashBytes,
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
    m.key, 
    ARRAY(SELECT outref FROM ${sql(inputsTableName)} WHERE spendingTxHash = m.key) AS inputs,
    ARRAY(SELECT output FROM ${sql(outputsTableName)} WHERE txId = m.key) AS outputs
  FROM ${sql(tableName)} m 
  WHERE m.key = ${txHash}
`;

    if (rows.length === 0) {
      yield* Effect.fail(new Error(`Transaction not found from ${tableName}`));
    }

    return rows[0];
  });

export const retrieveTxCborsByHashes = (txHashes: Buffer[]) =>
  retrieveValues(tableName, txHashes);

export const retrieve = () => retrieveKeyValues(tableName);

export const retrieveTxCount = () => retrieveNumberOfEntries(tableName);

export const clearTxs = (txHashes: Buffer[]) =>
  delMultiple(tableName, txHashes);

export const clear = () => clearTable(tableName);
