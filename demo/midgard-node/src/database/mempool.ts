import { Database } from "@/services/database.js";
import {
  clearTable,
  insertKeyValue,
  delMultiple,
  retrieveValues,
  retrieveKeyValues,
  retrieveNumberOfEntries,
} from "./utils.js";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import {CML, fromHex} from "@lucid-evolution/lucid";

export const tableName = "mempool";

export const inputsTableName = "mempoolSpentInputs";

export const outputsTableName = "mempoolOutputs";

export const init: Effect.Effect<void, Error, Database> = Effect.gen(
  function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      key BYTEA NOT NULL,
      value BYTEA NOT NULL,
      PRIMARY KEY (key)
    );`;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(inputsTableName)} (
      txId BYTEA NOT NULL,
      txIx SMALLINT NOT NULL,
      bytes BYTEA NOT NULL,
      spendingTxHash BYTEA NOT NULL,
      PRIMARY KEY (txId, txIx, bytes),
      FOREIGN KEY (spendingTxHash) REFERENCES ${sql(tableName)}(key) ON DELETE CASCADE
    );`;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(outputsTableName)} (
      txHash BYTEA NOT NULL,
      output BYTEA NOT NULL,
      FOREIGN KEY (txHash) REFERENCES ${sql(tableName)}(key) ON DELETE CASCADE
    );`;
  }
);

export const insert = (txString: string): Effect.Effect<
  void,
  Error,
  Database
> => Effect.gen(function* () {
  const txCbor = fromHex(txString)
  const deserializedTx = CML.Transaction.from_cbor_bytes(txCbor);
  const txBody = deserializedTx.body();
  const txHash = CML.hash_transaction(txBody);
  const txHashBytes = txHash.to_raw_bytes();
  const inputs = txBody.inputs();
  const inputsCount = inputs.len();
  const sql = yield* SqlClient.SqlClient;
  yield* insertKeyValue(tableName, txHashBytes, txCbor);
  for (let i = 0; i < inputsCount; i++) {
    const input = inputs.get(i);
    const inputBytes = input.to_cbor_bytes();
    const spendingTxHash = Buffer.from(txHashBytes);
    yield* sql`INSERT INTO ${sql(inputsTableName)} ${sql.insert({
      txId: Buffer.from(input.transaction_id().to_raw_bytes()),
      txIx: input.index(),
      spendingTxHash: spendingTxHash,
      bytes: inputBytes,
    })} ON CONFLICT (bytes) DO UPDATE SET (spendingTxHash) = (${spendingTxHash}, ${inputBytes})`;
  }
  const outputs = txBody.outputs();
  const outputsCount = outputs.len();
  for (let i = 0; i < outputsCount; i++) {
    yield* insertKeyValue(
      outputsTableName,
      txHashBytes,
      outputs.get(i).to_cbor_bytes(),
      "txHash",
      "output",
    );
  }
});

export type DBTx = {
  txHash: Uint8Array;
  txCbor: Uint8Array;
  inputs: Uint8Array[];
  outputs: Uint8Array[];
}

/** Given a txHash, retrieves all associated values with a single SQL query.
 */
export const retrieveByHash = (txHash: Uint8Array): Effect.Effect<
  DBTx,
  Error,
  Database
> => Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  // const rows = yield* sql<DBTx>`;
  //   SELECT
  //     ${sql(tableName)}.key AS txHash,
  //     ${sql(tableName)}.value AS txCbor,
  //     ARRAY_AGG(${sql(inputsTableName)}.input) AS inputs,
  //     ARRAY_AGG(${sql(outputsTableName)}.output) AS outputs
  //   FROM ${sql(tableName)}
  //   LEFT JOIN ${sql(inputsTableName)} ON ${sql(tableName)}.key = ${sql(inputsTableName)}.txHash
  //   LEFT JOIN ${sql(outputsTableName)} ON ${sql(tableName)}.key = ${sql(outputsTableName)}.txHash
  //   WHERE ${sql(tableName)}.key = ${Buffer.from(txHash)}
  //   GROUP BY ${sql(tableName)}.key, ${sql(tableName)}.value
  // `;

  const rows = yield* sql<DBTx>`
  SELECT 
    m.key, 
    ARRAY(SELECT bytes FROM ${sql(inputsTableName)} WHERE txHash = m.key) AS inputs,
    ARRAY(SELECT output FROM ${sql(outputsTableName)} WHERE txHash = m.key) AS outputs
  FROM ${sql(tableName)} m 
  WHERE m.key = ${Buffer.from(txHash)}
`;

  if (rows.length === 0) {
    yield* Effect.fail(new Error(`Transaction not found from ${tableName}`));
  }

  return rows[0];
});

export const retrieveTxCborsByHashes = (txHashes: Uint8Array[]) =>
  retrieveValues(tableName, txHashes);

export const retrieve = () => retrieveKeyValues(tableName);

export const retrieveTxCount = () => retrieveNumberOfEntries(tableName);

export const clearTxs = (txHashes: Uint8Array[]) =>
  delMultiple(tableName, txHashes);

export const clear = () => clearTable(tableName);
