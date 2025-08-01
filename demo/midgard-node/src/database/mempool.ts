import { Database } from "@/services/database.js";
import {
  clearTable,
  insertKeyValue,
  delMultiple,
  retrieveValues,
  retrieveNumberOfEntries,
  LedgerColumns,
  LedgerEntry,
  retrieveValue,
  KVPair,
  KVColumns,
  mapSqlError,
} from "./utils.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { Effect } from "effect";
import { CML, fromHex } from "@lucid-evolution/lucid";
import { SqlClient } from "@effect/sql";

export const tableName = "mempool";

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

    // Remove spent inputs from MempoolLedgerDB.
    const spent: Buffer[] = [];
    for (let i = 0; i < inputsCount; i++) {
      spent.push(Buffer.from(inputs.get(i).to_cbor_bytes()));
    }
    yield* MempoolLedgerDB.clearUTxOs(spent);

    const outputs = txBody.outputs();
    const outputsCount = outputs.len();
    const produced: LedgerEntry[] = [];
    // Insert produced UTxOs in `MempoolLedgerDB`.
    for (let i = 0; i < outputsCount; i++) {
      const output = outputs.get(i);
      produced.push({
        [LedgerColumns.TX_ID]: txHashBytes,
        [LedgerColumns.OUTREF]: Buffer.from(
          CML.TransactionInput.new(txHash, BigInt(i)).to_cbor_bytes(),
        ),
        [LedgerColumns.OUTPUT]: Buffer.from(output.to_cbor_bytes()),
        [LedgerColumns.ADDRESS]: output.address().to_bech32(),
      });
    }
    yield* MempoolLedgerDB.insert(produced);
  });

export const retrieveTxCborByHash = (txHash: Buffer) =>
  retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (txHashes: Buffer[]) =>
  retrieveValues(tableName, txHashes);

export const retrieve = (): Effect.Effect<readonly KVPair[], Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve keyValues`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<KVPair>`SELECT ${sql(
      KVColumns.KEY,
    )}, ${sql(KVColumns.VALUE)} FROM ${sql(tableName)} LIMIT 100000`;
  }).pipe(
    Effect.withLogSpan(`retrieve ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: retrieve: ${JSON.stringify(e)}`),
    ),
    mapSqlError,
  );

export const retrieveTxCount = () => retrieveNumberOfEntries(tableName);

export const clearTxs = (
  txHashes: Buffer[],
): Effect.Effect<void, Error, Database> => delMultiple(tableName, txHashes);

export const clear = () => clearTable(tableName);
