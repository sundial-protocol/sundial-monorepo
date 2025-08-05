import { Effect } from "effect";
import { NodeConfig } from "@/config.js";
import { Database } from "@/services/database.js";
import { LedgerColumns } from "./utils.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import {
  UTxO,
  utxoToTransactionInput,
  utxoToTransactionOutput,
} from "@lucid-evolution/lucid";
import { genesisUtxos } from "./../../lace-demo/genesis.js";

/**
 * Inserts genesis UTXOs from the imported TypeScript module into the MPT database
 * @returns Effect that succeeds if UTXOs were inserted, or fails on errors
 */
export const insertGenesisUtxos = (): Effect.Effect<
  void,
  Error,
  Database
> =>
  Effect.gen(function* () {
    if (!genesisUtxos || !Array.isArray(genesisUtxos)) {
      yield* Effect.fail(
        new Error(
          "Invalid genesis UTXOs: missing or invalid genesisUtxos array",
        ),
      );
    }

    yield* Effect.logInfo(
      `🟣 Found ${genesisUtxos.length} genesis UTXOs to insert`,
    );

    // Convert genesis UTXOs to LedgerEntry format and insert into MPT
    const ledgerEntries = genesisUtxos.map((utxo: UTxO) => {
      const input = utxoToTransactionInput(utxo);
      const output = utxoToTransactionOutput(utxo);

      return {
        [LedgerColumns.TX_ID]: Buffer.from(utxo.txHash, "hex"),
        [LedgerColumns.OUTREF]: Buffer.from(input.to_cbor_bytes()),
        [LedgerColumns.OUTPUT]: Buffer.from(output.to_cbor_bytes()),
        [LedgerColumns.ADDRESS]: utxo.address,
      };
    });

    yield* Effect.logInfo(
      `🟣 Debug: Inserting ${ledgerEntries.length} UTXOs into trie`,
    );

    yield* MempoolLedgerDB.insert(ledgerEntries);

    yield* Effect.logInfo(
      `🟣 Successfully inserted ${ledgerEntries.length} genesis UTXOs into MPT database`,
    );
  });
