import { Effect } from "effect";
import { NodeConfig } from "@/config.js";
import { Database } from "@/services/database.js";
import { LedgerColumns } from "./utils.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { makeMpts } from "@/workers/db.js";
import {
  UTxO,
  utxoToTransactionInput,
  utxoToTransactionOutput,
} from "@lucid-evolution/lucid";
import * as fs from "node:fs";

// Type for the JSON file structure using Lucid UTxO
export type GenesisUtxosFile = {
  utxos: UTxO[];
};

/**
 * Inserts genesis UTXOs from a JSON file into the MPT database
 * @param genesisFilePath - Path to the JSON file containing genesis UTXOs, or null to skip
 * @returns Effect that succeeds if UTXOs were inserted, or fails if file doesn't exist or other errors
 */
export const insertGenesisUtxos = (): Effect.Effect<void, Error, Database | NodeConfig> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const genesisFilePath = nodeConfig.GENESIS_UTXOS_PATH;

    if (genesisFilePath === null) {
      yield* Effect.logInfo(
        `🔹 No genesis UTXOs file path provided, skipping insertion`,
      );
      return;
    }

    yield* Effect.logInfo(
      `🔹 Checking for genesis UTXOs file: ${genesisFilePath}`,
    );

    const fileExists = yield* Effect.tryPromise({
      try: () => fs.promises.access(genesisFilePath, fs.constants.F_OK),
      catch: () =>
        new Error(`Genesis UTXOs file not found: ${genesisFilePath}`),
    }).pipe(
      Effect.map(() => true),
      Effect.catchAll(() => Effect.succeed(false)),
    );

    if (!fileExists) {
      yield* Effect.logInfo(
        `🔹 No genesis UTXOs file found at ${genesisFilePath}, skipping genesis UTXO insertion`,
      );
      return;
    }

    const fileContent = yield* Effect.tryPromise({
      try: () => fs.promises.readFile(genesisFilePath, "utf-8"),
      catch: (e) => new Error(`Failed to read genesis UTXOs file: ${e}`),
    });

    const genesisData: GenesisUtxosFile = yield* Effect.try({
      try: () => JSON.parse(fileContent),
      catch: (e) => new Error(`Failed to parse genesis UTXOs JSON: ${e}`),
    });

    if (!genesisData.utxos || !Array.isArray(genesisData.utxos)) {
      yield* Effect.fail(
        new Error(
          "Invalid genesis UTXOs file format: missing or invalid 'utxos' array",
        ),
      );
    }

    yield* Effect.logInfo(
      `🔹 Found ${genesisData.utxos.length} genesis UTXOs to insert`,
    );

    // Convert genesis UTXOs to LedgerEntry format and insert into MPT
    const ledgerEntries = genesisData.utxos.map((utxo) => {
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
      `🔹 Debug: Inserting ${ledgerEntries.length} UTXOs into trie`,
    );

    yield* Effect.forEach(ledgerEntries, (entry) =>
      Effect.gen(function* () {
        yield* MempoolLedgerDB.insert([entry]);
      }),
    );

    yield* Effect.logInfo(
      `🔹 Successfully inserted ${ledgerEntries.length} genesis UTXOs into MPT database`,
    );
  });
