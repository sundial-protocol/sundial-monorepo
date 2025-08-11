import { Effect } from "effect";
import { Database } from "@/services/database.js";
import { LedgerColumns, LedgerEntry } from "./utils.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import {
  UTxO,
  utxoToTransactionInput,
  utxoToTransactionOutput,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { NodeConfig, NodeConfigDep } from "@/config.js";
import { makeMpts } from "@/workers/utils/mpt.js";



/**
 * Inserts genesis UTXOs from the imported TypeScript module into the MPT database
 * @returns Effect that succeeds if UTXOs were inserted, or fails on errors
 */
export const insertGenesisUtxos: Effect.Effect<
  void,
  Error,
  NodeConfig | Database
> = Effect.gen(function* () {
  const config = yield* NodeConfig;

  if (config.NETWORK === "Mainnet") {
    yield* Effect.logInfo(`🟣 On mainnet—No genesis UTxOs will be inserted.`);
    return;
  }

  const ledgerEntries = config.GENESIS_UTXOS.map((utxo: UTxO) => {
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
    `🟣 On testnet—Inserting ${ledgerEntries.length} genesis UTxOs...`,
  );

  yield* Effect.logInfo(
    `🟣 Debug: Inserting ${ledgerEntries.length} UTxOs into MempoolLedgerDB...`,
  );

  yield* MempoolLedgerDB.insert(ledgerEntries);

  yield* Effect.logInfo(
    `🟣 Debug: Inserting ${ledgerEntries.length} UTxOs into trie...`,
  );

  const { ledgerTrie } = yield* makeMpts();

  yield* Effect.tryPromise({
    try: () =>
      ledgerTrie.batch(
        ledgerEntries.map((le) => ({
          type: "put",
          key: le[LedgerColumns.OUTREF],
          value: le[LedgerColumns.OUTPUT],
        })),
      ),
    catch: (e) => new Error(`${e}`),
  });

  yield* Effect.logInfo(
    `🟣 Successfully inserted ${ledgerEntries.length} genesis UTxOs into MPT database. Funded addresses are:
${Array.from(new Set(genesisUTxOs.map((u) => u.address))).join("\n")}`,
  );
});
