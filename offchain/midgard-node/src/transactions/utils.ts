import { LucidEvolution, TxSignBuilder, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { Database } from "sqlite3";
import * as BlocksDB from "../database/blocks.js";
import * as ImmutableDB from "../database/immutable.js";
import { findSpentAndProducedUTxOs } from "@/utils.js";

/**
 * Handle the signing and submission of a transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export const handleSignSubmit = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
) =>
  Effect.gen(function* () {
    const signed = yield* signBuilder.sign.withWallet().completeProgram();
    const txHash = yield* signed.submitProgram();
    yield* Effect.logDebug(`🚀 Transaction submitted: ${txHash}`);
    yield* Effect.logDebug(`Confirming Transaction...`);
    yield* Effect.tryPromise(() => lucid.awaitTx(txHash, 40_000));
    yield* Effect.logDebug(`✅ Transaction confirmed: ${txHash}`);
    yield* Effect.logDebug("Pausing for 10 seconds...");
    yield* Effect.sleep("10 seconds");
  });

/**
 * Build both Merkle roots (transactions and updated UTxO set).
 *
 * @param txs - An array of transaction CBORs.
 * @returns An Effect that resolves to an object containing the transaction root and UTxO root.
 */
export const buildMerkleRoots = (txs: string[]) =>
  Effect.gen(function* () {
    const utxos: UTxO[] = [];
    for (const txCBOR of txs) {
      const { produced } = findSpentAndProducedUTxOs(txCBOR);
      utxos.push(...produced);
    }
    const txRoot = yield* SDK.Utils.mptFromList(txs);
    const utxoRoot = yield* SDK.Utils.mptFromList(utxos);
    return { txRoot, utxoRoot };
  });

/**
 * Fetch transactions of the first block by querying BlocksDB and ImmutableDB.
 *
 * @param lucid - The LucidEvolution instance.
 * @param fetchConfig - The configuration for fetching data.
 * @param db - The database instance.
 * @returns An Effect that resolves to an array of transactions.
 */
export const fetchFirstBlockTxs = (
  lucid: LucidEvolution,
  fetchConfig: SDK.Types.FetchConfig,
  db: Database,
): Effect.Effect<{ txs: string[]; headerHash: string }, Error> =>
  Effect.gen(function* () {
    const { link: firstBlockUTxO } =
      yield* SDK.Endpoints.fetchConfirmedStateAndItsLinkProgram(
        lucid,
        fetchConfig,
      );
    if (!firstBlockUTxO) {
      return yield* Effect.fail(new Error("No blocks in queue"));
    } else {
      const blockHeader =
        yield* SDK.Utils.getHeaderFromBlockUTxO(firstBlockUTxO);
      const headerHash = yield* SDK.Utils.hashHeader(blockHeader);
      const txHashes = yield* Effect.tryPromise({
        try: () => BlocksDB.retrieveTxHashesByBlockHash(db, headerHash),
        catch: (e) => new Error(`${e}`),
      });
      const txs = yield* Effect.tryPromise({
        try: () => ImmutableDB.retrieveTxCborsByHashes(db, txHashes),
        catch: (e) => new Error(`${e}`),
      });
      return { txs, headerHash };
    }
  });
