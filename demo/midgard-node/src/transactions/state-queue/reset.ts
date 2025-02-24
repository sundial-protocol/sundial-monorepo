import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution, Script, UTxO, toUnit } from "@lucid-evolution/lucid";
import * as Blueprint from "../../../../always-succeeds/plutus.json" with { type: "json" };
import { Effect } from "effect";
import { handleSignSubmit } from "../utils.js";

const collectAndBurnStateQueueNodesProgram = (
  lucid: LucidEvolution,
  fetchConfig: SDK.Types.FetchConfig,
  utxosAndAssetNames: { utxo: UTxO; assetName: string }[],
): Effect.Effect<void, Error> =>
  Effect.gen(function* () {
    const stateQueueMintingScript: Script = {
      type: "PlutusV3",
      script: Blueprint.default.validators[0].compiledCode,
    };
    const stateQueueSpendingScript: Script = {
      type: "PlutusV3",
      script: Blueprint.default.validators[1].compiledCode,
    };
    const tx = lucid.newTx();
    utxosAndAssetNames.map(({ utxo, assetName }) => {
      tx.collectFrom([utxo], "d87980") // TODO: Placeholder redeemer.
        .mintAssets(
          { [toUnit(fetchConfig.stateQueuePolicyId, assetName)]: -1n },
          "d87980",
        );
    });
    tx.attach
      .Script(stateQueueSpendingScript)
      .attach.Script(stateQueueMintingScript);
    const completed = yield* tx.completeProgram();
    return yield* handleSignSubmit(lucid, completed);
  });

export const resetStateQueue = (
  lucid: LucidEvolution,
  fetchConfig: SDK.Types.FetchConfig,
) =>
  Effect.gen(function* () {
    const allUTxOsAndAssetNames =
      yield* SDK.Endpoints.fetchAllStateQueueUTxOsProgram(lucid, fetchConfig);
    // Collect and burn 10 UTxOs and asset names at a time:
    const batchSize = 10;
    for (let i = 0; i < allUTxOsAndAssetNames.length; i += batchSize) {
      const batch = allUTxOsAndAssetNames.slice(i, i + batchSize);
      yield* collectAndBurnStateQueueNodesProgram(lucid, fetchConfig, batch);
    }
  });
