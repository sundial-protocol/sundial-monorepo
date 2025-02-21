import {
  LucidEvolution,
  Address,
  validatorToAddress,
  Script,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { handleSignSubmit } from "../utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import * as Blueprint from "../../../../always-succeeds/plutus.json";

export const stateQueueInit = (lucid: LucidEvolution) =>
  Effect.gen(function* () {
    const spendingScript: Script = {
      type: "PlutusV3",
      script: Blueprint.default.validators[1].compiledCode,
    };
    const mintingScript: Script = {
      type: "PlutusV3",
      script: Blueprint.default.validators[0].compiledCode,
    };
    const networkType = lucid.config().network ?? "Preprod"
    const initParams: SDK.Types.InitParams = {
      address: validatorToAddress(networkType, spendingScript),
      policyId: Blueprint.default.validators[0].hash,
      stateQueueMintingScript: mintingScript,
    };

    const txBuilder = yield* SDK.Endpoints.initTxProgram(lucid, initParams);

    yield* handleSignSubmit(lucid, txBuilder);
  });
