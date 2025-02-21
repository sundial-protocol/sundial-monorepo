import { LucidEvolution, Address } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { handleSignSubmit } from "../utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import * as Blueprint from "../../../../always-succeeds/plutus.json";


export const stateQueueInit = (
    lucid: LucidEvolution,
    address: Address
  ) => Effect.gen(function* () {
    const initParams: SDK.Types.InitParams = {
          address: address,
          policyId: Blueprint.default.validators[0].hash,
          stateQueueMintingScript: {
            type: "PlutusV3",
            script: Blueprint.default.validators[0].compiledCode,
          }}

    const txBuilder = yield* SDK.Endpoints.initTxProgram(
        lucid,
        initParams,
      );

      yield* handleSignSubmit(lucid, txBuilder);
  });
