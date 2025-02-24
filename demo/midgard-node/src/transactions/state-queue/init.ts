import { Effect } from "effect";
import { handleSignSubmitWithoutConfirmation } from "../utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { User } from "@/config.js";

export const stateQueueInit = Effect.gen(function* () {
  const { user: lucid } = yield* User;
  const { spendScriptAddress, mintScript, policyId } =
    yield* AlwaysSucceedsContract;
  const initParams: SDK.Types.InitParams = {
    address: spendScriptAddress,
    policyId: policyId,
    stateQueueMintingScript: mintScript,
  };
  const txBuilder = yield* SDK.Endpoints.initTxProgram(lucid, initParams);
  return yield* handleSignSubmitWithoutConfirmation(lucid, txBuilder);
});
