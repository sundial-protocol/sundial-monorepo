import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { AlwaysSucceeds } from "@/services/index.js";
import { NodeConfig, User } from "@/config.js";
import { handleSignSubmit, TransactionError } from "../utils.js";

export const stateQueueInit = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const { user: lucid } = yield* User;
  const { spendScriptAddress, mintScript, policyId } =
    yield* AlwaysSucceeds.AlwaysSucceedsContract;
  const initParams: SDK.TxBuilder.StateQueue.InitParams = {
    address: spendScriptAddress,
    policyId: policyId,
    stateQueueMintingScript: mintScript,
  };
  lucid.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE);
  const txBuilder = yield* SDK.Endpoints.initTxProgram(lucid, initParams);
  const onSubmitFailure = (err: TransactionError) =>
    Effect.gen(function* () {
      yield* Effect.logError(`Submit tx error: ${err}`);
      yield* Effect.fail(err);
    });
  const onConfirmFailure = (err: TransactionError) =>
    Effect.logError(`Confirm tx error: ${err}`);
  return yield* handleSignSubmit(
    lucid,
    txBuilder,
    onSubmitFailure,
    onConfirmFailure,
  );
});
