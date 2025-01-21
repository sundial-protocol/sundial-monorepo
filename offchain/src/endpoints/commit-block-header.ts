import { makeReturn } from "../core.js";
import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { StateQueue, ActiveOperators } from "../tx-builder/index.js";

/**
 * Commits a block header using the provided LucidEvolution instance and parameters.
 *
 * @param lucid - The LucidEvolution instance to use for the commit.
 * @param sqCommitParams - The parameters required for committing the state queue.
 * @param aoUpdateParams - The parameters required for updating the active operators' commitment time.
 * @returns A promise that resolves to a TxSignBuilder instance.
 */
export const commitBlockHeader = (
  lucid: LucidEvolution,
  sqCommitParams: StateQueue.CommitParams,
  aoUpdateParams: ActiveOperators.UpdateCommitmentTimeParams
): Promise<TxSignBuilder> => {
  return makeReturn(
    StateQueue.commitTxBuilder(lucid, sqCommitParams)
      .compose(
        ActiveOperators.updateCommitmentTimeTxBuilder(lucid, aoUpdateParams)
      )
      .completeProgram()
  ).unsafeRun();
};
