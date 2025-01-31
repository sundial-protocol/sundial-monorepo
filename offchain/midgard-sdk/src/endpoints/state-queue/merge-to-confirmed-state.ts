import { makeReturn } from "@/core.js";
import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { StateQueue, ActiveOperators } from "@/tx-builder/index.js";
import { Effect } from "effect";
import { errorToString } from "@/utils/common.js";
import { FetchConfig } from "@/types/state-queue.js";

export type MergeParams = {};

export const mergeToConfirmedStateProgram = (
  lucid: LucidEvolution,
  fetchConfig: FetchConfig,
  params: MergeParams
): Effect.Effect<TxSignBuilder, string> =>
  Effect.gen(function* () {
    return yield* Effect.fail("TODO");
  });
