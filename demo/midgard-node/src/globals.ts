import { TxHash } from "@lucid-evolution/lucid";
import { SerializedStateQueueUTxO } from "./workers/utils/commit-block-header.js";
import { Context, Effect, Layer, Ref } from "effect";

class AvailableConfirmedBlockState extends Context.Tag("AvailableConfirmedBlockState")<
  AvailableConfirmedBlockState,
  Ref.Ref<"" | SerializedStateQueueUTxO>
>() {
  static readonly layer = Layer.effect(AvailableConfirmedBlockState, Effect.gen(function* () {
    return yield* Ref.make("" as ("" | SerializedStateQueueUTxO)) // The only way to make a ref of a union
  }));
}

class UnconfirmedSubmittedBlockState extends Context.Tag("UnconfirmedSubmittedBlockState")<
  UnconfirmedSubmittedBlockState,
  Ref.Ref<"" | TxHash>
>() {
    static readonly layer = Layer.effect(UnconfirmedSubmittedBlockState, Effect.gen(function* () {
    return yield* Ref.make("" as ("" | TxHash)) // The only way to make a ref of a union
  }));
}

export class Globals extends Effect.Service<Globals>()("Globals", {
  effect: Effect.gen(function* () {
    // In-memory state queue length.
    var BLOCKS_IN_QUEUE: Ref.Ref<number> = yield* Ref.make(0);

    // Latest moment the in-memory state queue length was synchronized with
    // on-chain state.
    var LATEST_SYNC_OF_STATE_QUEUE_LENGTH: Ref.Ref<number> = yield* Ref.make(0);

    // Needed for development to prevent other actions triggering while spending all
    // UTxOs at state queue.
    var RESET_IN_PROGRESS: Ref.Ref<boolean> = yield* Ref.make(false);

    // The state queue UTxO confirmed by the confirmation worker, unused for block
    // commitment.
    var AVAILABLE_CONFIRMED_BLOCK: Ref.Ref<"" | SerializedStateQueueUTxO> = yield* AvailableConfirmedBlockState;

    // Accumulator for the number of processed mempool transactions (only used in
    // metrics)
    var PROCESSED_UNSUBMITTED_TXS_COUNT: Ref.Ref<number> = yield* Ref.make(0);

    // Accumulator for the total size of L2 transactions submitted in a state
    // queue block.
    var PROCESSED_UNSUBMITTED_TXS_SIZE: Ref.Ref<number> = yield* Ref.make(0);

    var UNCONFIRMED_SUBMITTED_BLOCK: Ref.Ref<"" | TxHash> = yield* UnconfirmedSubmittedBlockState;

    return {
      BLOCKS_IN_QUEUE,
      LATEST_SYNC_OF_STATE_QUEUE_LENGTH,
      RESET_IN_PROGRESS,
      AVAILABLE_CONFIRMED_BLOCK,
      PROCESSED_UNSUBMITTED_TXS_COUNT,
      PROCESSED_UNSUBMITTED_TXS_SIZE,
      UNCONFIRMED_SUBMITTED_BLOCK,
    };
  }),

  dependencies: [
    AvailableConfirmedBlockState.layer,
    UnconfirmedSubmittedBlockState.layer,
  ],
}) {}
