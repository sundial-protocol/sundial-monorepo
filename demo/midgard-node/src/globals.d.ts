export {};

declare global {
  // In-memory state queue length.
  var BLOCKS_IN_QUEUE: number;

  // Latest moment the in-memory state queue length was synchronized with
  // on-chain state.
  var LATEST_SYNC_OF_STATE_QUEUE_LENGTH: number;

  // Needed for development to prevent other actions triggering while spending all
  // UTxOs at state queue.
  var RESET_IN_PROGRESS: boolean;
}
