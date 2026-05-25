import { Context, Data, Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";

export const TX_STREAM_MESSAGE_CBOR_FIELD = "tx_cbor" as const;

export type TxIngressMessage = {
  readonly id: string;
  readonly txCbor: string;
  readonly deliveryCount: number;
};

export type TxIngressFailureDisposition = "retry" | "dead_lettered";

export type TxIngressMetricsSnapshot = {
  readonly streamDepth: number;
  readonly pendingCount: number;
  readonly lagCount: number;
};

export type TxIngressQueueService = {
  readonly enqueue: (
    txCbor: string,
  ) => Effect.Effect<string, TxIngressQueueError>;
  readonly ensureConsumerGroup: Effect.Effect<void, TxIngressQueueError>;
  readonly consumeBatch: (
    maxCount: number,
    blockMs: number,
  ) => Effect.Effect<readonly TxIngressMessage[], TxIngressQueueError>;
  readonly ack: (
    messageIds: readonly string[],
  ) => Effect.Effect<number, TxIngressQueueError>;
  readonly handleFailedMessage: (
    message: TxIngressMessage,
    reason: string,
  ) => Effect.Effect<TxIngressFailureDisposition, TxIngressQueueError>;
  readonly snapshotMetrics: Effect.Effect<
    TxIngressMetricsSnapshot,
    TxIngressQueueError
  >;
  readonly clear: Effect.Effect<void, TxIngressQueueError>;
};

export class TxIngressQueue extends Context.Tag("TxIngressQueue")<
  TxIngressQueue,
  TxIngressQueueService
>() {}

export class TxIngressQueueError extends Data.TaggedError(
  "TxIngressQueueError",
)<
  SDK.GenericErrorFields & {
    readonly operation: string;
  }
> {}
