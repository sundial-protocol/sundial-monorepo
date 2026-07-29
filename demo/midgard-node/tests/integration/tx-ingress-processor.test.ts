import { describe, expect, vi } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer } from "effect";

import { makeTestSqlLayer } from "./harness/pglite-sql-layer.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import { txCborA, makeSeedLedgerEntry } from "./harness/fixtures.js";
import * as DBInitialization from "@/database/init.js";
import * as MempoolDB from "@/database/mempool.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import { txQueueProcessorAction } from "@/fibers/tx-queue-processor.js";
import {
  TxIngressQueue,
  TxIngressQueueService,
  type TxIngressMessage,
} from "@/services/index.js";

const makeQueueStub = (
  messages: ReadonlyArray<{
    id: string;
    txCbor: string;
    deliveryCount: number;
  }>,
) => {
  const ackSpy = vi.fn((ids: readonly string[]) => Effect.succeed(ids.length));
  const failedSpy = vi.fn((_message: TxIngressMessage, _reason: string) =>
    Effect.succeed("retry" as const),
  );

  const queue: TxIngressQueueService = {
    enqueue: (_txCbor: string) => Effect.succeed("1-0"),
    rawXadd: (_txCbor, callback) => callback(null, "1-0"),
    ensureConsumerGroup: Effect.void,
    consumeBatch: (_maxCount: number, _blockMs: number) =>
      Effect.succeed(messages),
    ack: ackSpy,
    handleFailedMessage: failedSpy,
    refreshSnapshotMetrics: Effect.succeed({
      streamDepth: messages.length,
      pendingCount: 0,
      lagCount: messages.length,
    }),
    snapshotMetrics: Effect.succeed({
      streamDepth: messages.length,
      pendingCount: 0,
      lagCount: messages.length,
    }),
    clear: Effect.void,
  };

  return { queue, ackSpy, failedSpy };
};

const makeBaseLayer = () =>
  Layer.mergeAll(makeTestSqlLayer(), makeTestNodeConfigLayer());

describe("tx ingress processor integration", () => {
  it.effect("persists valid ingress message into MempoolDB and acks it", () =>
    Effect.gen(function* () {
      const { queue, ackSpy, failedSpy } = makeQueueStub([
        {
          id: "100-0",
          txCbor: txCborA.toString("hex"),
          deliveryCount: 1,
        },
      ]);

      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([makeSeedLedgerEntry()]);

      yield* txQueueProcessorAction(10, 4, 1, true).pipe(
        Effect.provideService(TxIngressQueue, queue),
      );

      const count = yield* MempoolDB.retrieveTxCount;
      expect(count).toBe(1n);
      expect(ackSpy).toHaveBeenCalledWith(["100-0"]);
      expect(failedSpy).not.toHaveBeenCalled();
    }).pipe(Effect.provide(makeBaseLayer())),
  );

  it.effect("keeps mempool idempotent when ingress delivers duplicates", () =>
    Effect.gen(function* () {
      const { queue, ackSpy, failedSpy } = makeQueueStub([
        {
          id: "200-0",
          txCbor: txCborA.toString("hex"),
          deliveryCount: 1,
        },
        {
          id: "201-0",
          txCbor: txCborA.toString("hex"),
          deliveryCount: 1,
        },
      ]);

      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([makeSeedLedgerEntry()]);

      yield* txQueueProcessorAction(10, 4, 1, true).pipe(
        Effect.provideService(TxIngressQueue, queue),
      );

      const count = yield* MempoolDB.retrieveTxCount;
      expect(count).toBe(1n);
      expect(ackSpy).toHaveBeenCalledWith(["200-0", "201-0"]);
      expect(failedSpy).not.toHaveBeenCalled();
    }).pipe(Effect.provide(makeBaseLayer())),
  );

  it.effect(
    "does not persist or ack an ingress message that fails validation",
    () =>
      Effect.gen(function* () {
        const { queue, ackSpy, failedSpy } = makeQueueStub([
          {
            id: "300-0",
            txCbor: txCborA.toString("hex"),
            deliveryCount: 1,
          },
        ]);

        yield* DBInitialization.program;
        // Intentionally do NOT seed the ledger, so txCborA spends an input that
        // does not exist and Phase B rejects it.

        yield* txQueueProcessorAction(10, 4, 1, true).pipe(
          Effect.provideService(TxIngressQueue, queue),
        );

        const count = yield* MempoolDB.retrieveTxCount;
        expect(count).toBe(0n);
        expect(ackSpy).not.toHaveBeenCalled();
        expect(failedSpy).toHaveBeenCalledTimes(1);
        // The structured validation reason is propagated to the failure handler
        // (which decides retry vs dead-letter).
        const [failedMessage, failureReason] = failedSpy.mock.calls[0]!;
        expect(failedMessage.id).toBe("300-0");
        expect(failureReason).toContain("E_INPUT_NOT_FOUND");
      }).pipe(Effect.provide(makeBaseLayer())),
  );
});
