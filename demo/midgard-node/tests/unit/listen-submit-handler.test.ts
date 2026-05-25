import { describe, expect, it } from "vitest";
import { Duration, Effect } from "effect";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { postSubmitHandlerForTesting } from "@/commands/listen.js";
import { TxIngressQueueService } from "@/services/tx-ingress-queue.js";

const runSubmitHandler = (
  txIngressQueue: TxIngressQueueService,
  txQueueOfferTimeoutMs: number,
  request: Request,
) =>
  postSubmitHandlerForTesting({
    txIngressQueue,
    txQueueOfferTimeoutMs,
    txQueueCapacity: 10,
    txQueueMaxPending: 10,
  }).pipe(
    Effect.provideService(
      HttpServerRequest.HttpServerRequest,
      HttpServerRequest.fromWeb(request),
    ),
  );

const makeQueueStub = (
  overrides?: Partial<TxIngressQueueService>,
): TxIngressQueueService => ({
  enqueue: (_txCbor: string) => Effect.succeed("1-0"),
  ensureConsumerGroup: Effect.void,
  consumeBatch: (_maxCount: number, _blockMs: number) => Effect.succeed([]),
  ack: (_messageIds: readonly string[]) => Effect.succeed(0),
  handleFailedMessage: (_message, _reason) => Effect.succeed("retry"),
  snapshotMetrics: Effect.succeed({
    streamDepth: 0,
    pendingCount: 0,
    lagCount: 0,
  }),
  ...overrides,
});

describe("postSubmitHandler", () => {
  it("reads tx CBOR from request body and ignores query string", async () => {
    const request = new Request("http://localhost/submit?tx_cbor=aa", {
      method: "POST",
      body: "not-hex-body",
    });

    const response = await Effect.runPromise(
      runSubmitHandler(makeQueueStub(), 10, request),
    );
    const webResponse = HttpServerResponse.toWeb(response);
    const body = await webResponse.json();

    expect(webResponse.status).toBe(400);
    expect(body).toEqual({ error: "Invalid CBOR provided" });
  });

  it("returns 503 when enqueue times out", async () => {
    const queue = makeQueueStub({
      enqueue: (_value: string) =>
        Effect.sleep(Duration.seconds(60)).pipe(Effect.as("1-0")),
    });

    const request = new Request("http://localhost/submit", {
      method: "POST",
      body: "bb",
    });

    const response = await Effect.runPromise(
      runSubmitHandler(queue, 1, request),
    );
    const webResponse = HttpServerResponse.toWeb(response);
    const body = await webResponse.json();

    expect(webResponse.status).toBe(503);
    expect(body).toEqual({
      error: "Transaction queue is saturated; retry later",
    });
  });

  it("returns 503 when stream depth exceeds configured capacity", async () => {
    const queue = makeQueueStub({
      snapshotMetrics: Effect.succeed({
        streamDepth: 50,
        pendingCount: 0,
        lagCount: 50,
      }),
    });

    const request = new Request("http://localhost/submit", {
      method: "POST",
      body: "bb",
    });

    const response = await Effect.runPromise(
      postSubmitHandlerForTesting({
        txIngressQueue: queue,
        txQueueOfferTimeoutMs: 50,
        txQueueCapacity: 10,
        txQueueMaxPending: 10,
      }).pipe(
        Effect.provideService(
          HttpServerRequest.HttpServerRequest,
          HttpServerRequest.fromWeb(request),
        ),
      ),
    );

    const webResponse = HttpServerResponse.toWeb(response);
    const body = await webResponse.json();

    expect(webResponse.status).toBe(503);
    expect(body).toEqual({
      error: "Transaction queue is saturated; retry later",
    });
  });
});
