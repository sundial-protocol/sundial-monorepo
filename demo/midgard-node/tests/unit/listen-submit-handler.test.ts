import { describe, expect, it } from "vitest";
import { Effect } from "effect";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { postSubmitHandlerForTesting } from "@/commands/listen.js";
import { TxIngressQueueService } from "@/services/tx-ingress-queue.js";

const runSubmitHandler = (
  txIngressQueue: TxIngressQueueService,
  request: Request,
) =>
  postSubmitHandlerForTesting({ txIngressQueue }).pipe(
    Effect.provideService(
      HttpServerRequest.HttpServerRequest,
      HttpServerRequest.fromWeb(request),
    ),
  );

const makeQueueStub = (
  overrides?: Partial<TxIngressQueueService>,
): TxIngressQueueService => ({
  enqueue: (_txCbor: string) => Effect.succeed("1-0"),
  rawXadd: (_txCbor, callback) => callback(null, "1-0"),
  ensureConsumerGroup: Effect.void,
  consumeBatch: (_maxCount: number, _blockMs: number) => Effect.succeed([]),
  ack: (_messageIds: readonly string[]) => Effect.succeed(0),
  handleFailedMessage: (_message, _reason) => Effect.succeed("retry"),
  refreshSnapshotMetrics: Effect.succeed({
    streamDepth: 0,
    pendingCount: 0,
    lagCount: 0,
  }),
  snapshotMetrics: Effect.succeed({
    streamDepth: 0,
    pendingCount: 0,
    lagCount: 0,
  }),
  clear: Effect.void,
  ...overrides,
});

describe("postSubmitHandler", () => {
  it("reads tx CBOR from request body and ignores query string", async () => {
    const request = new Request("http://localhost/submit?tx_cbor=aa", {
      method: "POST",
      body: "not-hex-body",
    });

    const response = await Effect.runPromise(
      runSubmitHandler(makeQueueStub(), request),
    );
    const webResponse = HttpServerResponse.toWeb(response);
    const body = await webResponse.json();

    expect(webResponse.status).toBe(400);
    expect(body).toEqual({ error: "Invalid CBOR provided" });
  });

  it("returns 200 with message id on successful enqueue", async () => {
    const queue = makeQueueStub({
      enqueue: (_value: string) => Effect.succeed("42-0"),
    });

    const request = new Request("http://localhost/submit", {
      method: "POST",
      body: "bb",
    });

    const response = await Effect.runPromise(runSubmitHandler(queue, request));
    const webResponse = HttpServerResponse.toWeb(response);
    const body = await webResponse.json();

    expect(webResponse.status).toBe(200);
    expect(body).toMatchObject({
      message: "Successfully added the transaction to the queue",
      id: "42-0",
    });
  });
});
