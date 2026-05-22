import { describe, expect, it } from "vitest";
import { Duration, Effect, Queue } from "effect";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { postSubmitHandlerForTesting } from "@/commands/listen.js";

const runSubmitHandler = (
  queue: Queue.Enqueue<string>,
  txQueueOfferTimeoutMs: number,
  request: Request,
) =>
  postSubmitHandlerForTesting(queue, txQueueOfferTimeoutMs).pipe(
    Effect.provideService(
      HttpServerRequest.HttpServerRequest,
      HttpServerRequest.fromWeb(request),
    ),
  );

describe("postSubmitHandler", () => {
  it("reads tx CBOR from request body and ignores query string", async () => {
    const queue = await Effect.runPromise(Queue.bounded<string>(10));
    const request = new Request("http://localhost/submit?tx_cbor=aa", {
      method: "POST",
      body: "not-hex-body",
    });

    const response = await Effect.runPromise(
      runSubmitHandler(queue, 10, request),
    );
    const webResponse = HttpServerResponse.toWeb(response);
    const body = await webResponse.json();

    expect(webResponse.status).toBe(400);
    expect(body).toEqual({ error: "Invalid CBOR provided" });
    expect(await Effect.runPromise(queue.size)).toBe(0);
  });

  it("returns 503 when queue offer times out under backpressure", async () => {
    const queue = {
      offer: (_value: string) =>
        Effect.sleep(Duration.seconds(60)).pipe(Effect.as(true)),
      size: Effect.succeed(1),
    } as unknown as Queue.Enqueue<string>;

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
});
