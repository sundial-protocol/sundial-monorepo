import { describe, expect, it } from "vitest";
import { Effect, Ref } from "effect";
import { HttpServerResponse } from "@effect/platform";
import {
  createResetHandlerForTesting,
  getStateQueueRootUnitDiagnosticsHandlerForTesting,
} from "@/commands/listen.js";
import { Globals } from "@/services/globals.js";

describe("reset handler concurrency guard", () => {
  it("returns 409 and skips reset when reset is already in progress", async () => {
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const called = yield* Ref.make(false);
        const globals = yield* Globals;
        yield* Ref.set(globals.RESET_IN_PROGRESS, true);

        const response = yield* createResetHandlerForTesting(
          Ref.set(called, true).pipe(Effect.asVoid),
        );

        return {
          response,
          called: yield* Ref.get(called),
        };
      }).pipe(Effect.provide(Globals.Default)),
    );

    const webResponse = HttpServerResponse.toWeb(result.response);
    const body = await webResponse.json();

    expect(webResponse.status).toBe(409);
    expect(body).toEqual({ error: "Reset already in progress" });
    expect(result.called).toBe(false);
  });
});

describe("state-queue root unit diagnostics handler", () => {
  it("returns diagnostics payload when snapshot query succeeds", async () => {
    const response = await Effect.runPromise(
      getStateQueueRootUnitDiagnosticsHandlerForTesting(
        Effect.succeed({
          status: "invalid" as const,
          resetInProgress: false,
          stateQueueAddress: "addr_test1xyz",
          rootUnit: "policyplusasset",
          count: 2,
          outRefs: ["tx1#0", "tx2#1"],
        }),
      ),
    );

    const webResponse = HttpServerResponse.toWeb(response);
    const body = await webResponse.json();

    expect(webResponse.status).toBe(200);
    expect(body).toEqual({
      status: "invalid",
      resetInProgress: false,
      stateQueueAddress: "addr_test1xyz",
      rootUnit: "policyplusasset",
      count: 2,
      outRefs: ["tx1#0", "tx2#1"],
    });
  });

  it("returns 503 when snapshot query fails", async () => {
    const response = await Effect.runPromise(
      getStateQueueRootUnitDiagnosticsHandlerForTesting(
        Effect.fail(new Error("boom")),
      ),
    );

    const webResponse = HttpServerResponse.toWeb(response);
    const body = (await webResponse.json()) as {
      status: string;
      error: string;
      cause: string;
    };

    expect(webResponse.status).toBe(503);
    expect(body.status).toBe("error");
    expect(body.error).toBe(
      "Failed to query state-queue root-unit diagnostics",
    );
    expect(body.cause).toBe("boom");
  });
});
