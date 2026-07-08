import { beforeEach, describe, expect, it, vi } from "vitest";
import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import type { FaucetClaimInput, FaucetClaimResult } from "@/services/faucet.js";

// The handler calls `Faucet.processClaim` directly; mock that single
// collaborator at the module boundary while keeping the real `FaucetClaimError`
// class so the handler's `catchTag`/status mapping runs against production code.
let processClaimImpl: (
  input: FaucetClaimInput,
) => Effect.Effect<FaucetClaimResult, unknown>;

vi.mock("@/services/faucet.js", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@/services/faucet.js")>();
  return {
    ...actual,
    processClaim: (input: FaucetClaimInput) => processClaimImpl(input),
  };
});

import { postFaucetClaimsHandlerForTesting } from "@/commands/listen.js";
import { FaucetClaimError } from "@/services/faucet.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";

const API_KEY = "test-faucet-secret";

const runHandler = (request: Request, faucetEnabled = true) =>
  postFaucetClaimsHandlerForTesting.pipe(
    Effect.provideService(
      HttpServerRequest.HttpServerRequest,
      HttpServerRequest.fromWeb(request),
    ),
    Effect.provide(
      makeTestNodeConfigLayer({
        FAUCET_ENABLED: faucetEnabled,
        FAUCET_API_KEY: faucetEnabled ? API_KEY : "",
      }),
    ),
    // `processClaim` is mocked, so the SqlClient is never used at runtime; it is
    // only present in the handler's static requirement set.
    Effect.provideService(
      SqlClient.SqlClient,
      {} as unknown as SqlClient.SqlClient,
    ),
  );

const makeRequest = (
  body: unknown,
  headers: Record<string, string> = { authorization: `Bearer ${API_KEY}` },
  rawBody?: string,
) =>
  new Request("http://localhost/faucet/claims", {
    method: "POST",
    headers: { "content-type": "application/json", ...headers },
    body: rawBody ?? JSON.stringify(body),
  });

const validBody = {
  address:
    "addr_test1q9p0z0rz0t9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z9z",
  idempotencyKey: "idem-123",
  ipHash: "ip-abc",
};

const responseJson = async (
  response: HttpServerResponse.HttpServerResponse,
) => {
  const web = HttpServerResponse.toWeb(response);
  return { status: web.status, body: await web.json() };
};

describe("postFaucetClaimsHandler", () => {
  beforeEach(() => {
    processClaimImpl = () =>
      Effect.die("processClaim should not be called in this test");
  });

  it("returns 404 when the faucet is disabled", async () => {
    const response = await Effect.runPromise(
      runHandler(makeRequest(validBody), false),
    );
    const { status, body } = await responseJson(response);
    expect(status).toBe(404);
    expect(body).toMatchObject({ error: "Faucet is not enabled" });
  });

  it("returns 401 when the bearer token is missing", async () => {
    const response = await Effect.runPromise(
      runHandler(makeRequest(validBody, {})),
    );
    expect((await responseJson(response)).status).toBe(401);
  });

  it("returns 401 when the bearer token is wrong", async () => {
    const response = await Effect.runPromise(
      runHandler(makeRequest(validBody, { authorization: "Bearer nope" })),
    );
    expect((await responseJson(response)).status).toBe(401);
  });

  it("returns 400 on malformed JSON", async () => {
    const response = await Effect.runPromise(
      runHandler(makeRequest(undefined, undefined, "{not json")),
    );
    expect((await responseJson(response)).status).toBe(400);
  });

  it("returns 400 when required fields are missing", async () => {
    const response = await Effect.runPromise(
      runHandler(makeRequest({ address: validBody.address })),
    );
    expect((await responseJson(response)).status).toBe(400);
  });

  it("returns 200 and the claim receipt on success", async () => {
    const seen: FaucetClaimInput[] = [];
    const nextEligibleAt = new Date("2026-06-26T00:00:00.000Z");
    processClaimImpl = (input) => {
      seen.push(input);
      return Effect.succeed({
        claimId: "claim-1",
        txHash: "ab".repeat(32),
        amount: 100_000_000n,
        nextEligibleAt,
        idempotentReplay: false,
      });
    };

    const response = await Effect.runPromise(
      runHandler(makeRequest(validBody)),
    );
    const { status, body } = await responseJson(response);

    expect(status).toBe(200);
    expect(body).toEqual({
      claimId: "claim-1",
      txHash: "ab".repeat(32),
      amount: "100000000",
      nextEligibleAt: nextEligibleAt.toISOString(),
    });
    expect(seen).toEqual([
      {
        address: validBody.address,
        idempotencyKey: validBody.idempotencyKey,
        ipHash: validBody.ipHash,
      },
    ]);
  });

  it("maps COOLDOWN to 429 and surfaces nextEligibleAt", async () => {
    const nextEligibleAt = new Date("2026-06-27T00:00:00.000Z");
    processClaimImpl = () =>
      Effect.fail(
        new FaucetClaimError({
          code: "COOLDOWN",
          message: "Address is in faucet cooldown",
          nextEligibleAt,
        }),
      );

    const response = await Effect.runPromise(
      runHandler(makeRequest(validBody)),
    );
    const { status, body } = await responseJson(response);

    expect(status).toBe(429);
    expect(body).toMatchObject({
      code: "COOLDOWN",
      nextEligibleAt: nextEligibleAt.toISOString(),
    });
  });

  it("maps DEPLETED to 503", async () => {
    processClaimImpl = () =>
      Effect.fail(
        new FaucetClaimError({ code: "DEPLETED", message: "no funds" }),
      );
    const response = await Effect.runPromise(
      runHandler(makeRequest(validBody)),
    );
    expect((await responseJson(response)).status).toBe(503);
  });

  it("maps INTERNAL to 500 without leaking the internal message", async () => {
    processClaimImpl = () =>
      Effect.fail(
        new FaucetClaimError({
          code: "INTERNAL",
          message: "secret stack detail",
        }),
      );
    const response = await Effect.runPromise(
      runHandler(makeRequest(validBody)),
    );
    const { status, body } = await responseJson(response);
    expect(status).toBe(500);
    expect(body).toMatchObject({ error: "Faucet request failed" });
    expect(JSON.stringify(body)).not.toContain("secret stack detail");
  });
});
