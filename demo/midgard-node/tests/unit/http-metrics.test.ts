import { describe, expect, it } from "vitest";
import { Effect, Metric } from "effect";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import {
  makeHttpMetricsMiddleware,
  recordHttpRequest,
  registerBuildInfo,
} from "@/commands/http-metrics.js";

const snapshotFor = (name: string) =>
  Effect.gen(function* () {
    const pairs = yield* Metric.snapshot;
    return pairs.filter((pair) => pair.metricKey.name === name);
  });

const tagValue = (
  pair: {
    readonly metricKey: {
      readonly tags: ReadonlyArray<{ key: string; value: string }>;
    };
  },
  key: string,
) => pair.metricKey.tags.find((tag) => tag.key === key)?.value;

describe("recordHttpRequest", () => {
  it("records duration and count under the expected route/method/status_class tags", async () => {
    const pairs = await Effect.runPromise(
      Effect.gen(function* () {
        yield* recordHttpRequest("submit", "POST", 200, 0.25);
        yield* recordHttpRequest("submit", "POST", 503, 0.5);
        return yield* snapshotFor("http_server_requests");
      }),
    );

    const twoXX = pairs.find((p) => tagValue(p, "status_class") === "2xx");
    const fiveXX = pairs.find((p) => tagValue(p, "status_class") === "5xx");
    expect(twoXX).toBeDefined();
    expect(fiveXX).toBeDefined();
    expect(tagValue(twoXX!, "route")).toBe("submit");
    expect(tagValue(twoXX!, "method")).toBe("POST");
  });
});

describe("registerBuildInfo", () => {
  it("publishes midgard_node_build_info=1 with provenance labels", async () => {
    const pairs = await Effect.runPromise(
      Effect.gen(function* () {
        yield* registerBuildInfo({
          version: "9.9.9",
          commit: "abc1234",
          l1Provider: "Kupmios",
          network: "Preprod",
          role: "all",
        });
        return yield* snapshotFor("midgard_node_build_info");
      }),
    );
    const buildInfo = pairs.find((p) => tagValue(p, "version") === "9.9.9");

    expect(buildInfo).toBeDefined();
    expect(tagValue(buildInfo!, "version")).toBe("9.9.9");
    expect(tagValue(buildInfo!, "commit")).toBe("abc1234");
    expect(tagValue(buildInfo!, "l1_provider")).toBe("Kupmios");
  });
});

describe("makeHttpMetricsMiddleware", () => {
  const run = (
    app: Effect.Effect<
      HttpServerResponse.HttpServerResponse,
      unknown,
      HttpServerRequest.HttpServerRequest
    >,
    url: string,
  ) => {
    const middleware = makeHttpMetricsMiddleware(["block", "submit"]);
    const request = HttpServerRequest.HttpServerRequest.of({
      method: "GET",
      url,
      originalUrl: url,
    } as never);
    return middleware(app as never).pipe(
      Effect.provideService(HttpServerRequest.HttpServerRequest, request),
    ) as Effect.Effect<HttpServerResponse.HttpServerResponse, unknown, never>;
  };

  it("labels a known route by its path and an unknown route as other", async () => {
    const pairs = await Effect.runPromise(
      Effect.gen(function* () {
        yield* run(HttpServerResponse.json({ ok: true }), "/block?x=1");
        yield* run(HttpServerResponse.json({ ok: true }), "/logGlobals");
        return yield* snapshotFor("http_server_requests");
      }),
    );

    const routes = new Set(
      pairs.map((p) => tagValue(p, "route")).filter(Boolean),
    );
    expect(routes.has("block")).toBe(true);
    expect(routes.has("other")).toBe(true);
  });

  it("records 5xx when the inner app fails", async () => {
    const pairs = await Effect.runPromise(
      Effect.gen(function* () {
        yield* run(Effect.fail("boom") as never, "/block").pipe(
          Effect.catchAll(() => Effect.void),
        );
        return yield* snapshotFor("http_server_requests");
      }),
    );
    const failed = pairs.find((p) => tagValue(p, "status_class") === "5xx");
    expect(failed).toBeDefined();
  });
});
