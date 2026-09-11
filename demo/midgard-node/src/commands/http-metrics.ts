import { HttpMiddleware, HttpServerRequest } from "@effect/platform";
import { Context, Effect, Metric, MetricBoundaries, MetricLabel } from "effect";

// ─── Build / process identity ────────────────────────────────────────────────

// Not `.register()`-ed: this metric is only ever emitted with its label set
// (registering would also publish a meaningless untagged `...{} 0` series). It
// is set once at startup, so it appears immediately anyway.
const buildInfoGauge = Metric.gauge("midgard_node_build_info", {
  description:
    "Always 1. Labels carry the running node version, git commit, L1 provider, network and role for provenance in backward reporting.",
});

const startTimeGauge = Metric.gauge("midgard_node_start_time_seconds", {
  description: "Unix timestamp (seconds) at which this node process started.",
}).register();

export type BuildInfo = {
  readonly version: string;
  readonly commit: string;
  readonly l1Provider: string;
  readonly network: string;
  readonly role: string;
};

/**
 * Publishes `midgard_node_build_info` and `midgard_node_start_time_seconds`.
 * Call once at startup. The build_info series is the anchor the reliability
 * report uses to attribute a metrics window to a specific node build and to
 * detect redeploys/restarts across a long reporting period.
 */
export const registerBuildInfo = (info: BuildInfo): Effect.Effect<void> =>
  Effect.all(
    [
      Metric.set(
        Metric.taggedWithLabels(buildInfoGauge, [
          MetricLabel.make("version", info.version),
          MetricLabel.make("commit", info.commit),
          MetricLabel.make("l1_provider", info.l1Provider),
          MetricLabel.make("network", info.network),
          MetricLabel.make("role", info.role),
        ]),
        1,
      ),
      Metric.set(startTimeGauge, Date.now() / 1000),
    ],
    { discard: true },
  );

// ─── HTTP request metrics ────────────────────────────────────────────────────

// Not `.register()`-ed: always emitted with a bounded route/method/status_class
// label set, and both series appear within seconds of the first request on any
// live node. Registering would add a spurious untagged `...{} 0` series.
const httpRequestDurationSeconds = Metric.histogram(
  "http_server_request_duration_seconds",
  MetricBoundaries.exponential({ start: 0.005, factor: 2, count: 15 }),
  "HTTP server request duration in seconds, by route, method and status class.",
);

const httpRequestsTotal = Metric.counter("http_server_requests", {
  description:
    "HTTP server requests handled, by route, method and status class.",
  incremental: true,
});

const statusClass = (status: number): string =>
  status >= 500
    ? "5xx"
    : status >= 400
      ? "4xx"
      : status >= 300
        ? "3xx"
        : status >= 200
          ? "2xx"
          : "1xx";

/**
 * Records one HTTP request against the shared duration histogram and request
 * counter. Safe to call from plain Node callback code via `Effect.runSync`
 * (that is how the `POST /submit` raw interceptor path reports).
 */
export const recordHttpRequest = (
  route: string,
  method: string,
  status: number,
  durationSeconds: number,
): Effect.Effect<void> => {
  const labels = [
    MetricLabel.make("route", route),
    MetricLabel.make("method", method),
    MetricLabel.make("status_class", statusClass(status)),
  ];
  return Effect.all(
    [
      Metric.update(
        Metric.taggedWithLabels(httpRequestDurationSeconds, labels),
        durationSeconds,
      ),
      Metric.update(Metric.taggedWithLabels(httpRequestsTotal, labels), 1),
    ],
    { discard: true },
  );
};

const routeLabel = (knownRoutes: ReadonlySet<string>, url: string): string => {
  const pathname = url.split("?", 1)[0] ?? url;
  const normalized = pathname.startsWith("/") ? pathname : `/${pathname}`;
  return knownRoutes.has(normalized) ? normalized.replace(/^\//, "") : "other";
};

/**
 * Router middleware that times every request the Effect HTTP app handles and
 * records it with a bounded `route` label (known endpoints keep their path, all
 * others collapse to `other`). `POST /submit` is intercepted before the router
 * and is reported separately from the raw interceptor.
 */
export const makeHttpMetricsMiddleware = (
  knownRoutes: ReadonlyArray<string>,
) => {
  const known = new Set(
    knownRoutes.map((route) => (route.startsWith("/") ? route : `/${route}`)),
  );
  return HttpMiddleware.make((httpApp) =>
    Effect.withFiberRuntime((fiber) => {
      const request = Context.unsafeGet(
        fiber.currentContext,
        HttpServerRequest.HttpServerRequest,
      );
      const started = performance.now();
      return Effect.flatMap(Effect.exit(httpApp), (exit) => {
        const durationSeconds = (performance.now() - started) / 1000;
        const status = exit._tag === "Success" ? exit.value.status : 500;
        return Effect.zipRight(
          recordHttpRequest(
            routeLabel(known, request.url),
            request.method,
            status,
            durationSeconds,
          ),
          exit,
        );
      });
    }),
  );
};
