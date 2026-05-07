import { createServer } from "node:http";

// HTTP boundary mode harness for SDK integration tests.
//
// Use this when the test needs to prove that real HTTP/RPC wiring is exercised:
// SDK or node client makes a real HTTP call, hits a localhost fake server, and
// the test asserts the observed request method, path, headers, and body.
//
// Do NOT use this for most tests.  Default to makeFakeLucid (fast SDK
// integration mode) and only switch to HTTP boundary mode when the test
// explicitly verifies provider/RPC request wiring or submit wiring.
//
// ─── Usage ────────────────────────────────────────────────────────────────────
//
//   await withFakeCardanoHttp(
//     { routes: { "/api/submit/tx": { status: 200, body: { result: "ok" } } } },
//     async ({ port, requests }) => {
//       // configure SDK/node client to use http://127.0.0.1:${port}
//       // run the SDK operation
//       // assert requests[0].method === "POST"
//       // assert requests[0].path === "/api/submit/tx"
//     },
//   );
//
export type FakeHttpRoute = {
  status: number;
  body: Record<string, unknown>;
};

export type FakeCardanoHttpOptions = {
  /** Map of request path → response fixture. */
  routes?: Record<string, FakeHttpRoute>;
  /**
   * When true, a socket-bind failure calls the callback with `port=0`
   * instead of failing the test.
   */
  allowListenFailureFallback?: boolean;
};

export type ObservedRequest = {
  method: string;
  path: string;
  headers: Record<string, string>;
  body: string;
};

export type FakeCardanoHttpContext = {
  /** Port the fake server is listening on (use http://127.0.0.1:{port}). */
  port: number;
  /** Accumulated incoming requests for assertion. */
  requests: ObservedRequest[];
};

/** Starts a localhost fake Cardano provider HTTP server around `callback`. */
export const withFakeCardanoHttp = async (
  opts: FakeCardanoHttpOptions,
  callback: (ctx: FakeCardanoHttpContext) => Promise<void>,
): Promise<void> => {
  const requests: ObservedRequest[] = [];
  const routes = opts.routes ?? {};
  const server = createServer((req, res) => {
    const chunks: Buffer[] = [];
    req.on("data", (chunk) => chunks.push(Buffer.from(chunk)));
    req.on("end", () => {
      const path = req.url ?? "/";
      requests.push({
        method: req.method ?? "GET",
        path,
        headers: Object.fromEntries(
          Object.entries(req.headers).map(([k, v]) => [k, String(v ?? "")]),
        ),
        body: Buffer.concat(chunks).toString("utf8"),
      });
      const route = routes[path];
      if (!route) {
        res.statusCode = 404;
        res.setHeader("content-type", "application/json");
        res.end(JSON.stringify({ error: "not found" }));
        return;
      }
      res.statusCode = route.status;
      res.setHeader("content-type", "application/json");
      res.end(JSON.stringify(route.body));
    });
  });
  try {
    await new Promise<void>((resolve, reject) => {
      server.once("error", reject);
      server.listen(0, "127.0.0.1", () => resolve());
    });
  } catch (error) {
    if (!opts.allowListenFailureFallback) {
      throw error;
    }
    await callback({ port: 0, requests });
    return;
  }
  const address = server.address();
  const port = typeof address === "object" && address ? address.port : 0;
  try {
    await callback({ port, requests });
  } finally {
    await new Promise<void>((resolve, reject) => {
      server.close((err) => {
        if (err) {
          reject(err);
          return;
        }
        resolve();
      });
    });
  }
};
