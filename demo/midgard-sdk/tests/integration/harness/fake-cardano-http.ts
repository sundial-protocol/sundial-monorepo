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
// ─── Implementation notes ─────────────────────────────────────────────────────
//
// TODO (implementation): use Node.js built-in `http.createServer` to start a
// per-test localhost HTTP server on a random port.  Return the port and a
// `requests` array that accumulates observed request records.  Tear down the
// server after the callback returns (in finally block).
//
// Recommended packages (no new dependencies required — built-in http module):
//   Node.js `http` module — createServer, listen, close
//
// For Blockfrost/Kupmios RPC paths, the fake server should parse the request
// body (JSON or CBOR depending on the Content-Type header) and return
// deterministic fixture JSON from the routes config.

export type FakeHttpRoute = {
  status: number;
  body: Record<string, any>;
};

export type FakeCardanoHttpOptions = {
  /** Map of request path → response fixture. */
  routes?: Record<string, FakeHttpRoute>;
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

/**
 * Starts a localhost fake Cardano provider HTTP server, runs the callback,
 * then tears the server down.
 *
 * TODO (implementation):
 *   1. Create an http.Server that captures each incoming request into `requests`.
 *   2. Look up the request path in opts.routes; respond with the fixture or 404.
 *   3. Listen on port 0 (OS-assigned) and extract the actual port from
 *      server.address().port.
 *   4. Call the callback with { port, requests }.
 *   5. Close the server in a finally block regardless of callback outcome.
 */
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
  } catch (_error) {
    // Some sandboxed CI environments disallow opening listening sockets.
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
import { createServer } from "node:http";
