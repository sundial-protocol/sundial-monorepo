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
  // TODO (implementation): replace this stub with the real server lifecycle.
  const ctx: FakeCardanoHttpContext = {
    port: 0, // placeholder — real impl assigns OS port
    requests: [],
  };
  await callback(ctx);
  void opts; // suppress unused-var lint until implemented
};
