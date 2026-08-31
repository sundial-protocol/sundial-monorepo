import type { IncomingMessage, Server, ServerResponse } from "node:http";
import { isHexString } from "@/utils.js";

const JSON_HEADER = { "Content-Type": "application/json" } as const;

export type RawSubmitInterceptorOptions = {
  readonly xadd: XaddFn;
  readonly onEnqueued: () => void;
  readonly onRejected: () => void;
  // Called once per handled request with the final HTTP status code and the
  // wall-clock duration in seconds. Used to feed the shared HTTP request
  // metrics for the `POST /submit` ingress path, which bypasses the Effect
  // router (and therefore the router-level metrics middleware).
  readonly onResponse?: (statusCode: number, durationSeconds: number) => void;
};

export type RawSubmitInterceptor = {
  readonly attachToServer: (server: Server) => void;
};

// Narrow callback type used by handleRawSubmitRequest so it can be tested
// without constructing a real Redis instance.
export type XaddFn = (
  body: string,
  callback: (err: Error | null | undefined, id: string | null) => void,
) => void;

export const handleRawSubmitRequest = (
  req: IncomingMessage,
  res: ServerResponse,
  xadd: XaddFn,
  onEnqueued: () => void,
  onRejected: () => void,
  onResponse?: (statusCode: number, durationSeconds: number) => void,
): void => {
  const startedAt = performance.now();
  const report = (statusCode: number): void => {
    onResponse?.(statusCode, (performance.now() - startedAt) / 1000);
  };

  const chunks: Buffer[] = [];

  req.on("data", (chunk: Buffer) => {
    chunks.push(chunk);
  });

  req.on("error", () => {
    if (!res.writableEnded) {
      onRejected();
      res.writeHead(400, JSON_HEADER);
      res.end(JSON.stringify({ error: "Request read error" }));
      report(400);
    }
  });

  req.on("end", () => {
    const body = Buffer.concat(chunks).toString("utf8");

    if (!isHexString(body)) {
      onRejected();
      res.writeHead(400, JSON_HEADER);
      res.end(JSON.stringify({ error: "Invalid CBOR provided" }));
      report(400);
      return;
    }

    xadd(body, (err, id) => {
      if (err != null || id === null) {
        res.writeHead(500, JSON_HEADER);
        res.end(JSON.stringify({ error: "Failed to enqueue transaction" }));
        report(500);
        return;
      }
      onEnqueued();
      res.writeHead(200, JSON_HEADER);
      res.end(
        JSON.stringify({
          message: "Successfully added the transaction to the queue",
          id,
        }),
      );
      report(200);
    });
  });
};

export const createRawSubmitInterceptor = (
  options: RawSubmitInterceptorOptions,
): RawSubmitInterceptor => {
  const attachToServer = (server: Server): void => {
    const originalEmit = server.emit.bind(server);

    const interceptedEmit = (event: string, ...rest: unknown[]): boolean => {
      if (event === "request") {
        const req = rest[0] as IncomingMessage;
        const res = rest[1] as ServerResponse;
        if (req.method === "POST" && req.url === "/submit") {
          handleRawSubmitRequest(
            req,
            res,
            options.xadd,
            options.onEnqueued,
            options.onRejected,
            options.onResponse,
          );
          return true;
        }
      }
      return Reflect.apply(originalEmit, server, [event, ...rest]) as boolean;
    };

    // Double cast is unavoidable: http.Server.emit carries complex EventEmitter
    // overload signatures that a single replacement function cannot replicate in
    // TypeScript. The interception is necessary because Node.js EventEmitter
    // offers no stop-propagation mechanism — overriding emit at the source is
    // the only way to prevent Effect's 'request' listener from seeing an
    // already-handled POST /submit request.
    // TODO: remove if @effect/platform-node adds a pre-handler interceptor hook.
    server.emit = interceptedEmit as unknown as typeof server.emit;
  };

  return { attachToServer };
};
