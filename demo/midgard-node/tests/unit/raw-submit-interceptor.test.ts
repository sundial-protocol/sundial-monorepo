import { EventEmitter } from "node:events";
import type { IncomingMessage, Server, ServerResponse } from "node:http";
import { describe, expect, it, vi } from "vitest";
import {
  handleRawSubmitRequest,
  type XaddFn,
} from "@/services/raw-submit-interceptor.js";

// --- test helpers ---

const makeReq = (method = "POST", url = "/submit"): IncomingMessage => {
  const ee = new EventEmitter() as unknown as IncomingMessage;
  (ee as unknown as { method: string }).method = method;
  (ee as unknown as { url: string }).url = url;
  return ee;
};

const makeRes = () => {
  let statusCode = 200;
  let body = "";
  const res = {
    writableEnded: false,
    writeHead: vi.fn((code: number) => {
      statusCode = code;
    }),
    end: vi.fn((data: string) => {
      body = data ?? "";
    }),
    get statusCode() {
      return statusCode;
    },
    get body() {
      return body;
    },
  } as unknown as ServerResponse & { statusCode: number; body: string };
  return res;
};

const makeXadd = (result: string | null, err?: Error): XaddFn =>
  vi.fn((_body, callback) => callback(err, result));

const makeCallbacks = () => ({
  onEnqueued: vi.fn(),
  onRejected: vi.fn(),
});

const fireBody = (req: IncomingMessage, body: string): void => {
  req.emit("data", Buffer.from(body));
  req.emit("end");
};

// --- handleRawSubmitRequest ---

describe("handleRawSubmitRequest", () => {
  it("returns 400 for non-hex body", () => {
    const req = makeReq();
    const res = makeRes();
    const xadd = makeXadd("1-0");
    const { onEnqueued, onRejected } = makeCallbacks();

    handleRawSubmitRequest(req, res, xadd, onEnqueued, onRejected);
    fireBody(req, "not-hex!");

    expect(res.writeHead).toHaveBeenCalledWith(400, expect.any(Object));
    expect(JSON.parse(res.body)).toEqual({ error: "Invalid CBOR provided" });
    expect(xadd).not.toHaveBeenCalled();
  });

  it("returns 400 for empty body", () => {
    const req = makeReq();
    const res = makeRes();
    const xadd = makeXadd("1-0");
    const { onEnqueued, onRejected } = makeCallbacks();

    handleRawSubmitRequest(req, res, xadd, onEnqueued, onRejected);
    fireBody(req, "");

    expect(res.writeHead).toHaveBeenCalledWith(400, expect.any(Object));
    expect(xadd).not.toHaveBeenCalled();
  });

  it("calls xadd and returns 200 with id for valid hex body", () => {
    const req = makeReq();
    const res = makeRes();
    const xadd = makeXadd("42-0");
    const { onEnqueued, onRejected } = makeCallbacks();

    handleRawSubmitRequest(req, res, xadd, onEnqueued, onRejected);
    fireBody(req, "deadbeef");

    expect(xadd).toHaveBeenCalledWith("deadbeef", expect.any(Function));
    expect(res.writeHead).toHaveBeenCalledWith(200, expect.any(Object));
    expect(JSON.parse(res.body)).toMatchObject({
      message: "Successfully added the transaction to the queue",
      id: "42-0",
    });
  });

  it("returns 500 when xadd yields an error", () => {
    const req = makeReq();
    const res = makeRes();
    const xadd = makeXadd(null, new Error("redis down"));
    const { onEnqueued, onRejected } = makeCallbacks();

    handleRawSubmitRequest(req, res, xadd, onEnqueued, onRejected);
    fireBody(req, "ff");

    expect(res.writeHead).toHaveBeenCalledWith(500, expect.any(Object));
    expect(JSON.parse(res.body)).toEqual({
      error: "Failed to enqueue transaction",
    });
  });

  it("returns 500 when xadd returns null id without error", () => {
    const req = makeReq();
    const res = makeRes();
    const xadd = makeXadd(null);
    const { onEnqueued, onRejected } = makeCallbacks();

    handleRawSubmitRequest(req, res, xadd, onEnqueued, onRejected);
    fireBody(req, "ff");

    expect(res.writeHead).toHaveBeenCalledWith(500, expect.any(Object));
  });

  it("returns JSON Content-Type on success", () => {
    const req = makeReq();
    const res = makeRes();
    const xadd = makeXadd("1-0");
    const { onEnqueued, onRejected } = makeCallbacks();

    handleRawSubmitRequest(req, res, xadd, onEnqueued, onRejected);
    fireBody(req, "aa");

    expect(res.writeHead).toHaveBeenCalledWith(200, {
      "Content-Type": "application/json",
    });
  });

  it("calls onRejected for non-hex body and not onEnqueued", () => {
    const req = makeReq();
    const res = makeRes();
    const xadd = makeXadd("1-0");
    const { onEnqueued, onRejected } = makeCallbacks();

    handleRawSubmitRequest(req, res, xadd, onEnqueued, onRejected);
    fireBody(req, "not-hex!");

    expect(onRejected).toHaveBeenCalledOnce();
    expect(onEnqueued).not.toHaveBeenCalled();
  });

  it("calls onRejected for empty body and not onEnqueued", () => {
    const req = makeReq();
    const res = makeRes();
    const xadd = makeXadd("1-0");
    const { onEnqueued, onRejected } = makeCallbacks();

    handleRawSubmitRequest(req, res, xadd, onEnqueued, onRejected);
    fireBody(req, "");

    expect(onRejected).toHaveBeenCalledOnce();
    expect(onEnqueued).not.toHaveBeenCalled();
  });

  it("calls onEnqueued on successful xadd and not onRejected", () => {
    const req = makeReq();
    const res = makeRes();
    const xadd = makeXadd("5-0");
    const { onEnqueued, onRejected } = makeCallbacks();

    handleRawSubmitRequest(req, res, xadd, onEnqueued, onRejected);
    fireBody(req, "deadbeef");

    expect(onEnqueued).toHaveBeenCalledOnce();
    expect(onRejected).not.toHaveBeenCalled();
  });

  it("calls neither callback when xadd returns an error", () => {
    const req = makeReq();
    const res = makeRes();
    const xadd = makeXadd(null, new Error("redis down"));
    const { onEnqueued, onRejected } = makeCallbacks();

    handleRawSubmitRequest(req, res, xadd, onEnqueued, onRejected);
    fireBody(req, "ff");

    expect(onEnqueued).not.toHaveBeenCalled();
    expect(onRejected).not.toHaveBeenCalled();
  });
});

// --- attachToServer emit interception ---

describe("attachToServer emit interception", () => {
  it("intercepts POST /submit and does not call through to registered listeners", () => {
    // Simulate the interception closure directly (without real Redis):
    // create a fake server, install the same pattern used by attachToServer,
    // and verify POST /submit is swallowed while other events pass through.
    const fakeServer = new EventEmitter() as unknown as Server;
    const requestListener = vi.fn();
    const closeListener = vi.fn();
    fakeServer.on("request", requestListener);
    fakeServer.on("close", closeListener);

    const originalEmit = fakeServer.emit.bind(fakeServer);
    const xadd = makeXadd("1-0");
    const { onEnqueued, onRejected } = makeCallbacks();

    const interceptedEmit = (event: string, ...rest: unknown[]): boolean => {
      if (event === "request") {
        const req = rest[0] as IncomingMessage;
        const res = rest[1] as ServerResponse;
        if (req.method === "POST" && req.url === "/submit") {
          handleRawSubmitRequest(req, res, xadd, onEnqueued, onRejected);
          return true;
        }
      }
      return Reflect.apply(originalEmit, fakeServer, [
        event,
        ...rest,
      ]) as boolean;
    };

    fakeServer.emit = interceptedEmit as unknown as typeof fakeServer.emit;

    // POST /submit — intercepted, listener must NOT fire
    const submitReq = makeReq("POST", "/submit");
    const submitRes = makeRes();
    fakeServer.emit("request", submitReq, submitRes);
    expect(requestListener).not.toHaveBeenCalled();
    // body fires to complete the handler
    fireBody(submitReq, "aa");
    expect(xadd).toHaveBeenCalled();
    expect(onEnqueued).toHaveBeenCalledOnce();

    // GET / — passes through to the Effect request listener
    const getReq = makeReq("GET", "/");
    fakeServer.emit("request", getReq, submitRes);
    expect(requestListener).toHaveBeenCalledWith(getReq, submitRes);

    // Non-request event passes through
    fakeServer.emit("close");
    expect(closeListener).toHaveBeenCalled();
  });
});
