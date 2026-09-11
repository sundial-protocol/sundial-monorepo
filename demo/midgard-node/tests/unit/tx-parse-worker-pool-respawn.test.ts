import { EventEmitter } from "node:events";
import { afterEach, describe, expect, it, vi } from "vitest";
import { Effect, Exit } from "effect";

// A crashed worker used to leave a permanently dead slot in the pool: the
// "exit" handler failed in-flight requests but never replaced the slot, so
// every future round-robin turn to that index silently posted into the void
// (see H-0x: a single malformed tx submission kills a tx-parse worker slot
// forever). These tests force the worker path (rather than the in-process
// fallback used elsewhere in this suite) with a fake Worker so the crash +
// respawn behavior can be exercised without spawning a real thread.

class FakeWorker extends EventEmitter {
  postMessage = vi.fn();
  unref = vi.fn();
  terminate = vi.fn(() => {
    this.emit("exit", 0);
    return Promise.resolve(0);
  });
}

const createdWorkers: FakeWorker[] = [];

vi.mock("node:worker_threads", async (importOriginal) => {
  const actual = await importOriginal<typeof import("node:worker_threads")>();
  return {
    ...actual,
    Worker: vi.fn().mockImplementation(function () {
      const worker = new FakeWorker();
      createdWorkers.push(worker);
      return worker;
    }),
  };
});

vi.mock("node:fs", async (importOriginal) => {
  const actual = await importOriginal<typeof import("node:fs")>();
  return { ...actual, existsSync: () => true };
});

const { parseTxCborInWorkerPool, unsafeResetTxParseWorkerPoolForTesting } =
  await import("@/fibers/tx-parse-worker-pool.js");
const { TxParseWorkerMessageType } =
  await import("@/workers/utils/tx-parse.js");

describe("tx-parse worker pool: crashed slot recovery", () => {
  afterEach(() => {
    unsafeResetTxParseWorkerPoolForTesting();
    createdWorkers.length = 0;
  });

  it("replaces a worker that crashes so a later request isn't routed to a dead slot", async () => {
    const firstRequest = Effect.runPromiseExit(
      parseTxCborInWorkerPool("deadbeef", 1),
    );
    await vi.waitFor(() => expect(createdWorkers).toHaveLength(1));
    const deadWorker = createdWorkers[0]!;
    await vi.waitFor(() => expect(deadWorker.postMessage).toHaveBeenCalled());

    // Simulate the worker thread crashing (e.g. on a malformed payload).
    deadWorker.emit("exit", 1);

    const firstExit = await firstRequest;
    expect(Exit.isFailure(firstExit)).toBe(true);

    // A follow-up request should reach a freshly spawned worker instead of
    // the terminated one.
    const secondRequest = Effect.runPromiseExit(
      parseTxCborInWorkerPool("deadbeef", 1),
    );
    await vi.waitFor(() => expect(createdWorkers).toHaveLength(2));
    const liveWorker = createdWorkers[1]!;
    expect(liveWorker).not.toBe(deadWorker);
    await vi.waitFor(() => expect(liveWorker.postMessage).toHaveBeenCalled());

    const requestMessage = liveWorker.postMessage.mock.calls.at(-1)![0] as {
      requestId: number;
    };
    liveWorker.emit("message", {
      type: TxParseWorkerMessageType.ParseTxResult,
      requestId: requestMessage.requestId,
      outcome: "success",
      parsed: {
        txIdHex: "aa",
        txCborHex: "bb",
        spentHex: [],
        produced: [],
      },
    });

    const secondExit = await secondRequest;
    expect(Exit.isSuccess(secondExit)).toBe(true);
  });

  it("does not spawn a replacement worker on a deliberate pool reset", async () => {
    const request = Effect.runPromiseExit(
      parseTxCborInWorkerPool("deadbeef", 1),
    );
    await vi.waitFor(() => expect(createdWorkers).toHaveLength(1));

    unsafeResetTxParseWorkerPoolForTesting();
    await request;

    // Only the original worker should exist; reset must not trigger the
    // crash-recovery respawn path.
    expect(createdWorkers).toHaveLength(1);
  });
});
