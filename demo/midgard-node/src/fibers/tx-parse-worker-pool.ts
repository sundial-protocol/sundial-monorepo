import { Worker } from "node:worker_threads";
import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { fromHex } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import type { ProcessedTx } from "@/utils.js";
import { breakDownTx } from "@/utils.js";
import { WorkerError } from "@/workers/utils/common.js";
import {
  ParseTxRequestMessage,
  ParseTxResponseMessage,
  TxParseWorkerMessageType,
  deserializeProcessedTx,
} from "@/workers/utils/tx-parse.js";

const TX_PARSE_WORKER_NAME = "tx-parse";
const CURRENT_MODULE_DIR =
  typeof __filename !== "undefined"
    ? dirname(__filename)
    : dirname(fileURLToPath(import.meta.url));
const TX_PARSE_WORKER_URL = pathToFileURL(
  resolve(CURRENT_MODULE_DIR, "./tx-parse.js"),
);
const TX_PARSE_WORKER_FILE = fileURLToPath(TX_PARSE_WORKER_URL);

type PendingRequest = {
  complete: (
    effect: Effect.Effect<
      ProcessedTx,
      SDK.CmlDeserializationError | WorkerError
    >,
  ) => void;
};

type WorkerSlot = {
  worker: Worker;
  pending: Map<number, PendingRequest>;
  // Set before a deliberate `.terminate()` (pool reset/shutdown) so the
  // "exit" handler below can tell that apart from a crash and skip
  // respawning into a pool that's being torn down anyway.
  terminated: boolean;
};

type TxParseWorkerPoolState = {
  workers: WorkerSlot[];
  nextRequestId: number;
  nextWorkerIndex: number;
  size: number;
};

let txParseWorkerPoolState: TxParseWorkerPoolState | null = null;
const txParseWorkerPoolSemaphore = Effect.unsafeMakeSemaphore(1);
let shutdownHookRegistered = false;

const failAllPendingRequests = (
  workerSlot: WorkerSlot,
  cause: unknown,
  message: string,
) => {
  const pendingRequests = Array.from(workerSlot.pending.values());
  workerSlot.pending.clear();
  for (const pendingRequest of pendingRequests) {
    pendingRequest.complete(
      Effect.fail(
        new WorkerError({
          worker: TX_PARSE_WORKER_NAME,
          message,
          cause,
        }),
      ),
    );
  }
};

const spawnWorker = (): WorkerSlot => {
  const worker = new Worker(TX_PARSE_WORKER_URL);
  worker.unref();
  const workerSlot: WorkerSlot = {
    worker,
    pending: new Map(),
    terminated: false,
  };

  worker.on("message", (message: ParseTxResponseMessage) => {
    if (message.type !== TxParseWorkerMessageType.ParseTxResult) {
      return;
    }
    const pendingRequest = workerSlot.pending.get(message.requestId);
    if (pendingRequest === undefined) {
      return;
    }
    workerSlot.pending.delete(message.requestId);
    if (message.outcome === "failure") {
      pendingRequest.complete(
        Effect.fail(
          new SDK.CmlDeserializationError({
            message: `Failed to deserialize transaction: ${message.error}`,
            cause: message.error,
          }),
        ),
      );
      return;
    }
    pendingRequest.complete(
      Effect.succeed(deserializeProcessedTx(message.parsed)),
    );
  });

  worker.on("error", (error: Error) => {
    failAllPendingRequests(
      workerSlot,
      error,
      `tx parse worker crashed: ${error.message}`,
    );
  });

  worker.on("exit", (code: number) => {
    if (code !== 0) {
      failAllPendingRequests(
        workerSlot,
        `exit code ${code}`,
        `tx parse worker exited with code ${code}`,
      );
    } else {
      failAllPendingRequests(
        workerSlot,
        "exit code 0",
        "tx parse worker exited while request was in-flight",
      );
    }
    // A deliberate `.terminate()` (pool reset/shutdown) already marks the
    // slot as terminated, so only an unexpected crash reaches here. Replace
    // the dead slot in place so the pool doesn't permanently lose capacity
    // and future round-robin turns don't keep hitting a terminated worker.
    if (!workerSlot.terminated) {
      respawnWorkerSlot(workerSlot);
    }
  });

  return workerSlot;
};

const respawnWorkerSlot = (deadSlot: WorkerSlot) => {
  const state = txParseWorkerPoolState;
  if (state === null) {
    return;
  }
  const index = state.workers.indexOf(deadSlot);
  if (index === -1) {
    return;
  }
  state.workers[index] = spawnWorker();
};

const terminateWorkerPool = (state: TxParseWorkerPoolState | null) => {
  if (state === null) {
    return;
  }
  for (const workerSlot of state.workers) {
    workerSlot.terminated = true;
    failAllPendingRequests(
      workerSlot,
      "pool reset",
      "tx parse worker pool reset while request was in-flight",
    );
    void workerSlot.worker.terminate();
  }
};

const ensureShutdownHook = () => {
  if (shutdownHookRegistered) {
    return;
  }
  shutdownHookRegistered = true;
  process.once("exit", () => {
    terminateWorkerPool(txParseWorkerPoolState);
    txParseWorkerPoolState = null;
  });
};

const getOrSpawnWorkerPool = (size: number): TxParseWorkerPoolState => {
  if (txParseWorkerPoolState !== null && txParseWorkerPoolState.size === size) {
    return txParseWorkerPoolState;
  }
  terminateWorkerPool(txParseWorkerPoolState);
  txParseWorkerPoolState = {
    workers: Array.from({ length: size }, () => spawnWorker()),
    nextRequestId: 1,
    nextWorkerIndex: 0,
    size,
  };
  ensureShutdownHook();
  return txParseWorkerPoolState;
};

export const parseTxCborInWorkerPool = (
  txCborHex: string,
  workerPoolSize: number,
): Effect.Effect<ProcessedTx, SDK.CmlDeserializationError | WorkerError> =>
  existsSync(TX_PARSE_WORKER_FILE)
    ? txParseWorkerPoolSemaphore
        .withPermits(1)(
          Effect.sync(() => getOrSpawnWorkerPool(Math.max(1, workerPoolSize))),
        )
        .pipe(
          Effect.flatMap((state) =>
            Effect.async<
              ProcessedTx,
              SDK.CmlDeserializationError | WorkerError
            >((resume) => {
              const requestId = state.nextRequestId++;
              const workerIndex =
                state.nextWorkerIndex++ % state.workers.length;
              const workerSlot = state.workers[workerIndex];

              let done = false;
              const complete = (
                effect: Effect.Effect<
                  ProcessedTx,
                  SDK.CmlDeserializationError | WorkerError
                >,
              ) => {
                if (done) {
                  return;
                }
                done = true;
                workerSlot.pending.delete(requestId);
                resume(effect);
              };

              workerSlot.pending.set(requestId, { complete });
              const requestMessage: ParseTxRequestMessage = {
                type: TxParseWorkerMessageType.ParseTx,
                requestId,
                txCborHex,
              };
              workerSlot.worker.postMessage(requestMessage);

              return Effect.sync(() => {
                if (done) {
                  return;
                }
                workerSlot.pending.delete(requestId);
              });
            }),
          ),
        )
    : // fromHex() throws a plain synchronous Error on malformed input; run
      // it inside Effect.try so a bad payload surfaces as a normal Effect
      // failure instead of an uncaught exception on this path.
      Effect.try({
        try: () => fromHex(txCborHex),
        catch: (e) =>
          new SDK.CmlDeserializationError({
            message: `Failed to decode hex-encoded transaction: ${
              e instanceof Error ? e.message : String(e)
            }`,
            cause: e,
          }),
      }).pipe(Effect.flatMap(breakDownTx));

export const unsafeResetTxParseWorkerPoolForTesting = () => {
  terminateWorkerPool(txParseWorkerPoolState);
  txParseWorkerPoolState = null;
};
