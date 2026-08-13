import { parentPort } from "node:worker_threads";
import { fromHex } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { breakDownTx } from "@/utils.js";
import {
  TxParseWorkerMessageType,
  serializeProcessedTx,
} from "@/workers/utils/tx-parse.js";
import type { ParseTxRequestMessage } from "@/workers/utils/tx-parse.js";

if (parentPort === null) {
  throw new Error("tx-parse worker started without parentPort");
}

const workerParentPort = parentPort;

workerParentPort.on("message", (message: ParseTxRequestMessage) => {
  if (message.type !== TxParseWorkerMessageType.ParseTx) {
    return;
  }

  // fromHex() throws a plain synchronous Error on malformed input; run it
  // outside Effect but inside try/catch so it can't escape as an uncaught
  // exception and take down this worker thread.
  let txCbor: Uint8Array;
  try {
    txCbor = fromHex(message.txCborHex);
  } catch (error) {
    workerParentPort.postMessage({
      type: TxParseWorkerMessageType.ParseTxResult,
      requestId: message.requestId,
      outcome: "failure",
      error: error instanceof Error ? error.message : String(error),
    });
    return;
  }

  void Effect.runPromise(
    breakDownTx(txCbor).pipe(
      Effect.map(serializeProcessedTx),
      Effect.match({
        onFailure: (error) =>
          workerParentPort.postMessage({
            type: TxParseWorkerMessageType.ParseTxResult,
            requestId: message.requestId,
            outcome: "failure",
            error: error.message,
          }),
        onSuccess: (parsed) =>
          workerParentPort.postMessage({
            type: TxParseWorkerMessageType.ParseTxResult,
            requestId: message.requestId,
            outcome: "success",
            parsed,
          }),
      }),
    ),
  );
});
