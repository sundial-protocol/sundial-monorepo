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

  void Effect.runPromise(
    breakDownTx(fromHex(message.txCborHex)).pipe(
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
