import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

interface WorkerInput {
  items: any[];
  itemsType: "txs" | "utxos";
}

interface WorkerOutput {
  root: string;
}

const wrapper = (input: WorkerInput): Effect.Effect<WorkerOutput, Error> =>
  Effect.gen(function* () {
    const trieProgram = (() => {
      if (input.itemsType === "txs") {
        return SDK.Utils.mptFromTxs(input.items);
      } else {
        return SDK.Utils.mptFromUTxOs(input.items);
      }
    })();
    return yield* Effect.map(
      trieProgram,
      (t: Trie) => ({ root: t.hash.toString("hex") }) as WorkerOutput
    );
  });

if (parentPort === null) {
  throw new Error("MPT computation must be run as a worker");
}

const inputData = workerData as WorkerInput;

Effect.runPromise(
  wrapper(inputData).pipe(
    Effect.catchAll((e) =>
      Effect.succeed({ error: e instanceof Error ? e.message : "Unknown error from MPT worker" })
    )
  )
).then((output) => parentPort?.postMessage(output));
