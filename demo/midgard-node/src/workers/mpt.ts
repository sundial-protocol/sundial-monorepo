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

const wrapper = (input: WorkerInput): Promise<WorkerOutput> => {
  const trieProgram = (() => {
    if (input.itemsType === "txs") {
      return SDK.Utils.mptFromTxs(input.items);
    } else {
      return SDK.Utils.mptFromUTxOs(input.items);
    }
  })()
  const outputProgram = Effect.map(trieProgram, (t: Trie) => ({root: t.hash.toString("hex")} as WorkerOutput));
  return Effect.runPromise(outputProgram);
};

if (parentPort === null) {
  throw new Error("MPT computation must be run as a worker");
} else {
  const inputData = workerData as WorkerInput;
  
  (async () => {
    try {
      const output = await wrapper(inputData);
      parentPort?.postMessage(output);
    } catch (e) {
      parentPort?.postMessage({error: e instanceof Error ? e.message : "Unknown error from MPT worker"});
    }
  })
}
