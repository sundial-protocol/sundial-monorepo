#!/usr/bin/env node

import { Network } from "@lucid-evolution/lucid";
import { MidgardNodeClient } from "../lib/client/node-client.js";
import { startGenerator } from "../lib/scheduler/scheduler.js";
import { generateTestWallet } from "../utils/test-utils.js";

async function main() {
  try {
    const { privateKey, testUTxO } = await generateTestWallet();

    // Initialize node client
    const nodeEndpoint =
      process.env.MIDGARD_NODE_URL || "http://localhost:3000";

    // Parse transaction configuration
    const batchSize = parseInt(process.env.BATCH_SIZE || "10", 10);
    const interval = parseInt(process.env.INTERVAL_SECONDS || "5", 10);
    const concurrency = parseInt(process.env.CONCURRENCY || "5", 10);
    const transactionType = process.env.TX_TYPE || "mixed";
    const oneToOneRatio = parseInt(process.env.ONE_TO_ONE_RATIO || "70", 10);

    // Start the generator
    await startGenerator({
      nodeEndpoint,
      walletPrivateKey: privateKey,
      batchSize,
      interval,
      concurrency,
      transactionType: transactionType as
        | "one-to-one"
        | "multi-output"
        | "mixed",
      oneToOneRatio,
    });

    console.log(`Transaction generator started with configuration:
 - Type: ${transactionType}${
      transactionType === "mixed"
        ? ` (${oneToOneRatio}% one-to-one, ${
            100 - oneToOneRatio
          }% multi-output)`
        : ""
    }
 - Batch Size: ${batchSize}
 - Interval: ${interval} seconds
 - Concurrency: ${concurrency}
 - Node Endpoint: ${nodeEndpoint}
 
Use Ctrl+C to stop the generator`);

    // Keep process running
    process.on("SIGINT", () => {
      console.log("\nStopping generator...");
      process.exit(0);
    });
  } catch (error) {
    console.error("Failed to start generator:", error);
    process.exit(1);
  }
}

main();
