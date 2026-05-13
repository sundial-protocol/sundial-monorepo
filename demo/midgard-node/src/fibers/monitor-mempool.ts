import { Effect, Metric, pipe, Schedule } from "effect";
import { MempoolDB } from "@/database/index.js";
import { SqlClient } from "@effect/sql/SqlClient";
import { DatabaseError } from "@/database/utils/common.js";

const mempoolTxGauge = Metric.gauge("mempool_tx_count", {
  description:
    "A gauge for tracking the current number of transactions in the mempool",
  bigint: true,
}).register();

const monitorMempoolAction: Effect.Effect<void, DatabaseError, SqlClient> =
  Effect.gen(function* () {
    const numTx = yield* MempoolDB.retrieveTxCount;
    yield* Metric.set(mempoolTxGauge, numTx);
  });

export const monitorMempoolFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, SqlClient> =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🟢 Mempool monitor fiber started.");
      // Initialize with zero so dashboards don't start with missing series.
      yield* Metric.set(mempoolTxGauge, 0n);
      yield* Effect.repeat(
        monitorMempoolAction.pipe(Effect.catchAllCause(Effect.logWarning)),
        schedule,
      );
    }),
  );
