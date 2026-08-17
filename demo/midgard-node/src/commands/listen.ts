import {
  Database,
  NodeConfig,
  Lucid,
  AlwaysSucceedsContract,
  Globals,
  TxIngressQueue,
  TxIngressQueueService,
  Faucet,
} from "@/services/index.js";
import * as SDK from "@al-ft/midgard-sdk";
import { NodeSdk } from "@effect/opentelemetry";
import {
  TxSubmitError,
  fromHex,
  getAddressDetails,
  toHex,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import { BatchSpanProcessor } from "@opentelemetry/sdk-trace-base";
import {
  Cause,
  Duration,
  Effect,
  Layer,
  Metric,
  pipe,
  Ref,
  Schedule,
} from "effect";
import {
  AddressHistoryDB,
  BlocksTxsDB,
  HealthDB,
  ImmutableDB,
  DBInitialization,
  MempoolDB,
  MempoolLedgerDB,
} from "@/database/index.js";
import { isHexString } from "@/utils.js";
import { createRawSubmitInterceptor } from "@/services/raw-submit-interceptor.js";
import {
  HttpRouter,
  HttpServer,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import { createServer } from "node:http";
import { constants as Http2Constants } from "node:http2";
import { NodeHttpServer } from "@effect/platform-node";
import { HttpBodyError } from "@effect/platform/HttpBody";
import * as Genesis from "@/genesis.js";
import * as Initialization from "@/transactions/initialization.js";
import * as Reset from "@/reset.js";
import { DatabaseError } from "@/database/utils/common.js";
import { TxConfirmError, TxSignError } from "@/transactions/utils.js";
import {
  syncUserEventsFiber,
  blockCommitmentFiber,
  blockCommitmentAction,
  mergeFiber,
  mergeAction,
  monitorMempoolFiber,
  blockSubmissionFiber,
  txQueueProcessorFiber,
  txQueueMetricsRefreshFiber,
} from "@/fibers/index.js";

const TX_ENDPOINT: string = "tx";
const ADDRESS_HISTORY_ENDPOINT: string = "txs";
const MERGE_ENDPOINT: string = "merge";
const UTXOS_ENDPOINT: string = "utxos";
const BLOCK_ENDPOINT: string = "block";
const INIT_ENDPOINT: string = "init";
const COMMIT_ENDPOINT: string = "commit";
const RESET_ENDPOINT: string = "reset";
const SUBMIT_ENDPOINT: string = "submit";
const FAUCET_CLAIMS_ENDPOINT: string = "faucet/claims";
const STATE_QUEUE_ENDPOINT: string = "stateQueue";
const STATE_QUEUE_ROOT_UNIT_DIAGNOSTICS_ENDPOINT: string =
  "stateQueue/root-unit-diagnostics";
const STATE_QUEUE_REPAIR_ROOT_UNITS_ENDPOINT: string =
  "stateQueue/repair-root-units";
const COMMITMENT_WALLET_BALANCE_ENDPOINT: string = "commitment-wallet/balance";
const HEALTH_LIVE_ENDPOINT: string = "health/live";
const HEALTH_READY_ENDPOINT: string = "health/ready";
const COMMITMENT_WALLET_BALANCE_QUERY_TIMEOUT_MS = 4_000;
const txAcceptedCounter = Metric.counter("tx_submissions_enqueued", {
  description:
    "A counter for tracking L2 transaction submissions that passed hex validation and were enqueued to the durable ingress stream",
  bigint: true,
  incremental: true,
}).register();

const txRejectedCounter = Metric.counter("tx_submissions_rejected", {
  description:
    "A counter for tracking L2 transaction submissions rejected due to malformed or non-hex CBOR",
  bigint: true,
  incremental: true,
}).register();

const failWith500Helper = (
  logLabel: string,
  logMsg: string,
  error: any,
  msgOverride?: string,
) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${logLabel} - ${logMsg}: ${error}`);
    return yield* HttpServerResponse.json(
      { error: msgOverride ?? "Something went wrong" },
      { status: 500 },
    );
  });

const failWith500 = (
  method: "GET" | "POST",
  endpoint: string,
  error: HttpBodyError | string | any,
  msgOverride?: string,
) => failWith500Helper(`${method} /${endpoint}`, "failure", error, msgOverride);

const handleDBGetFailure = (endpoint: string, e: DatabaseError) =>
  failWith500("GET", endpoint, e.cause, `db failure with table ${e.table}`);

const handleTxGetFailure = (
  endpoint: string,
  e: TxSignError | TxConfirmError | TxSubmitError,
) => failWith500("GET", endpoint, e.cause, `${e._tag}: ${e.message}`);

const handleGenericGetFailure = (endpoint: string, e: SDK.GenericErrorFields) =>
  failWith500("GET", endpoint, e.cause, e.message);

const getHealthLiveHandler = HttpServerResponse.json({ status: "ok" });

type ReadinessSubsystem = "database" | "redis" | "l1Provider";

const HEALTH_READY_CHECK_TIMEOUT_MS = 3_000;

// Runs a subsystem probe with a bounded timeout, collapsing any failure
// (probe error or timeout) to the subsystem's name rather than propagating it,
// so one slow/unreachable dependency can't hang the whole readiness check.
const checkReadinessSubsystem = <E, R>(
  name: ReadinessSubsystem,
  probe: Effect.Effect<void, E, R>,
): Effect.Effect<ReadinessSubsystem | null, never, R> =>
  probe.pipe(
    Effect.timeout(`${HEALTH_READY_CHECK_TIMEOUT_MS} millis`),
    Effect.as(null as ReadinessSubsystem | null),
    Effect.catchAll(() => Effect.succeed(name)),
  );

const getHealthReadyHandler = Effect.gen(function* () {
  const txIngressQueue = yield* TxIngressQueue;
  const lucid = yield* Lucid;

  const failing = yield* Effect.all(
    [
      checkReadinessSubsystem("database", HealthDB.checkReady),
      checkReadinessSubsystem("redis", txIngressQueue.ping),
      checkReadinessSubsystem("l1Provider", lucid.checkReady),
    ],
    { concurrency: "unbounded" },
  ).pipe(
    Effect.map((results) =>
      results.filter((r): r is ReadinessSubsystem => r !== null),
    ),
  );

  if (failing.length > 0) {
    yield* Effect.logError(
      `GET /${HEALTH_READY_ENDPOINT} - readiness failure: ${failing.join(", ")}`,
    );
    return yield* HttpServerResponse.json(
      { status: "not_ready", failing },
      { status: Http2Constants.HTTP_STATUS_SERVICE_UNAVAILABLE },
    );
  }

  return yield* HttpServerResponse.json({ status: "ready" });
});

const lookupTxCbor = (txHashBytes: Buffer, txHashParam: string) =>
  MempoolDB.retrieveTxCborByHash(txHashBytes).pipe(
    Effect.tap(() =>
      Effect.logInfo(
        `GET /${TX_ENDPOINT} - Transaction found in mempool: ${txHashParam}`,
      ),
    ),
    Effect.catchTag("NotFoundError", () =>
      ImmutableDB.retrieveTxCborByHash(txHashBytes).pipe(
        Effect.tap(() =>
          Effect.logInfo(
            `GET /${TX_ENDPOINT} - Transaction found in ImmutableDB: ${txHashParam}`,
          ),
        ),
      ),
    ),
  );

const getTxHandler = Effect.gen(function* () {
  const params = yield* HttpServerRequest.ParsedSearchParams;
  const txHashParam = params["tx_hash"];
  if (
    typeof txHashParam !== "string" ||
    !isHexString(txHashParam) ||
    txHashParam.length !== 64
  ) {
    yield* Effect.logInfo(
      `GET /${TX_ENDPOINT} - Invalid transaction hash: ${txHashParam}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: ${txHashParam}` },
      { status: 400 },
    );
  }

  const txHashBytes = Buffer.from(fromHex(txHashParam));
  return yield* lookupTxCbor(txHashBytes, txHashParam).pipe(
    Effect.tap((foundCbor) =>
      Effect.logInfo("foundCbor", SDK.bufferToHex(foundCbor)),
    ),
    Effect.flatMap((foundCbor) =>
      HttpServerResponse.json({ tx: SDK.bufferToHex(foundCbor) }),
    ),
    Effect.catchTag("NotFoundError", () =>
      HttpServerResponse.json(
        { error: `Transaction not found: ${txHashParam}` },
        { status: 404 },
      ),
    ),
  );
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", TX_ENDPOINT, e)),
  Effect.catchTag("DatabaseError", (e) => handleDBGetFailure(TX_ENDPOINT, e)),
);

const getUtxosHandler = Effect.gen(function* () {
  const params = yield* HttpServerRequest.ParsedSearchParams;
  const addr = params["address"];

  if (typeof addr !== "string") {
    yield* Effect.logInfo(
      `GET /${UTXOS_ENDPOINT} - Invalid address type: ${addr}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid address type: ${addr}` },
      { status: 400 },
    );
  }
  try {
    const addrDetails = getAddressDetails(addr);
    if (!addrDetails.paymentCredential) {
      yield* Effect.logInfo(`Invalid address format: ${addr}`);
      return yield* HttpServerResponse.json(
        { error: `Invalid address format: ${addr}` },
        { status: 400 },
      );
    }

    const utxosWithAddress = yield* MempoolLedgerDB.retrieveByAddress(
      addrDetails.address.bech32,
    );

    const response = utxosWithAddress.map((entry) => ({
      outref: SDK.bufferToHex(entry.outref),
      value: SDK.bufferToHex(entry.output),
    }));

    yield* Effect.logInfo(`Found ${response.length} UTxOs for ${addr}`);
    return yield* HttpServerResponse.json({
      utxos: response,
    });
  } catch (error) {
    yield* Effect.logInfo(`Invalid address: ${addr}`);
    return yield* HttpServerResponse.json(
      { error: `Invalid address: ${addr}` },
      { status: 400 },
    );
  }
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", UTXOS_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    handleDBGetFailure(UTXOS_ENDPOINT, e),
  ),
);

const getBlockHandler = Effect.gen(function* () {
  const params = yield* HttpServerRequest.ParsedSearchParams;
  const hdrHash = params["header_hash"];
  yield* Effect.logInfo(
    `GET /block - Request received for header_hash: ${hdrHash}`,
  );

  if (
    typeof hdrHash !== "string" ||
    !isHexString(hdrHash) ||
    hdrHash.length !== 56
  ) {
    yield* Effect.logInfo(
      `GET /${BLOCK_ENDPOINT} - Invalid block hash: ${hdrHash}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid block hash: ${hdrHash}` },
      { status: 400 },
    );
  }
  const hashes = yield* BlocksTxsDB.retrieveTxHashesByHeaderHash(
    Buffer.from(fromHex(hdrHash)),
  );
  yield* Effect.logInfo(
    `GET /${BLOCK_ENDPOINT} - Found ${hashes.length} txs for block: ${hdrHash}`,
  );
  return yield* HttpServerResponse.json({
    hashes: hashes.map(SDK.bufferToHex),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", BLOCK_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    handleDBGetFailure(BLOCK_ENDPOINT, e),
  ),
);

const getInitHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✨ Initialization request received`);
  const globals = yield* Globals;
  const clearResetInProgress = Ref.set(globals.RESET_IN_PROGRESS, false).pipe(
    Effect.catchAllCause((cause) =>
      Effect.logWarning(
        `🚧 Failed to clear RESET_IN_PROGRESS after init attempt. cause=${String(cause)}`,
      ),
    ),
  );
  const lockAcquired = yield* Ref.modify(
    globals.RESET_IN_PROGRESS,
    (inProgress): [boolean, boolean] =>
      inProgress ? [false, true] : [true, true],
  );

  if (!lockAcquired) {
    yield* Effect.logWarning(
      `GET /${INIT_ENDPOINT} - initialization rejected because another reset/repair/init action is in progress`,
    );
    return yield* HttpServerResponse.json(
      { error: "Reset already in progress" },
      { status: Http2Constants.HTTP_STATUS_CONFLICT },
    );
  }

  return yield* Effect.gen(function* () {
    const lucid = yield* Lucid;
    const alwaysSucceeds = yield* AlwaysSucceedsContract;
    const stateQueueAddress = alwaysSucceeds.stateQueue.spendingScriptAddress;
    const rootUnit = alwaysSucceeds.stateQueue.policyId + SDK.NODE_ASSET_NAME;
    const rootUnitUtxos = yield* Effect.tryPromise({
      try: () => lucid.api.utxosAtWithUnit(stateQueueAddress, rootUnit),
      catch: (e) =>
        new SDK.LucidError({
          message:
            "Failed to query state-queue root unit before initialization",
          cause: e,
        }),
    });

    if (rootUnitUtxos.length > 0) {
      const outRefs = rootUnitUtxos.map((u) => `${u.txHash}#${u.outputIndex}`);
      yield* Effect.logWarning(
        `GET /${INIT_ENDPOINT} - initialization blocked: state queue already has ${rootUnitUtxos.length} root unit UTxO(s). root_unit=${rootUnit} outrefs=${outRefs.join(",")}`,
      );
      return yield* HttpServerResponse.json(
        {
          error:
            "State queue already initialized (or dirty). Refusing to mint another root unit.",
          rootUnit,
          stateQueueAddress,
          count: rootUnitUtxos.length,
          outRefs,
        },
        { status: Http2Constants.HTTP_STATUS_CONFLICT },
      );
    }

    const txHash = yield* Initialization.program;
    yield* Genesis.program;
    yield* Effect.logInfo(
      `GET /${INIT_ENDPOINT} - Initialization successful: ${txHash}`,
    );
    return yield* HttpServerResponse.json({
      message: `Initiation successful: ${txHash}`,
    });
  }).pipe(Effect.ensuring(clearResetInProgress));
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", INIT_ENDPOINT, e)),
  Effect.catchTag("LucidError", (e) =>
    handleGenericGetFailure(INIT_ENDPOINT, e),
  ),
  Effect.catchTag("MptError", (e) => handleGenericGetFailure(INIT_ENDPOINT, e)),
  Effect.catchTag("TxSubmitError", (e) => handleTxGetFailure(INIT_ENDPOINT, e)),
  Effect.catchTag("TxSignError", (e) => handleTxGetFailure(INIT_ENDPOINT, e)),
  Effect.catchTag("UnspecifiedNetworkError", (e) =>
    handleGenericGetFailure(INIT_ENDPOINT, e),
  ),
);

const getCommitEndpoint = Effect.gen(function* () {
  yield* Effect.logInfo(
    `GET /${COMMIT_ENDPOINT} - Manual block commitment order received`,
  );
  const result = yield* blockCommitmentAction;
  yield* Effect.logInfo(
    `GET /${COMMIT_ENDPOINT} - Block commitment successful: ${result}`,
  );
  return yield* HttpServerResponse.json({
    message: `Block commitment successful: ${result}`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", COMMIT_ENDPOINT, e),
  ),
  Effect.catchTag("WorkerError", (e) =>
    failWith500("GET", COMMIT_ENDPOINT, e.cause, "failed worker"),
  ),
);

const getMergeHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`GET /${MERGE_ENDPOINT} - Manual merge order received`);
  const result = yield* mergeAction;
  yield* Effect.logInfo(
    `GET /${MERGE_ENDPOINT} - Merging confirmed state successful: ${result}`,
  );
  return yield* HttpServerResponse.json({
    message: `Merging confirmed state successful: ${result}`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    handleDBGetFailure(MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("TxSubmitError", (e) =>
    handleTxGetFailure(MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("TxSignError", (e) => handleTxGetFailure(MERGE_ENDPOINT, e)),
  Effect.catchTag("CmlDeserializationError", (e) =>
    handleGenericGetFailure(MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("DataCoercionError", (e) =>
    handleGenericGetFailure(MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("HashingError", (e) =>
    handleGenericGetFailure(MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("LucidError", (e) =>
    handleGenericGetFailure(MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("StateQueueError", (e) =>
    handleGenericGetFailure(MERGE_ENDPOINT, e),
  ),
);

const createLockedActionHandler = <R>(
  endpoint: string,
  successMessage: string,
  actionProgram: Effect.Effect<void, any, R>,
) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`GET /${endpoint} - request received`);
    const globals = yield* Globals;
    const lockAcquired = yield* Ref.modify(
      globals.RESET_IN_PROGRESS,
      (inProgress): [boolean, boolean] =>
        inProgress ? [false, true] : [true, true],
    );

    if (!lockAcquired) {
      yield* Effect.logWarning(
        `GET /${endpoint} - action already in progress; rejecting concurrent request`,
      );
      return yield* HttpServerResponse.json(
        { error: "Reset already in progress" },
        { status: Http2Constants.HTTP_STATUS_CONFLICT },
      );
    }

    yield* actionProgram;

    return yield* HttpServerResponse.json({
      message: successMessage,
    });
  }).pipe(
    Effect.catchTag("HttpBodyError", (e) => failWith500("GET", endpoint, e)),
    Effect.catchTag("DatabaseError", (e) => handleDBGetFailure(endpoint, e)),
    Effect.catchTag("TxSubmitError", (e) => handleTxGetFailure(endpoint, e)),
    Effect.catchTag("TxSignError", (e) => handleTxGetFailure(endpoint, e)),
    Effect.catchTag("TxConfirmError", (e) => handleTxGetFailure(endpoint, e)),
    Effect.catchTag("LucidError", (e) => handleGenericGetFailure(endpoint, e)),
  );

const createResetHandler = <R>(resetProgram: Effect.Effect<void, any, R>) =>
  createLockedActionHandler(
    RESET_ENDPOINT,
    "Collected all UTxOs successfully!",
    resetProgram,
  );

const createStateQueueRootUnitRepairHandler = <R>(
  repairProgram: Effect.Effect<void, any, R>,
) =>
  createLockedActionHandler(
    STATE_QUEUE_REPAIR_ROOT_UNITS_ENDPOINT,
    "State-queue root-unit repair completed successfully!",
    repairProgram,
  );

const stateQueueRootUnitDiagnosticsSnapshot = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const alwaysSucceeds = yield* AlwaysSucceedsContract;
  const globals = yield* Globals;
  const stateQueueAddress = alwaysSucceeds.stateQueue.spendingScriptAddress;
  const rootUnit = alwaysSucceeds.stateQueue.policyId + SDK.NODE_ASSET_NAME;
  const resetInProgress = yield* Ref.get(globals.RESET_IN_PROGRESS);
  const utxos = yield* Effect.tryPromise({
    try: () => lucid.api.utxosAtWithUnit(stateQueueAddress, rootUnit),
    catch: (e) =>
      new Error(
        `Failed to query state-queue root unit UTxOs: ${e instanceof Error ? e.message : String(e)}`,
      ),
  });
  const outRefs = utxos.map((u) => `${u.txHash}#${u.outputIndex}`);

  return {
    status: (utxos.length === 1 ? "ok" : "invalid") as "ok" | "invalid",
    resetInProgress,
    stateQueueAddress,
    rootUnit,
    count: utxos.length,
    outRefs,
  };
});

const createStateQueueRootUnitDiagnosticsHandler = <E, R>(
  snapshotProgram: Effect.Effect<
    {
      status: "ok" | "invalid";
      resetInProgress: boolean;
      stateQueueAddress: string;
      rootUnit: string;
      count: number;
      outRefs: string[];
    },
    E,
    R
  >,
) =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotProgram;
    return yield* HttpServerResponse.json(snapshot);
  }).pipe(
    Effect.catchAll((e) =>
      Effect.gen(function* () {
        const cause = e instanceof Error ? e.message : String(e);
        yield* Effect.logWarning(
          `GET /${STATE_QUEUE_ROOT_UNIT_DIAGNOSTICS_ENDPOINT} - diagnostics query failed: ${cause}`,
        );
        return yield* HttpServerResponse.json(
          {
            status: "error",
            error: "Failed to query state-queue root-unit diagnostics",
            cause,
          },
          { status: Http2Constants.HTTP_STATUS_SERVICE_UNAVAILABLE },
        );
      }),
    ),
  );

const getResetHandler = createResetHandler(Reset.program);
const getStateQueueRootUnitRepairHandler =
  createStateQueueRootUnitRepairHandler(Reset.repairStateQueueRootUnitsProgram);
const getStateQueueRootUnitDiagnosticsHandler =
  createStateQueueRootUnitDiagnosticsHandler(
    stateQueueRootUnitDiagnosticsSnapshot,
  );

const NON_NEGATIVE_INTEGER_RE = /^\d+$/;

const getTxsOfAddressHandler = Effect.gen(function* () {
  const params = yield* HttpServerRequest.ParsedSearchParams;
  const addr = params["address"];
  const limitParam = params["limit"];
  const offsetParam = params["offset"];

  if (typeof addr !== "string") {
    yield* Effect.logInfo(
      `GET /${ADDRESS_HISTORY_ENDPOINT} - Invalid address type: ${addr}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid address type: ${addr}` },
      { status: 400 },
    );
  }

  if (
    limitParam !== undefined &&
    (typeof limitParam !== "string" ||
      !NON_NEGATIVE_INTEGER_RE.test(limitParam) ||
      Number(limitParam) < 1)
  ) {
    yield* Effect.logInfo(
      `GET /${ADDRESS_HISTORY_ENDPOINT} - Invalid limit: ${limitParam}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid limit: ${limitParam}` },
      { status: 400 },
    );
  }

  if (
    offsetParam !== undefined &&
    (typeof offsetParam !== "string" ||
      !NON_NEGATIVE_INTEGER_RE.test(offsetParam))
  ) {
    yield* Effect.logInfo(
      `GET /${ADDRESS_HISTORY_ENDPOINT} - Invalid offset: ${offsetParam}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid offset: ${offsetParam}` },
      { status: 400 },
    );
  }

  const limit = Math.min(
    limitParam !== undefined
      ? Number(limitParam)
      : AddressHistoryDB.DEFAULT_ADDRESS_HISTORY_LIMIT,
    AddressHistoryDB.MAX_ADDRESS_HISTORY_LIMIT,
  );
  const offset = offsetParam !== undefined ? Number(offsetParam) : 0;

  try {
    const addrDetails = getAddressDetails(addr);
    if (!addrDetails.paymentCredential) {
      yield* Effect.logInfo(`Invalid address format: ${addr}`);
      return yield* HttpServerResponse.json(
        { error: `Invalid address format: ${addr}` },
        { status: 400 },
      );
    }

    const cbors = yield* AddressHistoryDB.retrieve(addrDetails.address.bech32, {
      limit,
      offset,
    });
    yield* Effect.logInfo(
      `Found ${cbors.length} CBORs with ${addr} (limit=${limit}, offset=${offset})`,
    );
    return yield* HttpServerResponse.json({
      txs: cbors.map(SDK.bufferToHex),
      limit,
      offset,
      hasMore: cbors.length === limit,
    });
  } catch (error) {
    yield* Effect.logInfo(`Invalid address: ${addr}`);
    return yield* HttpServerResponse.json(
      { error: `Invalid address: ${addr}` },
      { status: 400 },
    );
  }
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "txs", e)),
  Effect.catchTag("DatabaseError", (e) =>
    handleDBGetFailure(ADDRESS_HISTORY_ENDPOINT, e),
  ),
);

const getStateQueueHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Drawing state queue UTxOs...`);
  const lucid = yield* Lucid;
  const alwaysSucceeds = yield* AlwaysSucceedsContract;
  const fetchConfig: SDK.StateQueueFetchConfig = {
    stateQueuePolicyId: alwaysSucceeds.stateQueue.policyId,
    stateQueueAddress: alwaysSucceeds.stateQueue.spendingScriptAddress,
  };
  const sortedUTxOs = yield* SDK.fetchSortedStateQueueUTxOsProgram(
    lucid.api,
    fetchConfig,
  );
  const headers = sortedUTxOs.flatMap((u) =>
    u.datum.key === "Empty" ? [] : [u.datum.key.Key.key],
  );
  let drawn = `
---------------------------- STATE QUEUE ----------------------------`;
  yield* Effect.allSuccesses(
    sortedUTxOs.map((u) =>
      Effect.gen(function* () {
        let info = "";
        const isHead = u.datum.key === "Empty";
        const isEnd = u.datum.next === "Empty";
        const emoji = isHead ? "🚢" : isEnd ? "⚓" : "⛓ ";
        if (u.datum.key !== "Empty") {
          // if (isHead) {
          const icon = isEnd ? "  " : emoji;
          info = `
${icon} ╰─ header: ${u.datum.key.Key.key}`;
        }
        drawn = `${drawn}
${emoji} ${u.utxo.txHash}#${u.utxo.outputIndex}${info}`;
      }),
    ),
  );
  drawn += `
---------------------------------------------------------------------
`;
  yield* Effect.logInfo(drawn);
  return yield* HttpServerResponse.json({
    headers,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", "logStateQueue", e),
  ),
  Effect.catchTag("LinkedListError", (e) =>
    handleGenericGetFailure("logStateQueue", e),
  ),
  Effect.catchTag("LucidError", (e) =>
    handleGenericGetFailure("logStateQueue", e),
  ),
);

const getLogBlocksTxsDBHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Querying BlocksTxsDB...`);
  const allBlocksData = yield* BlocksTxsDB.retrieve;
  const keyValues: Record<string, number> = allBlocksData.reduce(
    (acc: Record<string, number>, entry) => {
      const bHex = toHex(entry.header_hash);
      if (!acc[bHex]) {
        acc[bHex] = 1;
      } else {
        acc[bHex] += 1;
      }
      return acc;
    },
    {} as Record<string, number>,
  );
  let drawn = `
------------------------------ BLOCKS DB ----------------------------`;
  for (const bHex in keyValues) {
    drawn = `${drawn}
${bHex} -──▶ ${keyValues[bHex]} tx(s)`;
  }
  drawn += `
---------------------------------------------------------------------
`;
  yield* Effect.logInfo(drawn);
  return yield* HttpServerResponse.json({
    message: `BlocksTxsDB drawn in server logs!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", "logBlocksTxsDB", e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    handleDBGetFailure("logBlocksTxsDB", e),
  ),
);

const getLogGlobalsHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Logging global variables...`);
  const globals = yield* Globals;
  const BLOCKS_IN_QUEUE: number = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
  const LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH: number = yield* Ref.get(
    globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
  );
  const RESET_IN_PROGRESS: boolean = yield* Ref.get(globals.RESET_IN_PROGRESS);

  yield* Effect.logInfo(`
  BLOCKS_IN_QUEUE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${BLOCKS_IN_QUEUE}
  LATEST_SYNC ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${new Date(Number(LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH)).toLocaleString()}
  RESET_IN_PROGRESS ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${RESET_IN_PROGRESS}
`);
  return yield* HttpServerResponse.json({
    message: `Global variables logged!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "logGlobals", e)),
);

type SubmitIngressConfig = {
  readonly txIngressQueue: TxIngressQueueService;
};

const postSubmitHandler = (submitIngressConfig: SubmitIngressConfig) =>
  Effect.gen(function* () {
    const request = yield* HttpServerRequest.HttpServerRequest;
    const txString = yield* request.text;
    if (!isHexString(txString)) {
      yield* Effect.logInfo(`▫️ Invalid CBOR provided`);
      yield* Metric.increment(txRejectedCounter);
      return yield* HttpServerResponse.json(
        { error: `Invalid CBOR provided` },
        { status: 400 },
      );
    }
    const messageId =
      yield* submitIngressConfig.txIngressQueue.enqueue(txString);
    yield* Metric.increment(txAcceptedCounter);
    return yield* HttpServerResponse.json({
      message: `Successfully added the transaction to the queue`,
      id: messageId,
    });
  }).pipe(
    Effect.catchTag("HttpBodyError", (e) =>
      failWith500("POST", "submit", e, "▫️ L2 transaction failed"),
    ),
    Effect.catchTag("TxIngressQueueError", (e) =>
      failWith500(
        "POST",
        "submit",
        e.cause,
        "Failed to enqueue transaction into ingress stream",
      ),
    ),
  );

const FAUCET_CLAIM_ERROR_STATUS: Record<Faucet.FaucetClaimCode, number> = {
  DISABLED: 404,
  ADDRESS_INVALID: 400,
  ADDRESS_NETWORK_MISMATCH: 400,
  ADDRESS_NO_PAYMENT_CREDENTIAL: 400,
  ADDRESS_SCRIPT: 400,
  COOLDOWN: 429,
  IP_LIMIT: 429,
  DEPLETED: 503,
  VALIDATION_FAILED: 500,
  INTERNAL: 500,
};

const isNonEmptyString = (value: unknown): value is string =>
  typeof value === "string" && value.trim().length > 0;

const extractBearerToken = (headers: Record<string, string>): string | null => {
  const header = headers["authorization"];
  if (typeof header !== "string") {
    return null;
  }
  const match = header.match(/^Bearer\s+(.+)$/i);
  return match ? match[1].trim() : null;
};

const postFaucetClaimsHandler = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const request = yield* HttpServerRequest.HttpServerRequest;

  // Server-to-server bearer authentication. When the faucet is disabled the
  // endpoint reports 404 so it is indistinguishable from a non-existent route.
  if (!nodeConfig.FAUCET_ENABLED || nodeConfig.FAUCET_API_KEY === "") {
    return yield* HttpServerResponse.json(
      { error: "Faucet is not enabled" },
      { status: 404 },
    );
  }
  const token = extractBearerToken(request.headers);
  if (token === null || token !== nodeConfig.FAUCET_API_KEY) {
    yield* Effect.logWarning(
      `POST /${FAUCET_CLAIMS_ENDPOINT} - missing or invalid bearer token`,
    );
    return yield* HttpServerResponse.json(
      { error: "Unauthorized" },
      { status: 401 },
    );
  }

  const parsedBody = yield* request.json.pipe(
    Effect.map((value) => ({ ok: true as const, value })),
    Effect.catchAll(() => Effect.succeed({ ok: false as const })),
  );
  if (
    !parsedBody.ok ||
    typeof parsedBody.value !== "object" ||
    parsedBody.value === null
  ) {
    return yield* HttpServerResponse.json(
      { error: "Request body must be a JSON object" },
      { status: 400 },
    );
  }
  const body = parsedBody.value;
  const { address, idempotencyKey, ipHash } = body as Record<string, unknown>;
  if (
    !isNonEmptyString(address) ||
    !isNonEmptyString(idempotencyKey) ||
    !isNonEmptyString(ipHash)
  ) {
    return yield* HttpServerResponse.json(
      {
        error:
          "Request body must contain non-empty 'address', 'idempotencyKey' and 'ipHash' strings",
      },
      { status: 400 },
    );
  }

  const result = yield* Faucet.processClaim({
    address,
    idempotencyKey,
    ipHash,
  });
  yield* Effect.logInfo(
    `POST /${FAUCET_CLAIMS_ENDPOINT} - claim ${result.claimId} ${result.idempotentReplay ? "(idempotent replay) " : ""}tx ${result.txHash}`,
  );
  return yield* HttpServerResponse.json({
    claimId: result.claimId,
    txHash: result.txHash,
    amount: result.amount.toString(),
    nextEligibleAt: result.nextEligibleAt.toISOString(),
  });
}).pipe(
  Effect.catchTag("FaucetClaimError", (e) =>
    Effect.gen(function* () {
      const status = FAUCET_CLAIM_ERROR_STATUS[e.code] ?? 500;
      if (status >= 500) {
        yield* Effect.logError(
          `POST /${FAUCET_CLAIMS_ENDPOINT} - ${e.code}: ${e.message}`,
        );
      } else {
        yield* Effect.logInfo(
          `POST /${FAUCET_CLAIMS_ENDPOINT} - ${e.code}: ${e.message}`,
        );
      }
      return yield* HttpServerResponse.json(
        {
          error: status >= 500 ? "Faucet request failed" : e.message,
          code: e.code,
          ...(e.nextEligibleAt
            ? { nextEligibleAt: e.nextEligibleAt.toISOString() }
            : {}),
        },
        { status },
      );
    }),
  ),
  Effect.catchAll((e) =>
    failWith500("POST", FAUCET_CLAIMS_ENDPOINT, e, "Faucet request failed"),
  ),
);

export const postFaucetClaimsHandlerForTesting = postFaucetClaimsHandler;
export const postSubmitHandlerForTesting = postSubmitHandler;
export const getHealthReadyHandlerForTesting = getHealthReadyHandler;
export const createResetHandlerForTesting = createResetHandler;
export const createLockedActionHandlerForTesting = createLockedActionHandler;
export const getStateQueueRootUnitDiagnosticsHandlerForTesting =
  createStateQueueRootUnitDiagnosticsHandler;
export const getTxsOfAddressHandlerForTesting = getTxsOfAddressHandler;

const getCommitmentWalletBalanceHandler = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const { api: lucidApi } = yield* Lucid;
  let address: string;
  try {
    address = walletFromSeed(
      nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT,
      { network: nodeConfig.NETWORK },
    ).address;
  } catch (e) {
    yield* Effect.logError(
      `GET /${COMMITMENT_WALLET_BALANCE_ENDPOINT} - failed to derive commitment wallet address: ${e}`,
    );
    return yield* HttpServerResponse.json(
      { error: "Failed to derive commitment wallet address" },
      { status: 500 },
    );
  }
  yield* Effect.logInfo(
    `GET /${COMMITMENT_WALLET_BALANCE_ENDPOINT} - querying commitment wallet UTxOs`,
  );
  const utxos = yield* Effect.tryPromise({
    try: () => lucidApi.utxosAt(address),
    catch: (e) =>
      new Error(
        `Failed to query commitment wallet UTxOs: ${e instanceof Error ? e.message : String(e)}`,
      ),
  }).pipe(
    Effect.timeoutFail({
      duration: `${COMMITMENT_WALLET_BALANCE_QUERY_TIMEOUT_MS} millis`,
      onTimeout: () =>
        new Error(
          `Timed out after ${COMMITMENT_WALLET_BALANCE_QUERY_TIMEOUT_MS}ms while querying commitment wallet UTxOs`,
        ),
    }),
  );
  const lovelaceBalance = utxos.reduce(
    (sum, u) => sum + (u.assets.lovelace ?? 0n),
    0n,
  );
  yield* Effect.logInfo(
    `GET /${COMMITMENT_WALLET_BALANCE_ENDPOINT} - balance: ${lovelaceBalance} lovelace`,
  );
  return yield* HttpServerResponse.json({
    lovelaceBalance: String(lovelaceBalance),
  });
}).pipe(
  Effect.catchAll((e) =>
    failWith500(
      "GET",
      COMMITMENT_WALLET_BALANCE_ENDPOINT,
      e,
      "Failed to query commitment wallet balance",
    ),
  ),
);

const router = (
  submitIngressConfig: SubmitIngressConfig,
): Effect.Effect<
  HttpServerResponse.HttpServerResponse,
  HttpBodyError,
  | Database
  | Lucid
  | NodeConfig
  | AlwaysSucceedsContract
  | HttpServerRequest.HttpServerRequest
  | Globals
  | TxIngressQueue
> =>
  HttpRouter.empty
    .pipe(
      HttpRouter.get(`/${HEALTH_LIVE_ENDPOINT}`, getHealthLiveHandler),
      HttpRouter.get(`/${HEALTH_READY_ENDPOINT}`, getHealthReadyHandler),
      HttpRouter.get(`/${TX_ENDPOINT}`, getTxHandler),
      HttpRouter.get(`/${ADDRESS_HISTORY_ENDPOINT}`, getTxsOfAddressHandler),
      HttpRouter.get(`/${UTXOS_ENDPOINT}`, getUtxosHandler),
      HttpRouter.get(`/${BLOCK_ENDPOINT}`, getBlockHandler),
      HttpRouter.get(`/${INIT_ENDPOINT}`, getInitHandler),
      HttpRouter.get(`/${COMMIT_ENDPOINT}`, getCommitEndpoint),
      HttpRouter.get(`/${MERGE_ENDPOINT}`, getMergeHandler),
      HttpRouter.get(`/${RESET_ENDPOINT}`, getResetHandler),
      HttpRouter.get(`/${STATE_QUEUE_ENDPOINT}`, getStateQueueHandler),
      HttpRouter.get(
        `/${STATE_QUEUE_ROOT_UNIT_DIAGNOSTICS_ENDPOINT}`,
        getStateQueueRootUnitDiagnosticsHandler,
      ),
      HttpRouter.get(
        `/${STATE_QUEUE_REPAIR_ROOT_UNITS_ENDPOINT}`,
        getStateQueueRootUnitRepairHandler,
      ),
      HttpRouter.get(
        `/${COMMITMENT_WALLET_BALANCE_ENDPOINT}`,
        getCommitmentWalletBalanceHandler,
      ),
      HttpRouter.get(`/logBlocksTxsDB`, getLogBlocksTxsDBHandler),
      HttpRouter.get(`/logGlobals`, getLogGlobalsHandler),
      HttpRouter.post(
        `/${SUBMIT_ENDPOINT}`,
        postSubmitHandler(submitIngressConfig),
      ),
      HttpRouter.post(`/${FAUCET_CLAIMS_ENDPOINT}`, postFaucetClaimsHandler),
    )
    .pipe(
      Effect.catchAllCause((cause) =>
        failWith500Helper(
          "Router unexpected failure",
          "unknown endpoint",
          Cause.pretty(cause),
        ),
      ),
    );

const apiIngressRouter = (
  submitIngressConfig: SubmitIngressConfig,
): Effect.Effect<
  HttpServerResponse.HttpServerResponse,
  HttpBodyError,
  | Database
  | Lucid
  | NodeConfig
  | AlwaysSucceedsContract
  | HttpServerRequest.HttpServerRequest
  | Globals
  | TxIngressQueue
> =>
  HttpRouter.empty
    .pipe(
      HttpRouter.get(`/${HEALTH_LIVE_ENDPOINT}`, getHealthLiveHandler),
      HttpRouter.get(`/${HEALTH_READY_ENDPOINT}`, getHealthReadyHandler),
      HttpRouter.get(`/${COMMIT_ENDPOINT}`, getCommitEndpoint),
      HttpRouter.get(
        `/${STATE_QUEUE_ROOT_UNIT_DIAGNOSTICS_ENDPOINT}`,
        getStateQueueRootUnitDiagnosticsHandler,
      ),
      HttpRouter.get(
        `/${COMMITMENT_WALLET_BALANCE_ENDPOINT}`,
        getCommitmentWalletBalanceHandler,
      ),
      HttpRouter.post(
        `/${SUBMIT_ENDPOINT}`,
        postSubmitHandler(submitIngressConfig),
      ),
      HttpRouter.post(`/${FAUCET_CLAIMS_ENDPOINT}`, postFaucetClaimsHandler),
    )
    .pipe(
      Effect.catchAllCause((cause) =>
        failWith500Helper(
          "API ingress router unexpected failure",
          "unknown endpoint",
          Cause.pretty(cause),
        ),
      ),
    );

export const runNode = (withMonitoring?: boolean) =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const shouldRunApiRole =
      nodeConfig.NODE_ROLE === "all" || nodeConfig.NODE_ROLE === "api";
    const shouldRunTxProcessorRole =
      nodeConfig.NODE_ROLE === "all" || nodeConfig.NODE_ROLE === "tx-processor";
    const shouldRunSequencerRole =
      nodeConfig.NODE_ROLE === "all" || nodeConfig.NODE_ROLE === "sequencer";

    const txIngressQueue =
      shouldRunApiRole || shouldRunTxProcessorRole
        ? yield* TxIngressQueue
        : null;
    if (txIngressQueue !== null) {
      yield* txIngressQueue.ensureConsumerGroup;
    }

    yield* DBInitialization.program.pipe(
      Effect.provide(Database.Sequencer.layer),
    );

    if (shouldRunSequencerRole) {
      yield* Genesis.program.pipe(Effect.provide(Database.Sequencer.layer));
    }

    // Pre-create the HTTP server so the raw submit interceptor can be attached
    // before Effect registers its 'request' listener via NodeHttpServer.layer.
    const httpServer =
      shouldRunApiRole && txIngressQueue !== null ? createServer() : null;

    if (httpServer !== null && txIngressQueue !== null) {
      createRawSubmitInterceptor({
        xadd: txIngressQueue.rawXadd,
        onEnqueued: () => Effect.runSync(Metric.increment(txAcceptedCounter)),
        onRejected: () => Effect.runSync(Metric.increment(txRejectedCounter)),
      }).attachToServer(httpServer);
    }

    const appThread = (() => {
      if (!shouldRunApiRole || txIngressQueue === null || httpServer === null) {
        return Effect.void;
      }
      const srv = httpServer;
      return Layer.launch(
        Layer.provide(
          HttpServer.serve(
            nodeConfig.NODE_ROLE === "api"
              ? apiIngressRouter({ txIngressQueue })
              : router({ txIngressQueue }),
          ),
          NodeHttpServer.layer(() => srv, { port: nodeConfig.PORT }),
        ),
      );
    })();

    const mkSchedule = (millisBetweenRuns: number) =>
      Schedule.spaced(Duration.millis(millisBetweenRuns));

    const sequencerProgram = shouldRunSequencerRole
      ? Effect.all(
          [
            blockCommitmentFiber(
              mkSchedule(nodeConfig.WAIT_BETWEEN_BLOCK_COMMITMENTS),
            ),
            blockSubmissionFiber(
              mkSchedule(nodeConfig.WAIT_BETWEEN_BLOCK_SUBMISSIONS),
            ),
            syncUserEventsFiber(
              mkSchedule(nodeConfig.WAIT_BETWEEN_USER_EVENT_FETCHES),
            ),
            mergeFiber(mkSchedule(nodeConfig.WAIT_BETWEEN_MERGE_TXS)),
          ],
          {
            concurrency: "unbounded",
          },
        ).pipe(Effect.provide(Database.Sequencer.layer))
      : Effect.void;

    const rpcProgram = Effect.all(
      [
        appThread,
        withMonitoring && (shouldRunApiRole || shouldRunTxProcessorRole)
          ? monitorMempoolFiber(mkSchedule(1000))
          : Effect.void,
        shouldRunTxProcessorRole
          ? txQueueProcessorFiber(
              mkSchedule(nodeConfig.TX_QUEUE_PROCESSOR_INTERVAL_MS),
              nodeConfig.TX_QUEUE_DRAIN_BATCH_SIZE,
              nodeConfig.TX_PARSE_CONCURRENCY,
              nodeConfig.REDIS_STREAM_BLOCK_MS,
              nodeConfig.TX_QUEUE_CONSUMER_WORKER_COUNT,
              nodeConfig.REDIS_STREAM_CONSUMER_NAME,
              withMonitoring,
            )
          : Effect.void,
        shouldRunApiRole && !shouldRunTxProcessorRole
          ? txQueueMetricsRefreshFiber(
              mkSchedule(nodeConfig.TX_QUEUE_PROCESSOR_INTERVAL_MS),
              withMonitoring,
            )
          : Effect.void,
      ],
      {
        concurrency: "unbounded",
      },
    ).pipe(Effect.provide(Database.Rpc.layer));

    const program = Effect.all([sequencerProgram, rpcProgram], {
      concurrency: "unbounded",
    });

    if (withMonitoring) {
      const prometheusExporter = new PrometheusExporter(
        {
          port: nodeConfig.PROM_METRICS_PORT,
        },
        () => {
          console.log(
            `Prometheus metrics available at http://0.0.0.0:${nodeConfig.PROM_METRICS_PORT}/metrics`,
          );
        },
      );

      const originalStop = prometheusExporter.stopServer;
      prometheusExporter.stopServer = async function () {
        Effect.runSync(Effect.logInfo("Prometheus exporter is stopping!"));
        return originalStop();
      };

      const MetricsLive = NodeSdk.layer(() => ({
        resource: { serviceName: "midgard-node" },
        metricReader: prometheusExporter,
        spanProcessor: new BatchSpanProcessor(
          new OTLPTraceExporter({ url: nodeConfig.OLTP_EXPORTER_URL }),
        ),
      }));

      yield* pipe(
        program,
        Effect.withSpan("midgard"),
        Effect.provide(MetricsLive),
        Effect.catchAllCause(Effect.logError),
      );
    } else {
      yield* pipe(
        program,
        Effect.withSpan("midgard"),
        Effect.catchAllCause(Effect.logError),
      );
    }
  });
