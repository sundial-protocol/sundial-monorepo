import { NodeConfig } from "@/services/config.js";
import {
  TX_STREAM_MESSAGE_CBOR_FIELD,
  TxIngressFailureDisposition,
  TxIngressMessage,
  TxIngressMetricsSnapshot,
  TxIngressQueue,
  TxIngressQueueError,
  TxIngressQueueService,
} from "@/services/tx-ingress-queue.js";
import { Effect, Layer } from "effect";
import { Redis } from "ioredis";

type RedisReadEntry = readonly [id: string, fields: readonly string[]];
type RedisReadResponse = readonly [
  stream: string,
  entries: readonly RedisReadEntry[],
];
type RedisAutoClaimResponse = readonly unknown[];

type RedisPendingEntry = readonly [
  id: string,
  consumer: string,
  idleTimeMs: number,
  deliveryCount: number,
];
type RedisXInfoGroup = Record<string, unknown>;

const toTxIngressQueueError = (
  operation: string,
  cause: unknown,
): TxIngressQueueError =>
  new TxIngressQueueError({
    operation,
    message: `Redis Streams operation failed: ${operation}`,
    cause,
  });

const tryRedis = <T>(operation: string, task: () => Promise<T>) =>
  Effect.tryPromise({
    try: task,
    catch: (cause) => toTxIngressQueueError(operation, cause),
  });

const toCborField = (fields: readonly string[]): string | null => {
  for (let i = 0; i < fields.length - 1; i += 2) {
    if (fields[i] === TX_STREAM_MESSAGE_CBOR_FIELD) {
      return fields[i + 1] ?? null;
    }
  }
  return null;
};

const normalizeXReadGroupEntries = (
  response: readonly RedisReadResponse[] | null,
): readonly RedisReadEntry[] => {
  if (response === null || response.length === 0) {
    return [];
  }

  return response.flatMap(([, entries]) => entries);
};

const normalizeXAutoClaimEntries = (
  response: RedisAutoClaimResponse,
): readonly RedisReadEntry[] => {
  if (response.length < 2) {
    return [];
  }
  const entries = response[1];
  return Array.isArray(entries) ? (entries as readonly RedisReadEntry[]) : [];
};

const normalizePendingSummaryCount = (value: unknown): number => {
  const normalized = Number(value);
  return Number.isFinite(normalized) && normalized >= 0 ? normalized : 0;
};

const toRedisXInfoGroup = (raw: unknown): RedisXInfoGroup | null => {
  if (raw === null || raw === undefined) {
    return null;
  }

  if (Array.isArray(raw)) {
    if (raw.length % 2 !== 0) {
      return null;
    }
    const group: RedisXInfoGroup = {};
    for (let i = 0; i < raw.length; i += 2) {
      const key = raw[i];
      if (typeof key !== "string") {
        return null;
      }
      group[key] = raw[i + 1];
    }
    return group;
  }

  if (typeof raw === "object") {
    return raw as RedisXInfoGroup;
  }

  return null;
};

const normalizeConsumerGroupLag = (
  groupInfoRaw: unknown,
  consumerGroupName: string,
): number | null => {
  const groupCandidates = Array.isArray(groupInfoRaw)
    ? groupInfoRaw
    : [groupInfoRaw];

  for (const candidate of groupCandidates) {
    const groupInfo = toRedisXInfoGroup(candidate);
    if (groupInfo === null) {
      continue;
    }

    if (String(groupInfo.name ?? "") !== consumerGroupName) {
      continue;
    }

    const lagRaw = groupInfo.lag;
    if (lagRaw === null || lagRaw === undefined) {
      return null;
    }

    const lag = Number(lagRaw);
    return Number.isFinite(lag) && lag >= 0 ? lag : null;
  }

  return null;
};

const makeRedisStreamsTxIngressQueue = Effect.acquireRelease(
  Effect.gen(function* () {
    const config = yield* NodeConfig;

    const producerClient = new Redis(config.REDIS_URL);
    const consumerClient = producerClient.duplicate();
    // Dedicated connection for XREADGROUP BLOCK. A blocking Redis command holds
    // the connection open for its full timeout duration, which would delay every
    // non-blocking command (XGROUP CREATE, XACK, XAUTOCLAIM) queued behind it
    // on the same connection. Using a separate connection for XREADGROUP BLOCK
    // keeps consumerClient free for low-latency operations used by the submit
    // path (ensureGroup) and the processor's non-blocking calls.
    const blockingConsumerClient = producerClient.duplicate();
    const statsClient = producerClient.duplicate();
    const emptySnapshot: TxIngressMetricsSnapshot = {
      streamDepth: 0,
      pendingCount: 0,
      lagCount: 0,
    };
    let cachedMetricsSnapshot: TxIngressMetricsSnapshot = emptySnapshot;

    const readDeliveryCount = (messageId: string) =>
      tryRedis<readonly RedisPendingEntry[]>("XPENDING_RANGE", () =>
        consumerClient
          .xpending(
            config.REDIS_STREAM_KEY,
            config.REDIS_STREAM_CONSUMER_GROUP,
            messageId,
            messageId,
            1,
          )
          .then((value) => value as readonly RedisPendingEntry[]),
      ).pipe(
        Effect.map((rawPending) => {
          if (rawPending.length === 0) {
            return 1;
          }

          const rawCount = Number(rawPending[0][3]);
          return Number.isFinite(rawCount) && rawCount > 0 ? rawCount : 1;
        }),
      );

    const toIngressMessages = (entries: readonly RedisReadEntry[]) =>
      Effect.forEach(entries, ([id, fields]) =>
        Effect.gen(function* () {
          const txCbor = toCborField(fields);
          if (txCbor === null) {
            return yield* Effect.fail(
              new TxIngressQueueError({
                operation: "PARSE_STREAM_ENTRY",
                message: `Redis stream entry is missing '${TX_STREAM_MESSAGE_CBOR_FIELD}' field`,
                cause: { id },
              }),
            );
          }

          const deliveryCount = yield* readDeliveryCount(id);
          const message: TxIngressMessage = { id, txCbor, deliveryCount };
          return message;
        }),
      );

    const ensureGroup: Effect.Effect<void, TxIngressQueueError> = tryRedis(
      "XGROUP_CREATE",
      () =>
        consumerClient.xgroup(
          "CREATE",
          config.REDIS_STREAM_KEY,
          config.REDIS_STREAM_CONSUMER_GROUP,
          "0",
          "MKSTREAM",
        ),
    ).pipe(
      Effect.catchTag("TxIngressQueueError", (error) => {
        const causeMessage = String(error.cause);
        return causeMessage.includes("BUSYGROUP")
          ? Effect.void
          : Effect.fail(error);
      }),
    );

    const fetchSnapshotMetrics = Effect.gen(function* () {
      const [streamDepthRaw, pendingSummary, groupsInfoRaw] = yield* Effect.all(
        [
          tryRedis("XLEN", () => statsClient.xlen(config.REDIS_STREAM_KEY)),
          tryRedis("XPENDING_SUMMARY", () =>
            statsClient.xpending(
              config.REDIS_STREAM_KEY,
              config.REDIS_STREAM_CONSUMER_GROUP,
            ),
          ).pipe(
            Effect.catchTag("TxIngressQueueError", (e) => {
              const causeMessage = String(e.cause);
              return causeMessage.includes("NOGROUP")
                ? Effect.succeed([0, null, null, []] as unknown[])
                : Effect.fail(e);
            }),
          ),
          tryRedis(
            "XINFO_GROUPS",
            () =>
              statsClient.call(
                "XINFO",
                "GROUPS",
                config.REDIS_STREAM_KEY,
              ) as Promise<unknown>,
          ).pipe(
            Effect.catchTag("TxIngressQueueError", (e) => {
              const causeMessage = String(e.cause);
              return causeMessage.includes("no such key")
                ? Effect.succeed([] as unknown[])
                : Effect.fail(e);
            }),
          ),
        ],
        { concurrency: "unbounded" },
      );

      const pendingCount = normalizePendingSummaryCount(pendingSummary[0]);
      const lagFromGroup = normalizeConsumerGroupLag(
        groupsInfoRaw,
        config.REDIS_STREAM_CONSUMER_GROUP,
      );
      const lagCount =
        lagFromGroup ?? Math.max(0, streamDepthRaw - pendingCount);
      const streamDepth = lagCount;
      const snapshot: TxIngressMetricsSnapshot = {
        streamDepth,
        pendingCount,
        lagCount,
      };
      return snapshot;
    });

    const queueService: TxIngressQueueService = {
      enqueue: (txCbor: string) =>
        tryRedis("XADD", () =>
          producerClient.xadd(
            config.REDIS_STREAM_KEY,
            "*",
            TX_STREAM_MESSAGE_CBOR_FIELD,
            txCbor,
          ),
        ).pipe(
          Effect.flatMap((id) =>
            id === null
              ? Effect.fail(
                  new TxIngressQueueError({
                    operation: "XADD",
                    message: "Redis did not return a stream id for enqueued tx",
                    cause: undefined,
                  }),
                )
              : Effect.succeed(String(id)),
          ),
        ),
      rawXadd: (txCbor, callback) => {
        producerClient
          .xadd(
            config.REDIS_STREAM_KEY,
            "*",
            TX_STREAM_MESSAGE_CBOR_FIELD,
            txCbor,
          )
          .then((id) => callback(null, id))
          .catch((err: unknown) => {
            callback(err instanceof Error ? err : new Error(String(err)), null);
          });
      },
      ensureConsumerGroup: ensureGroup,
      consumeBatch: (
        maxCount: number,
        blockMs: number,
        consumerName: string = config.REDIS_STREAM_CONSUMER_NAME,
      ) =>
        Effect.gen(function* () {
          const reclaimCount = Math.min(
            maxCount,
            config.TX_QUEUE_CLAIM_BATCH_SIZE,
          );
          const reclaimedRaw = yield* tryRedis<RedisAutoClaimResponse>(
            "XAUTOCLAIM",
            () =>
              consumerClient
                .xautoclaim(
                  config.REDIS_STREAM_KEY,
                  config.REDIS_STREAM_CONSUMER_GROUP,
                  consumerName,
                  config.TX_QUEUE_CLAIM_IDLE_MS,
                  "0-0",
                  "COUNT",
                  reclaimCount,
                )
                .then((value) => value as RedisAutoClaimResponse),
          );
          const reclaimedEntries = normalizeXAutoClaimEntries(reclaimedRaw);

          const remainingCount = Math.max(
            0,
            maxCount - reclaimedEntries.length,
          );
          const freshEntries =
            remainingCount === 0
              ? []
              : normalizeXReadGroupEntries(
                  yield* tryRedis<readonly RedisReadResponse[] | null>(
                    "XREADGROUP",
                    () =>
                      blockingConsumerClient.xreadgroup(
                        "GROUP",
                        config.REDIS_STREAM_CONSUMER_GROUP,
                        consumerName,
                        "COUNT",
                        remainingCount,
                        "BLOCK",
                        blockMs,
                        "STREAMS",
                        config.REDIS_STREAM_KEY,
                        ">",
                      ) as Promise<readonly RedisReadResponse[] | null>,
                  ),
                );

          const combinedEntries: readonly RedisReadEntry[] = [
            ...reclaimedEntries,
            ...freshEntries,
          ];

          return yield* toIngressMessages(combinedEntries);
        }).pipe(
          Effect.catchTag("TxIngressQueueError", (error) => {
            const causeMessage = String(error.cause);
            return causeMessage.includes("NOGROUP")
              ? ensureGroup.pipe(
                  Effect.zipRight(
                    Effect.succeed([] as readonly TxIngressMessage[]),
                  ),
                )
              : Effect.fail(error);
          }),
        ),
      ack: (messageIds: readonly string[]) =>
        messageIds.length === 0
          ? Effect.succeed(0)
          : tryRedis("XACK", () =>
              consumerClient.xack(
                config.REDIS_STREAM_KEY,
                config.REDIS_STREAM_CONSUMER_GROUP,
                ...messageIds,
              ),
            ),
      handleFailedMessage: (message: TxIngressMessage, reason: string) =>
        Effect.gen(function* () {
          if (message.deliveryCount < config.TX_QUEUE_MAX_DELIVERY_ATTEMPTS) {
            const retryDisposition: TxIngressFailureDisposition = "retry";
            return retryDisposition;
          }

          const failedAtMs = `${Date.now()}`;
          yield* tryRedis("XADD_DEAD_LETTER", () =>
            producerClient.xadd(
              config.TX_QUEUE_DEAD_LETTER_STREAM,
              "*",
              "original_stream",
              config.REDIS_STREAM_KEY,
              "original_message_id",
              message.id,
              TX_STREAM_MESSAGE_CBOR_FIELD,
              message.txCbor,
              "delivery_count",
              `${message.deliveryCount}`,
              "failed_at_ms",
              failedAtMs,
              "reason",
              reason,
            ),
          );

          yield* tryRedis("XACK_DEAD_LETTERED", () =>
            consumerClient.xack(
              config.REDIS_STREAM_KEY,
              config.REDIS_STREAM_CONSUMER_GROUP,
              message.id,
            ),
          );

          const deadLetteredDisposition: TxIngressFailureDisposition =
            "dead_lettered";
          return deadLetteredDisposition;
        }),
      refreshSnapshotMetrics: fetchSnapshotMetrics.pipe(
        Effect.tap((snapshot) =>
          Effect.sync(() => {
            cachedMetricsSnapshot = snapshot;
          }),
        ),
      ),
      snapshotMetrics: Effect.sync(() => cachedMetricsSnapshot),
      clear: Effect.gen(function* () {
        yield* tryRedis("DEL_STREAM", () =>
          producerClient.del(config.REDIS_STREAM_KEY),
        );
        yield* tryRedis("DEL_DEAD_LETTER_STREAM", () =>
          producerClient.del(config.TX_QUEUE_DEAD_LETTER_STREAM),
        );
        // DEL destroys consumer groups along with the stream. Recreate the
        // group immediately so the processor fiber and snapshotMetrics remain
        // functional without requiring a node restart.
        yield* ensureGroup;
        cachedMetricsSnapshot = emptySnapshot;
      }),
    };

    return {
      queueService,
      clients: [
        producerClient,
        consumerClient,
        blockingConsumerClient,
        statsClient,
      ] as const,
    };
  }),
  ({ clients }) =>
    Effect.promise(() =>
      Promise.all(clients.map((client) => client.quit())),
    ).pipe(
      Effect.catchAllCause(() => Effect.void),
      Effect.asVoid,
    ),
).pipe(Effect.map(({ queueService }) => queueService));

export const RedisStreamsTxIngressQueueLive = Layer.effect(
  TxIngressQueue,
  makeRedisStreamsTxIngressQueue,
);
