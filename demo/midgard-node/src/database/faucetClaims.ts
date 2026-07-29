import { Database } from "@/services/database.js";
import { Effect, Option } from "effect";
import { SqlClient } from "@effect/sql";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";

export const tableName = "faucet_claims";

export enum Columns {
  CLAIM_ID = "claim_id",
  IDEMPOTENCY_KEY = "idempotency_key",
  ADDRESS = "address",
  IP_HASH = "ip_hash",
  AMOUNT_LOVELACE = "amount_lovelace",
  TX_ID = "tx_id",
  CREATED_AT = "created_at",
  NEXT_ELIGIBLE_AT = "next_eligible_at",
}

export type Entry = {
  [Columns.CLAIM_ID]: string;
  [Columns.IDEMPOTENCY_KEY]: string;
  [Columns.ADDRESS]: string;
  [Columns.IP_HASH]: string;
  [Columns.AMOUNT_LOVELACE]: bigint;
  [Columns.TX_ID]: Buffer;
  [Columns.NEXT_ELIGIBLE_AT]: Date;
};

// Row shape as returned by PostgreSQL. `amount_lovelace` comes back as a string
// for BIGINT and `tx_id` as a Buffer for BYTEA.
type Row = {
  [Columns.CLAIM_ID]: string;
  [Columns.IDEMPOTENCY_KEY]: string;
  [Columns.ADDRESS]: string;
  [Columns.IP_HASH]: string;
  [Columns.AMOUNT_LOVELACE]: string | bigint;
  [Columns.TX_ID]: Buffer | Uint8Array | string;
  [Columns.CREATED_AT]: Date;
  [Columns.NEXT_ELIGIBLE_AT]: Date;
};

export type ClaimRecord = {
  readonly claimId: string;
  readonly idempotencyKey: string;
  readonly address: string;
  readonly ipHash: string;
  readonly amountLovelace: bigint;
  readonly txHashHex: string;
  readonly createdAt: Date;
  readonly nextEligibleAt: Date;
};

const decodeBytea = (value: Buffer | Uint8Array | string): Buffer =>
  typeof value === "string"
    ? Buffer.from(value.startsWith("\\x") ? value.slice(2) : value, "hex")
    : Buffer.from(value);

const rowToRecord = (row: Row): ClaimRecord => ({
  claimId: row[Columns.CLAIM_ID],
  idempotencyKey: row[Columns.IDEMPOTENCY_KEY],
  address: row[Columns.ADDRESS],
  ipHash: row[Columns.IP_HASH],
  amountLovelace: BigInt(row[Columns.AMOUNT_LOVELACE]),
  txHashHex: decodeBytea(row[Columns.TX_ID]).toString("hex"),
  createdAt: row[Columns.CREATED_AT],
  nextEligibleAt: row[Columns.NEXT_ELIGIBLE_AT],
});

export const createTable = (): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
          ${sql(Columns.CLAIM_ID)} TEXT NOT NULL,
          ${sql(Columns.IDEMPOTENCY_KEY)} TEXT NOT NULL,
          ${sql(Columns.ADDRESS)} TEXT NOT NULL,
          ${sql(Columns.IP_HASH)} TEXT NOT NULL,
          ${sql(Columns.AMOUNT_LOVELACE)} BIGINT NOT NULL,
          ${sql(Columns.TX_ID)} BYTEA NOT NULL,
          ${sql(Columns.CREATED_AT)} TIMESTAMPTZ NOT NULL DEFAULT(NOW()),
          ${sql(Columns.NEXT_ELIGIBLE_AT)} TIMESTAMPTZ NOT NULL,
          PRIMARY KEY (${sql(Columns.CLAIM_ID)})
        );`;
        // Idempotency keys must be unique so a retried request maps to exactly
        // one claim (and one faucet payout).
        yield* sql`CREATE UNIQUE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.IDEMPOTENCY_KEY}`,
        )} ON ${sql(tableName)} (${sql(Columns.IDEMPOTENCY_KEY)});`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_address_created`,
        )} ON ${sql(tableName)} (${sql(Columns.ADDRESS)}, ${sql(Columns.CREATED_AT)});`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_ip_created`,
        )} ON ${sql(tableName)} (${sql(Columns.IP_HASH)}, ${sql(Columns.CREATED_AT)});`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to create the faucet_claims table",
    ),
  );

export const findByIdempotencyKey = (
  idempotencyKey: string,
): Effect.Effect<Option.Option<ClaimRecord>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.IDEMPOTENCY_KEY)} = ${idempotencyKey}
      LIMIT 1`;
    return rows.length > 0 ? Option.some(rowToRecord(rows[0])) : Option.none();
  }).pipe(
    Effect.withLogSpan(`findByIdempotencyKey ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to look up faucet claim by idempotency key",
    ),
  );

/**
 * Returns the most recent claim for `address` whose cooldown window
 * (`next_eligible_at`) has not yet elapsed at `now`, if any.
 */
export const findActiveCooldownByAddress = (
  address: string,
  now: Date,
): Effect.Effect<Option.Option<ClaimRecord>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.ADDRESS)} = ${address}
      AND ${sql(Columns.NEXT_ELIGIBLE_AT)} > ${now}
      ORDER BY ${sql(Columns.NEXT_ELIGIBLE_AT)} DESC
      LIMIT 1`;
    return rows.length > 0 ? Option.some(rowToRecord(rows[0])) : Option.none();
  }).pipe(
    Effect.withLogSpan(`findActiveCooldownByAddress ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to look up active faucet cooldown for address",
    ),
  );

/** Counts successful claims for `ipHash` since `since` (inclusive). */
export const countByIpSince = (
  ipHash: string,
  since: Date,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      count: bigint | string;
    }>`SELECT COUNT(*)::bigint AS count
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.IP_HASH)} = ${ipHash}
      AND ${sql(Columns.CREATED_AT)} >= ${since}`;
    return rows.length === 0 ? 0 : Number(rows[0].count);
  }).pipe(
    Effect.withLogSpan(`countByIpSince ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to count recent faucet claims for IP",
    ),
  );

export const insertClaim = (
  entry: Entry,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert({
      [Columns.CLAIM_ID]: entry[Columns.CLAIM_ID],
      [Columns.IDEMPOTENCY_KEY]: entry[Columns.IDEMPOTENCY_KEY],
      [Columns.ADDRESS]: entry[Columns.ADDRESS],
      [Columns.IP_HASH]: entry[Columns.IP_HASH],
      // BIGINT columns are bound as strings to avoid precision loss.
      [Columns.AMOUNT_LOVELACE]: entry[Columns.AMOUNT_LOVELACE].toString(),
      [Columns.TX_ID]: entry[Columns.TX_ID],
      [Columns.NEXT_ELIGIBLE_AT]: entry[Columns.NEXT_ELIGIBLE_AT],
    })}`;
  }).pipe(
    Effect.withLogSpan(`insertClaim ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertClaim: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to insert faucet claim"),
  );

export const clear = clearTable(tableName);
