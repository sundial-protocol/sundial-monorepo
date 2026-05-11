import { Effect } from "effect";

export const Columns = {
  TX_ID: "tx_id",
  OUTREF: "outref",
  OUTPUT: "output",
  ADDRESS: "address",
  TIMESTAMPTZ: "time_stamp_tz",
} as const;

export type Entry = Record<string, unknown>;
export type MinimalEntry = Record<string, unknown>;

export const createTable = (_tableName: string) => Effect.succeed(undefined);
export const insertEntry = (_tableName: string, _entry: unknown) =>
  Effect.succeed(undefined);
export const insertEntries = (_tableName: string, _entries: unknown[]) =>
  Effect.succeed(undefined);
export const retrieveAllEntries = (_tableName: string) =>
  Effect.succeed([] as Entry[]);
export const retrieveAllEntriesNoTimeStamps = (_tableName: string) =>
  Effect.succeed([] as Entry[]);
export const retrieveByOutRef = (_tableName: string, _outRef: unknown) =>
  Effect.succeed({} as Entry);
export const retrieveByOutRefs = (_tableName: string, _outRefs: unknown[]) =>
  Effect.succeed([] as Entry[]);
export const retrieveEntriesWithAddress = (
  _tableName: string,
  _address: string,
) => Effect.succeed([] as Entry[]);
export const delEntries = (_tableName: string, _outrefs: unknown[]) =>
  Effect.succeed(undefined);
export const removeSpentOutRef = (
  _ledger: unknown[],
  _spentOutRefCBOR: unknown,
) => Effect.succeed([] as Entry[]);
export const applyTx = (_ledger: unknown[], _txCbor: unknown) =>
  Effect.succeed([] as Entry[]);
