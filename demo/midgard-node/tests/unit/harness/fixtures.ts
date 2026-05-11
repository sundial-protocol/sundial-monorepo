import { Effect } from "effect";
import { vi } from "vitest";
import type * as Ledger from "@/database/utils/ledger.js";
import type * as Tx from "@/database/utils/tx.js";
import type * as UserEvents from "@/database/utils/user-events.js";

export const COMMON_ADDRESSES = {
  produced: "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58",
  spent: "addr_test1vz0p8k0ekk5xvms5jlqmajgddmqm4xp58yd8c92lvd63hwcv6znrl",
} as const;

export const makeUserEventEntry = (
  seed: number,
  overrides: Partial<UserEvents.Entry> = {},
): UserEvents.Entry => ({
  event_id: Buffer.alloc(32, seed),
  event_info: Buffer.alloc(32, seed + 0x10),
  asset_name: seed.toString(16).padStart(2, "0").repeat(10),
  l1_utxo_cbor: Buffer.alloc(64, seed + 0x20),
  inclusion_time: new Date(1_700_000_000_000 + seed * 1_000),
  ...overrides,
});

export const makeLedgerEntry = (
  seed: number,
  overrides: Partial<Ledger.EntryNoTimeStamp> = {},
): Ledger.EntryNoTimeStamp => ({
  tx_id: Buffer.alloc(32, seed),
  outref: Buffer.alloc(32, seed + 1),
  output: Buffer.alloc(16, seed + 2),
  address: COMMON_ADDRESSES.produced,
  ...overrides,
});

export const makeTxEntryNoTimeStamp = (
  seed: number,
  overrides: Partial<Tx.EntryNoTimeStamp> = {},
): Tx.EntryNoTimeStamp => ({
  tx_id: Buffer.alloc(32, seed),
  tx: Buffer.alloc(64, seed + 1),
  ...overrides,
});

export const makeTxEntryWithTimeStamp = (
  seed: number,
  overrides: Partial<Tx.EntryWithTimeStamp> = {},
): Tx.EntryWithTimeStamp => ({
  ...makeTxEntryNoTimeStamp(seed),
  time_stamp_tz: new Date(1_700_000_000_000 + seed * 1_000),
  ...overrides,
});

export type FakeBatchOp = {
  type: "put" | "del";
  key: Buffer;
  value?: Buffer;
};

export type FakeTrie = {
  batch: ReturnType<typeof vi.fn>;
  getRootHex: ReturnType<typeof vi.fn>;
};

export const makeFakeTrie = (rootHex: string): FakeTrie => ({
  batch: vi.fn((_ops: readonly FakeBatchOp[]) => Effect.void),
  getRootHex: vi.fn(() => Effect.succeed(rootHex)),
});
