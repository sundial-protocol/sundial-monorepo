import { expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer, Option } from "effect";
import * as os from "node:os";
import * as path from "node:path";
import { randomUUID } from "node:crypto";

import {
  makeTestSqlLayer,
  makeTestSqlLayerWithPath,
} from "./harness/pglite-sql-layer.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import * as DBInitialization from "@/database/init.js";
import * as FaucetClaimsDB from "@/database/faucetClaims.js";

const makeBaseLayers = () =>
  Layer.mergeAll(makeTestSqlLayer(), makeTestNodeConfigLayer());

const makeEntry = (
  overrides: Partial<FaucetClaimsDB.Entry> = {},
): FaucetClaimsDB.Entry => ({
  [FaucetClaimsDB.Columns.CLAIM_ID]: randomUUID(),
  [FaucetClaimsDB.Columns.IDEMPOTENCY_KEY]: randomUUID(),
  [FaucetClaimsDB.Columns.ADDRESS]: "addr_test1_recipient",
  [FaucetClaimsDB.Columns.IP_HASH]: "ip-hash-1",
  [FaucetClaimsDB.Columns.AMOUNT_LOVELACE]: 100_000_000n,
  [FaucetClaimsDB.Columns.TX_ID]: Buffer.alloc(32, 0x11),
  [FaucetClaimsDB.Columns.NEXT_ELIGIBLE_AT]: new Date(Date.now() + 3_600_000),
  ...overrides,
});

it.effect("insert and look up a claim by idempotency key", () =>
  Effect.gen(function* () {
    yield* DBInitialization.program;

    const entry = makeEntry({
      [FaucetClaimsDB.Columns.AMOUNT_LOVELACE]: 123_456_789n,
      [FaucetClaimsDB.Columns.TX_ID]: Buffer.alloc(32, 0xab),
    });
    yield* FaucetClaimsDB.insertClaim(entry);

    const found = yield* FaucetClaimsDB.findByIdempotencyKey(
      entry[FaucetClaimsDB.Columns.IDEMPOTENCY_KEY],
    );

    expect(Option.isSome(found)).toBe(true);
    if (Option.isSome(found)) {
      // BIGINT round-trips without precision loss, and tx_id decodes to hex.
      expect(found.value.amountLovelace).toBe(123_456_789n);
      expect(found.value.txHashHex).toBe("ab".repeat(32));
      expect(found.value.address).toBe(entry[FaucetClaimsDB.Columns.ADDRESS]);
    }

    const missing = yield* FaucetClaimsDB.findByIdempotencyKey("no-such-key");
    expect(Option.isNone(missing)).toBe(true);
  }).pipe(Effect.provide(makeBaseLayers())),
);

it.effect("a repeated idempotency key cannot be inserted twice", () =>
  Effect.gen(function* () {
    yield* DBInitialization.program;

    const idempotencyKey = "repeat-key";
    yield* FaucetClaimsDB.insertClaim(
      makeEntry({ [FaucetClaimsDB.Columns.IDEMPOTENCY_KEY]: idempotencyKey }),
    );

    const secondInsert = yield* FaucetClaimsDB.insertClaim(
      makeEntry({ [FaucetClaimsDB.Columns.IDEMPOTENCY_KEY]: idempotencyKey }),
    ).pipe(Effect.either);

    // The unique index on idempotency_key rejects the duplicate payout.
    expect(secondInsert._tag).toBe("Left");

    const allForKey =
      yield* FaucetClaimsDB.findByIdempotencyKey(idempotencyKey);
    expect(Option.isSome(allForKey)).toBe(true);
  }).pipe(Effect.provide(makeBaseLayers())),
);

it.effect(
  "active cooldown is found only while next_eligible_at is in future",
  () =>
    Effect.gen(function* () {
      yield* DBInitialization.program;

      const now = new Date();
      const address = "addr_test1_cooldown";

      yield* FaucetClaimsDB.insertClaim(
        makeEntry({
          [FaucetClaimsDB.Columns.ADDRESS]: address,
          [FaucetClaimsDB.Columns.NEXT_ELIGIBLE_AT]: new Date(
            now.getTime() + 3_600_000,
          ),
        }),
      );

      const active = yield* FaucetClaimsDB.findActiveCooldownByAddress(
        address,
        now,
      );
      expect(Option.isSome(active)).toBe(true);

      // A second address whose window already elapsed must not be in cooldown.
      const expiredAddress = "addr_test1_expired";
      yield* FaucetClaimsDB.insertClaim(
        makeEntry({
          [FaucetClaimsDB.Columns.ADDRESS]: expiredAddress,
          [FaucetClaimsDB.Columns.NEXT_ELIGIBLE_AT]: new Date(
            now.getTime() - 3_600_000,
          ),
        }),
      );
      const expired = yield* FaucetClaimsDB.findActiveCooldownByAddress(
        expiredAddress,
        now,
      );
      expect(Option.isNone(expired)).toBe(true);
    }).pipe(Effect.provide(makeBaseLayers())),
);

it.effect("countByIpSince counts only claims inside the window", () =>
  Effect.gen(function* () {
    yield* DBInitialization.program;

    const ipHash = "ip-hash-window";
    yield* FaucetClaimsDB.insertClaim(
      makeEntry({ [FaucetClaimsDB.Columns.IP_HASH]: ipHash }),
    );
    yield* FaucetClaimsDB.insertClaim(
      makeEntry({ [FaucetClaimsDB.Columns.IP_HASH]: ipHash }),
    );
    // Different IP must not be counted.
    yield* FaucetClaimsDB.insertClaim(
      makeEntry({ [FaucetClaimsDB.Columns.IP_HASH]: "other-ip" }),
    );

    const within = yield* FaucetClaimsDB.countByIpSince(
      ipHash,
      new Date(Date.now() - 60_000),
    );
    expect(within).toBe(2);

    const future = yield* FaucetClaimsDB.countByIpSince(
      ipHash,
      new Date(Date.now() + 60_000),
    );
    expect(future).toBe(0);
  }).pipe(Effect.provide(makeBaseLayers())),
);

it.effect("claims and cooldowns survive a node restart", () =>
  Effect.gen(function* () {
    const dbPath = path.join(os.tmpdir(), `faucet-claims-${randomUUID()}`);
    const nodeConfigLayer = makeTestNodeConfigLayer();
    const address = "addr_test1_restart";
    const idempotencyKey = "restart-key";
    const entry = makeEntry({
      [FaucetClaimsDB.Columns.ADDRESS]: address,
      [FaucetClaimsDB.Columns.IDEMPOTENCY_KEY]: idempotencyKey,
      [FaucetClaimsDB.Columns.NEXT_ELIGIBLE_AT]: new Date(
        Date.now() + 3_600_000,
      ),
    });

    // First "boot": initialize the schema and persist a claim to disk.
    yield* Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* FaucetClaimsDB.insertClaim(entry);
    }).pipe(
      Effect.provide(
        Layer.mergeAll(makeTestSqlLayerWithPath(dbPath), nodeConfigLayer),
      ),
    );

    // Second "boot": a brand new connection over the same data directory must
    // still observe the persisted claim and its active cooldown.
    const { found, cooldown } = yield* Effect.gen(function* () {
      const found = yield* FaucetClaimsDB.findByIdempotencyKey(idempotencyKey);
      const cooldown = yield* FaucetClaimsDB.findActiveCooldownByAddress(
        address,
        new Date(),
      );
      return { found, cooldown };
    }).pipe(
      Effect.provide(
        Layer.mergeAll(makeTestSqlLayerWithPath(dbPath), nodeConfigLayer),
      ),
    );

    expect(Option.isSome(found)).toBe(true);
    expect(Option.isSome(cooldown)).toBe(true);
  }),
);
