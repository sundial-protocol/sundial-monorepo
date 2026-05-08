import { afterEach, describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import * as os from "os";
import * as path from "path";
import { randomUUID } from "crypto";
import { MidgardMpt, deleteMpt } from "@/workers/utils/mpt.js";
import * as ETH_UTILS from "@ethereumjs/util";
import { EMPTY_ROOT } from "../constants.js";

const txIdA = Buffer.alloc(32, 0xaa);
const txCborA = Buffer.alloc(64, 0xbb);

it.effect("Empty in-memory MPT has expected root", () =>
  Effect.gen(function* () {
    const mpt = yield* MidgardMpt.create("unit-test-empty");
    const root = yield* mpt.getRootHex();
    const isEmpty = yield* mpt.rootIsEmpty();
    expect(root).toBe(EMPTY_ROOT);
    expect(isEmpty).toBe(true);
  }),
);

it.effect("In-memory MPT put updates root", () =>
  Effect.gen(function* () {
    const mpt = yield* MidgardMpt.create("unit-test-put");
    const emptyRoot = yield* mpt.getRootHex();
    const putOp: ETH_UTILS.BatchDBOp = {
      type: "put",
      key: txIdA,
      value: txCborA,
    };
    yield* mpt.batch([putOp]);
    const newRoot = yield* mpt.getRootHex();
    expect(newRoot).not.toBe(emptyRoot);
  }),
);

it.effect("In-memory MPT delete restores empty root", () =>
  Effect.gen(function* () {
    const mpt = yield* MidgardMpt.create("unit-test-del");
    yield* mpt.batch([{ type: "put", key: txIdA, value: txCborA }]);
    yield* mpt.batch([{ type: "del", key: txIdA }]);
    const root = yield* mpt.getRootHex();
    const isEmpty = yield* mpt.rootIsEmpty();
    expect(root).toBe(EMPTY_ROOT);
    expect(isEmpty).toBe(true);
  }),
);

it.effect("In-memory MPT checkpoint commit persists change", () =>
  Effect.gen(function* () {
    const mpt = yield* MidgardMpt.create("unit-test-commit");
    yield* mpt.checkpoint();
    yield* mpt.batch([{ type: "put", key: txIdA, value: txCborA }]);
    yield* mpt.commit();
    const root = yield* mpt.getRootHex();
    expect(root).not.toBe(EMPTY_ROOT);
  }),
);

it.effect("In-memory MPT checkpoint revert rolls back change", () =>
  Effect.gen(function* () {
    const mpt = yield* MidgardMpt.create("unit-test-revert");
    yield* mpt.checkpoint();
    yield* mpt.batch([{ type: "put", key: txIdA, value: txCborA }]);
    yield* mpt.revert();
    const root = yield* mpt.getRootHex();
    const isEmpty = yield* mpt.rootIsEmpty();
    expect(root).toBe(EMPTY_ROOT);
    expect(isEmpty).toBe(true);
  }),
);

describe("LevelDB-backed MPT", () => {
  let tmpPath: string;

  afterEach(async () => {
    if (tmpPath) {
      await Effect.runPromise(deleteMpt(tmpPath, "unit-test-leveldb"));
    }
  });

  it.effect("persists root across reopen", () =>
    Effect.gen(function* () {
      tmpPath = path.join(os.tmpdir(), `midgard-unit-${randomUUID()}`);
      const mpt1 = yield* MidgardMpt.create("unit-test-leveldb", tmpPath);
      const emptyRoot = yield* mpt1.getRootHex();
      yield* mpt1.batch([{ type: "put", key: txIdA, value: txCborA }]);
      const rootAfterPut = yield* mpt1.getRootHex();
      yield* Effect.tryPromise({
        try: () => mpt1.databaseAndPath!.database._leveldb.close(),
        catch: (e) => new Error(`${e}`),
      });

      const mpt2 = yield* MidgardMpt.create("unit-test-leveldb", tmpPath);
      const reopenedRoot = yield* mpt2.getRootHex();
      yield* Effect.tryPromise({
        try: () => mpt2.databaseAndPath!.database._leveldb.close(),
        catch: (e) => new Error(`${e}`),
      });

      expect(rootAfterPut).not.toBe(emptyRoot);
      expect(reopenedRoot).toBe(rootAfterPut);
    }),
  );
});
