import { describe, expect, afterEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import * as os from "os";
import * as path from "path";
import { randomUUID } from "crypto";
import {
  MptError,
  MidgardMpt,
  deleteMpt,
  emptyRootHexProgram,
  withTrieTransaction,
} from "@/workers/utils/mpt.js";
import { WorkerError } from "@/workers/utils/common.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

const EMPTY_ROOT =
  "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421";

const mockDbLayer = createMockSqlHarness().layer;

describe("MptError constructors include trie name", () => {
  const ctorCases = [
    ["get", () => MptError.get("myTrie", new Error("cause"))],
    ["put", () => MptError.put("myTrie", new Error("cause"))],
    ["batch", () => MptError.batch("myTrie", new Error("cause"))],
    ["del", () => MptError.del("myTrie", new Error("cause"))],
    ["trieCreate", () => MptError.trieCreate("myTrie", new Error("cause"))],
    ["trieCommit", () => MptError.trieCommit("myTrie", new Error("cause"))],
    ["trieRevert", () => MptError.trieRevert("myTrie", new Error("cause"))],
    ["rootNotSet", () => MptError.rootNotSet("myTrie", null)],
  ] as const;

  for (const [name, makeError] of ctorCases) {
    it(`${name} creates MptError with trie name`, () => {
      const err = makeError();
      expect(err).toBeInstanceOf(MptError);
      expect(err.message).toContain("myTrie");
    });
  }
});

describe("emptyRootHexProgram returns the canonical empty root", () => {
  it.effect("emptyRootHexProgram returns the canonical empty root", () =>
    Effect.gen(function* () {
      const root = yield* emptyRootHexProgram;
      expect(root).toBe(EMPTY_ROOT);
    }),
  );
});

describe("MidgardMpt.delete on in-memory trie succeeds", () => {
  it.effect("MidgardMpt.delete on in-memory trie succeeds", () =>
    Effect.gen(function* () {
      const mpt = yield* MidgardMpt.create("delete-test");
      yield* mpt.delete();
    }),
  );
});

describe("MidgardMpt.databaseStats returns stats object", () => {
  it.effect("MidgardMpt.databaseStats returns stats object", () =>
    Effect.gen(function* () {
      const mpt = yield* MidgardMpt.create("stats-test");
      const stats = mpt.databaseStats();
      expect(typeof stats).toBe("object");
      expect(stats).not.toBeNull();
    }),
  );
});

describe("deleteMpt on nonexistent path succeeds with force", () => {
  it.effect("deleteMpt on nonexistent path succeeds with force", () =>
    Effect.gen(function* () {
      const tmpPath = path.join(
        os.tmpdir(),
        `midgard-del-nonexistent-${randomUUID()}`,
      );
      yield* deleteMpt(tmpPath, "test");
    }),
  );
});

describe("deleteMpt on existing directory removes it", () => {
  let tmpPath: string;

  afterEach(async () => {
    if (tmpPath) {
      await Effect.runPromise(deleteMpt(tmpPath, "cleanup"));
    }
  });

  it.effect("deleteMpt on existing directory removes it", () =>
    Effect.gen(function* () {
      tmpPath = path.join(os.tmpdir(), `midgard-del-exists-${randomUUID()}`);
      const mpt = yield* MidgardMpt.create("del-exists", tmpPath);
      yield* Effect.tryPromise({
        try: () => mpt.databaseAndPath!.database._leveldb.close(),
        catch: (e) => new Error(`${e}`),
      });
      yield* deleteMpt(tmpPath, "del-exists");
    }),
  );
});

describe("withTrieTransaction commits on success", () => {
  it.effect("withTrieTransaction commits on success", () =>
    Effect.gen(function* () {
      const mpt = yield* MidgardMpt.create("with-tx-test");
      const result = yield* withTrieTransaction(mpt, Effect.succeed("ok")).pipe(
        Effect.provide(mockDbLayer),
      );
      expect(result).toBe("ok");
      const isEmpty = yield* mpt.rootIsEmpty();
      expect(isEmpty).toBe(true);
    }),
  );
});

describe("WorkerError has expected tag and worker field", () => {
  it("WorkerError has expected tag and worker field", () => {
    const err = new WorkerError({
      message: "something went wrong",
      cause: undefined,
      worker: "block-commitment",
    });
    expect(err._tag).toBe("WorkerError");
    expect(err.message).toBe("something went wrong");
    expect(err.worker).toBe("block-commitment");
  });
});
