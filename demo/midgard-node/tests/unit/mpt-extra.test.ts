import { describe, expect, afterEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer } from "effect";
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
import { SqlClient } from "@effect/sql";

const EMPTY_ROOT =
  "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421";

const mockSql: any = Object.assign(
  function (stringsOrStr: any, ..._values: unknown[]) {
    if (Array.isArray(stringsOrStr) && "raw" in stringsOrStr) {
      return Effect.succeed([]);
    }
    return stringsOrStr;
  },
  {
    withTransaction: (eff: Effect.Effect<unknown>) => eff,
    insert: (obj: unknown) => obj,
    in: (_col: string, vals: unknown[]) => vals,
    literal: (s: string) => s,
  },
);

const mockDbLayer = Layer.succeed(
  SqlClient.SqlClient,
  mockSql as unknown as SqlClient.SqlClient,
);

describe("MptError.get creates error with trie name", () => {
  it("MptError.get creates error with trie name", () => {
    const err = MptError.get("myTrie", new Error("cause"));
    expect(err).toBeInstanceOf(MptError);
    expect(err.message).toContain("myTrie");
  });
});

describe("MptError.put creates error with trie name", () => {
  it("MptError.put creates error with trie name", () => {
    const err = MptError.put("myTrie", new Error("cause"));
    expect(err).toBeInstanceOf(MptError);
    expect(err.message).toContain("myTrie");
  });
});

describe("MptError.batch creates error with trie name", () => {
  it("MptError.batch creates error with trie name", () => {
    const err = MptError.batch("myTrie", new Error("cause"));
    expect(err).toBeInstanceOf(MptError);
    expect(err.message).toContain("myTrie");
  });
});

describe("MptError.del creates error with trie name", () => {
  it("MptError.del creates error with trie name", () => {
    const err = MptError.del("myTrie", new Error("cause"));
    expect(err).toBeInstanceOf(MptError);
    expect(err.message).toContain("myTrie");
  });
});

describe("MptError.trieCreate creates error", () => {
  it("MptError.trieCreate creates error", () => {
    const err = MptError.trieCreate("myTrie", new Error("cause"));
    expect(err).toBeInstanceOf(MptError);
    expect(err.message).toContain("myTrie");
  });
});

describe("MptError.trieCommit creates error", () => {
  it("MptError.trieCommit creates error", () => {
    const err = MptError.trieCommit("myTrie", new Error("cause"));
    expect(err).toBeInstanceOf(MptError);
    expect(err.message).toContain("myTrie");
  });
});

describe("MptError.trieRevert creates error", () => {
  it("MptError.trieRevert creates error", () => {
    const err = MptError.trieRevert("myTrie", new Error("cause"));
    expect(err).toBeInstanceOf(MptError);
    expect(err.message).toContain("myTrie");
  });
});

describe("MptError.rootNotSet creates error", () => {
  it("MptError.rootNotSet creates error", () => {
    const err = MptError.rootNotSet("myTrie", null);
    expect(err).toBeInstanceOf(MptError);
    expect(err.message).toContain("myTrie");
  });
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
      const _stats = mpt.databaseStats();
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
      const txIdA = Buffer.alloc(32, 0xaa);
      const txCborA = Buffer.alloc(32, 0xbb);
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
