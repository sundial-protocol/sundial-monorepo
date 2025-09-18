import { ConfigError, Effect } from "effect";
import { Database } from "../src/services/database.js";
import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import {
  BlocksDB,
  ConfirmedLedgerDB,
  ImmutableDB,
  LatestLedgerDB,
  MempoolDB,
  MempoolLedgerDB,
  InitDB,
} from "../src/database/index.js";
import {
  LevelDB,
  MptError,
  makeMpts,
  processMpts,
  withTrieTransaction,
} from "../src/workers/utils/mpt.js";
import * as ETH from "@ethereumjs/mpt";
import * as ETH_UTILS from "@ethereumjs/util";
import { NodeConfig, User } from "../src/config.js";
import { TestServices } from "effect/TestServices";
import { DBCreateError, DBOtherError } from "@/database/utils/common.js";
import { SqlClient, SqlError } from "@effect/sql";
import { UTxO, toHex, utxoToCore } from "@lucid-evolution/lucid";

import dotenv from "dotenv";
import { NodeRuntime } from "@effect/platform-node";
dotenv.config({ path: ".env" });

ETH.ROOT_DB_KEY

describe("The makeMpts tests", () => {
  it.effect("Trivial makeMpts", (_) =>
    Effect.gen( function* () {
      yield* InitDB.initializeDb();
      const { ledgerTrie, mempoolTrie } = yield* makeMpts;
      const { utxoRoot, txRoot } = yield* processMpts(
        ledgerTrie,
        mempoolTrie,
        [],
      );

      expect(utxoRoot).toBe(toHex(ledgerTrie.EMPTY_TRIE_ROOT))
      expect(txRoot).toBe(toHex(ledgerTrie.EMPTY_TRIE_ROOT))
    }).pipe(
      Effect.provide(Database.layer),
      Effect.provide(User.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );
});
