// Temporary MPT path helpers for SDK integration tests.
//
// Creates unique LevelDB paths under os.tmpdir() and deletes them after
// the test via the existing MPT cleanup helper from the node source tree.
//
// Use MemoryLevel-backed paths (non-persistent) for most scenarios.
// Use LevelDB temp paths only when the test needs to verify reopen,
// checkpoint, or root-persistence behaviour.
//
// ─── Usage ────────────────────────────────────────────────────────────────────
//
//   const { ledgerPath, mempoolPath, cleanup } = makeTempMptPaths("sdk-int-039");
//   // pass ledgerPath / mempoolPath into makeTestNodeConfigLayer(...)
//   afterEach(() => cleanup());
//
// ─── Implementation notes ─────────────────────────────────────────────────────
//
// TODO (implementation):
//   import * as os from "node:os";
//   import * as path from "node:path";
//   import { randomUUID } from "node:crypto";
//   import { deleteMpt } from "@node/workers/utils/mpt.ts";
//   import { Effect } from "effect";
//
//   export const makeTempMptPaths = (label: string) => {
//     const ledgerPath = path.join(os.tmpdir(), `${label}-ledger-${randomUUID()}`);
//     const mempoolPath = path.join(os.tmpdir(), `${label}-mempool-${randomUUID()}`);
//     const cleanup = async () => {
//       await Effect.runPromise(deleteMpt(ledgerPath, "ledger"));
//       await Effect.runPromise(deleteMpt(mempoolPath, "mempool"));
//     };
//     return { ledgerPath, mempoolPath, cleanup };
//   };
//
// The cleanup function must be called in afterEach (or afterAll) of every
// test that opens a real LevelDB handle.  Not calling cleanup leaks temp
// directories but does not affect test correctness.

import * as os from "node:os";
import * as path from "node:path";
import { randomUUID } from "node:crypto";
import { rm } from "node:fs/promises";

export type TempMptPaths = {
  ledgerPath: string;
  mempoolPath: string;
  /** Call in afterEach to delete temp LevelDB directories. */
  cleanup: () => Promise<void>;
};

/**
 * Creates unique temp LevelDB paths for ledger and mempool MPTs.
 *
 * TODO (implementation): replace the no-op cleanup with real deleteMpt calls:
 *   import { deleteMpt } from "@node/workers/utils/mpt.ts";
 *   import { Effect } from "effect";
 *   const cleanup = async () => {
 *     await Effect.runPromise(deleteMpt(ledgerPath, "ledger"));
 *     await Effect.runPromise(deleteMpt(mempoolPath, "mempool"));
 *   };
 */
export const makeTempMptPaths = (label: string): TempMptPaths => {
  const ledgerPath = path.join(os.tmpdir(), `${label}-ledger-${randomUUID()}`);
  const mempoolPath = path.join(
    os.tmpdir(),
    `${label}-mempool-${randomUUID()}`,
  );

  const cleanup = async (): Promise<void> => {
    await Promise.all([
      rm(ledgerPath, { recursive: true, force: true }),
      rm(mempoolPath, { recursive: true, force: true }),
    ]);
  };

  return { ledgerPath, mempoolPath, cleanup };
};
