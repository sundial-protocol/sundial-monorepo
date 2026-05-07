import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Ref } from "effect";
import { Globals } from "@/services/globals.js";

describe("Globals initializes BLOCKS_IN_QUEUE to 0", () => {
  it.effect("Globals initializes BLOCKS_IN_QUEUE to 0", () =>
    Effect.gen(function* () {
      const globals = yield* Globals;
      const val = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
      expect(val).toBe(0);
    }).pipe(Effect.provide(Globals.Default)),
  );
});

describe("Globals initializes RESET_IN_PROGRESS to false", () => {
  it.effect("Globals initializes RESET_IN_PROGRESS to false", () =>
    Effect.gen(function* () {
      const globals = yield* Globals;
      const val = yield* Ref.get(globals.RESET_IN_PROGRESS);
      expect(val).toBe(false);
    }).pipe(Effect.provide(Globals.Default)),
  );
});

describe("Globals initializes LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH to 0", () => {
  it.effect(
    "Globals initializes LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH to 0",
    () =>
      Effect.gen(function* () {
        const globals = yield* Globals;
        const val = yield* Ref.get(
          globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
        );
        expect(val).toBe(0);
      }).pipe(Effect.provide(Globals.Default)),
  );
});

describe("Globals BLOCKS_IN_QUEUE can be updated", () => {
  it.effect("Globals BLOCKS_IN_QUEUE can be updated", () =>
    Effect.gen(function* () {
      const globals = yield* Globals;
      yield* Ref.set(globals.BLOCKS_IN_QUEUE, 7);
      const val = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
      expect(val).toBe(7);
    }).pipe(Effect.provide(Globals.Default)),
  );
});

describe("Globals RESET_IN_PROGRESS can be toggled", () => {
  it.effect("Globals RESET_IN_PROGRESS can be toggled", () =>
    Effect.gen(function* () {
      const globals = yield* Globals;
      yield* Ref.set(globals.RESET_IN_PROGRESS, true);
      const val = yield* Ref.get(globals.RESET_IN_PROGRESS);
      expect(val).toBe(true);
    }).pipe(Effect.provide(Globals.Default)),
  );
});
