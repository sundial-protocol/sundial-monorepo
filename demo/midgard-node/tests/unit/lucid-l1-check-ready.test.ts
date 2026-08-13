import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { makeL1ProviderCheckReadyForTesting } from "@/services/lucid.js";

const makeFakeApi = (
  provider: { getProtocolParameters: () => Promise<unknown> } | undefined,
) =>
  ({
    config: () => ({ provider }),
  }) as unknown as Parameters<typeof makeL1ProviderCheckReadyForTesting>[0];

describe("L1 provider readiness check", () => {
  it.effect("succeeds when the provider responds", () =>
    Effect.gen(function* () {
      const api = makeFakeApi({
        getProtocolParameters: () => Promise.resolve({}),
      });

      yield* makeL1ProviderCheckReadyForTesting(api);
    }),
  );

  it.effect("fails with LucidError when the provider call rejects", () =>
    Effect.gen(function* () {
      const api = makeFakeApi({
        getProtocolParameters: () =>
          Promise.reject(new Error("connect ECONNREFUSED")),
      });

      const result = yield* Effect.either(
        makeL1ProviderCheckReadyForTesting(api),
      );

      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left._tag).toBe("LucidError");
      }
    }),
  );

  it.effect("fails with LucidError when no provider is configured", () =>
    Effect.gen(function* () {
      const api = makeFakeApi(undefined);

      const result = yield* Effect.either(
        makeL1ProviderCheckReadyForTesting(api),
      );

      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left._tag).toBe("LucidError");
      }
    }),
  );
});
