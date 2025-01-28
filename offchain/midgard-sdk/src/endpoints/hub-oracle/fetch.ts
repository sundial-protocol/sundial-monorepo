import { Effect } from "effect";
import {
  Address,
  LucidEvolution,
  PolicyId,
  UTxO,
  toUnit,
} from "@lucid-evolution/lucid";
import { makeReturn } from "@/core.js";

export type Config = {
  hubOracleAddress: Address;
  hubOraclePolicyId: PolicyId;
};

// TODO: This should ideally come from Aiken env directory.
export const hubOracleAssetName = "";

export const fetchHubOracleUTxOProgram = (
  lucid: LucidEvolution,
  config: Config
): Effect.Effect<UTxO, string> =>
  Effect.gen(function* () {
    const hubOracleUTxOs = yield* Effect.tryPromise({
      try: async () => {
        return await lucid.utxosAtWithUnit(
          config.hubOracleAddress,
          toUnit(config.hubOraclePolicyId, hubOracleAssetName)
        );
      },
      catch: (_) => "Failed to fetch the hub oracle UTxO",
    });
    if (hubOracleUTxOs.length === 1) {
      return hubOracleUTxOs[0];
    } else {
      return yield* Effect.fail(
        "Exactly one hub oracle UTxO was expected, but none or more were found"
      );
    }
  });

/**
 * Attempts fetching the hub oracle UTxO.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param config - Configuration values required to know where to look for which NFT.
 * @returns {UTxO} - The authentic hub oracle UTxO.
 */
export const fetchHubOracleUTxO = (lucid: LucidEvolution, config: Config) =>
  makeReturn(fetchHubOracleUTxOProgram(lucid, config)).unsafeRun();
