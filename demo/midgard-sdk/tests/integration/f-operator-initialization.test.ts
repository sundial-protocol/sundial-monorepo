import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  ActiveOperatorMintRedeemer,
  incompleteActiveOperatorInitTxProgram,
} from "@sdk/active-operators.ts";
import {
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  FraudProofCatalogueDatum,
  incompleteFraudProofCatalogueInitTxProgram,
} from "@sdk/fraud-proof/catalogue.ts";
import {
  getInitializedValidatorsFromMidgardValidators,
  incompleteInitializationTxProgram,
} from "@sdk/initialization.ts";
import { StateQueueDatum } from "@sdk/state-queue.ts";
import { makeFakeLucid } from "./harness/fake-lucid.ts";
import {
  FIXTURE_MIDGARD_VALIDATORS,
  FIXTURE_NONCE_UTXO,
  FIXTURE_VALIDATOR,
} from "./harness/fixtures.ts";

describe("SDK operator and initialization integration", () => {
  it.effect("active-operator init builds linked-list root node", () =>
    Effect.gen(function* () {
      const { lucid } = makeFakeLucid();
      const tx = yield* incompleteActiveOperatorInitTxProgram(lucid as any, {
        validator: FIXTURE_VALIDATOR as any,
      });

      const payCall = (tx as any).__calls.payToAddressWithData[0];
      const mintCall = (tx as any).__calls.mintAssets[0];
      const datum = Data.from(payCall[1].value, StateQueueDatum);
      const redeemer = Data.from(mintCall[1], ActiveOperatorMintRedeemer);

      expect(datum.key).toBe("Empty");
      expect(datum.next).toBe("Empty");
      expect(redeemer).toBe("Init");
    }),
  );

  it.effect("fraud-proof catalogue init encodes provided root hash", () =>
    Effect.gen(function* () {
      const { lucid } = makeFakeLucid();
      const rootHash = "ab".repeat(32);
      const tx = yield* incompleteFraudProofCatalogueInitTxProgram(
        lucid as any,
        {
          validator: FIXTURE_VALIDATOR as any,
          mptRootHash: rootHash,
        },
      );

      const payCall = (tx as any).__calls.payToAddressWithData[0];
      const datum = Data.from(payCall[1].value, FraudProofCatalogueDatum);

      expect(datum).toBe(rootHash);
      expect((tx as any).__calls.attachScript).toHaveLength(1);
      expect((tx as any).__calls.mintAssets).toHaveLength(1);
    }),
  );

  it.effect("validator initialization list preserves expected order", () =>
    Effect.gen(function* () {
      const initialized = getInitializedValidatorsFromMidgardValidators(
        FIXTURE_MIDGARD_VALIDATORS,
      );

      expect(initialized).toHaveLength(7);
      expect(initialized[0]).toBe(FIXTURE_MIDGARD_VALIDATORS.hubOracle);
      expect(initialized[1]).toBe(FIXTURE_MIDGARD_VALIDATORS.stateQueue);
      expect(initialized[6]).toBe(
        FIXTURE_MIDGARD_VALIDATORS.fraudProofCatalogue,
      );
    }),
  );

  it.effect("full initialization composes all contract-init fragments", () =>
    Effect.gen(function* () {
      const { lucid } = makeFakeLucid({ walletUtxos: [FIXTURE_NONCE_UTXO] });
      const tx = yield* incompleteInitializationTxProgram(lucid as any, {
        midgardValidators: FIXTURE_MIDGARD_VALIDATORS,
        fraudProofCatalogueMerkleRoot: "ff".repeat(32),
      });

      const calls = (tx as any).__calls;
      expect(calls.collectFrom).toHaveLength(1);
      expect(calls.validTo).toHaveLength(1);
      expect(calls.compose.length).toBeGreaterThanOrEqual(6);
      expect(calls.compose[0][0]).toBeTruthy();
    }),
  );
});
