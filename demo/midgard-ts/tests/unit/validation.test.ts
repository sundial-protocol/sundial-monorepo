import * as CML from "@dcspark/cardano-multiplatform-lib-nodejs";
import { cmlToMidgard } from "../../src/cardano";
import { runPhaseAValidation, outRefKey } from "../../src/validation/phase-a";
import { runPhaseBValidationWithPatch } from "../../src/validation/phase-b";
import {
  buildSimpleAdaTransferFixture,
  filledBytes,
} from "./helpers/cardano-fixtures";
import {
  type QueuedTx,
  type PhaseAAccepted,
  type PhaseAConfig,
  type PhaseBConfig,
  type UTxOState,
} from "../../src/validation/types";

it("Phase A accepts one hash-matched local transaction", () => {
  const { cmlTx, txId } = buildSimpleAdaTransferFixture();

  // Convert to Midgard representation
  const midgardTx = cmlToMidgard(cmlTx);

  // Build the queued transaction
  const queued: QueuedTx = { txId, tx: midgardTx, arrivalSeq: 0n };

  // Config with zero fees so the fixture fee of 170_000n always passes R11
  const config: PhaseAConfig = {
    expectedNetworkId: 1,
    cardanoNetwork: 1,
    minFeeA: 0n,
    minFeeB: 0n,
  };

  // Run Phase A
  const result = runPhaseAValidation([queued], config);

  expect(result.accepted.length).toBe(1);
  expect(result.rejected.length).toBe(0);
  // One output produced: {tx_id: txId, index: 0}
  expect(result.accepted[0].produced.length).toBe(1);
});

it("Phase B accepts one in-memory candidate and materializes patch", () => {
  const { addressBytes, paymentKeyHashBytes } = buildSimpleAdaTransferFixture();
  const keyHashHex =
    CML.Ed25519KeyHash.from_raw_bytes(paymentKeyHashBytes).to_hex();

  // Output references
  const spentOutRef = { tx_id: filledBytes(32, 0x11), index: 0 };
  const txId = filledBytes(32, 0x99);
  const producedOutRef = { tx_id: txId, index: 0 };

  // Spent UTxO: 2_000_000n lovelace at the enterprise address
  const spentOutput = {
    address: addressBytes,
    value: { type: "Coin" as const, coin: 2_000_000n },
    datum: undefined,
    script_ref: undefined,
  };

  // Produced UTxO: 1_830_000n lovelace (2_000_000 - 170_000 fee)
  const producedOutput = {
    address: addressBytes,
    value: { type: "Coin" as const, coin: 1_830_000n },
    datum: undefined,
    script_ref: undefined,
  };

  // Minimal Transaction for the PhaseAAccepted fixture
  const tx = {
    body: {
      inputs: [spentOutRef],
      outputs: [producedOutput],
      fee: 170_000n,
      ttl: undefined,
      auxiliary_data_hash: undefined,
      validity_interval_start: undefined,
      mint: undefined,
      script_data_hash: undefined,
      required_signers: undefined,
      network_id: undefined,
      reference_inputs: undefined,
      required_observers: undefined,
    },
    witness_set: {
      vkey_witnesses: undefined,
      native_scripts: undefined,
      redeemers: undefined,
      plutus_v3_scripts: undefined,
    },
    is_valid: true,
  };

  // Build the PhaseAAccepted fixture directly (bypassing Phase A)
  const accepted: PhaseAAccepted = {
    txId,
    tx,
    arrivalSeq: 0n,
    fee: 170_000n,
    validityIntervalStart: undefined,
    validityIntervalEnd: undefined,
    referenceInputs: [],
    // outputSum = Σproduced outputs (excluding fee), used for R12
    outputSum: CML.Value.from_coin(1_830_000n),
    witnessKeyHashes: [keyHashHex],
    nativeScriptHashes: [],
    spent: [spentOutRef],
    produced: [{ outRef: producedOutRef, output: producedOutput }],
  };

  // UTxO pre-state: the spent input must exist
  const utxoState: UTxOState = new Map();
  utxoState.set(outRefKey(spentOutRef), spentOutput);

  // Phase B config: fixed wall-clock well inside any validity interval
  const phaseBConfig: PhaseBConfig = { nowMillis: 1_000_000 };

  // Run Phase B
  const result = runPhaseBValidationWithPatch([accepted], utxoState, phaseBConfig);

  expect(result.accepted.length).toBe(1);
  expect(result.rejected.length).toBe(0);

  // State patch: spent input deleted, produced output upserted
  expect(result.statePatch.deletedOutRefs.length).toBe(1);
  expect(result.statePatch.upsertedOutRefs.length).toBe(1);
  expect(result.statePatch.deletedOutRefs[0]).toBe(outRefKey(spentOutRef));
  expect(result.statePatch.upsertedOutRefs[0][0]).toBe(outRefKey(producedOutRef));
});
