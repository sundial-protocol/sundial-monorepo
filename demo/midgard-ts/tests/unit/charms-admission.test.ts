/**
 * Stage one of script support (docs-v2 L2-1): with `admitUnevaluatedScripts`
 * the node admits transactions that mint and carry Plutus V3 scripts, without
 * evaluating them, so a Charms beam-receive can land on the L2.
 *
 * Fixtures (tests/unit/fixtures):
 * - charms-l2-mint-2026-08-12.hex: a real Charms Cardano transaction (a mint
 *   co-signed by Scrolls, built for the Sundial L2 testnet on 2026-08-12 in
 *   charms-test). Like every transaction Charms' Cardano builder makes, it has
 *   a collateral input.
 * - charms-l2-mint-2026-08-12.no-collateral.hex: the same transaction with
 *   collateral_inputs removed at the byte level, everything else encoded
 *   exactly as Charms encoded it. required_signers and the vkey witnesses were
 *   removed too, because the witnesses signed the original body and no longer
 *   verify. It still mints under the charms-cardano policy and carries the
 *   redeemers, the Plutus V3 script, the script_data_hash, the Scrolls
 *   zero-ADA withdrawal and the reference input.
 */
import * as CML from "@dcspark/cardano-multiplatform-lib-nodejs";
import { readFileSync } from "fs";
import { join } from "path";
import {
  ConversionError,
  cmlToMidgard,
  midgardToCml,
  midgardValueToCml,
} from "../../src/cardano";
import { runPhaseAValidation } from "../../src/validation/phase-a";
import { runPhaseBValidationWithPatch } from "../../src/validation/phase-b";
import {
  type PhaseAAccepted,
  type PhaseAConfig,
  type UTxOState,
  RejectCodes,
} from "../../src/validation/types";
import {
  buildSimpleAdaTransferFixture,
  filledBytes,
} from "./helpers/cardano-fixtures";

const loadTx = (name: string): CML.Transaction =>
  CML.Transaction.from_cbor_hex(
    readFileSync(join(__dirname, "fixtures", name), "utf8").trim(),
  );

// The node's settings for a non-mainnet network (see midgard-node mempool.ts).
const testnetConfig: PhaseAConfig = {
  expectedNetworkId: 0,
  cardanoNetwork: 0,
  minFeeA: 44n,
  minFeeB: 155381n,
};

describe("Charms transactions", () => {
  it("are rejected at conversion while they carry collateral", () => {
    const cmlTx = loadTx("charms-l2-mint-2026-08-12.hex");
    expect(() => cmlToMidgard(cmlTx)).toThrow(ConversionError);
    expect(() => cmlToMidgard(cmlTx)).toThrow(/Collateral/);
  });

  const cmlTx = loadTx("charms-l2-mint-2026-08-12.no-collateral.hex");
  const txId = CML.hash_transaction(cmlTx.body()).to_raw_bytes();
  const queued = { txId, tx: cmlToMidgard(cmlTx), arrivalSeq: 0n };

  it("convert without collateral and keep the Cardano transaction id (R2)", () => {
    const result = runPhaseAValidation([queued], {
      ...testnetConfig,
      admitUnevaluatedScripts: true,
    });
    expect(result.rejected).toEqual([]);
    expect(result.accepted.length).toBe(1);
    expect(Buffer.from(result.accepted[0].txId)).toEqual(Buffer.from(txId));
  });

  it("are rejected by default (R23, no minting)", () => {
    const result = runPhaseAValidation([queued], testnetConfig);
    expect(result.accepted.length).toBe(0);
    expect(result.rejected[0].code).toBe(RejectCodes.MintForbidden);
  });
});

describe("R3 with admitUnevaluatedScripts unset", () => {
  it("still rejects redeemers on a transaction that does not mint", () => {
    const cmlTx = loadTx("charms-l2-mint-2026-08-12.no-collateral.hex");
    const tx = cmlToMidgard(cmlTx);
    const noMint = { ...tx, body: { ...tx.body, mint: undefined } };
    // R2 runs first, so give it the id of the edited body.
    const txId = CML.hash_transaction(
      midgardToCml(noMint, 0).body(),
    ).to_raw_bytes();
    const result = runPhaseAValidation(
      [{ txId, tx: noMint, arrivalSeq: 0n }],
      testnetConfig,
    );
    expect(result.rejected[0].code).toBe(RejectCodes.UnsupportedFieldNonEmpty);
  });
});

// ---------------------------------------------------------------------------
// R12 with minting and burning
// ---------------------------------------------------------------------------

const policy = filledBytes(28, 0xcc);
const assetName = new TextEncoder().encode("CHARM");

const multiassetValue = (coin: bigint, amount: bigint) =>
  amount === 0n
    ? { type: "Coin" as const, coin }
    : {
        type: "MultiAsset" as const,
        coin,
        assets: [[policy, [[assetName, amount]]]] as [
          Uint8Array,
          [Uint8Array, bigint][],
        ][],
      };

/**
 * One candidate spending `inputValue` at a key address and producing
 * `outputValue`, with `mintAmount` of policy.CHARM (negative burns).
 */
const runR12 = (
  inputAsset: bigint,
  outputAsset: bigint,
  mintAmount: bigint,
) => {
  const { addressBytes, paymentKeyHashBytes } = buildSimpleAdaTransferFixture();
  const keyHashHex =
    CML.Ed25519KeyHash.from_raw_bytes(paymentKeyHashBytes).to_hex();
  const spentOutRef = { tx_id: filledBytes(32, 0x11), index: 0 };
  const txId = filledBytes(32, 0x99);
  const fee = 170_000n;
  const spentOutput = {
    address: addressBytes,
    value: multiassetValue(2_000_000n, inputAsset),
    datum: undefined,
    script_ref: undefined,
  };
  const producedOutput = {
    address: addressBytes,
    value: multiassetValue(2_000_000n - fee, outputAsset),
    datum: undefined,
    script_ref: undefined,
  };
  const tx = {
    body: {
      inputs: [spentOutRef],
      outputs: [producedOutput],
      fee,
      ttl: undefined,
      auxiliary_data_hash: undefined,
      validity_interval_start: undefined,
      mint:
        mintAmount === 0n
          ? undefined
          : ([[policy, [[assetName, mintAmount]]]] as [
              Uint8Array,
              [Uint8Array, bigint][],
            ][]),
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
  const accepted: PhaseAAccepted = {
    txId,
    tx,
    arrivalSeq: 0n,
    fee,
    validityIntervalStart: undefined,
    validityIntervalEnd: undefined,
    referenceInputs: [],
    outputSum: midgardValueToCml(producedOutput.value),
    witnessKeyHashes: [keyHashHex],
    nativeScriptHashes: [],
    spent: [spentOutRef],
    produced: [{ outRef: { tx_id: txId, index: 0 }, output: producedOutput }],
  };
  const state: UTxOState = new Map([
    [`${Buffer.from(spentOutRef.tx_id).toString("hex")}:0`, spentOutput],
  ]);
  return runPhaseBValidationWithPatch([accepted], state, { nowSlot: 0 });
};

describe("R12 counts minted and burned value", () => {
  it("accepts a balanced mint", () => {
    const r = runR12(0n, 5n, 5n);
    expect(r.rejected).toEqual([]);
    expect(r.accepted.length).toBe(1);
  });

  it("accepts a balanced burn", () => {
    const r = runR12(5n, 2n, -3n);
    expect(r.rejected).toEqual([]);
    expect(r.accepted.length).toBe(1);
  });

  it("rejects outputs holding more than was minted", () => {
    const r = runR12(0n, 6n, 5n);
    expect(r.rejected[0].code).toBe(RejectCodes.ValueNotPreserved);
  });

  it("rejects a burn whose tokens are still in the outputs", () => {
    const r = runR12(5n, 5n, -3n);
    expect(r.rejected[0].code).toBe(RejectCodes.ValueNotPreserved);
  });

  it("rejects tokens that appear without a mint", () => {
    const r = runR12(0n, 5n, 0n);
    expect(r.rejected[0].code).toBe(RejectCodes.ValueNotPreserved);
  });
});
