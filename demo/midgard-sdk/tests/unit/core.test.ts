import { afterEach, describe, expect, it, vi } from "vitest";
import { Effect, Either } from "effect";

import * as sdk from "../../src/index.ts";
import {
  makeReturn,
  midgardAddressFromBech32,
  midgardAddressToBech32,
} from "../../src/index.ts";
import {
  addressKeyA,
  addressScriptA,
  assetNameA,
  captureLogs,
  hexA,
  makeLucidMock,
  makeUtxo,
  policyIdA,
  posixT2,
  pubKeyHashA,
  scriptHashA,
  txHashA,
  txHashB,
} from "./helpers.ts";

afterEach(() => {
  vi.restoreAllMocks();
});

describe("SDK unit core helpers", () => {
  it("Public index exports common helpers", () => {
    expect(typeof sdk.isHexString).toBe("function");
    expect(typeof sdk.makeReturn).toBe("function");
    expect(typeof sdk.hashHexWithBlake2b256).toBe("function");
  });

  it("Public index exports user-event helpers", () => {
    expect(typeof sdk.incompleteDepositTxProgram).toBe("function");
    expect(typeof sdk.incompleteWithdrawalTxProgram).toBe("function");
    expect(typeof sdk.incompleteTxOrderTxProgram).toBe("function");
  });

  it("Public index exports fraud-proof helpers", () => {
    expect(typeof sdk.incompleteFraudProofCatalogueInitTxProgram).toBe(
      "function",
    );
    expect(typeof sdk.incompleteFraudProofComputationThreadInitTxProgram).toBe(
      "function",
    );
    expect(typeof sdk.incompleteFraudProofTokenMintTxProgram).toBe("function");
  });

  it("makeReturn unsafeRun resolves successful program", async () => {
    const result = await makeReturn(Effect.succeed("ok")).unsafeRun();
    expect(result).toBe("ok");
  });

  it("makeReturn safeRun wraps successful program", async () => {
    const result = await makeReturn(Effect.succeed("ok")).safeRun();
    expect(result._tag).toBe("Right");
    expect(result.right).toBe("ok");
  });

  it("Accept lowercase hex string", () => {
    expect(sdk.isHexString("deadbeef0123456789")).toBe(true);
  });

  it("Accept uppercase hex string", () => {
    expect(sdk.isHexString("DEADBEEF0123456789")).toBe(true);
  });

  it("Blake2b224 hashes valid hex", async () => {
    const a = await Effect.runPromise(sdk.hashHexWithBlake2b224(hexA));
    const b = await Effect.runPromise(sdk.hashHexWithBlake2b224(hexA));
    expect(a).toHaveLength(56);
    expect(a).toBe(b);
  });

  it("Blake2b256 hashes valid hex", async () => {
    const a = await Effect.runPromise(sdk.hashHexWithBlake2b256(hexA));
    const b = await Effect.runPromise(sdk.hashHexWithBlake2b256(hexA));
    expect(a).toHaveLength(64);
    expect(a).toBe(b);
  });

  it("Buffer converts to hex", () => {
    expect(sdk.bufferToHex(Buffer.from("abcd", "hex"))).toBe("abcd");
  });

  it("Public-key Midgard address converts to bech32 and back", async () => {
    const midgard = { PublicKeyCredential: [pubKeyHashA] } as const;
    const bech32 = midgardAddressToBech32("Preview", midgard);
    const decoded = await Effect.runPromise(midgardAddressFromBech32(bech32));
    expect("PublicKeyCredential" in decoded).toBe(true);
    if ("PublicKeyCredential" in decoded) {
      expect(decoded.PublicKeyCredential[0]).toBe(pubKeyHashA);
    }
  });

  it("Script Midgard address converts to bech32 and back", async () => {
    const midgard = { ScriptCredential: [scriptHashA] } as const;
    const bech32 = midgardAddressToBech32("Preview", midgard);
    const decoded = await Effect.runPromise(midgardAddressFromBech32(bech32));
    expect("ScriptCredential" in decoded).toBe(true);
    if ("ScriptCredential" in decoded) {
      expect(decoded.ScriptCredential[0]).toBe(scriptHashA);
    }
  });

  it("AddressData extracts payment credential", async () => {
    const decoded = await Effect.runPromise(
      sdk.addressDataFromBech32(addressKeyA),
    );
    expect("PublicKeyCredential" in decoded.paymentCredential).toBe(true);
    if ("PublicKeyCredential" in decoded.paymentCredential) {
      expect(decoded.paymentCredential.PublicKeyCredential[0]).toBe(
        pubKeyHashA,
      );
    }
  });

  it("Find active operator by public-key hash", async () => {
    const active = [
      {
        utxo: makeUtxo({ txHash: txHashA, outputIndex: 0 }),
        datum: { key: pubKeyHashA, link: null, bondUnlockTime: null },
        assetName: assetNameA,
      },
    ];
    const retired = [
      {
        utxo: makeUtxo({ txHash: txHashB, outputIndex: 1 }),
        datum: { key: "99".repeat(28), link: null, bondUnlockTime: null },
        assetName: assetNameA,
      },
    ];
    const found = await Effect.runPromise(
      sdk.findOperatorByPKH(active as any, retired as any, pubKeyHashA),
    );
    expect(found.isActive).toBe(true);
    expect(found.datum.key).toBe(pubKeyHashA);
  });

  it("Find retired operator by public-key hash", async () => {
    const retired = [
      {
        utxo: makeUtxo({ txHash: txHashB, outputIndex: 1 }),
        datum: { key: pubKeyHashA, link: scriptHashA, bondUnlockTime: posixT2 },
        assetName: assetNameA,
      },
    ];
    const found = await Effect.runPromise(
      sdk.findOperatorByPKH([], retired as any, pubKeyHashA),
    );
    expect(found.isActive).toBe(false);
    expect(found.datum.key).toBe(pubKeyHashA);
  });

  it("Mainnet protocol parameters are selected", () => {
    const params = sdk.getProtocolParameters("Mainnet");
    expect(params.event_wait_duration).toBe(60_000);
    expect(params.maturity_duration).toBe(30n);
    expect(params.slashing_penalty).toBe(2_000_000n);
  });

  it("Testnet protocol parameters are selected", () => {
    const params = sdk.getProtocolParameters("Preview");
    expect(params.event_wait_duration).toBe(50_000);
    expect(params.maturity_duration).toBe(1n);
    expect(params.slashing_penalty).toBe(1_000_000n);
  });

  it("Asset-name constants encode expected labels", () => {
    const values = [
      sdk.ESCAPE_HATCH_ASSET_NAME,
      sdk.SCHEDULER_ASSET_NAME,
      sdk.HUB_ORACLE_ASSET_NAME,
      sdk.FRAUD_PROOF_CATALOGUE_ASSET_NAME,
    ];

    for (const value of values) {
      expect(typeof value).toBe("string");
      expect(value.length).toBeGreaterThan(0);
      expect(/^[a-zA-Z0-9_]+$/.test(value)).toBe(true);
    }
  });

  describe("utxosAtByNFTPolicyId", () => {
    it("logs a warning with dropped UTxO details and returns only authentic ones", async () => {
      const authenticUtxo = makeUtxo({ txHash: txHashA, outputIndex: 0 });
      const otherPolicyUtxo = makeUtxo({
        txHash: txHashB,
        outputIndex: 1,
        unit: `${"bb".repeat(28)}${assetNameA}`,
      });
      const lucid = makeLucidMock();
      lucid.utxosAt.mockResolvedValue([authenticUtxo, otherPolicyUtxo]);

      const { entries, layer } = captureLogs();

      const result = await Effect.runPromise(
        sdk
          .utxosAtByNFTPolicyId(lucid as any, addressScriptA, policyIdA)
          .pipe(Effect.provide(layer)),
      );

      expect(result).toHaveLength(1);
      expect(result[0].utxo.txHash).toBe(txHashA);

      const warning = entries.find(
        (entry) =>
          typeof entry[0] === "string" &&
          entry[0].includes("dropped 1 invalid UTxO"),
      );
      expect(warning).toBeDefined();
      const details = warning?.[1] as Array<{ utxo: string; tag: string }>;
      expect(details).toHaveLength(1);
      expect(details[0].utxo).toBe(`${txHashB}#1`);
      expect(details[0].tag).toBe("UnauthenticUtxoError");
    });
  });

  describe("utxoAtByNFTUnit", () => {
    const unit = `${policyIdA}${assetNameA}`;
    const utxo = makeUtxo({ txHash: txHashA, outputIndex: 0 });

    it("succeeds when provider returns exactly one UTxO", async () => {
      const lucid = makeLucidMock();
      lucid.utxosAtWithUnit.mockResolvedValue([utxo]);

      const result = await Effect.runPromise(
        sdk.utxoAtByNFTUnit(lucid as any, addressScriptA, unit),
      );

      expect(result.policyId).toBe(policyIdA);
      expect(result.assetName).toBe(assetNameA);
      expect(result.utxo.txHash).toBe(txHashA);
      expect(lucid.utxosAtWithUnit).toHaveBeenCalledWith(addressScriptA, unit);
    });

    it("fails with LucidError when provider returns no UTxOs", async () => {
      const lucid = makeLucidMock();
      lucid.utxosAtWithUnit.mockResolvedValue([]);

      const result = await Effect.runPromise(
        Effect.either(sdk.utxoAtByNFTUnit(lucid as any, addressScriptA, unit)),
      );

      expect(Either.isLeft(result)).toBe(true);
      if (Either.isLeft(result)) {
        expect(result.left._tag).toBe("LucidError");
        expect(result.left.message).toContain("got 0");
      }
    });

    it("fails with LucidError when provider returns more than one UTxO", async () => {
      const lucid = makeLucidMock();
      lucid.utxosAtWithUnit.mockResolvedValue([utxo, utxo]);

      const result = await Effect.runPromise(
        Effect.either(sdk.utxoAtByNFTUnit(lucid as any, addressScriptA, unit)),
      );

      expect(Either.isLeft(result)).toBe(true);
      if (Either.isLeft(result)) {
        expect(result.left._tag).toBe("LucidError");
        expect(result.left.message).toContain("got 2");
      }
    });

    it("fails with LucidError when provider throws", async () => {
      const lucid = makeLucidMock();
      lucid.utxosAtWithUnit.mockRejectedValue(new Error("network failure"));

      const result = await Effect.runPromise(
        Effect.either(sdk.utxoAtByNFTUnit(lucid as any, addressScriptA, unit)),
      );

      expect(Either.isLeft(result)).toBe(true);
      if (Either.isLeft(result)) {
        expect(result.left._tag).toBe("LucidError");
      }
    });

    it("parses policyId and assetName from unit correctly", async () => {
      const expectedPolicy = "aa".repeat(28);
      const expectedAsset = "4e6f6465" + "bb".repeat(28);
      const testUnit = `${expectedPolicy}${expectedAsset}`;
      const lucid = makeLucidMock();
      lucid.utxosAtWithUnit.mockResolvedValue([utxo]);

      const result = await Effect.runPromise(
        sdk.utxoAtByNFTUnit(lucid as any, addressScriptA, testUnit),
      );

      expect(result.policyId).toBe(expectedPolicy);
      expect(result.assetName).toBe(expectedAsset);
    });
  });
});
