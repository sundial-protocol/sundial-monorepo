import { afterEach, describe, expect, it, vi } from "vitest";
import { Effect } from "effect";
import { toUnit } from "@lucid-evolution/lucid";

import {
  getDatumFromUTxO,
  getStateToken,
  authenticateUTxO,
  authenticateUTxOs,
  fetchSingleAuthenticUTxOProgram,
} from "../../src/internals.ts";
import {
  buildUserEventMintTransaction,
  fetchUserEventUTxOsProgram,
  findInclusionTimeForUserEvent,
  getNonceInputAndAssetName,
} from "../../src/user-events/internals.ts";
import {
  DepositDatum,
  TxOrderDatum,
  WithdrawalOrderDatum,
  utxosToDepositUTxOs,
  utxosToTxOrderUTxOs,
  utxosToWithdrawalUTxOs,
} from "../../src/index.ts";
import {
  addressDataKeyA,
  addressScriptA,
  policyIdA,
  posixT0,
  posixT1,
  posixT2,
  posixT3,
  txHashA,
  txHashB,
  makeBuilderSpy,
  makeLucidMock,
  makeUtxo,
  validDepositDatumCbor,
  validTxOrderDatumCbor,
  validWithdrawalDatumCbor,
  validatorA,
  assetNameA,
} from "./helpers.ts";

afterEach(() => {
  vi.restoreAllMocks();
});

describe("SDK unit internals and user-event programs", () => {
  it("getStateToken returns single NFT", async () => {
    const unit = toUnit(policyIdA, assetNameA);
    const [policyId, assetName] = await Effect.runPromise(
      getStateToken({ lovelace: 2_000_000n, [unit]: 1n }),
    );
    expect(policyId).toBe(policyIdA);
    expect(assetName).toBe(assetNameA);
  });

  it("getDatumFromUTxO decodes valid inline datum", async () => {
    const utxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validDepositDatumCbor,
    });
    const decoded = await Effect.runPromise(
      getDatumFromUTxO(utxo as any, DepositDatum),
    );
    expect(decoded.event.id.txHash.hash).toBe(txHashA);
    expect(decoded.inclusionTime).toBe(posixT1);
  });

  it("authenticateUTxO returns authentic datum and asset name", async () => {
    const utxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validDepositDatumCbor,
    });
    const result = await Effect.runPromise(
      authenticateUTxO(utxo as any, policyIdA, DepositDatum),
    );
    expect(result.assetName).toBe(assetNameA);
    expect(result.datum.event.id.txHash.hash).toBe(txHashA);
  });

  it("authenticateUTxO adds extra fields", async () => {
    const utxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validDepositDatumCbor,
    });
    const result = await Effect.runPromise(
      authenticateUTxO(utxo as any, policyIdA, DepositDatum, () => ({
        extra: "ok",
      })),
    );
    expect(result.extra).toBe("ok");
    expect(result.datum.inclusionTime).toBe(posixT1);
  });

  it("authenticateUTxOs returns all valid UTxOs", async () => {
    const utxoA = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validDepositDatumCbor,
    });
    const utxoB = makeUtxo({
      txHash: txHashB,
      outputIndex: 1,
      datum: validDepositDatumCbor,
    });
    const result = await Effect.runPromise(
      authenticateUTxOs([utxoA as any, utxoB as any], policyIdA, DepositDatum),
    );
    expect(result).toHaveLength(2);
    expect(result[0].utxo.txHash).toBe(txHashA);
    expect(result[1].utxo.txHash).toBe(txHashB);
  });

  it("fetchSingleAuthenticUTxOProgram returns one converted UTxO", async () => {
    const lucid = makeLucidMock();
    const rawUtxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validDepositDatumCbor,
    });
    lucid.utxosAt.mockResolvedValue([rawUtxo]);
    const converted = [{ id: "one" }];
    const conversionFunction = vi.fn(() => Effect.succeed(converted));

    const result = await Effect.runPromise(
      fetchSingleAuthenticUTxOProgram(lucid as any, {
        address: addressScriptA,
        policyId: policyIdA,
        utxoLabel: "test",
        conversionFunction,
        onUnexpectedAuthenticUTxOCount: () => new Error("bad count"),
      }),
    );

    expect(result).toEqual({ id: "one" });
    expect(lucid.utxosAt).toHaveBeenCalledWith(addressScriptA);
    expect(conversionFunction).toHaveBeenCalledTimes(1);
  });

  it("fetchUserEventUTxOsProgram filters inside inclusion window", async () => {
    const lucid = makeLucidMock();
    lucid.utxosAt.mockResolvedValue([
      makeUtxo({ txHash: txHashA, outputIndex: 0 }),
    ]);

    const conversionFunction = vi.fn(() =>
      Effect.succeed([
        { datum: { inclusionTime: posixT0 }, id: "too-early" },
        { datum: { inclusionTime: posixT1 }, id: "keep" },
        { datum: { inclusionTime: posixT3 }, id: "too-late" },
      ]),
    );

    const result = await Effect.runPromise(
      fetchUserEventUTxOsProgram(
        lucid as any,
        {
          eventAddress: addressScriptA,
          eventPolicyId: policyIdA,
          inclusionTimeLowerBound: posixT1,
          inclusionTimeUpperBound: posixT2,
        },
        conversionFunction,
      ),
    );

    expect(result).toHaveLength(1);
    expect(result[0].id).toBe("keep");
  });

  it("Deposit UTxO conversion adds derived extra fields", async () => {
    const utxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validDepositDatumCbor,
    });
    const result = await Effect.runPromise(
      utxosToDepositUTxOs([utxo as any], policyIdA),
    );
    expect(result).toHaveLength(1);
    expect(result[0].idCbor).toBeInstanceOf(Buffer);
    expect(result[0].infoCbor).toBeInstanceOf(Buffer);
    expect(result[0].inclusionTime).toBeInstanceOf(Date);
  });

  it("Tx-order UTxO conversion adds derived extra fields", async () => {
    const utxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validTxOrderDatumCbor,
    });
    const result = await Effect.runPromise(
      utxosToTxOrderUTxOs([utxo as any], policyIdA),
    );
    expect(result).toHaveLength(1);
    expect(result[0].idCbor).toBeInstanceOf(Buffer);
    expect(result[0].infoCbor).toBeInstanceOf(Buffer);
    expect(result[0].inclusionTime).toBeInstanceOf(Date);
  });

  it("Withdrawal UTxO conversion adds derived extra fields", async () => {
    const utxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validWithdrawalDatumCbor,
    });
    const result = await Effect.runPromise(
      utxosToWithdrawalUTxOs([utxo as any], policyIdA),
    );
    expect(result).toHaveLength(1);
    expect(result[0].idCbor).toBeInstanceOf(Buffer);
    expect(result[0].infoCbor).toBeInstanceOf(Buffer);
    expect(result[0].inclusionTime).toBeInstanceOf(Date);
  });

  it("Inclusion time uses network protocol wait", async () => {
    const lucid = makeLucidMock();
    const nowSpy = vi.spyOn(Date, "now").mockReturnValue(1_000_000);
    const inclusionTime = await Effect.runPromise(
      findInclusionTimeForUserEvent(lucid as any),
    );
    expect(inclusionTime).toBe(1_050_000);
    expect(nowSpy).toHaveBeenCalled();
  });

  it("Nonce asset name uses provided nonce UTxO", async () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);
    const nonceUTxO = makeUtxo({ txHash: txHashA, outputIndex: 0 });

    const result = await Effect.runPromise(
      getNonceInputAndAssetName(lucid as any, "deposit", nonceUTxO as any),
    );

    expect(result.inputUtxo.txHash).toBe(txHashA);
    expect(result.assetName).toHaveLength(64);
  });

  it("User-event mint transaction calls fluent builder", () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);
    const inputUtxo = makeUtxo({ txHash: txHashA, outputIndex: 0 });

    const result = buildUserEventMintTransaction({
      lucid: lucid as any,
      inputUtxo: inputUtxo as any,
      nft: toUnit(policyIdA, assetNameA),
      mintRedeemer: "00",
      scriptAddress: addressScriptA,
      datum: validDepositDatumCbor,
      validTo: Number(posixT2),
      mintingPolicy: validatorA.mintingScript,
    });

    expect(result).toBe(builder);
    expect(builder.collectFrom).toHaveBeenCalledWith([inputUtxo]);
    expect(builder.mintAssets).toHaveBeenCalledTimes(1);
    expect(builder.pay.ToAddressWithData).toHaveBeenCalledTimes(1);
    expect(builder.validTo).toHaveBeenCalledWith(Number(posixT2));
    expect(builder.attach.MintingPolicy).toHaveBeenCalledWith(
      validatorA.mintingScript,
    );
  });
});
