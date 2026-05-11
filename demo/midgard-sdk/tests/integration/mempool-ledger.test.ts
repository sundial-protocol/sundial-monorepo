import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import {
  DepositInfo,
  TxOrderEvent,
  WithdrawalInfo,
} from "@sdk/ledger-state.ts";
import {
  bufferToHex,
  hashHexWithBlake2b256,
  isHexString,
  makeReturn,
  OutputReference,
} from "@sdk/common.ts";
import { authenticateUTxOs, getStateToken } from "@sdk/internals.ts";
import { DepositDatum, utxosToDepositUTxOs } from "@sdk/user-events/deposit.ts";
import {
  TxOrderDatum,
  utxosToTxOrderUTxOs,
} from "@sdk/user-events/tx-order.ts";
import {
  buildUserEventMintTransaction,
  getNonceInputAndAssetName,
} from "@sdk/user-events/internals.ts";
import { makeFakeLucid } from "./harness/fake-lucid.ts";
import {
  FIXTURE_ADDRESS_SCRIPT_A,
  FIXTURE_NONCE_UTXO,
  FIXTURE_OUTREF_A,
  FIXTURE_POLICY_ID_A,
  FIXTURE_PUB_KEY_HASH_A,
  FIXTURE_TX_CBOR_HEX,
  FIXTURE_TX_HASH_A,
  FIXTURE_VALIDATOR,
  FIXTURE_WITHDRAWAL_INFO,
} from "./harness/fixtures.ts";

const assetName = "12".repeat(32);

const makeDepositDatum = () =>
  Data.to(
    {
      event: {
        id: FIXTURE_OUTREF_A,
        info: {
          l2Address: { PublicKeyCredential: [FIXTURE_PUB_KEY_HASH_A] },
          l2Datum: null,
        },
      },
      inclusionTime: 1n,
    },
    DepositDatum,
  );

describe("SDK mempool and ledger-shape integration", () => {
  it.effect("tx-order event round-trips through Lucid Data schema", () =>
    Effect.gen(function* () {
      const cbor = Data.to(
        { id: FIXTURE_OUTREF_A, tx: FIXTURE_TX_CBOR_HEX },
        TxOrderEvent,
      );
      const decoded = Data.from(cbor, TxOrderEvent);

      expect(decoded.id.txHash.hash).toBe(FIXTURE_OUTREF_A.txHash.hash);
      expect(decoded.tx).toBe(FIXTURE_TX_CBOR_HEX);
    }),
  );

  it.effect("withdrawal info round-trips and keeps validity marker", () =>
    Effect.gen(function* () {
      const cbor = Data.to(FIXTURE_WITHDRAWAL_INFO, WithdrawalInfo);
      const decoded = Data.from(cbor, WithdrawalInfo);
      expect(decoded.validity).toBe("WithdrawalIsValid");
    }),
  );

  it.effect("deposit info round-trips for ledger persistence shape", () =>
    Effect.gen(function* () {
      const cbor = Data.to(
        {
          l2Address: { PublicKeyCredential: [FIXTURE_PUB_KEY_HASH_A] },
          l2Datum: null,
        },
        DepositInfo,
      );
      const decoded = Data.from(cbor, DepositInfo);
      expect(decoded.l2Address).toEqual({
        PublicKeyCredential: [FIXTURE_PUB_KEY_HASH_A],
      });
    }),
  );

  it.effect("nonce input hashing yields deterministic 32-byte asset name", () =>
    Effect.gen(function* () {
      const { lucid } = makeFakeLucid({ walletUtxos: [FIXTURE_NONCE_UTXO] });
      const a = yield* getNonceInputAndAssetName(lucid as any, "deposit");
      const b = yield* getNonceInputAndAssetName(lucid as any, "deposit");

      expect(a.inputUtxo.txHash).toBe(FIXTURE_NONCE_UTXO.txHash);
      expect(a.assetName).toBe(b.assetName);
      expect(a.assetName).toHaveLength(64);
    }),
  );

  it.effect("mint-transaction builder composes expected chain calls", () =>
    Effect.gen(function* () {
      const { lucid } = makeFakeLucid({ walletUtxos: [FIXTURE_NONCE_UTXO] });
      const nft = toUnit(FIXTURE_POLICY_ID_A, assetName);

      const tx = buildUserEventMintTransaction({
        lucid: lucid as any,
        inputUtxo: FIXTURE_NONCE_UTXO as any,
        nft,
        mintRedeemer: Data.void(),
        scriptAddress: FIXTURE_ADDRESS_SCRIPT_A,
        datum: makeDepositDatum(),
        validTo: 123,
        mintingPolicy: FIXTURE_VALIDATOR.mintingScript,
      });

      const calls = (tx as any).__calls;
      expect(calls.collectFrom).toHaveLength(1);
      expect(calls.mintAssets).toHaveLength(1);
      expect(calls.payToAddressWithData).toHaveLength(1);
      expect(calls.attachMintingPolicy).toHaveLength(1);
    }),
  );

  it.effect("state token extractor accepts exactly one NFT plus lovelace", () =>
    Effect.gen(function* () {
      const assets = {
        lovelace: 2_000_000n,
        [toUnit(FIXTURE_POLICY_ID_A, assetName)]: 1n,
      };
      const [policy, decodedAssetName] = yield* getStateToken(assets);

      expect(policy).toBe(FIXTURE_POLICY_ID_A);
      expect(decodedAssetName).toBe(assetName);
    }),
  );

  it.effect("state token extractor rejects multi-asset payloads", () =>
    Effect.gen(function* () {
      const result = yield* Effect.either(
        getStateToken({
          lovelace: 2_000_000n,
          [toUnit(FIXTURE_POLICY_ID_A, assetName)]: 1n,
          [toUnit(FIXTURE_POLICY_ID_A, "34")]: 1n,
        }),
      );
      expect(Either.isLeft(result)).toBe(true);
    }),
  );

  it.effect("authenticateUTxOs returns only valid deposit entries", () =>
    Effect.gen(function* () {
      const valid = {
        txHash: FIXTURE_TX_HASH_A,
        outputIndex: 0,
        address: FIXTURE_ADDRESS_SCRIPT_A,
        assets: {
          lovelace: 2_000_000n,
          [toUnit(FIXTURE_POLICY_ID_A, assetName)]: 1n,
        },
        datum: makeDepositDatum(),
      };
      const invalid = { ...valid, datum: "ff" };

      const decoded = yield* authenticateUTxOs<any>(
        [valid, invalid] as any,
        FIXTURE_POLICY_ID_A,
        DepositDatum,
      );
      expect(decoded).toHaveLength(1);
    }),
  );

  it.effect(
    "deposit and tx-order UTxO conversion produce CBOR payload buffers",
    () =>
      Effect.gen(function* () {
        const deposit = {
          txHash: FIXTURE_TX_HASH_A,
          outputIndex: 0,
          address: FIXTURE_ADDRESS_SCRIPT_A,
          assets: {
            lovelace: 2_000_000n,
            [toUnit(FIXTURE_POLICY_ID_A, assetName)]: 1n,
          },
          datum: makeDepositDatum(),
        };
        const txOrder = {
          txHash: FIXTURE_TX_HASH_A,
          outputIndex: 1,
          address: FIXTURE_ADDRESS_SCRIPT_A,
          assets: {
            lovelace: 2_000_000n,
            [toUnit(FIXTURE_POLICY_ID_A, assetName)]: 1n,
          },
          datum: Data.to(
            {
              event: { id: FIXTURE_OUTREF_A, tx: FIXTURE_TX_CBOR_HEX },
              inclusionTime: 1n,
              refundAddress: {
                paymentCredential: {
                  PublicKeyCredential: [FIXTURE_PUB_KEY_HASH_A],
                },
                stakeCredential: null,
              },
              refundDatum: null,
            },
            TxOrderDatum,
          ),
        };

        const deposits = yield* utxosToDepositUTxOs(
          [deposit] as any,
          FIXTURE_POLICY_ID_A,
        );
        const txOrders = yield* utxosToTxOrderUTxOs(
          [txOrder] as any,
          FIXTURE_POLICY_ID_A,
        );

        expect(deposits[0].idCbor.length).toBeGreaterThan(0);
        expect(deposits[0].infoCbor.length).toBeGreaterThan(0);
        expect(txOrders[0].idCbor.length).toBeGreaterThan(0);
        expect(txOrders[0].infoCbor.toString("hex")).toBe(FIXTURE_TX_CBOR_HEX);
      }),
  );

  it.effect("hex helpers normalize storage payloads", () =>
    Effect.gen(function* () {
      const hash = yield* hashHexWithBlake2b256("deadbeef");
      const outrefCbor = Data.to(FIXTURE_OUTREF_A, OutputReference);
      const outrefBuf = Buffer.from(outrefCbor, "hex");

      expect(isHexString(hash)).toBe(true);
      expect(isHexString("zz-not-hex")).toBe(false);
      expect(bufferToHex(outrefBuf)).toBe(outrefCbor);
    }),
  );

  it.effect("makeReturn.safeRun exposes success through Either", () =>
    Effect.gen(function* () {
      const result = yield* Effect.promise(() =>
        makeReturn(hashHexWithBlake2b256("deadbeef")).safeRun(),
      );

      expect(Either.isRight(result)).toBe(true);
    }),
  );
});
