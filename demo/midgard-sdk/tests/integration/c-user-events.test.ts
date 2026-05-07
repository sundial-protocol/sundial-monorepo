import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { CML, Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  DepositDatum,
  fetchDepositUTxOsProgram,
  incompleteDepositTxProgram,
  utxosToDepositUTxOs,
} from "@sdk/user-events/deposit.ts";
import {
  TxOrderDatum,
  fetchTxOrderUTxOsProgram,
  incompleteTxOrderTxProgram,
  utxosToTxOrderUTxOs,
} from "@sdk/user-events/tx-order.ts";
import {
  WithdrawalOrderDatum,
  fetchWithdrawalUTxOsProgram,
  incompleteWithdrawalTxProgram,
  utxosToWithdrawalUTxOs,
} from "@sdk/user-events/withdrawal.ts";
import { makeReturn } from "@sdk/common.ts";
import { makeFakeLucid } from "./harness/fake-lucid.ts";
import {
  FIXTURE_ADDRESS_DATA_KEY_A,
  FIXTURE_ADDRESS_SCRIPT_A,
  FIXTURE_NONCE_UTXO,
  FIXTURE_OUTREF_A,
  FIXTURE_POLICY_ID_A,
  FIXTURE_PUB_KEY_HASH_A,
  FIXTURE_TX_CBOR_HEX,
  FIXTURE_TX_HASH_A,
  FIXTURE_VALIDATOR,
  FIXTURE_WITHDRAWAL_BODY,
  FIXTURE_WITHDRAWAL_INFO,
} from "./harness/fixtures.ts";

const DEPOSIT_ASSET_NAME = "ab".repeat(32);
const TX_ORDER_ASSET_NAME = "cd".repeat(32);
const WITHDRAW_ASSET_NAME = "ef".repeat(32);

const makeUserEventAssets = (assetName: string) => ({
  lovelace: 2_000_000n,
  [toUnit(FIXTURE_POLICY_ID_A, assetName)]: 1n,
});

const makeDepositUtxo = (inclusionTime: bigint) => {
  const datum = Data.to(
    {
      event: {
        id: FIXTURE_OUTREF_A,
        info: {
          l2Address: { PublicKeyCredential: [FIXTURE_PUB_KEY_HASH_A] },
          l2Datum: null,
        },
      },
      inclusionTime,
    },
    DepositDatum,
  );

  return {
    txHash: FIXTURE_TX_HASH_A,
    outputIndex: 0,
    address: FIXTURE_ADDRESS_SCRIPT_A,
    assets: makeUserEventAssets(DEPOSIT_ASSET_NAME),
    datum,
  } as any;
};

const makeTxOrderUtxo = (inclusionTime: bigint) => {
  const datum = Data.to(
    {
      event: { id: FIXTURE_OUTREF_A, tx: FIXTURE_TX_CBOR_HEX },
      inclusionTime,
      refundAddress: FIXTURE_ADDRESS_DATA_KEY_A,
      refundDatum: null,
    },
    TxOrderDatum,
  );

  return {
    txHash: FIXTURE_TX_HASH_A,
    outputIndex: 1,
    address: FIXTURE_ADDRESS_SCRIPT_A,
    assets: makeUserEventAssets(TX_ORDER_ASSET_NAME),
    datum,
  } as any;
};

const makeWithdrawalUtxo = (inclusionTime: bigint) => {
  const datum = Data.to(
    {
      event: { id: FIXTURE_OUTREF_A, info: FIXTURE_WITHDRAWAL_INFO },
      inclusionTime,
      refundAddress: FIXTURE_ADDRESS_DATA_KEY_A,
      refundDatum: null,
    },
    WithdrawalOrderDatum,
  );

  return {
    txHash: FIXTURE_TX_HASH_A,
    outputIndex: 2,
    address: FIXTURE_ADDRESS_SCRIPT_A,
    assets: makeUserEventAssets(WITHDRAW_ASSET_NAME),
    datum,
  } as any;
};

describe("SDK user-events integration", () => {
  it.effect("deposit builder emits a valid deposit datum", () =>
    Effect.gen(function* () {
      const { lucid } = makeFakeLucid({ walletUtxos: [FIXTURE_NONCE_UTXO] });
      const tx = yield* incompleteDepositTxProgram(lucid as any, {
        depositScriptAddress: FIXTURE_ADDRESS_SCRIPT_A,
        mintingPolicy: FIXTURE_VALIDATOR.mintingScript,
        policyId: FIXTURE_POLICY_ID_A,
        nonceUTxO: FIXTURE_NONCE_UTXO as any,
        depositAmount: 5_000_000n,
        depositInfo: {
          l2Address: { PublicKeyCredential: [FIXTURE_PUB_KEY_HASH_A] },
          l2Datum: null,
        },
      });

      const datumCbor = (tx as any).__calls.payToAddressWithData[0][1].value;
      const decoded = Data.from(datumCbor, DepositDatum);

      expect(decoded.event.id.txHash.hash).toBe(FIXTURE_NONCE_UTXO.txHash);
      expect(decoded.event.info.l2Address).toEqual({
        PublicKeyCredential: [FIXTURE_PUB_KEY_HASH_A],
      });
      expect(decoded.inclusionTime).toBeGreaterThan(0n);
    }),
  );

  it.effect("deposit fetch decodes one authentic UTxO", () =>
    Effect.gen(function* () {
      const depositUtxo = makeDepositUtxo(1_000_000n);
      const { lucid } = makeFakeLucid({
        utxosAt: { [FIXTURE_ADDRESS_SCRIPT_A]: [depositUtxo] },
      });

      const deposits = yield* fetchDepositUTxOsProgram(lucid as any, {
        eventAddress: FIXTURE_ADDRESS_SCRIPT_A,
        eventPolicyId: FIXTURE_POLICY_ID_A,
        inclusionTimeLowerBound: 0n,
        inclusionTimeUpperBound: 2_000_000n,
      });

      expect(deposits).toHaveLength(1);
      expect(deposits[0].utxo.txHash).toBe(FIXTURE_TX_HASH_A);
      expect(deposits[0].idCbor.length).toBeGreaterThan(0);
      expect(deposits[0].infoCbor.length).toBeGreaterThan(0);
    }),
  );

  it.effect("deposit conversion filters unauthentic policy UTxOs", () =>
    Effect.gen(function* () {
      const good = makeDepositUtxo(1n);
      const bad = {
        ...makeDepositUtxo(1n),
        assets: { lovelace: 2_000_000n, [toUnit("aa".repeat(28), "00")]: 1n },
      };

      const converted = yield* utxosToDepositUTxOs([good, bad] as any, FIXTURE_POLICY_ID_A);
      expect(converted).toHaveLength(1);
      expect(converted[0].assetName).toBe(DEPOSIT_ASSET_NAME);
    }),
  );

  it.effect("tx-order builder emits event datum carrying tx CBOR", () =>
    Effect.gen(function* () {
      const { lucid } = makeFakeLucid({ walletUtxos: [FIXTURE_NONCE_UTXO] });
      const tx = yield* incompleteTxOrderTxProgram(lucid as any, {
        txOrderScriptAddress: FIXTURE_ADDRESS_SCRIPT_A,
        mintingPolicy: FIXTURE_VALIDATOR.mintingScript,
        policyId: FIXTURE_POLICY_ID_A,
        nonceUTxO: FIXTURE_NONCE_UTXO as any,
        cardanoTx: { to_cbor_hex: () => FIXTURE_TX_CBOR_HEX } as unknown as CML.Transaction,
        refundAddress: FIXTURE_ADDRESS_DATA_KEY_A,
        refundDatum: null,
      });

      const datumCbor = (tx as any).__calls.payToAddressWithData[0][1].value;
      const decoded = Data.from(datumCbor, TxOrderDatum);

      expect(decoded.event.tx).toBe(FIXTURE_TX_CBOR_HEX);
      expect(decoded.refundAddress.paymentCredential).toEqual(
        FIXTURE_ADDRESS_DATA_KEY_A.paymentCredential,
      );
    }),
  );

  it.effect("tx-order fetch decodes one authentic UTxO", () =>
    Effect.gen(function* () {
      const txOrderUtxo = makeTxOrderUtxo(1_000_000n);
      const { lucid } = makeFakeLucid({
        utxosAt: { [FIXTURE_ADDRESS_SCRIPT_A]: [txOrderUtxo] },
      });

      const txOrders = yield* fetchTxOrderUTxOsProgram(lucid as any, {
        eventAddress: FIXTURE_ADDRESS_SCRIPT_A,
        eventPolicyId: FIXTURE_POLICY_ID_A,
      });

      expect(txOrders).toHaveLength(1);
      expect(txOrders[0].assetName).toBe(TX_ORDER_ASSET_NAME);
      expect(txOrders[0].infoCbor.toString("hex")).toBe(FIXTURE_TX_CBOR_HEX);
    }),
  );

  it.effect("tx-order conversion materializes id/info CBOR buffers", () =>
    Effect.gen(function* () {
      const txOrders = yield* utxosToTxOrderUTxOs(
        [makeTxOrderUtxo(5n)] as any,
        FIXTURE_POLICY_ID_A,
      );

      expect(txOrders).toHaveLength(1);
      expect(txOrders[0].idCbor).toBeInstanceOf(Buffer);
      expect(txOrders[0].infoCbor).toBeInstanceOf(Buffer);
      expect(txOrders[0].inclusionTime).toBeInstanceOf(Date);
    }),
  );

  it.effect("withdrawal builder emits a valid withdrawal datum", () =>
    Effect.gen(function* () {
      const { lucid } = makeFakeLucid({ walletUtxos: [FIXTURE_NONCE_UTXO] });
      const tx = yield* incompleteWithdrawalTxProgram(lucid as any, {
        withdrawalScriptAddress: FIXTURE_ADDRESS_SCRIPT_A,
        mintingPolicy: FIXTURE_VALIDATOR.mintingScript,
        policyId: FIXTURE_POLICY_ID_A,
        nonceUTxO: FIXTURE_NONCE_UTXO as any,
        withdrawalBody: FIXTURE_WITHDRAWAL_BODY,
        withdrawalSignature: new Map(),
        refundAddress: FIXTURE_ADDRESS_DATA_KEY_A,
        refundDatum: null,
      });

      const datumCbor = (tx as any).__calls.payToAddressWithData[0][1].value;
      const decoded = Data.from(datumCbor, WithdrawalOrderDatum);

      expect(decoded.event.info.body.l2_outref.txHash.hash).toBe(
        FIXTURE_WITHDRAWAL_BODY.l2_outref.txHash.hash,
      );
      expect(decoded.event.info.validity).toBe("WithdrawalIsValid");
    }),
  );

  it.effect("withdrawal fetch decodes one authentic UTxO", () =>
    Effect.gen(function* () {
      const withdrawalUtxo = makeWithdrawalUtxo(1_000_000n);
      const { lucid } = makeFakeLucid({
        utxosAt: { [FIXTURE_ADDRESS_SCRIPT_A]: [withdrawalUtxo] },
      });

      const withdrawals = yield* fetchWithdrawalUTxOsProgram(lucid as any, {
        eventAddress: FIXTURE_ADDRESS_SCRIPT_A,
        eventPolicyId: FIXTURE_POLICY_ID_A,
      });

      expect(withdrawals).toHaveLength(1);
      expect(withdrawals[0].assetName).toBe(WITHDRAW_ASSET_NAME);
      expect(withdrawals[0].infoCbor.length).toBeGreaterThan(0);
    }),
  );

  it.effect("withdrawal conversion materializes id/info CBOR buffers", () =>
    Effect.gen(function* () {
      const converted = yield* utxosToWithdrawalUTxOs(
        [makeWithdrawalUtxo(42n)] as any,
        FIXTURE_POLICY_ID_A,
      );

      expect(converted).toHaveLength(1);
      expect(converted[0].idCbor).toBeInstanceOf(Buffer);
      expect(converted[0].infoCbor).toBeInstanceOf(Buffer);
      expect(converted[0].inclusionTime.getTime()).toBeGreaterThan(0);
    }),
  );

  it.effect("makeReturn wrapper matches direct fetch program output", () =>
    Effect.gen(function* () {
      const depositUtxo = makeDepositUtxo(10n);
      const { lucid } = makeFakeLucid({
        utxosAt: { [FIXTURE_ADDRESS_SCRIPT_A]: [depositUtxo] },
      });
      const config = {
        eventAddress: FIXTURE_ADDRESS_SCRIPT_A,
        eventPolicyId: FIXTURE_POLICY_ID_A,
      };

      const direct = yield* fetchDepositUTxOsProgram(lucid as any, config);
      const wrapped = yield* Effect.promise(() =>
        makeReturn(fetchDepositUTxOsProgram(lucid as any, config)).unsafeRun(),
      );

      expect(wrapped.length).toBe(direct.length);
      expect(wrapped[0].idCbor.toString("hex")).toBe(direct[0].idCbor.toString("hex"));
    }),
  );

  it.effect("inclusion-time bounds correctly filter fetched deposits", () =>
    Effect.gen(function* () {
      const inRange = makeDepositUtxo(100n);
      const outOfRange = makeDepositUtxo(10_000n);
      const { lucid } = makeFakeLucid({
        utxosAt: { [FIXTURE_ADDRESS_SCRIPT_A]: [inRange, outOfRange] },
      });

      const filtered = yield* fetchDepositUTxOsProgram(lucid as any, {
        eventAddress: FIXTURE_ADDRESS_SCRIPT_A,
        eventPolicyId: FIXTURE_POLICY_ID_A,
        inclusionTimeLowerBound: 50n,
        inclusionTimeUpperBound: 200n,
      });

      expect(filtered).toHaveLength(1);
      expect(filtered[0].datum.inclusionTime).toBe(100n);
    }),
  );

  it.effect("invalid event datum is dropped during conversion", () =>
    Effect.gen(function* () {
      const invalidDatumUtxo = {
        txHash: FIXTURE_TX_HASH_A,
        outputIndex: 0,
        address: FIXTURE_ADDRESS_SCRIPT_A,
        assets: makeUserEventAssets(DEPOSIT_ASSET_NAME),
        datum: "ff",
      };

      const converted = yield* utxosToDepositUTxOs(
        [invalidDatumUtxo] as any,
        FIXTURE_POLICY_ID_A,
      );
      expect(converted).toHaveLength(0);
    }),
  );
});
