import { describe, expect, beforeEach, vi } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";

const lucidMocks = vi.hoisted(() => {
  const l2Amount = { tag: "l2-amount" };
  const checkedSub = vi.fn(() => l2Amount);
  const l1Amount = { checked_sub: checkedSub };
  const l1Output = { amount: () => l1Amount };

  const scriptHashFromHex = vi.fn((hex: string) => ({ hex }));
  const assetNameFromHex = vi.fn((hex: string) => ({ hex }));
  const mapAssetInsert = vi.fn();
  const multiAssetInsert = vi.fn();
  const valueNew = vi.fn((coin: bigint, multiasset: unknown) => ({
    coin,
    multiasset,
  }));
  const datumOptionFromCborHex = vi.fn((hex: string) => ({
    to_cbor_hex: () => hex,
  }));
  const addressFromBech32 = vi.fn((bech32: string) => ({
    to_bech32: () => bech32,
  }));
  const transactionOutputNew = vi.fn(
    (address: unknown, amount: unknown, datum: unknown) => ({
      address: () => address,
      amount: () => amount,
      datum: () => datum,
      to_cbor_bytes: () => Buffer.from("output"),
    }),
  );
  const conwayFormatTxOutNew = vi.fn((address: unknown, amount: unknown) => {
    let datum: unknown = undefined;
    return {
      address,
      amount,
      set_datum_option: (d: unknown) => {
        datum = d;
      },
      getDatum: () => datum,
    };
  });
  const transactionOutputNewConwayFormatTxOut = vi.fn(
    (conway: {
      address: unknown;
      amount: unknown;
      getDatum: () => unknown;
    }) => ({
      address: () => conway.address,
      amount: () => conway.amount,
      datum: () => conway.getDatum(),
      to_cbor_bytes: () => Buffer.from("output"),
    }),
  );
  const transactionHashFromHex = vi.fn((hex: string) => ({
    to_hex: () => hex,
    to_raw_bytes: () => Buffer.from(hex, "hex"),
  }));
  const transactionInputNew = vi.fn(
    (hash: { to_hex: () => string }, index: bigint | number) => ({
      transaction_id: () => hash,
      index: () => index,
      to_cbor_bytes: () => Buffer.from(`${hash.to_hex()}:${index}`),
    }),
  );
  const transactionUnspentOutputFromCborBytes = vi.fn(() => ({
    output: () => l1Output,
  }));
  const transactionUnspentOutputNew = vi.fn(
    (input: unknown, output: unknown) => ({
      input: () => input,
      output: () => output,
    }),
  );
  const dataFrom = vi.fn((hex: string) =>
    JSON.parse(Buffer.from(hex, "hex").toString("utf8")),
  );

  return {
    addressFromBech32,
    assetNameFromHex,
    checkedSub,
    conwayFormatTxOutNew,
    dataFrom,
    datumOptionFromCborHex,
    mapAssetInsert,
    multiAssetInsert,
    scriptHashFromHex,
    transactionHashFromHex,
    transactionInputNew,
    transactionOutputNew,
    transactionOutputNewConwayFormatTxOut,
    transactionUnspentOutputFromCborBytes,
    transactionUnspentOutputNew,
    valueNew,
  };
});

vi.mock("@lucid-evolution/lucid", () => ({
  CML: {
    Address: {
      from_bech32: lucidMocks.addressFromBech32,
    },
    AssetName: {
      from_hex: lucidMocks.assetNameFromHex,
    },
    ConwayFormatTxOut: {
      new: lucidMocks.conwayFormatTxOutNew,
    },
    DatumOption: {
      from_cbor_hex: lucidMocks.datumOptionFromCborHex,
    },
    MapAssetNameToCoin: {
      new: () => ({ insert: lucidMocks.mapAssetInsert }),
    },
    MultiAsset: {
      new: () => ({ insert_assets: lucidMocks.multiAssetInsert }),
    },
    ScriptHash: {
      from_hex: lucidMocks.scriptHashFromHex,
    },
    TransactionHash: {
      from_hex: lucidMocks.transactionHashFromHex,
    },
    TransactionInput: {
      new: lucidMocks.transactionInputNew,
    },
    TransactionOutput: {
      new: lucidMocks.transactionOutputNew,
      new_conway_format_tx_out:
        lucidMocks.transactionOutputNewConwayFormatTxOut,
    },
    TransactionUnspentOutput: {
      from_cbor_bytes: lucidMocks.transactionUnspentOutputFromCborBytes,
      new: lucidMocks.transactionUnspentOutputNew,
    },
    Value: {
      new: lucidMocks.valueNew,
    },
  },
  Data: {
    from: lucidMocks.dataFrom,
  },
  walletFromSeed: () => ({
    address: "addr_test1mock",
  }),
}));

vi.mock("@al-ft/midgard-sdk", () => {
  class CmlDeserializationError extends Error {
    constructor(fields: { message: string; cause?: unknown }) {
      super(fields.message);
      this.name = "CmlDeserializationError";
    }
  }

  class DataCoercionError extends Error {
    constructor(fields: { message: string; cause?: unknown }) {
      super(fields.message);
      this.name = "DataCoercionError";
    }
  }

  return {
    CmlDeserializationError,
    DataCoercionError,
    DepositInfo: Symbol("DepositInfo"),
    WithdrawalInfo: Symbol("WithdrawalInfo"),
    bufferToHex: (bytes: Buffer) => bytes.toString("hex"),
    getProtocolParameters: () => ({
      event_wait_duration: 50_000,
      maturity_duration: 1n,
      slashing_penalty: 1_000_000n,
    }),
    midgardAddressToBech32: (_network: string, address: string) =>
      `addr_${address}`,
  };
});

import * as Deposits from "@/database/deposits.js";
import * as Withdrawals from "@/database/withdrawals.js";
import { makeUserEventEntry } from "./harness/fixtures.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";

const jsonBuffer = (value: unknown) => Buffer.from(JSON.stringify(value));

beforeEach(() => {
  vi.clearAllMocks();
});

describe("Deposits entry conversion", () => {
  it.effect("converts a deposit event into an L2 CML UTxO", () => {
    const assetName = "ab".repeat(32);
    const entry = makeUserEventEntry(0x10, {
      asset_name: assetName,
      event_info: jsonBuffer({
        l2Address: "midgard-user",
        l2Datum: null,
      }),
    });

    return Deposits.entryToCMLUTxO(entry, "cd".repeat(28)).pipe(
      Effect.map((utxo) => {
        expect(
          lucidMocks.transactionUnspentOutputFromCborBytes,
        ).toHaveBeenCalledWith(entry.l1_utxo_cbor);
        expect(lucidMocks.checkedSub).toHaveBeenCalledOnce();
        expect(lucidMocks.datumOptionFromCborHex).not.toHaveBeenCalled();
        expect(utxo.input().transaction_id().to_hex()).toBe(assetName);
        expect(utxo.output().address().to_bech32()).toBe("addr_midgard-user");
      }),
      Effect.provide(makeTestNodeConfigLayer()),
    );
  });

  it.effect("preserves an inline L2 datum when present", () => {
    const datumCbor = "d8799f";
    const entry = makeUserEventEntry(0x11, {
      asset_name: "ef".repeat(32),
      event_info: jsonBuffer({
        l2Address: "midgard-user-with-datum",
        l2Datum: datumCbor,
      }),
    });

    return Deposits.entryToCMLUTxO(entry, "cd".repeat(28)).pipe(
      Effect.map((utxo) => {
        expect(lucidMocks.datumOptionFromCborHex).toHaveBeenCalledWith(
          datumCbor,
        );
        const datum = utxo.output().datum();
        expect(datum).toBeDefined();
        expect(datum?.to_cbor_hex()).toBe(datumCbor);
      }),
      Effect.provide(makeTestNodeConfigLayer()),
    );
  });
});

describe("Withdrawals entry conversion", () => {
  it.effect("extracts the L2 outref from withdrawal info", () => {
    const txHash = "12".repeat(32);
    const entry = makeUserEventEntry(0x20, {
      event_info: jsonBuffer({
        body: {
          l2_outref: {
            txHash: { hash: txHash },
            outputIndex: 7,
          },
        },
      }),
    });

    return Withdrawals.entryToOutRef(entry).pipe(
      Effect.map((outref) => {
        expect(lucidMocks.transactionHashFromHex).toHaveBeenCalledWith(txHash);
        expect(lucidMocks.transactionInputNew).toHaveBeenCalled();
        expect(outref).toEqual(Buffer.from(`${txHash}:7`));
      }),
    );
  });
});
