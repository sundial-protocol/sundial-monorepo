import {
  encodeHeader,
  decodeHeader,
  encodeBlockBody,
  decodeBlockBody,
  encodeBlock,
  decodeBlock,
  type Header,
  type BlockBody,
  type Block,
} from "../../src/types/block";

import { bytesSeq } from "./helpers/cardano-fixtures.js";

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const hash28A = new Uint8Array(28).fill(0xaa);
const hash32A = new Uint8Array(32).fill(0x11);
const hash32B = new Uint8Array(32).fill(0x22);
const hash32C = new Uint8Array(32).fill(0x33);
const hash32D = new Uint8Array(32).fill(0x44);
const hash32E = new Uint8Array(32).fill(0x55);
const hash32F = new Uint8Array(32).fill(0x66);
const addressA = bytesSeq(29);

function mkHeader(overrides: Partial<Header> = {}): Header {
  return {
    prev_utxos_root: hash32A,
    utxos_root: hash32B,
    transactions_root: hash32C,
    deposits_root: hash32D,
    withdrawals_root: hash32E,
    start_time: 1000,
    event_start_time: 2000,
    end_time: 3000,
    prev_header_hash: undefined,
    operator_vkey: hash32F,
    protocol_version: 1,
    ...overrides,
  };
}

const emptyBlockBody: BlockBody = {
  utxos: [],
  transactions: [],
  deposits: [],
  withdrawals: [],
};

// Minimal transaction fixture for block body entries
const minimalTx = {
  body: {
    inputs: [{ tx_id: hash32A, index: 0 }],
    outputs: [
      {
        address: addressA,
        value: { type: "Coin" as const, coin: 2_000_000n },
        datum: undefined,
        script_ref: undefined,
      },
    ],
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

it("round trips header without previous hash", () => {
  const header = mkHeader();
  const encoded = encodeHeader(header);
  // 5 × 32 (roots) + 3 × 8 (times) + 8 (presence flag) + 32 (vkey) + 8 (version) = 232
  expect(encoded.length).toBe(232);
  const decoded = decodeHeader(encoded);
  expect(decoded.prev_header_hash).toBeUndefined();
  expect(decoded.utxos_root).toEqual(hash32B);
  expect(decoded.start_time).toBe(1000);
  expect(decoded.protocol_version).toBe(1);
  expect(decoded.operator_vkey).toEqual(hash32F);
});

it("round trips header with previous hash", () => {
  const header = mkHeader({ prev_header_hash: hash28A });
  const encoded = encodeHeader(header);
  // 232 (base) + 32 (hash28 padded to 32) = 264
  expect(encoded.length).toBe(264);
  const decoded = decodeHeader(encoded);
  expect(decoded.prev_header_hash).toEqual(hash28A);
});

it("round trips an empty block body", () => {
  const encoded = encodeBlockBody(emptyBlockBody);
  const decoded = decodeBlockBody(encoded);
  expect(decoded.utxos.length).toBe(0);
  expect(decoded.transactions.length).toBe(0);
  expect(decoded.deposits.length).toBe(0);
  expect(decoded.withdrawals.length).toBe(0);
});

it("round trips a populated block body", () => {
  const body: BlockBody = {
    utxos: [
      [
        { tx_id: hash32A, index: 0 },
        {
          address: addressA,
          value: { type: "Coin", coin: 1_000_000n },
          datum: undefined,
          script_ref: undefined,
        },
      ],
    ],
    transactions: [[hash32B, minimalTx]],
    deposits: [
      [
        { tx_id: hash32C, index: 0 },
        { l2_address: addressA, l2_datum: undefined },
      ],
    ],
    withdrawals: [
      [
        { tx_id: hash32D, index: 0 },
        {
          l2_outref: { tx_id: hash32A, index: 0 },
          l1_address: addressA,
          l1_datum: undefined,
        },
      ],
    ],
  };

  const encoded = encodeBlockBody(body);
  const decoded = decodeBlockBody(encoded);
  expect(decoded.utxos.length).toBe(1);
  expect(decoded.transactions.length).toBe(1);
  expect(decoded.deposits.length).toBe(1);
  expect(decoded.withdrawals.length).toBe(1);
  expect(decoded.utxos[0][0].tx_id).toEqual(hash32A);
  const decodedUtxoValue = decoded.utxos[0][1].value;
  expect(decodedUtxoValue.type).toBe("Coin");
  if (decodedUtxoValue.type !== "Coin") {
    throw new Error("Expected coin value in decoded UTxO");
  }
  expect(decodedUtxoValue.coin).toBe(1_000_000n);
});

it("round trips a full block", () => {
  const block: Block = {
    header_hash: hash28A,
    header: mkHeader(),
    block_body: {
      utxos: [
        [
          { tx_id: hash32A, index: 0 },
          {
            address: addressA,
            value: { type: "Coin", coin: 500_000n },
            datum: undefined,
            script_ref: undefined,
          },
        ],
      ],
      transactions: [],
      deposits: [],
      withdrawals: [],
    },
  };

  const encoded = encodeBlock(block);
  const decoded = decodeBlock(encoded);
  expect(decoded.header_hash).toEqual(hash28A);
  expect(decoded.header.utxos_root).toEqual(hash32B);
  expect(decoded.block_body.utxos.length).toBe(1);
  expect(decoded.block_body.transactions.length).toBe(0);
});
