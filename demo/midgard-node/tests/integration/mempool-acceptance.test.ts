import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer } from "effect";
import { CML } from "@lucid-evolution/lucid";

import { makeTestSqlLayer } from "./harness/pglite-sql-layer.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import * as DBInitialization from "@/database/init.js";
import * as MempoolDB from "@/database/mempool.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import * as Ledger from "@/database/utils/ledger.js";
import { breakDownTx } from "@/utils.js";
import type { TxIngressMessage } from "@/services/index.js";

// These tests exercise the Phase A/B "hardening" path wired into mempool
// acceptance via MempoolDB.validateAndInsertMultiple: only transactions that
// are well-formed, correctly signed, fee-paying, value-preserving and spend
// known unspent UTxOs are inserted; everything else is reported as a structured
// rejection and never reaches the mempool.

// The integration NodeConfig harness uses NETWORK="Preview" (Cardano testnet,
// network id 0); transactions must carry the matching network id.
const NETWORK_ID = 0;

const VALID_INPUT_COIN = 10_000_000n;
const VALID_FEE = 300_000n;
const VALID_OUTPUT_COIN = VALID_INPUT_COIN - VALID_FEE;

const keyFromByte = (byte: number) =>
  CML.PrivateKey.from_normal_bytes(new Uint8Array(32).fill(byte));

const enterpriseAddress = (key: CML.PrivateKey) =>
  CML.EnterpriseAddress.new(
    NETWORK_ID,
    CML.Credential.new_pub_key(key.to_public().hash()),
  ).to_address();

// "owner" funds and signs the happy-path UTxOs; "stranger" drives the
// wrong-signer case; "recipient" only ever receives outputs.
const ownerKey = keyFromByte(0x42);
const ownerAddress = enterpriseAddress(ownerKey);
const strangerKey = keyFromByte(0x99);
const recipientAddress = enterpriseAddress(keyFromByte(0x21));

const seedHash = (byte: number) =>
  CML.TransactionHash.from_raw_bytes(new Uint8Array(32).fill(byte));

const seedInput = (byte: number, index: number) =>
  CML.TransactionInput.new(seedHash(byte), BigInt(index));

const spendOf = (txId: Buffer, index: number) =>
  CML.TransactionInput.new(
    CML.TransactionHash.from_raw_bytes(txId),
    BigInt(index),
  );

const conwayOutput = (address: CML.Address, coin: bigint) =>
  CML.TransactionOutput.new_conway_format_tx_out(
    CML.ConwayFormatTxOut.new(address, CML.Value.from_coin(coin)),
  );

type BuildTxOptions = {
  readonly inputs: readonly CML.TransactionInput[];
  readonly outputs: readonly {
    readonly address: CML.Address;
    readonly coin: bigint;
  }[];
  readonly fee: bigint;
  readonly signers?: readonly CML.PrivateKey[];
};

const buildTx = (
  opts: BuildTxOptions,
): { readonly txCbor: Buffer; readonly txId: Buffer } => {
  const inputs = CML.TransactionInputList.new();
  for (const input of opts.inputs) inputs.add(input);
  const outputs = CML.TransactionOutputList.new();
  for (const out of opts.outputs)
    outputs.add(conwayOutput(out.address, out.coin));

  const body = CML.TransactionBody.new(inputs, outputs, opts.fee);
  body.set_network_id(CML.NetworkId.new(BigInt(NETWORK_ID)));
  const bodyHash = CML.hash_transaction(body);

  const witnessSet = CML.TransactionWitnessSet.new();
  const vkeyWitnesses = CML.VkeywitnessList.new();
  for (const signer of opts.signers ?? [ownerKey]) {
    vkeyWitnesses.add(
      CML.Vkeywitness.new(
        signer.to_public(),
        signer.sign(bodyHash.to_raw_bytes()),
      ),
    );
  }
  witnessSet.set_vkeywitnesses(vkeyWitnesses);

  const tx = CML.Transaction.new(body, witnessSet, true);
  return {
    txCbor: Buffer.from(tx.to_cbor_bytes()),
    txId: Buffer.from(bodyHash.to_raw_bytes()),
  };
};

// A ledger UTxO that buildTx inputs can spend. `address` decides whose
// signature is required to spend it (defaults to the owner).
const seedLedgerEntry = (opts: {
  readonly hashByte: number;
  readonly index: number;
  readonly coin: bigint;
  readonly address?: CML.Address;
}): Ledger.Entry => {
  const address = opts.address ?? ownerAddress;
  return {
    [Ledger.Columns.TX_ID]: Buffer.from(seedHash(opts.hashByte).to_raw_bytes()),
    [Ledger.Columns.OUTREF]: Buffer.from(
      seedInput(opts.hashByte, opts.index).to_cbor_bytes(),
    ),
    [Ledger.Columns.OUTPUT]: Buffer.from(
      conwayOutput(address, opts.coin).to_cbor_bytes(),
    ),
    [Ledger.Columns.ADDRESS]: address.to_bech32(),
  };
};

const makeCandidate = (txCbor: Buffer, id: string, arrivalSeq: number) =>
  breakDownTx(txCbor).pipe(
    Effect.map(
      (processedTx): MempoolDB.AcceptanceCandidate => ({
        arrivalSeq: BigInt(arrivalSeq),
        message: {
          id,
          txCbor: txCbor.toString("hex"),
          deliveryCount: 1,
        } satisfies TxIngressMessage,
        processedTx,
      }),
    ),
  );

const ledgerOutRefHexes = MempoolLedgerDB.retrieve.pipe(
  Effect.map((entries) =>
    entries.map((entry) =>
      Buffer.from(entry[Ledger.Columns.OUTREF]).toString("hex"),
    ),
  ),
);

const outRefHex = (input: CML.TransactionInput) =>
  Buffer.from(input.to_cbor_bytes()).toString("hex");

const makeBaseLayer = () =>
  Layer.mergeAll(makeTestSqlLayer(), makeTestNodeConfigLayer());

describe("mempool acceptance validation (phase A/B hardening)", () => {
  // -----------------------------------------------------------------------
  // Positive cases
  // -----------------------------------------------------------------------

  it.effect(
    "accepts a well-formed transaction and applies it to the ledger",
    () =>
      Effect.gen(function* () {
        yield* DBInitialization.program;
        yield* MempoolLedgerDB.insert([
          seedLedgerEntry({ hashByte: 0x11, index: 0, coin: VALID_INPUT_COIN }),
        ]);

        const { txCbor, txId } = buildTx({
          inputs: [seedInput(0x11, 0)],
          outputs: [{ address: recipientAddress, coin: VALID_OUTPUT_COIN }],
          fee: VALID_FEE,
        });
        const candidate = yield* makeCandidate(txCbor, "100-0", 0);

        const result = yield* MempoolDB.validateAndInsertMultiple([candidate]);

        expect(result.insertedCount).toBe(1);
        expect(result.rejected).toHaveLength(0);
        expect(result.acceptedMessages.map((m) => m.id)).toEqual(["100-0"]);
        expect(yield* MempoolDB.retrieveTxCount).toBe(1n);

        // Spent UTxO removed, produced UTxO inserted.
        const ledger = yield* ledgerOutRefHexes;
        expect(ledger).not.toContain(outRefHex(seedInput(0x11, 0)));
        expect(ledger).toContain(outRefHex(spendOf(txId, 0)));
      }).pipe(Effect.provide(makeBaseLayer())),
  );

  it.effect("accepts two independent transactions in one batch", () =>
    Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([
        seedLedgerEntry({ hashByte: 0x11, index: 0, coin: VALID_INPUT_COIN }),
        seedLedgerEntry({ hashByte: 0x22, index: 0, coin: VALID_INPUT_COIN }),
      ]);

      const first = buildTx({
        inputs: [seedInput(0x11, 0)],
        outputs: [{ address: recipientAddress, coin: VALID_OUTPUT_COIN }],
        fee: VALID_FEE,
      });
      const second = buildTx({
        inputs: [seedInput(0x22, 0)],
        outputs: [{ address: recipientAddress, coin: VALID_OUTPUT_COIN }],
        fee: VALID_FEE,
      });

      const result = yield* MempoolDB.validateAndInsertMultiple([
        yield* makeCandidate(first.txCbor, "200-0", 0),
        yield* makeCandidate(second.txCbor, "201-0", 1),
      ]);

      expect(result.insertedCount).toBe(2);
      expect(result.rejected).toHaveLength(0);
      expect(yield* MempoolDB.retrieveTxCount).toBe(2n);
    }).pipe(Effect.provide(makeBaseLayer())),
  );

  it.effect(
    "accepts a dependent transaction that spends an earlier tx in the same batch",
    () =>
      Effect.gen(function* () {
        yield* DBInitialization.program;
        yield* MempoolLedgerDB.insert([
          seedLedgerEntry({ hashByte: 0x11, index: 0, coin: VALID_INPUT_COIN }),
        ]);

        // parent pays its change back to the owner so the child can spend it.
        const parent = buildTx({
          inputs: [seedInput(0x11, 0)],
          outputs: [{ address: ownerAddress, coin: VALID_OUTPUT_COIN }],
          fee: VALID_FEE,
        });
        const child = buildTx({
          inputs: [spendOf(parent.txId, 0)],
          outputs: [
            { address: recipientAddress, coin: VALID_OUTPUT_COIN - VALID_FEE },
          ],
          fee: VALID_FEE,
        });

        // Child is presented BEFORE the parent to prove the dependency graph
        // (topological sort) handles intra-batch ordering.
        const result = yield* MempoolDB.validateAndInsertMultiple([
          yield* makeCandidate(child.txCbor, "300-1", 0),
          yield* makeCandidate(parent.txCbor, "300-0", 1),
        ]);

        expect(result.insertedCount).toBe(2);
        expect(result.rejected).toHaveLength(0);
        expect(yield* MempoolDB.retrieveTxCount).toBe(2n);
      }).pipe(Effect.provide(makeBaseLayer())),
  );

  it.effect(
    "is idempotent when the same transaction is delivered twice in a batch",
    () =>
      Effect.gen(function* () {
        yield* DBInitialization.program;
        yield* MempoolLedgerDB.insert([
          seedLedgerEntry({ hashByte: 0x11, index: 0, coin: VALID_INPUT_COIN }),
        ]);

        const { txCbor } = buildTx({
          inputs: [seedInput(0x11, 0)],
          outputs: [{ address: recipientAddress, coin: VALID_OUTPUT_COIN }],
          fee: VALID_FEE,
        });

        const result = yield* MempoolDB.validateAndInsertMultiple([
          yield* makeCandidate(txCbor, "400-0", 0),
          yield* makeCandidate(txCbor, "401-0", 1),
        ]);

        // Inserted once, but both delivery copies are acknowledged.
        expect(result.insertedCount).toBe(1);
        expect(result.rejected).toHaveLength(0);
        expect(result.acceptedMessages.map((m) => m.id).sort()).toEqual([
          "400-0",
          "401-0",
        ]);
        expect(yield* MempoolDB.retrieveTxCount).toBe(1n);
      }).pipe(Effect.provide(makeBaseLayer())),
  );

  // -----------------------------------------------------------------------
  // Negative cases
  // -----------------------------------------------------------------------

  const expectSingleRejection = (
    result: MempoolDB.AcceptanceResult,
    code: string,
  ) => {
    expect(result.insertedCount).toBe(0);
    expect(result.acceptedMessages).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0]?.reason).toContain(code);
  };

  it.effect("rejects a transaction whose fee is below the minimum", () =>
    Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([
        seedLedgerEntry({ hashByte: 0x11, index: 0, coin: VALID_INPUT_COIN }),
      ]);

      // Fee far below 44*size + 155381; value still preserved so only the fee
      // gate trips.
      const tinyFee = 1_000n;
      const { txCbor } = buildTx({
        inputs: [seedInput(0x11, 0)],
        outputs: [
          { address: recipientAddress, coin: VALID_INPUT_COIN - tinyFee },
        ],
        fee: tinyFee,
      });

      const result = yield* MempoolDB.validateAndInsertMultiple([
        yield* makeCandidate(txCbor, "500-0", 0),
      ]);

      expectSingleRejection(result, "E_MIN_FEE");
      expect(yield* MempoolDB.retrieveTxCount).toBe(0n);
    }).pipe(Effect.provide(makeBaseLayer())),
  );

  it.effect("rejects a transaction spending an unknown input", () =>
    Effect.gen(function* () {
      yield* DBInitialization.program;
      // Seed an unrelated UTxO so the ledger is non-empty.
      yield* MempoolLedgerDB.insert([
        seedLedgerEntry({ hashByte: 0x11, index: 0, coin: VALID_INPUT_COIN }),
      ]);

      const { txCbor } = buildTx({
        inputs: [seedInput(0xaa, 0)], // never seeded
        outputs: [{ address: recipientAddress, coin: VALID_OUTPUT_COIN }],
        fee: VALID_FEE,
      });

      const result = yield* MempoolDB.validateAndInsertMultiple([
        yield* makeCandidate(txCbor, "600-0", 0),
      ]);

      expectSingleRejection(result, "E_INPUT_NOT_FOUND");
      expect(yield* MempoolDB.retrieveTxCount).toBe(0n);
    }).pipe(Effect.provide(makeBaseLayer())),
  );

  it.effect("rejects a transaction not signed by the input owner", () =>
    Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([
        seedLedgerEntry({ hashByte: 0x11, index: 0, coin: VALID_INPUT_COIN }),
      ]);

      const { txCbor } = buildTx({
        inputs: [seedInput(0x11, 0)],
        outputs: [{ address: recipientAddress, coin: VALID_OUTPUT_COIN }],
        fee: VALID_FEE,
        signers: [strangerKey], // valid signature, wrong key
      });

      const result = yield* MempoolDB.validateAndInsertMultiple([
        yield* makeCandidate(txCbor, "700-0", 0),
      ]);

      expectSingleRejection(result, "E_MISSING_REQUIRED_WITNESS");
      expect(yield* MempoolDB.retrieveTxCount).toBe(0n);
    }).pipe(Effect.provide(makeBaseLayer())),
  );

  it.effect("rejects a transaction that does not preserve value", () =>
    Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([
        seedLedgerEntry({ hashByte: 0x11, index: 0, coin: VALID_INPUT_COIN }),
      ]);

      // Output is far smaller than input - fee: lovelace conjured away.
      const { txCbor } = buildTx({
        inputs: [seedInput(0x11, 0)],
        outputs: [{ address: recipientAddress, coin: 1_000_000n }],
        fee: VALID_FEE,
      });

      const result = yield* MempoolDB.validateAndInsertMultiple([
        yield* makeCandidate(txCbor, "800-0", 0),
      ]);

      expectSingleRejection(result, "E_VALUE_NOT_PRESERVED");
      expect(yield* MempoolDB.retrieveTxCount).toBe(0n);
    }).pipe(Effect.provide(makeBaseLayer())),
  );

  it.effect(
    "rejects the second of two transactions that double-spend one input",
    () =>
      Effect.gen(function* () {
        yield* DBInitialization.program;
        yield* MempoolLedgerDB.insert([
          seedLedgerEntry({ hashByte: 0x11, index: 0, coin: VALID_INPUT_COIN }),
        ]);

        // Two distinct transactions (different outputs => different tx ids)
        // both spending the same UTxO.
        const first = buildTx({
          inputs: [seedInput(0x11, 0)],
          outputs: [{ address: recipientAddress, coin: VALID_OUTPUT_COIN }],
          fee: VALID_FEE,
        });
        const second = buildTx({
          inputs: [seedInput(0x11, 0)],
          outputs: [{ address: ownerAddress, coin: VALID_OUTPUT_COIN }],
          fee: VALID_FEE,
        });

        const result = yield* MempoolDB.validateAndInsertMultiple([
          yield* makeCandidate(first.txCbor, "900-0", 0),
          yield* makeCandidate(second.txCbor, "900-1", 1),
        ]);

        expect(result.insertedCount).toBe(1);
        expect(result.rejected).toHaveLength(1);
        expect(result.rejected[0]?.reason).toContain("E_DOUBLE_SPEND");
        expect(yield* MempoolDB.retrieveTxCount).toBe(1n);
      }).pipe(Effect.provide(makeBaseLayer())),
  );

  it.effect(
    "accepts the valid tx and rejects the invalid one in a mixed batch",
    () =>
      Effect.gen(function* () {
        yield* DBInitialization.program;
        yield* MempoolLedgerDB.insert([
          seedLedgerEntry({ hashByte: 0x11, index: 0, coin: VALID_INPUT_COIN }),
          seedLedgerEntry({ hashByte: 0x22, index: 0, coin: VALID_INPUT_COIN }),
        ]);

        const valid = buildTx({
          inputs: [seedInput(0x11, 0)],
          outputs: [{ address: recipientAddress, coin: VALID_OUTPUT_COIN }],
          fee: VALID_FEE,
        });
        const underpaid = buildTx({
          inputs: [seedInput(0x22, 0)],
          outputs: [
            { address: recipientAddress, coin: VALID_INPUT_COIN - 1_000n },
          ],
          fee: 1_000n,
        });

        const result = yield* MempoolDB.validateAndInsertMultiple([
          yield* makeCandidate(valid.txCbor, "a-0", 0),
          yield* makeCandidate(underpaid.txCbor, "a-1", 1),
        ]);

        expect(result.insertedCount).toBe(1);
        expect(result.acceptedMessages.map((m) => m.id)).toEqual(["a-0"]);
        expect(result.rejected).toHaveLength(1);
        expect(result.rejected[0]?.message.id).toBe("a-1");
        expect(result.rejected[0]?.reason).toContain("E_MIN_FEE");
        expect(yield* MempoolDB.retrieveTxCount).toBe(1n);
      }).pipe(Effect.provide(makeBaseLayer())),
  );
});
