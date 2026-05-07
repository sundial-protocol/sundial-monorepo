import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Data, credentialToAddress } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  addressDataFromBech32,
  hashHexWithBlake2b224,
  hashHexWithBlake2b256,
  makeReturn,
  midgardAddressFromBech32,
  midgardAddressToBech32,
  OutputReference,
  Value,
} from "@sdk/common.ts";
import { ConfirmedState, Header } from "@sdk/ledger-state.ts";
import { hashBlockHeader } from "@sdk/state-queue.ts";
import {
  FIXTURE_ASSET_NAME_A,
  FIXTURE_CONFIRMED_STATE,
  FIXTURE_HEADER,
  FIXTURE_MERKLE_ROOT_A,
  FIXTURE_OUTREF_A,
  FIXTURE_POLICY_ID_A,
  FIXTURE_PUB_KEY_HASH_A,
  FIXTURE_SCRIPT_HASH_A,
  FIXTURE_VALUE,
  NETWORK,
} from "./harness/fixtures.ts";

describe("SDK common types and encoding integration", () => {
  it.effect("address data can be parsed from bech32 and serialized", () =>
    Effect.gen(function* () {
      const bech32 = credentialToAddress(NETWORK, {
        type: "Key",
        hash: FIXTURE_PUB_KEY_HASH_A,
      });
      const addressData = yield* addressDataFromBech32(bech32);
      const serialized = Buffer.from(JSON.stringify(addressData));
      const decoded = JSON.parse(serialized.toString("utf8"));

      expect("PublicKeyCredential" in addressData.paymentCredential).toBe(true);
      expect(decoded.paymentCredential.PublicKeyCredential[0]).toBe(
        FIXTURE_PUB_KEY_HASH_A,
      );
    }),
  );

  it.effect("midgard address key credential round-trips via bech32", () =>
    Effect.gen(function* () {
      const encoded = midgardAddressToBech32(NETWORK, {
        PublicKeyCredential: [FIXTURE_PUB_KEY_HASH_A],
      });
      const decoded = yield* midgardAddressFromBech32(encoded);

      expect("PublicKeyCredential" in decoded).toBe(true);
      if ("PublicKeyCredential" in decoded) {
        expect(decoded.PublicKeyCredential[0]).toBe(FIXTURE_PUB_KEY_HASH_A);
      }
    }),
  );

  it.effect("midgard address script credential round-trips via bech32", () =>
    Effect.gen(function* () {
      const encoded = midgardAddressToBech32(NETWORK, {
        ScriptCredential: [FIXTURE_SCRIPT_HASH_A],
      });
      const decoded = yield* midgardAddressFromBech32(encoded);

      expect("ScriptCredential" in decoded).toBe(true);
      if ("ScriptCredential" in decoded) {
        expect(decoded.ScriptCredential[0]).toBe(FIXTURE_SCRIPT_HASH_A);
      }
    }),
  );

  it.effect("output reference encodes and decodes consistently", () =>
    Effect.gen(function* () {
      const cbor = Data.to(FIXTURE_OUTREF_A, OutputReference);
      const decoded = Data.from(cbor, OutputReference);
      const asBuffer = Buffer.from(cbor, "hex");
      const decodedAgain = Data.from(asBuffer.toString("hex"), OutputReference);

      expect(decoded.txHash.hash).toBe(FIXTURE_OUTREF_A.txHash.hash);
      expect(decoded.outputIndex).toBe(0n);
      expect(decodedAgain.txHash.hash).toBe(FIXTURE_OUTREF_A.txHash.hash);
      expect(asBuffer.length).toBeGreaterThan(0);
    }),
  );

  it.effect("value schema preserves policy, asset name, and quantity", () =>
    Effect.gen(function* () {
      const cbor = Data.to(FIXTURE_VALUE, Value);
      const decoded = Data.from(cbor, Value);
      const qty = decoded.inner.get(FIXTURE_POLICY_ID_A)?.get(FIXTURE_ASSET_NAME_A);

      expect(decoded.inner.size).toBe(1);
      expect(qty).toBe(42n);
    }),
  );

  it.effect("confirmed state schema round-trips through CBOR", () =>
    Effect.gen(function* () {
      const cbor = Data.to(FIXTURE_CONFIRMED_STATE, ConfirmedState);
      const decoded = Data.from(cbor, ConfirmedState);

      expect(decoded.headerHash).toBe(FIXTURE_CONFIRMED_STATE.headerHash);
      expect(decoded.utxoRoot).toBe(FIXTURE_MERKLE_ROOT_A);
      expect(decoded.protocolVersion).toBe(1n);
    }),
  );

  it.effect("blake2b-256 and wrapper return stable output", () =>
    Effect.gen(function* () {
      const direct = yield* hashHexWithBlake2b256("deadbeef");
      const wrapped = yield* Effect.promise(() =>
        makeReturn(hashHexWithBlake2b256("deadbeef")).unsafeRun(),
      );

      expect(direct).toBe(wrapped);
      expect(direct).toHaveLength(64);
    }),
  );

  it.effect("header hashing is deterministic across repeated calls", () =>
    Effect.gen(function* () {
      const hashA = yield* hashBlockHeader(FIXTURE_HEADER);
      const hashB = yield* hashBlockHeader(FIXTURE_HEADER);
      const encoded = Data.to(FIXTURE_HEADER, Header);

      expect(hashA).toBe(hashB);
      expect(hashA).toHaveLength(56);
      expect(typeof encoded).toBe("string");
    }),
  );

  it.effect("blake2b-224 hash length is correct for state-queue keys", () =>
    Effect.gen(function* () {
      const hash = yield* hashHexWithBlake2b224("deadbeef");
      expect(hash).toHaveLength(56);
    }),
  );
});
