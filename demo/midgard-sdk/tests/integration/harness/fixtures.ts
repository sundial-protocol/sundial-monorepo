// Shared deterministic test fixtures for the SDK integration suite.
//
// One fixture set is shared across all 50 tests to keep them hermetic and
// predictable.  Concrete values are chosen to be valid hex strings of the
// correct lengths expected by Lucid Data schemas and CML serialization.
//
// When implementing individual tests, extend these fixtures as needed but keep
// the base identifiers stable so harness helpers remain reusable.

// ─── Network ──────────────────────────────────────────────────────────────────

export const NETWORK = "Preview" as const;

// ─── Key hashes, policy ids, script hashes ───────────────────────────────────

export const FIXTURE_TX_HASH_A = "aa".repeat(32); // 64 hex chars (32 bytes)
export const FIXTURE_TX_HASH_B = "bb".repeat(32);
export const FIXTURE_POLICY_ID_A = "cc".repeat(28); // 56 hex chars (28 bytes)
export const FIXTURE_PUB_KEY_HASH_A = "dd".repeat(28);
export const FIXTURE_SCRIPT_HASH_A = "ee".repeat(28);
export const FIXTURE_MERKLE_ROOT_A = "ff".repeat(32);
export const FIXTURE_MERKLE_ROOT_B = "11".repeat(32);
export const FIXTURE_HEX_PAYLOAD = "deadbeef0123456789abcdef";

// ─── Output references ────────────────────────────────────────────────────────

export const FIXTURE_OUTREF_A = {
  txHash: { hash: FIXTURE_TX_HASH_A },
  outputIndex: 0n,
};

export const FIXTURE_OUTREF_B = {
  txHash: { hash: FIXTURE_TX_HASH_B },
  outputIndex: 1n,
};

// ─── Posix times ─────────────────────────────────────────────────────────────

export const FIXTURE_POSIX_T0 = 1_000_000n;
export const FIXTURE_POSIX_T1 = 1_050_000n; // T0 + event_wait_duration
export const FIXTURE_POSIX_T2 = 1_100_000n;
export const FIXTURE_POSIX_T3 = 1_150_000n;

// ─── Addresses ───────────────────────────────────────────────────────────────

// TODO (implementation): use credentialToAddress("Preview", { type: "Key", hash: FIXTURE_PUB_KEY_HASH_A })
// from @lucid-evolution/lucid to build a real bech32 address.
export const FIXTURE_ADDRESS_KEY_A =
  "addr_test1vz0p8k0ekk5xvms5jlqmajgddmqm4xp58yd8c92lvd63hwcv6znrl";

// TODO (implementation): use credentialToAddress("Preview", { type: "Script", hash: FIXTURE_SCRIPT_HASH_A })
export const FIXTURE_ADDRESS_SCRIPT_A =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

// AddressData for FIXTURE_PUB_KEY_HASH_A (Lucid Data schema shape)
export const FIXTURE_ADDRESS_DATA_KEY_A = {
  paymentCredential: {
    PublicKeyCredential: [FIXTURE_PUB_KEY_HASH_A],
  },
  stakeCredential: null,
};

// ─── Validator fixture (always-succeeds script) ───────────────────────────────
//
// The real always-succeeds CBOR is "4d01000033222220051200120011".
// TODO (implementation): import ALWAYS_SUCCEEDS_SCRIPT from the always-succeeds
// fixture already present in demo/midgard-node/src/services/always-succeeds.ts.

export const FIXTURE_ALWAYS_SUCCEEDS_CBOR = "4d01000033222220051200120011";

export const FIXTURE_VALIDATOR = {
  mintingScriptCBOR: FIXTURE_ALWAYS_SUCCEEDS_CBOR,
  mintingScript: {
    type: "PlutusV3" as const,
    script: FIXTURE_ALWAYS_SUCCEEDS_CBOR,
  },
  policyId: FIXTURE_POLICY_ID_A,
  spendingScriptCBOR: FIXTURE_ALWAYS_SUCCEEDS_CBOR,
  spendingScript: {
    type: "PlutusV3" as const,
    script: FIXTURE_ALWAYS_SUCCEEDS_CBOR,
  },
  spendingScriptHash: FIXTURE_SCRIPT_HASH_A,
  spendingScriptAddress: FIXTURE_ADDRESS_SCRIPT_A,
};

// ─── Protocol parameters ─────────────────────────────────────────────────────
//
// TODO (implementation): call getProtocolParameters(NETWORK) from @sdk/protocol-parameters.ts
// instead of using this literal object.
export const FIXTURE_PROTOCOL_PARAMS = {
  event_wait_duration: 50_000,
  maturity_duration: 1n,
  slashing_penalty: 1_000_000n,
};

// ─── Nonce UTxO ───────────────────────────────────────────────────────────────
//
// One nonce UTxO per transaction family keeps asset names deterministic.
// The nonce's txHash+outputIndex are hashed to produce the NFT asset name.

export const FIXTURE_NONCE_UTXO = {
  txHash: FIXTURE_TX_HASH_A,
  outputIndex: 0,
  assets: { lovelace: 2_000_000n } as Record<string, bigint>,
  address: FIXTURE_ADDRESS_KEY_A,
};

// ─── Value fixture ────────────────────────────────────────────────────────────

export const FIXTURE_ASSET_NAME_A = "abcd";

export const FIXTURE_VALUE = {
  inner: new Map([
    [FIXTURE_POLICY_ID_A, new Map([[FIXTURE_ASSET_NAME_A, 42n]])],
  ]),
};

// ─── Withdrawal body and info ─────────────────────────────────────────────────

export const FIXTURE_WITHDRAWAL_BODY = {
  l2_outref: FIXTURE_OUTREF_B,
  l2_owner: FIXTURE_PUB_KEY_HASH_A,
  l2_value: FIXTURE_VALUE,
  l1_address: FIXTURE_ADDRESS_DATA_KEY_A,
  l1_datum: "NoDatum" as const,
};

export const FIXTURE_WITHDRAWAL_INFO = {
  body: FIXTURE_WITHDRAWAL_BODY,
  signature: new Map() as Map<string, string>,
  validity: "WithdrawalIsValid" as const,
};

// ─── Transaction CBOR fixture ─────────────────────────────────────────────────
//
// A minimal placeholder compact Cardano transaction CBOR (hex).
// TODO (implementation): replace with a real valid compact Cardano tx CBOR
// that passes the node transaction breakdown helpers.
export const FIXTURE_TX_CBOR_HEX = "deadbeef".repeat(16);

// ─── Header fixture ───────────────────────────────────────────────────────────

export const FIXTURE_HEADER = {
  prevUtxosRoot: FIXTURE_MERKLE_ROOT_A,
  utxosRoot: FIXTURE_MERKLE_ROOT_B,
  transactionsRoot: FIXTURE_MERKLE_ROOT_A,
  depositsRoot: FIXTURE_MERKLE_ROOT_B,
  withdrawalsRoot: FIXTURE_MERKLE_ROOT_A,
  startTime: FIXTURE_POSIX_T0,
  endTime: FIXTURE_POSIX_T1,
  prevHeaderHash: "77".repeat(28),
  operatorVkey: FIXTURE_PUB_KEY_HASH_A,
  protocolVersion: 1n,
};

// ─── Confirmed state fixture ──────────────────────────────────────────────────

export const FIXTURE_CONFIRMED_STATE = {
  headerHash: "88".repeat(28),
  prevHeaderHash: "99".repeat(28),
  utxoRoot: FIXTURE_MERKLE_ROOT_A,
  startTime: FIXTURE_POSIX_T0,
  endTime: FIXTURE_POSIX_T1,
  protocolVersion: 1n,
};
