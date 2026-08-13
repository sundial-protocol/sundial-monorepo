export type GenericErrorFields = {
  message: string;
  cause?: unknown;
};

export type Network = "Mainnet" | "Preview" | "Preprod" | "Custom";

export type ProtocolParameters = {
  event_wait_duration: number;
  maturity_duration: bigint;
  slashing_penalty: bigint;
};

export const getProtocolParameters = (network: Network): ProtocolParameters => {
  if (network === "Mainnet") {
    return {
      event_wait_duration: 60_000,
      maturity_duration: 30n,
      slashing_penalty: 2000000n,
    };
  }
  return {
    event_wait_duration: 50_000,
    maturity_duration: 1n,
    slashing_penalty: 1000000n,
  };
};

export class CmlUnexpectedError extends Error {
  constructor(fields: GenericErrorFields) {
    super(fields.message);
    this.name = "CmlUnexpectedError";
  }
}

export class CmlDeserializationError extends Error {
  constructor(fields: GenericErrorFields) {
    super(fields.message);
    this.name = "CmlDeserializationError";
  }
}

export class LucidError extends Error {
  readonly _tag = "LucidError";
  readonly cause?: unknown;
  constructor(fields: GenericErrorFields) {
    super(fields.message);
    this.name = "LucidError";
    this.cause = fields.cause;
  }
}

export const outRefKey = (ref: { tx_id: Uint8Array; index: number }): string =>
  `${Buffer.from(ref.tx_id).toString("hex")}:${ref.index}`;

export const cmlToMidgard = (_tx: unknown) => ({
  body: {
    inputs: [{ tx_id: Buffer.alloc(32, 0xaa), index: 0 }],
    outputs: [
      {
        address: Buffer.from([0x00]),
        value: { type: "Coin" as const, coin: 1n },
        datum: undefined,
        script_ref: undefined,
      },
    ],
    fee: 0n,
    ttl: undefined,
    auxiliary_data_hash: undefined,
    validity_interval_start: undefined,
    mint: undefined,
    script_data_hash: undefined,
    required_signers: undefined,
    network_id: 0,
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
});

export const cmlOutputToMidgard = (_output: unknown) => ({
  address: Buffer.from([0x00]),
  value: { type: "Coin" as const, coin: 1n },
  datum: undefined,
  script_ref: undefined,
});

export const runPhaseAValidation = (
  queuedTxs: ReadonlyArray<{
    txId: Uint8Array;
    tx: ReturnType<typeof cmlToMidgard>;
    arrivalSeq: bigint;
  }>,
) => ({
  accepted: queuedTxs.map((queuedTx) => ({
    txId: queuedTx.txId,
    tx: queuedTx.tx,
    arrivalSeq: queuedTx.arrivalSeq,
    fee: queuedTx.tx.body.fee,
    validityIntervalStart: undefined,
    validityIntervalEnd: undefined,
    referenceInputs: [],
    outputSum: {
      coin: () => 1n,
      has_multiassets: () => false,
      is_zero: () => true,
    },
    witnessKeyHashes: [],
    nativeScriptHashes: [],
    spent: queuedTx.tx.body.inputs,
    produced: queuedTx.tx.body.outputs.map((output, index) => ({
      outRef: { tx_id: queuedTx.txId, index },
      output,
    })),
  })),
  rejected: [],
});

export const runPhaseBValidationWithPatch = (
  accepted: ReturnType<typeof runPhaseAValidation>["accepted"],
) => ({
  accepted,
  rejected: [],
  statePatch: {
    deletedOutRefs: [],
    upsertedOutRefs: [],
  },
});
