// Minimal SDK stub for integration tests.
//
// Integration tests that exercise the L1-provider boundary directly should
// replace this with a localhost HTTP stub server (see test plan §External
// Dependency Replacement Matrix).  Tests that care only about node-internal
// behavior after the boundary returns values can use this stub directly.

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

// Exported for tests that reference SDK error types.
export class CborDeserializationError extends Error {
  constructor(fields: GenericErrorFields) {
    super(fields.message);
    this.name = "CborDeserializationError";
  }
}

export class StateQueueError extends Error {
  constructor(fields: GenericErrorFields) {
    super(fields.message);
    this.name = "StateQueueError";
  }
}

export class LucidError extends Error {
  constructor(fields: GenericErrorFields) {
    super(fields.message);
    this.name = "LucidError";
  }
}
