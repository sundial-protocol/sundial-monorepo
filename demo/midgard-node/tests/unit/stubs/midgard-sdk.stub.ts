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
