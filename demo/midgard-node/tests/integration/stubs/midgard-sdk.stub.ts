export {
  type GenericErrorFields,
  type Network,
  type ProtocolParameters,
  outRefKey,
  cmlToMidgard,
  cmlOutputToMidgard,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  getProtocolParameters,
  CmlUnexpectedError,
  CmlDeserializationError,
} from "../../unit/stubs/midgard-sdk.stub.js";

export class CborDeserializationError extends Error {
  constructor(fields: { message: string; cause?: unknown }) {
    super(fields.message);
    this.name = "CborDeserializationError";
  }
}

export class StateQueueError extends Error {
  constructor(fields: { message: string; cause?: unknown }) {
    super(fields.message);
    this.name = "StateQueueError";
  }
}

export class LucidError extends Error {
  constructor(fields: { message: string; cause?: unknown }) {
    super(fields.message);
    this.name = "LucidError";
  }
}
