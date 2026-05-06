export type GenericErrorFields = {
  message: string;
  cause?: unknown;
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
