import { Data } from "effect";

// A module for general errors that doesn't have a specific domain
export class DeserializationError extends Data.TaggedError(
  "DeserializationError",
)<{
  readonly message: string;
  readonly cause?: unknown;
}> {}

export class SerializationError extends Data.TaggedError("SerializationError")<{
  readonly message: string;
  readonly cause?: unknown;
}> {}

export class LucidError extends Data.TaggedError("LucidError")<{
  readonly message: string;
  readonly cause?: unknown;
}> {}
