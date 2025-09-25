import { Data } from "effect";

export class WorkerError extends Data.TaggedError("WorkerError")<{
  readonly worker: string;
  readonly message: string;
  readonly cause: unknown;
}> {}
