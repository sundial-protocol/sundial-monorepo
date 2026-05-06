type GenericFields = {
  message?: string;
  cause?: unknown;
};

export const Data = {
  TaggedError: (name: string) =>
    class extends Error {
      cause?: unknown;

      constructor(fields: GenericFields = {}) {
        super(fields.message ?? name);
        this.name = name;
        this.cause = fields.cause;
      }
    },
};

const notImplemented = (name: string): never => {
  throw new Error(`${name} is not implemented in unit stubs.`);
};

export const Effect = {
  runSync: (_value: unknown): void => {},
  logInfo: (_msg: string): undefined => undefined,
  logWarning: (_msg: string): undefined => undefined,
  logError: (_msg: string): undefined => undefined,
  gen: (): never => notImplemented("Effect.gen"),
  try: (): never => notImplemented("Effect.try"),
  forEach: (): never => notImplemented("Effect.forEach"),
  withSpan:
    (_name: string) =>
    <T>(value: T): T =>
      value,
};

export const pipe = <T>(value: T): T => value;
