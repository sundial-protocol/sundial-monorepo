import { Effect } from "effect";

const subtractMetricValues = (
  after: number | bigint,
  before: number | bigint,
): number | bigint =>
  typeof after === "bigint" && typeof before === "bigint"
    ? after - before
    : Number(after) - Number(before);

export const metricDelta = <State, Error, Requirements>(
  readState: Effect.Effect<State, never, Requirements>,
  action: Effect.Effect<void, Error, Requirements>,
  select: (state: State) => number | bigint,
): Effect.Effect<number | bigint, Error, Requirements> =>
  Effect.gen(function* () {
    const before = select(yield* readState);
    yield* action;
    const after = select(yield* readState);
    return subtractMetricValues(after, before);
  });
