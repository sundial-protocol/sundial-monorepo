import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Context, Effect, Option, Tracer } from "effect";

// Minimal Span implementation for capturing tracer calls.
function makeCapturingSpan(
  name: string,
  parent: Option.Option<Tracer.AnySpan>,
): Tracer.Span {
  return {
    _tag: "Span",
    name,
    spanId: "test-span-id",
    traceId: "test-trace-id",
    parent,
    context: Context.empty(),
    sampled: true,
    attributes: new Map(),
    status: { _tag: "Started", startTime: 0n },
    links: [],
    kind: "internal",
    end: () => {},
    attribute: () => {},
    event: () => {},
    addLinks: () => {},
  };
}

// Build a tracer that records the parent passed when a named span is created.
function makeRecordingTracer(
  record: Map<string, Option.Option<Tracer.AnySpan>>,
): Tracer.Tracer {
  return Tracer.make({
    span: (name, parent) => {
      record.set(name, parent);
      return makeCapturingSpan(name, parent);
    },
    context: (f) => f(),
  });
}

// A fake ambient parent span to inject as the outer context.
const AMBIENT_SPAN: Tracer.Span = makeCapturingSpan(
  "ambient-parent",
  Option.none(),
);

// Run a span-wrapped void effect inside an ambient parent span context and
// return the parent value that the tracer received for the named span.
function captureParentFor(
  spanName: string,
  opts: { root: boolean },
): Effect.Effect<Option.Option<Tracer.AnySpan>> {
  return Effect.gen(function* () {
    const record = new Map<string, Option.Option<Tracer.AnySpan>>();
    yield* Effect.void.pipe(
      Effect.withSpan(spanName, opts.root ? { root: true } : {}),
      // Provide an ambient parent so we can verify root:true ignores it.
      Effect.withSpan("ambient-parent"),
      Effect.withTracer(makeRecordingTracer(record)),
    );
    return record.get(spanName) ?? Option.none();
  });
}

describe("fiber per-iteration spans are root spans", () => {
  const fiberSpans = [
    "block-commitment-fiber",
    "submit-blocks-fiber",
    "merge-confirmed-state-fiber",
    "sync-user-events-fiber",
  ] as const;

  it.effect.each(fiberSpans.map((name) => ({ name })))(
    "$name span has no parent even when an ambient parent span is active",
    ({ name }) =>
      Effect.gen(function* () {
        const parent = yield* captureParentFor(name, { root: true });
        expect(Option.isNone(parent)).toBe(true);
      }),
  );

  it.effect.each(fiberSpans.map((name) => ({ name })))(
    "$name span without root:true inherits the ambient parent (control case)",
    ({ name }) =>
      Effect.gen(function* () {
        const parent = yield* captureParentFor(name, { root: false });
        expect(Option.isSome(parent)).toBe(true);
      }),
  );
});

describe("AMBIENT_SPAN fixture", () => {
  it("ambient span itself has no parent", () => {
    expect(Option.isNone(AMBIENT_SPAN.parent)).toBe(true);
  });
});
