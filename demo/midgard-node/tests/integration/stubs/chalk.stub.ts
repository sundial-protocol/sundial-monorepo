// Passthrough stub — keeps chalk from pulling in colour-output dependencies
// during integration test runs.
export class Chalk {
  bold(value: string): string {
    return value;
  }
}
