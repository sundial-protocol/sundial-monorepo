import { describe, expect, it } from 'vitest';

import { createSeededRandom, randomHex, randomInt } from '../../src/random.js';

describe('random helpers', () => {
  describe('createSeededRandom', () => {
    it('returns identical sequences for identical seeds', () => {
      const a = createSeededRandom('alpha-seed');
      const b = createSeededRandom('alpha-seed');

      const seqA = Array.from({ length: 10 }, () => a());
      const seqB = Array.from({ length: 10 }, () => b());

      expect(seqA).toEqual(seqB);
    });

    it('returns different sequences for different seeds', () => {
      const a = createSeededRandom('alpha-seed');
      const b = createSeededRandom('beta-seed');

      const seqA = Array.from({ length: 5 }, () => a());
      const seqB = Array.from({ length: 5 }, () => b());

      expect(seqA).not.toEqual(seqB);
    });

    it('works with an empty seed', () => {
      const a = createSeededRandom('');
      const b = createSeededRandom('');
      expect(Array.from({ length: 6 }, () => a())).toEqual(Array.from({ length: 6 }, () => b()));
    });

    it('always returns values >= 0', () => {
      const random = createSeededRandom('lower-bound-check');
      const values = Array.from({ length: 200 }, () => random());
      expect(values.every((value) => value >= 0)).toBe(true);
    });

    it('always returns values < 1', () => {
      const random = createSeededRandom('upper-bound-check');
      const values = Array.from({ length: 200 }, () => random());
      expect(values.every((value) => value < 1)).toBe(true);
    });

    it('advances internal state on each call', () => {
      const random = createSeededRandom('stateful-seed');
      const values = Array.from({ length: 5 }, () => random());
      expect(new Set(values).size).toBeGreaterThan(1);
    });

    it('supports long seeds deterministically', () => {
      const seed = 'long-'.repeat(100);
      const a = createSeededRandom(seed);
      const b = createSeededRandom(seed);
      expect(Array.from({ length: 8 }, () => a())).toEqual(Array.from({ length: 8 }, () => b()));
    });
  });

  describe('randomInt', () => {
    it('returns 0 when random() returns 0', () => {
      expect(randomInt(() => 0, 7)).toBe(0);
    });

    it('returns upperExclusive - 1 when random() is very close to 1', () => {
      expect(randomInt(() => 0.9999999999, 7)).toBe(6);
    });

    it('returns only values inside [0, upperExclusive)', () => {
      const random = createSeededRandom('int-range');
      const values = Array.from({ length: 200 }, () => randomInt(random, 13));
      expect(values.every((value) => value >= 0 && value < 13)).toBe(true);
    });

    it('maps upperExclusive=1 to the only valid result 0', () => {
      expect(randomInt(() => 0.5, 1)).toBe(0);
    });

    it('floors values for upperExclusive=2 at random=0.49', () => {
      expect(randomInt(() => 0.49, 2)).toBe(0);
    });

    it('floors values for upperExclusive=2 at random=0.5', () => {
      expect(randomInt(() => 0.5, 2)).toBe(1);
    });

    it('floors values for upperExclusive=10 at random=0.123', () => {
      expect(randomInt(() => 0.123, 10)).toBe(1);
    });

    it('floors values for upperExclusive=10 at random=0.999', () => {
      expect(randomInt(() => 0.999, 10)).toBe(9);
    });

    it('throws for upperExclusive=0', () => {
      expect(() => randomInt(() => 0.5, 0)).toThrow('upperExclusive must be a positive integer');
    });

    it('throws for negative upperExclusive', () => {
      expect(() => randomInt(() => 0.5, -1)).toThrow('upperExclusive must be a positive integer');
    });

    it('throws for non-integer upperExclusive', () => {
      expect(() => randomInt(() => 0.5, 1.2)).toThrow('upperExclusive must be a positive integer');
    });

    it('throws for NaN upperExclusive', () => {
      expect(() => randomInt(() => 0.5, Number.NaN)).toThrow(
        'upperExclusive must be a positive integer'
      );
    });

    it('throws for Infinity upperExclusive', () => {
      expect(() => randomInt(() => 0.5, Number.POSITIVE_INFINITY)).toThrow(
        'upperExclusive must be a positive integer'
      );
    });
  });

  describe('randomHex', () => {
    it('returns a hex string with requested length 1', () => {
      const random = createSeededRandom('len-1');
      expect(randomHex(random, 1)).toHaveLength(1);
    });

    it('returns a hex string with requested length 64', () => {
      const random = createSeededRandom('len-64');
      expect(randomHex(random, 64)).toHaveLength(64);
    });

    it('returns only lowercase hex characters', () => {
      const random = createSeededRandom('charset');
      const value = randomHex(random, 128);
      expect(value).toMatch(/^[0-9a-f]+$/);
    });

    it('is deterministic for the same seed and length', () => {
      const a = createSeededRandom('hex-seed');
      const b = createSeededRandom('hex-seed');
      expect(randomHex(a, 32)).toBe(randomHex(b, 32));
    });

    it('usually differs for different seeds', () => {
      const a = createSeededRandom('hex-seed-a');
      const b = createSeededRandom('hex-seed-b');
      expect(randomHex(a, 32)).not.toBe(randomHex(b, 32));
    });

    it('advances RNG state between calls', () => {
      const random = createSeededRandom('hex-state');
      const first = randomHex(random, 16);
      const second = randomHex(random, 16);
      expect(first).not.toBe(second);
    });

    it('throws when length is 0', () => {
      expect(() => randomHex(() => 0.1, 0)).toThrow('length must be a positive integer');
    });

    it('throws when length is negative', () => {
      expect(() => randomHex(() => 0.1, -2)).toThrow('length must be a positive integer');
    });

    it('throws when length is non-integer', () => {
      expect(() => randomHex(() => 0.1, 2.5)).toThrow('length must be a positive integer');
    });

    it('throws when length is NaN', () => {
      expect(() => randomHex(() => 0.1, Number.NaN)).toThrow('length must be a positive integer');
    });

    it('returns same output for two independent identical RNG streams', () => {
      const seed = 'parallel-streams';
      const a = createSeededRandom(seed);
      const b = createSeededRandom(seed);
      const valuesA = [randomHex(a, 8), randomHex(a, 8), randomHex(a, 8)];
      const valuesB = [randomHex(b, 8), randomHex(b, 8), randomHex(b, 8)];
      expect(valuesA).toEqual(valuesB);
    });

    it('handles very short deterministic sequences', () => {
      const random = () => 0;
      expect(randomHex(random, 6)).toBe('000000');
    });
  });
});
