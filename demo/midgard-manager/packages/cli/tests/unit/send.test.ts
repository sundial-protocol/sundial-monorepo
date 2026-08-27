import { describe, expect, it } from 'vitest';

import { parseSbtcToLovelace } from '../../src/commands/send.js';

describe('parseSbtcToLovelace', () => {
  it('treats a bare integer as whole sBTC (6 implied decimals)', () => {
    expect(parseSbtcToLovelace('1')).toBe(1_000_000n);
    expect(parseSbtcToLovelace('0')).toBe(0n);
    expect(parseSbtcToLovelace('123')).toBe(123_000_000n);
  });

  it('scales the fractional part to 6 decimal places', () => {
    expect(parseSbtcToLovelace('1.5')).toBe(1_500_000n);
    expect(parseSbtcToLovelace('10.5')).toBe(10_500_000n);
    expect(parseSbtcToLovelace('1.123456')).toBe(1_123_456n);
    expect(parseSbtcToLovelace('0.000001')).toBe(1n);
    expect(parseSbtcToLovelace('0.0')).toBe(0n);
  });

  it('accepts a trailing dot with no fractional digits', () => {
    expect(parseSbtcToLovelace('2.')).toBe(2_000_000n);
  });

  it('rejects more than 6 fractional digits', () => {
    expect(() => parseSbtcToLovelace('1.1234567')).toThrow(/Invalid amount/);
  });

  it('rejects a missing or non-numeric whole part', () => {
    expect(() => parseSbtcToLovelace('')).toThrow(/Invalid amount/);
    expect(() => parseSbtcToLovelace('.5')).toThrow(/Invalid amount/);
    expect(() => parseSbtcToLovelace('abc')).toThrow(/Invalid amount/);
    expect(() => parseSbtcToLovelace('-1')).toThrow(/Invalid amount/);
  });

  it('rejects a non-numeric fractional part', () => {
    expect(() => parseSbtcToLovelace('1.5e3')).toThrow(/Invalid amount/);
    expect(() => parseSbtcToLovelace('1,5')).toThrow(/Invalid amount/);
  });
});
