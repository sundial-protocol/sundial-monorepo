const HEX_ALPHABET = '0123456789abcdef';
const U32_MAX_PLUS_ONE = 0x1_0000_0000;

const createSeedHash = (seed: string): number => {
  let h = 2166136261 >>> 0;
  for (let i = 0; i < seed.length; i++) {
    h ^= seed.charCodeAt(i);
    h = Math.imul(h, 16777619);
  }
  return h >>> 0;
};

const createMulberry32 = (seed: number): (() => number) => {
  let state = seed >>> 0;
  return () => {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = Math.imul(state ^ (state >>> 15), 1 | state);
    t ^= t + Math.imul(t ^ (t >>> 7), 61 | t);
    return ((t ^ (t >>> 14)) >>> 0) / U32_MAX_PLUS_ONE;
  };
};

export const createSeededRandom = (seed: string): (() => number) =>
  createMulberry32(createSeedHash(seed));

export const randomInt = (random: () => number, upperExclusive: number): number => {
  if (!Number.isInteger(upperExclusive) || upperExclusive <= 0) {
    throw new Error(`upperExclusive must be a positive integer, got ${upperExclusive}`);
  }
  return Math.floor(random() * upperExclusive);
};

export const randomHex = (random: () => number, length: number): string => {
  if (!Number.isInteger(length) || length < 1) {
    throw new Error(`length must be a positive integer, got ${length}`);
  }
  let result = '';
  for (let i = 0; i < length; i++) {
    result += HEX_ALPHABET[randomInt(random, HEX_ALPHABET.length)];
  }
  return result;
};
