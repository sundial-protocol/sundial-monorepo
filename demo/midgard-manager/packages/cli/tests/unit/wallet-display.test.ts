import { describe, expect, it } from 'vitest';

import { maskKey } from '../../src/commands/wallet.js';

describe('maskKey', () => {
  it('keeps the first 10 and last 5 characters, hiding the middle', () => {
    const key = 'ed25519_sk1qqqqqqqqqqqqqqqqqqqqqqqqqqzzzzz';
    expect(maskKey(key)).toBe('ed25519_sk...zzzzz');
  });

  it('never reveals the full secret for a realistic-length key', () => {
    const key = `ed25519_sk1${'a'.repeat(50)}wxyz9`;
    const masked = maskKey(key);
    expect(masked.startsWith('ed25519_sk')).toBe(true);
    expect(masked.endsWith('wxyz9')).toBe(true);
    expect(masked).not.toContain('aaaaaaaaaa');
  });
});
