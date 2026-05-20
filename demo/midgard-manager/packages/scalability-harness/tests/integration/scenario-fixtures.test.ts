import { readdir, readFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, expect, it } from 'vitest';

import { validateScenario } from '../../src/config/scenario.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const SCENARIOS_DIR = path.resolve(__dirname, '../../scenarios');

describe('validateScenario fixtures', () => {
  it('validates all scenario JSON files in scenarios/', async () => {
    const files = (await readdir(SCENARIOS_DIR)).filter((name) => name.endsWith('.json'));
    for (const fileName of files) {
      const filePath = path.join(SCENARIOS_DIR, fileName);
      const rawText = await readFile(filePath, 'utf8');
      const parsed = JSON.parse(rawText) as unknown;
      expect(() => validateScenario(parsed), fileName).not.toThrow();
    }
  });
});
