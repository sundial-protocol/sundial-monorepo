import { fileURLToPath } from 'node:url';

import tsParser from '@typescript-eslint/parser';
import tsPlugin from '@typescript-eslint/eslint-plugin';
import eslintConfigPrettier from 'eslint-config-prettier';

const tsconfigRootDir = fileURLToPath(new URL('.', import.meta.url));
const sourceFiles = ['src/**/*.ts'];
const typeCheckedConfig = tsPlugin.configs[
  'flat/recommended-type-checked'
].map((config) => ({
  ...config,
  files: sourceFiles,
  languageOptions: {
    ...config.languageOptions,
    parserOptions: {
      ...config.languageOptions?.parserOptions,
      projectService: true,
      tsconfigRootDir,
    },
  },
}));

export default [
  {
    ignores: ['dist/**', 'node_modules/**'],
  },
  ...typeCheckedConfig,
  {
    files: sourceFiles,
    languageOptions: {
      parser: tsParser,
      ecmaVersion: 'latest',
      sourceType: 'module',
    },
    rules: {
      '@typescript-eslint/no-base-to-string': 'off',
      '@typescript-eslint/no-empty-object-type': 'off',
      '@typescript-eslint/no-explicit-any': 'error',
      '@typescript-eslint/no-floating-promises': 'error',
      '@typescript-eslint/no-misused-promises': 'error',
      '@typescript-eslint/no-non-null-assertion': 'error',
      '@typescript-eslint/no-unsafe-assignment': 'error',
      '@typescript-eslint/no-unsafe-member-access': 'error',
      '@typescript-eslint/no-unused-vars': 'off',
      '@typescript-eslint/restrict-template-expressions': 'off',
    },
  },
  eslintConfigPrettier,
];
