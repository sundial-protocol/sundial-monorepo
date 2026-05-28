const TRUE_VALUES = new Set(['1', 'true', 'yes', 'on']);

const readBooleanEnvFlag = (name: string): boolean => {
  const raw = process.env[name];
  if (raw === undefined) {
    return false;
  }
  return TRUE_VALUES.has(raw.trim().toLowerCase());
};

export const isRelaxedGatesEnabled = (): boolean => readBooleanEnvFlag('SCALABILITY_RELAXED_GATES');
