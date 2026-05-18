const FILESYSTEM_SAFE_PLAN_ID = /^[A-Za-z0-9_-]+$/;

export interface PlanConfig {
  planId: string;
  description?: string;
  outputDir: string;
  // When true (default), abort the plan if any scenario returns Failed or Blocked.
  // Scenarios that are Passed with Observations are not treated as failures.
  stopOnFailure?: boolean;
  // Paths to scenario JSON files, relative to the plan file's directory.
  scenarios: string[];
}

export class PlanValidationError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'PlanValidationError';
  }
}

export function validatePlan(raw: unknown): PlanConfig {
  if (typeof raw !== 'object' || raw === null) {
    throw new PlanValidationError('Plan must be a JSON object');
  }

  const p = raw as Record<string, unknown>;

  if (typeof p.planId !== 'string' || p.planId.length === 0) {
    throw new PlanValidationError('planId must be a non-empty string');
  }
  if (!FILESYSTEM_SAFE_PLAN_ID.test(p.planId)) {
    throw new PlanValidationError(
      `planId must contain only alphanumeric characters, dashes, and underscores, got: ${p.planId}`
    );
  }

  if (p.description !== undefined) {
    if (typeof p.description !== 'string' || p.description.trim().length === 0) {
      throw new PlanValidationError('description must be a non-empty string when provided');
    }
  }

  if (typeof p.outputDir !== 'string' || p.outputDir.length === 0) {
    throw new PlanValidationError('outputDir must be a non-empty string');
  }

  if (p.stopOnFailure !== undefined && typeof p.stopOnFailure !== 'boolean') {
    throw new PlanValidationError('stopOnFailure must be a boolean when provided');
  }

  if (!Array.isArray(p.scenarios)) {
    throw new PlanValidationError('scenarios must be an array');
  }
  if ((p.scenarios as unknown[]).length === 0) {
    throw new PlanValidationError('scenarios must contain at least one entry');
  }
  for (const [i, s] of (p.scenarios as unknown[]).entries()) {
    if (typeof s !== 'string' || (s as string).trim().length === 0) {
      throw new PlanValidationError(`scenarios[${i}] must be a non-empty string path`);
    }
  }

  return {
    planId: p.planId,
    description: p.description as string | undefined,
    outputDir: p.outputDir as string,
    stopOnFailure: (p.stopOnFailure as boolean | undefined) ?? true,
    scenarios: p.scenarios as string[],
  };
}
