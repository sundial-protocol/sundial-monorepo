#!/usr/bin/env node
import { execFile } from 'node:child_process';
import process from 'node:process';
import { promisify } from 'node:util';
import { Command } from 'commander';
import {
  resolveEnvironment,
  loadAwsSessionEnv,
  resolveRegion,
} from './schema-version.mjs';
import { resolveServiceName } from './service-version.mjs';
import { pathToFileURL } from 'node:url';
import { mkdtemp, readdir, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import path from 'node:path';

const execFileAsync = promisify(execFile);

export function formatReleaseTimestamp(value = new Date()) {
  const iso = value.toISOString();
  const compact = iso.replace(/[-:]/g, '');
  return compact.slice(0, 15) + 'Z';
}

export function buildReleaseTagName({
  service,
  sha,
  timestamp = formatReleaseTimestamp(),
}) {
  const shortSha = sha.slice(0, 7);
  return `${service}-${timestamp}-${shortSha}`;
}

export function buildReleaseManifest({
  environment,
  service,
  branch,
  repo,
  sha,
  tag,
  releaseTitle,
  pr,
  requiredContexts,
  repositorySchemaVersion,
  imageDigest,
  generatedAt = new Date().toISOString(),
}) {
  return {
    schemaVersion: 1,
    generatedAt,
    environment,
    service,
    branch,
    repository: repo,
    commitSha: sha,
    gitTag: tag,
    releaseTitle,
    pullRequest: {
      number: pr.number,
      title: pr.title,
      mergedAt: pr.merged_at,
    },
    requiredChecks: requiredContexts,
    artifacts: {
      imageTag: tag,
      imageDigest,
      imageDigestStatus: imageDigest ? 'resolved' : 'resolved_during_deploy',
    },
    databaseSchema: {
      repositoryMigrationHead: repositorySchemaVersion,
      verificationCommand: `npm run infra:${environment}:schema:version`,
    },
    rollout: {
      sourceBranch: branch,
      deployCommand: `npm run infra:${environment}:apply:${service}`,
      deployMode: 'digest_pinned_targeted_terraform_apply',
    },
  };
}

async function resolveRepositorySchemaVersion(rootDir = process.cwd()) {
  const migrationsDir = path.join(rootDir, 'packages/db/prisma/migrations');
  const entries = await readdir(migrationsDir, { withFileTypes: true });
  const migrationNames = entries
    .filter((entry) => entry.isDirectory())
    .map((entry) => entry.name)
    .sort();
  if (migrationNames.length === 0) {
    return '(none)';
  }

  return migrationNames.at(-1) ?? '(none)';
}

async function createReleaseManifestFile(manifest) {
  const tempDir = await mkdtemp(path.join(tmpdir(), 'btc-yield-release-'));
  const manifestFileName = `${manifest.gitTag}.release-manifest.json`;
  const filePath = path.join(tempDir, manifestFileName);
  await writeFile(filePath, `${JSON.stringify(manifest, null, 2)}\n`, 'utf8');

  return {
    filePath,
    fileName: manifestFileName,
    cleanup: async () => {
      await rm(tempDir, { recursive: true, force: true });
    },
  };
}

function resolveServiceRepositoryName(service, env) {
  if (service === 'api') {
    return env.AWS_ECR_API_REPOSITORY ?? 'btc-yield/api';
  }

  if (service === 'indexer') {
    return env.AWS_ECR_INDEXER_REPOSITORY ?? 'btc-yield/indexer';
  }

  throw new Error(
    `unsupported service repository resolution service=${service}`,
  );
}

async function resolveServiceImageDigest({
  service,
  environment,
  tag,
  env,
  commandJsonFn,
}) {
  const region = resolveRegion(environment);
  const repositoryName = resolveServiceRepositoryName(service, env);

  try {
    const payload = await commandJsonFn(
      'aws',
      [
        'ecr',
        'describe-images',
        '--repository-name',
        repositoryName,
        '--image-ids',
        `imageTag=${tag}`,
        '--region',
        region,
        '--output',
        'json',
      ],
      { env },
    );
    const digest = payload?.imageDetails?.[0]?.imageDigest;
    return typeof digest === 'string' && digest.length > 0 ? digest : null;
  } catch {
    return null;
  }
}

function isCliNotFound(error) {
  return error && typeof error === 'object' && error.code === 'ENOENT';
}

function isGhNotFoundError(error) {
  if (!error || typeof error !== 'object') {
    return false;
  }

  const stdout =
    'stdout' in error && typeof error.stdout === 'string' ? error.stdout : '';
  const stderr =
    'stderr' in error && typeof error.stderr === 'string' ? error.stderr : '';
  const message = error instanceof Error ? error.message : '';
  const combined = `${stdout}\n${stderr}\n${message}`;
  return (
    combined.includes('HTTP 404') ||
    combined.includes('Not Found') ||
    combined.includes('Branch not protected')
  );
}

async function commandJson(command, args, options = {}) {
  const { stdout } = await execFileAsync(command, args, options);
  return JSON.parse(stdout);
}

async function commandText(command, args, options = {}) {
  const { stdout } = await execFileAsync(command, args, options);
  return stdout.trim();
}

async function runCommand(command, args, options = {}) {
  await execFileAsync(command, args, options);
}

async function fetchRepoFullName(env, deps) {
  if (env.GITHUB_REPOSITORY && env.GITHUB_REPOSITORY.length > 0) {
    return env.GITHUB_REPOSITORY;
  }

  return deps.commandText(
    'gh',
    ['repo', 'view', '--json', 'nameWithOwner', '--jq', '.nameWithOwner'],
    { env },
  );
}

export function resolveRequiredCheckContexts(requiredChecksResponse) {
  const contexts = new Set(requiredChecksResponse?.contexts ?? []);
  for (const check of requiredChecksResponse?.checks ?? []) {
    if (
      check &&
      typeof check.context === 'string' &&
      check.context.length > 0
    ) {
      contexts.add(check.context);
    }
  }

  return [...contexts].sort();
}

export function resolveRulesetRequiredCheckContexts(rulesResponse) {
  const contexts = new Set();

  for (const rule of rulesResponse ?? []) {
    if (rule?.type !== 'required_status_checks') {
      continue;
    }

    const requiredChecks = rule?.parameters?.required_status_checks;
    if (!Array.isArray(requiredChecks)) {
      continue;
    }

    for (const check of requiredChecks) {
      if (
        check &&
        typeof check.context === 'string' &&
        check.context.length > 0
      ) {
        contexts.add(check.context);
      }
    }
  }

  return [...contexts].sort();
}

async function fetchRequiredCheckContexts(branch, repo, env, deps) {
  try {
    const requiredChecksPayload = await deps.commandJson(
      'gh',
      [
        'api',
        '-H',
        'Accept: application/vnd.github+json',
        `repos/${repo}/branches/${branch}/protection/required_status_checks`,
      ],
      { env },
    );

    return resolveRequiredCheckContexts(requiredChecksPayload);
  } catch (error) {
    if (!isGhNotFoundError(error)) {
      throw error;
    }
  }

  const rulesPayload = await deps.commandJson(
    'gh',
    [
      'api',
      '-H',
      'Accept: application/vnd.github+json',
      `repos/${repo}/rules/branches/${branch}`,
    ],
    { env },
  );

  return resolveRulesetRequiredCheckContexts(rulesPayload);
}

function parseTimestamp(value) {
  if (typeof value !== 'string') {
    return 0;
  }

  const parsed = Date.parse(value);
  return Number.isNaN(parsed) ? 0 : parsed;
}

export function evaluateRequiredChecks({
  requiredContexts,
  commitStatusResponse,
  checkRunsResponse,
}) {
  const statusByContext = new Map();
  for (const status of commitStatusResponse?.statuses ?? []) {
    if (
      status &&
      typeof status.context === 'string' &&
      typeof status.state === 'string'
    ) {
      const statusTime = parseTimestamp(status.updated_at ?? status.created_at);
      const statusId = Number.isFinite(Number(status.id))
        ? Number(status.id)
        : 0;
      const previous = statusByContext.get(status.context);
      if (
        !previous ||
        statusTime > previous.statusTime ||
        (statusTime === previous.statusTime && statusId > previous.statusId)
      ) {
        statusByContext.set(status.context, {
          state: status.state,
          statusTime,
          statusId,
        });
      }
    }
  }

  const checksByName = new Map();
  for (const run of checkRunsResponse?.check_runs ?? []) {
    if (!run || typeof run.name !== 'string') {
      continue;
    }

    const checkTime = parseTimestamp(
      run.completed_at ?? run.started_at ?? run.created_at,
    );
    const checkId = Number.isFinite(Number(run.id)) ? Number(run.id) : 0;
    const isSuccess =
      run.status === 'completed' && run.conclusion === 'success';
    const previous = checksByName.get(run.name);
    if (
      !previous ||
      checkTime > previous.checkTime ||
      (checkTime === previous.checkTime && checkId > previous.checkId)
    ) {
      checksByName.set(run.name, {
        success: isSuccess,
        checkTime,
        checkId,
      });
    }
  }

  const failedContexts = [];
  for (const context of requiredContexts) {
    const statusState = statusByContext.get(context)?.state;
    const statusPassed = statusState === 'success';
    const checkPassed = checksByName.get(context)?.success === true;
    if (!statusPassed && !checkPassed) {
      failedContexts.push(context);
    }
  }

  return {
    failedContexts,
    passed: failedContexts.length === 0,
  };
}

async function assertTagDoesNotExist(tag, repo, env, deps) {
  try {
    await deps.commandJson(
      'gh',
      [
        'api',
        '-H',
        'Accept: application/vnd.github+json',
        `repos/${repo}/git/ref/tags/${encodeURIComponent(tag)}`,
      ],
      { env },
    );
    throw new Error(`tag "${tag}" already exists in ${repo}`);
  } catch (error) {
    if (isGhNotFoundError(error)) {
      return;
    }

    if (error instanceof Error && error.message.includes('already exists')) {
      throw error;
    }

    throw error;
  }
}

function buildReleaseNotes({
  environment,
  service,
  branch,
  sha,
  pr,
  requiredContexts,
}) {
  const checks =
    requiredContexts.length > 0 ? requiredContexts.join(', ') : '(none)';
  return [
    `Environment: ${environment}`,
    `Service: ${service}`,
    `Branch: ${branch}`,
    `Commit: ${sha}`,
    `Merged PR: #${pr.number} ${pr.title}`,
    `Merged at: ${pr.merged_at}`,
    `Required checks: ${checks}`,
  ].join('\n');
}

export async function runReleaseTagFlow(input, overrides = {}) {
  const service = resolveServiceName(input.service);
  const environment = resolveEnvironment(input.environment ?? 'testnet');
  const branch = input.branch ?? environment;

  const deps = {
    commandJson: overrides.commandJson ?? commandJson,
    commandText: overrides.commandText ?? commandText,
    runCommand: overrides.runCommand ?? runCommand,
    createReleaseManifestFile:
      overrides.createReleaseManifestFile ?? createReleaseManifestFile,
    resolveServiceImageDigest:
      overrides.resolveServiceImageDigest ?? resolveServiceImageDigest,
    resolveRepositorySchemaVersion:
      overrides.resolveRepositorySchemaVersion ??
      resolveRepositorySchemaVersion,
    loadAwsSessionEnv: overrides.loadAwsSessionEnv ?? loadAwsSessionEnv,
    fetchRequiredCheckContexts:
      overrides.fetchRequiredCheckContexts ?? fetchRequiredCheckContexts,
    env: overrides.env ?? process.env,
  };

  const runtimeEnv = await deps.loadAwsSessionEnv(deps.env);
  const repo = input.repo ?? (await fetchRepoFullName(runtimeEnv, deps));
  const branchResponse = await deps.commandJson(
    'gh',
    [
      'api',
      '-H',
      'Accept: application/vnd.github+json',
      `repos/${repo}/branches/${branch}`,
    ],
    { env: runtimeEnv },
  );
  const branchHeadSha = branchResponse?.commit?.sha;
  if (!branchHeadSha || typeof branchHeadSha !== 'string') {
    throw new Error(`unable to resolve HEAD SHA for branch "${branch}"`);
  }

  const sha = input.sha ?? branchHeadSha;
  if (sha !== branchHeadSha) {
    throw new Error(
      `commit "${sha}" is not HEAD of branch "${branch}" (${branchHeadSha})`,
    );
  }

  const commitPrs = await deps.commandJson(
    'gh',
    [
      'api',
      '-H',
      'Accept: application/vnd.github+json',
      `repos/${repo}/commits/${sha}/pulls`,
    ],
    { env: runtimeEnv },
  );
  const mergedPrs = (commitPrs ?? []).filter(
    (pr) =>
      pr &&
      typeof pr.number === 'number' &&
      pr.merged_at &&
      pr.base?.ref === branch,
  );
  if (mergedPrs.length === 0) {
    throw new Error(
      `commit "${sha}" is not associated with a merged PR into branch "${branch}"`,
    );
  }
  mergedPrs.sort(
    (a, b) => new Date(b.merged_at).getTime() - new Date(a.merged_at).getTime(),
  );
  const mergedPr = mergedPrs[0];

  const requiredContexts = await deps.fetchRequiredCheckContexts(
    branch,
    repo,
    runtimeEnv,
    deps,
  );
  if (requiredContexts.length === 0) {
    throw new Error(
      `branch "${branch}" has no configured required checks; refusing to tag`,
    );
  }

  const commitStatusResponse = await deps.commandJson(
    'gh',
    [
      'api',
      '-H',
      'Accept: application/vnd.github+json',
      `repos/${repo}/commits/${sha}/status`,
    ],
    { env: runtimeEnv },
  );
  const checkRunsResponse = await deps.commandJson(
    'gh',
    [
      'api',
      '-H',
      'Accept: application/vnd.github+json',
      `repos/${repo}/commits/${sha}/check-runs`,
    ],
    { env: runtimeEnv },
  );
  const checksEvaluation = evaluateRequiredChecks({
    requiredContexts,
    commitStatusResponse,
    checkRunsResponse,
  });
  if (!checksEvaluation.passed) {
    throw new Error(
      `required checks not passed for commit "${sha}": ${checksEvaluation.failedContexts.join(', ')}`,
    );
  }

  const tag =
    input.tag ??
    buildReleaseTagName({
      service,
      sha,
      timestamp: formatReleaseTimestamp(),
    });
  await assertTagDoesNotExist(tag, repo, runtimeEnv, deps);

  const releaseTitle = input.releaseTitle ?? tag;
  const releaseNotes =
    input.releaseNotes ??
    buildReleaseNotes({
      environment,
      service,
      branch,
      sha,
      pr: mergedPr,
      requiredContexts,
    });
  const releaseManifest = buildReleaseManifest({
    environment,
    service,
    branch,
    repo,
    sha,
    tag,
    releaseTitle,
    pr: mergedPr,
    requiredContexts,
    repositorySchemaVersion: await deps.resolveRepositorySchemaVersion(),
    imageDigest: await deps.resolveServiceImageDigest({
      service,
      environment,
      tag,
      env: runtimeEnv,
      commandJsonFn: deps.commandJson,
    }),
  });
  const releaseManifestName = `${tag}.release-manifest.json`;

  const summary = {
    environment,
    service,
    branch,
    repo,
    sha,
    prNumber: mergedPr.number,
    prTitle: mergedPr.title,
    requiredContexts,
    tag,
    releaseTitle,
    releaseManifestName,
  };

  if (input.dryRun === true) {
    return {
      ...summary,
      outcome: 'dry_run',
      releaseManifest,
    };
  }

  const manifestFile = await deps.createReleaseManifestFile(releaseManifest);
  if (!manifestFile || typeof manifestFile.filePath !== 'string') {
    throw new Error('release manifest file creation failed');
  }

  try {
    await deps.runCommand('git', ['fetch', 'origin', branch], {
      env: runtimeEnv,
    });
    await deps.runCommand(
      'git',
      [
        'tag',
        '-a',
        tag,
        sha,
        '-m',
        `release ${tag} env=${environment} service=${service} pr=#${mergedPr.number}`,
      ],
      { env: runtimeEnv },
    );
    await deps.runCommand('git', ['push', 'origin', `refs/tags/${tag}`], {
      env: runtimeEnv,
    });
    await deps.runCommand(
      'gh',
      [
        'release',
        'create',
        tag,
        manifestFile.filePath,
        '--repo',
        repo,
        '--target',
        sha,
        '--title',
        releaseTitle,
        '--notes',
        releaseNotes,
        '--verify-tag',
      ],
      { env: runtimeEnv },
    );
  } finally {
    await manifestFile.cleanup();
  }

  return {
    ...summary,
    outcome: 'created',
  };
}

export async function runReleaseTagCli(argv = process.argv, overrides = {}) {
  const program = new Command();
  program
    .name('release-tag')
    .description(
      'Tag and create a GitHub release only when commit provenance and required checks pass',
    )
    .showHelpAfterError()
    .requiredOption('--service <service>', 'service: api|indexer')
    .option(
      '--environment <environment>',
      'deployment environment: testnet|mainnet',
      'testnet',
    )
    .option(
      '--branch <branch>',
      'environment branch to verify/tag (default: same as --environment)',
    )
    .option('--repo <owner/name>', 'GitHub repository override')
    .option('--sha <sha>', 'explicit commit SHA (must equal branch HEAD)')
    .option('--tag <tag>', 'override tag name')
    .option('--release-title <title>', 'override release title')
    .option('--release-notes <notes>', 'override release notes')
    .option('--dry-run', 'run validations without creating tag/release', false)
    .option('--json', 'print JSON output', false);

  await program.parseAsync(argv);
  const options = program.opts();
  const result = await runReleaseTagFlow(options, overrides);

  const writeLine =
    overrides.writeLine ??
    ((line) => {
      process.stdout.write(`${line}\n`);
    });
  if (options.json) {
    writeLine(JSON.stringify(result, null, 2));
    return;
  }

  writeLine(
    [
      `outcome=${result.outcome}`,
      `environment=${result.environment}`,
      `service=${result.service}`,
      `branch=${result.branch}`,
      `repo=${result.repo}`,
      `sha=${result.sha}`,
      `pr_number=${result.prNumber}`,
      `tag=${result.tag}`,
      `release_title=${result.releaseTitle}`,
      `release_manifest=${result.releaseManifestName}`,
      `required_checks=${result.requiredContexts.join(',')}`,
    ].join('\n'),
  );
}

async function main() {
  try {
    await runReleaseTagCli(process.argv);
  } catch (error) {
    const message =
      error instanceof Error
        ? error.message
        : `Unknown error: ${String(error)}`;
    if (isCliNotFound(error)) {
      process.stderr.write(
        `${message}. Ensure required CLIs are installed: gh, git.\n`,
      );
    } else {
      process.stderr.write(`${message}\n`);
    }
    process.exitCode = 1;
  }
}

const isEntrypoint =
  process.argv[1] !== undefined &&
  import.meta.url === pathToFileURL(process.argv[1]).href;
if (isEntrypoint) {
  await main();
}
