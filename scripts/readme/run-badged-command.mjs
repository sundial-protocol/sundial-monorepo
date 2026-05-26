#!/usr/bin/env node
import { spawn } from 'node:child_process';
import { closeSync, openSync, unlinkSync } from 'node:fs';
import { constants as osConstants } from 'node:os';
import { resolve } from 'node:path';
import process from 'node:process';
import { fileURLToPath } from 'node:url';
import { updateReadmeBadge } from './refresh-badges.mjs';

const REPO_ROOT = resolve(fileURLToPath(import.meta.url), '../../..');
const LOCK_PATH = resolve(REPO_ROOT, 'README.md.lock');
const LOCK_POLL_MS = 50;
const LOCK_TIMEOUT_MS = 30_000;

async function acquireLock() {
  const deadline = Date.now() + LOCK_TIMEOUT_MS;
  while (Date.now() < deadline) {
    try {
      closeSync(openSync(LOCK_PATH, 'wx'));
      return;
    } catch (err) {
      if (err.code !== 'EEXIST') throw err;
    }
    await new Promise((res) => setTimeout(res, LOCK_POLL_MS));
  }
  throw new Error(`Timed out waiting for README lock after ${LOCK_TIMEOUT_MS}ms`);
}

function releaseLock() {
  try {
    unlinkSync(LOCK_PATH);
  } catch {
    // already gone — nothing to do
  }
}

function parseArgs(argv) {
  const [badgeName, ...commandParts] = argv;
  const command = commandParts.join(' ').trim();

  if (!badgeName || command.length === 0) {
    throw new Error(
      'Usage: node scripts/readme/run-badged-command.mjs <badge-name> <command>',
    );
  }

  return { badgeName, command };
}

function exitCodeFromSignal(signal) {
  const signalCode = osConstants.signals[signal];
  return typeof signalCode === 'number' ? 128 + signalCode : 1;
}

function runShellCommand(command) {
  return new Promise((resolve) => {
    const shell = process.env.SHELL ?? 'bash';
    const child = spawn(shell, ['-c', command], {
      stdio: 'inherit',
    });

    child.once('error', (error) => {
      process.stderr.write(
        `${error instanceof Error ? error.message : String(error)}\n`,
      );
      resolve(1);
    });

    child.once('exit', (code, signal) => {
      if (signal) {
        resolve(exitCodeFromSignal(signal));
        return;
      }

      resolve(code ?? 1);
    });
  });
}

async function main() {
  const { badgeName, command } = parseArgs(process.argv.slice(2));
  const commandStatus = await runShellCommand(command);

  let badgeStatus = 0;
  await acquireLock();
  try {
    updateReadmeBadge(badgeName, commandStatus);
  } catch (error) {
    process.stderr.write(
      `${error instanceof Error ? error.message : String(error)}\n`,
    );
    badgeStatus = 1;
  } finally {
    releaseLock();
  }

  process.exit(commandStatus !== 0 ? commandStatus : badgeStatus);
}

await main();
