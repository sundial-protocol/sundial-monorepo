#!/usr/bin/env node
import { spawn } from 'node:child_process';
import { closeSync, openSync, readFileSync, unlinkSync, writeSync } from 'node:fs';
import { constants as osConstants } from 'node:os';
import { resolve } from 'node:path';
import process from 'node:process';
import { fileURLToPath } from 'node:url';
import { updateReadmeBadge } from './refresh-badges.mjs';

const REPO_ROOT = resolve(fileURLToPath(import.meta.url), '../../..');
const LOCK_PATH = resolve(REPO_ROOT, 'README.md.lock');
const LOCK_POLL_MS = 50;
const LOCK_TIMEOUT_MS = 30_000;

function isProcessAlive(pid) {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}

async function acquireLock() {
  const deadline = Date.now() + LOCK_TIMEOUT_MS;
  while (Date.now() < deadline) {
    try {
      const fd = openSync(LOCK_PATH, 'wx');
      writeSync(fd, String(process.pid));
      closeSync(fd);
      return;
    } catch (err) {
      if (err.code !== 'EEXIST') throw err;
      // Check if the owning process is still alive; reclaim stale locks.
      try {
        const ownerPid = parseInt(readFileSync(LOCK_PATH, 'utf8'), 10);
        if (!isNaN(ownerPid) && !isProcessAlive(ownerPid)) {
          unlinkSync(LOCK_PATH);
          continue;
        }
      } catch {
        // lock disappeared between checks — retry naturally
      }
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
  for (const sig of ['SIGINT', 'SIGTERM', 'SIGHUP']) {
    process.once(sig, () => {
      releaseLock();
      process.kill(process.pid, sig);
    });
  }

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
