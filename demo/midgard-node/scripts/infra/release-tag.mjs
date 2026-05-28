#!/usr/bin/env node
import { execFileSync } from "node:child_process";

const args = new Map(
  process.argv.slice(2).map((arg) => {
    const [key, value = ""] = arg.split("=");
    return [key, value];
  }),
);

const environment = args.get("--environment") || "testnet";
const branch = args.get("--branch") || environment;
const service = args.get("--service") || "sundial-node";

if (environment !== "testnet") {
  throw new Error("environment must be testnet.");
}

if (service !== "sundial-node") {
  throw new Error("Only service=sundial-node is supported.");
}

const run = (command, commandArgs) =>
  execFileSync(command, commandArgs, {
    encoding: "utf8",
    stdio: ["ignore", "pipe", "inherit"],
  }).trim();

const currentBranch = run("git", ["rev-parse", "--abbrev-ref", "HEAD"]);
if (currentBranch !== branch) {
  throw new Error(
    `release tags must be created from branch=${branch}; current=${currentBranch}`,
  );
}

run("git", ["fetch", "--quiet", "origin", branch]);
run("git", ["fetch", "--quiet", "--tags", "origin"]);

const head = run("git", ["rev-parse", "HEAD"]);
const remoteHead = run("git", ["rev-parse", `origin/${branch}`]);
if (head !== remoteHead) {
  throw new Error(
    `local HEAD ${head} must match origin/${branch} ${remoteHead}`,
  );
}

const shortSha = run("git", ["rev-parse", "--short=12", "HEAD"]);
const timestamp = new Date()
  .toISOString()
  .replace(/[-:]/g, "")
  .replace(/\.\d{3}Z$/, "Z");
const tag = `${service}-${timestamp}-${shortSha}`;

run("git", [
  "tag",
  "--annotate",
  tag,
  "--message",
  `${service} ${environment} release ${shortSha}`,
]);
run("git", ["push", "origin", tag]);

console.log(tag);
