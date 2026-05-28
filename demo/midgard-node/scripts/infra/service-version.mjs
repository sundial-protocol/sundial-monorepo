#!/usr/bin/env node
import { execFileSync } from "node:child_process";

const options = new Map(
  process.argv.slice(2).map((arg) => {
    const [key, value = ""] = arg.split("=");
    return [key, value];
  }),
);

const environment = options.get("--environment") || "testnet";
const service = options.get("--service") || "sundial-node";
const defaultRegion = environment === "testnet" ? "us-west-2" : null;
const region = options.get("--region") || defaultRegion;

if (environment !== "testnet") {
  throw new Error("environment must be testnet.");
}

const cluster = `sundial-node-${environment}`;

const aws = (args) =>
  execFileSync("aws", args, {
    encoding: "utf8",
    stdio: ["ignore", "pipe", "inherit"],
  }).trim();

const serviceJson = aws([
  "ecs",
  "describe-services",
  "--cluster",
  cluster,
  "--services",
  service,
  "--region",
  region,
  "--output",
  "json",
]);

const parsed = JSON.parse(serviceJson);
const taskDefinition = parsed.services?.[0]?.taskDefinition;
if (!taskDefinition) {
  throw new Error(`No task definition found for service=${service}`);
}

const taskJson = aws([
  "ecs",
  "describe-task-definition",
  "--task-definition",
  taskDefinition,
  "--region",
  region,
  "--output",
  "json",
]);

const task = JSON.parse(taskJson).taskDefinition;
const container = task.containerDefinitions.find(
  (entry) => entry.name === service,
);

console.log(
  JSON.stringify(
    {
      environment,
      region,
      cluster,
      service,
      taskDefinition,
      image: container?.image ?? null,
      taskDefinitionRevision: task.revision,
    },
    null,
    2,
  ),
);
