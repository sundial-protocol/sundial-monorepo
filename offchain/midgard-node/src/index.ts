#!/usr/bin/env node

import { Command } from "commander";
import { chalk } from "./utils.js";
import * as packageJson from "../package.json" with {type: "json"};

// @ts-ignore
const VERSION = packageJson.version;

// @ts-ignore
const DESCRIPTION = packageJson.description;

const program = new Command();

program
  .version(VERSION)
  .description(DESCRIPTION)
  .option("-n, --name <name>", "Your name")
  .action((options) => {
    const name = options.name || "World";
    console.log(chalk.green(`Hello, ${name}!`));
  });

program.parse(process.argv);
