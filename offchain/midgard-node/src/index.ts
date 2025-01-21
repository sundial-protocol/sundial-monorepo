#!/usr/bin/env node

import { Command } from "commander";
import { chalk, logAbort, logInfo } from "./utils.js";
import { run } from "./commands/run.js";
import * as packageJson from "../package.json";

const VERSION = packageJson.default.version;

const program = new Command();

program.version(VERSION).description(
  `
  ${chalk.red(
    `                        @#
                         @@%#
                        %@@@%#
                       %%%%%%##
                      %%%%%%%%%#
                     %%%%%%%%%%%#
                    %%%%%%%%%%####
                   %%%%%%%%%#######
                  %%%%%%%%  ########
                 %%%%%%%%%  #########
                %%%%%%%%%%  ##########
               %%%%%%%%%%    ##########
              %%%%%%%%%%      ##########
             %%%%%%%%%%        ##########
            %%%%%%%%%%          ##########
           %%%%%%%%%%            ##########
          ###%%%%%%%              ##########
         #########                  #########
  
   ${chalk.bgGray(
     "    " +
       chalk.bold(
         chalk.whiteBright("A  N  A  S  T  A  S  I  A") +
           "     " +
           chalk.redBright("L  A  B  S")
       ) +
       "    "
   )}
  `
  )}
          ${"Midgard Node – Demo CLI Application"}`
);

program
  .command("run")
  .option("-p, --port <number>", "Port to listen on", "8080")
  .action((options) => {
    logInfo(`Listening on port ${options.port}`);
    run();
    process.exit(1);
  });

program.parse(process.argv);
