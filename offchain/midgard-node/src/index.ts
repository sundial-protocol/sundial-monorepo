#!/usr/bin/env node

import { Command } from "commander";
import { chalk, logInfo } from "./utils.js";
import { listen } from "./commands/listen.js";
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
  .command("listen")
  .option("-p, --port <number>", "Port to listen on", "8080")
  .option("--db-file-path <string>", "Path to SQLite DB file", "db")
  .action((options) => {
    logInfo(`Listening on port ${options.port}`);
    listen();
  });

program.parse(process.argv);
