#!/usr/bin/env node

import { Command } from "commander";
import { ENV_VARS_GUIDE, chalk } from "./utils.js";
import { runNode } from "./commands/listen.js";
import * as packageJson from "../package.json";
import { Effect, pipe } from "effect";
import { NodeConfig, User } from "./config.js";
import dotenv from "dotenv";
import { init } from "./commands/init.js";

dotenv.config();
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
           chalk.redBright("L  A  B  S"),
       ) +
       "    ",
   )}
  `,
  )}
          ${"Midgard Node – Demo CLI Application"}
  ${ENV_VARS_GUIDE}`,
);

program.command("listen").action(async () => {
  const program = pipe(
    runNode,
    Effect.provide(User.layer),
    Effect.provide(NodeConfig.layer),
  );

  await Effect.runPromiseExit(program);
});

program.command("init").action(async () => {
  const program = pipe(init, Effect.provide(User.layer));

  await Effect.runPromiseExit(program);
});

program.parse(process.argv);
