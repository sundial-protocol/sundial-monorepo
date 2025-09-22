#!/usr/bin/env node

import { Command } from "commander";
import { ENV_VARS_GUIDE, chalk } from "@/utils.js";
import { runNode } from "@/commands/index.js";
import * as Services from "@/services/index.js";
import packageJson from "../package.json" with { type: "json" };
import { ConfigError, Effect, pipe } from "effect";
import dotenv from "dotenv";
import { NodeRuntime } from "@effect/platform-node";
import { DBCreateError, DBOtherError } from "@/database/utils/common.js";
import { SqlError } from "@effect/sql";

dotenv.config();
const VERSION = packageJson.version;

const program = new Command();

program.version(VERSION).description(
  `
  ${chalk.red(
    `                       @#
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
  const program: Effect.Effect<
    void,
    | DBCreateError
    | DBOtherError
    | SqlError.SqlError
    | ConfigError.ConfigError
    | Services.ConfigError,
    never
  > = pipe(
    runNode,
    Effect.provide(Services.Lucid.Default),
    Effect.provide(Services.AlwaysSucceedsContract.Default),
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.Globals.Default),
  );

  NodeRuntime.runMain(program, { teardown: undefined });
});

program.parse(process.argv);
