import {
    LucidEvolution,
    Address,
  } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { stateQueueInit } from "@/transactions/state-queue/init.js";
import { User } from "@/config.js";

export const init = (
    lucid: LucidEvolution,
    address: Address
  ) => Effect.gen(function* () {
    const { user } = yield* User;
    stateQueueInit(user)
  });
