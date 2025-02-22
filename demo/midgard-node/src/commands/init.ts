import { Effect } from "effect";
import { stateQueueInit } from "@/transactions/state-queue/init.js";
import { User } from "@/config.js";

export const init = Effect.gen(function* () {
  const { user } = yield* User;
  stateQueueInit(user);
});
