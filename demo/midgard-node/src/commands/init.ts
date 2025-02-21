import {
    LucidEvolution,
    Address,
    Data,
    OutRef,
    ScriptType,
    UTxO,
    CML,
    getAddressDetails,
  } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import * as stateQueueInit from "../../../midgard-sdk/src/endpoints/state-queue/init.js";

export const init = (
    lucid: LucidEvolution,
    address: Address
  ) => {
    stateQueueInit(lucid, address)
  };
