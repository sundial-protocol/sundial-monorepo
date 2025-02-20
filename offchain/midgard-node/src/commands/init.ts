import {
    Data,
    LucidEvolution,
    OutRef,
    ScriptType,
    UTxO,
    CML,
    getAddressDetails,
  } from "@lucid-evolution/lucid";
import sqlite3 from "sqlite3";
import { Effect, Option } from "effect";
import * as queueInit from "../../../midgard-sdk/src/endpoints/state-queue/init.js";

export const init = (
    lucid: LucidEvolution,
    db: sqlite3.Database,
    port: number,
    pollingInterval: number,
    confirmedStatePollingInterval: number,
  ) => {

  };
