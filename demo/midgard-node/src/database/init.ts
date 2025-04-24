import * as BlocksDB from "./blocks.js";
import * as ConfirmedLedgerDB from "./confirmedLedger.js";
import * as ImmutableDB from "./immutable.js";
import * as LatestLedgerDB from "./latestLedger.js";
import * as MempoolDB from "./mempool.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { mkKeyValueCreateQuery } from "./utils.js";
import { Sql } from "postgres";
import { logAbort, logInfo } from "@/utils.js";

export const initializeDb = async (sql: Sql) => {
  try {
    // await sql`SET default_transaction_read_only TO 'off'`;
    await sql`SET default_transaction_isolation TO 'serializable'`;

    await BlocksDB.createQuery(sql);
    await mkKeyValueCreateQuery(sql, MempoolDB.tableName);
    await mkKeyValueCreateQuery(sql, MempoolLedgerDB.tableName);
    await mkKeyValueCreateQuery(sql, ImmutableDB.tableName);
    await mkKeyValueCreateQuery(sql, ConfirmedLedgerDB.tableName);
    await mkKeyValueCreateQuery(sql, LatestLedgerDB.tableName);

    logInfo("Connected to the PostgreSQL database");
    return sql;
  } catch (err) {
    logAbort(`Error initializing database: ${err}`);
    throw err;
  }
};
