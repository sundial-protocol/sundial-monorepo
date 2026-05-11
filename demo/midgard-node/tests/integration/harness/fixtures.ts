import * as Ledger from "@/database/utils/ledger.js";

export const txCborA = Buffer.alloc(64, 0xbb);
export const txIdA = Buffer.alloc(32, 0xaa);

// Spent outref produced by the lucid stub (mockInputList.get returns inputCborBytes).
export const inputCborBytes = Buffer.from([0x82, 0x01, 0x02]);
// Produced outref from CML.TransactionInput.new in the lucid stub.
export const outrefCborBytes = Buffer.from([0x82, 0xab, 0xcd]);
export const outputCborBytes = Buffer.alloc(16, 0xcc);

// Address returned by the lucid stub for all produced outputs.
export const testAddress =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";
// Different address for spent-input seeds so (event_id, address) is unique.
export const spentAddress =
  "addr_test1vz0p8k0ekk5xvms5jlqmajgddmqm4xp58yd8c92lvd63hwcv6znrl";

export const makeSeedLedgerEntry = (): Ledger.Entry => ({
  [Ledger.Columns.TX_ID]: txIdA,
  [Ledger.Columns.OUTREF]: inputCborBytes,
  [Ledger.Columns.OUTPUT]: outputCborBytes,
  [Ledger.Columns.ADDRESS]: spentAddress,
});
