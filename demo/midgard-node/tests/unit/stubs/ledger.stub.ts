export const Columns = {
  TX_ID: "tx_id",
  OUTREF: "outref",
  OUTPUT: "output",
  ADDRESS: "address",
} as const;

export type Entry = Record<string, unknown>;
export type MinimalEntry = Record<string, unknown>;
