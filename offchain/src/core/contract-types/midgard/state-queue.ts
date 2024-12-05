import { Data } from "@lucid-evolution/lucid";
import {
  OutputReferenceSchema,
  POSIXTimeSchema,
  PubKeyHashSchema,
} from "../common";

export const ConfigSchema = Data.Object({
  initUTxO: OutputReferenceSchema,
  refundWaitingPeriod: POSIXTimeSchema,
});
export type Config = Data.Static<typeof ConfigSchema>;
export const Config = ConfigSchema as unknown as Config;

export const DatumSchema = Data.Object({
  stateTime: Data.Integer(),
  publisherKey: PubKeyHashSchema,
});
export type Datum = Data.Static<typeof DatumSchema>;
export const Datum = DatumSchema as unknown as Datum;

export const RedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Object({
    Append: Data.Object({
      prevNodeKey: PubKeyHashSchema,
      publisherKey: PubKeyHashSchema,
      publisherRefInputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    Merge: Data.Object({
      rootNodeKey: PubKeyHashSchema,
      mergeableNodeKey: PubKeyHashSchema,
    }),
  }),
  Data.Object({
    Remove_Fraud_Child: Data.Object({
      prevNodeKey: PubKeyHashSchema,
      removeNodeKey: PubKeyHashSchema,
      publisherSetPrevNodeKey: PubKeyHashSchema,
      publisherSetRemoveNodeKey: PubKeyHashSchema,
      fraudTokenRefInputIndex: Data.Integer(),
      nodeInputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    Remove_Fraud_Node: Data.Object({
      prevValidNodeKey: PubKeyHashSchema,
      removeNodeKey: PubKeyHashSchema,
      publisherSetPrevNodeKey: PubKeyHashSchema,
      publisherSetRemoveNodeKey: PubKeyHashSchema,
      fraudTokenInputIndex: Data.Integer(),
      nodeInputIndex: Data.Integer(),
    }),
  }),
]);
export type Redeemer = Data.Static<typeof RedeemerSchema>;
export const Redeemer = RedeemerSchema as unknown as Redeemer;
