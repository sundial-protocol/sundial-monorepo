import { Address, PolicyId, Script } from "@lucid-evolution/lucid";
import { MerkleRoot, POSIXTime } from "@/types/contracts/common.js";

export type FetchConfig = {
  stateQueueAddress: Address;
  stateQueuePolicyId: PolicyId;
};

export type CommitBlockParams = {
  newUTxOsRoot: MerkleRoot;
  transactionsRoot: MerkleRoot;
  endTime: POSIXTime;
  stateQueueSpendingScript: Script;
};

export type MergeParams = {
  stateQueueSpendingScript: Script;
  stateQueueMintingScript: Script;
};

export type InitParams = {
  policyId : PolicyId;
  address : Address;
  stateQueueMintingScript: Script
};
