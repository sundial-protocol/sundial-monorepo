use aiken/crypto.{VerificationKeyHash}
use aiken/merkle_patricia_forestry as mpf
use aiken_design_patterns/linked_list/unordered.{NodeDatum}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/types.{MerkleRoot, PosixTime}
use midgard/common/utils

pub type Datum {
  deposits_root: MerkleRoot,
  withdrawals_root: MerkleRoot,
  transactions_root: MerkleRoot,
  resolution_claim: Option<ResolutionClaim>,
}

pub type ResolutionClaim {
  resolution_time: PosixTime,
  operator: VerificationKeyHash,
}

pub type SpendRedeemer {
  AttachResolutionClaim {
    settlement_input_index: Int,
    settlement_output_index: Int,
    hub_ref_input_index: Int,
    active_operators_node_input_index: Int,
    active_operators_redeemer_index: Int,
    operator: VerificationKeyHash,
    scheduler_ref_input_index: Int,
  }
  DisproveResolutionClaim {
    settlement_input_index: Int,
    settlement_output_index: Int,
    hub_ref_input_index: Int,
    operators_redeemer_index: Int,
    operator: VerificationKeyHash,
    operator_status: OperatorStatus,
    unresolved_event_ref_input_index: Int,
    unresolved_event_asset_name: AssetName,
    event_type: EventType,
    membership_proof: mpf.Proof,
  }
}

pub type EventType {
  Deposit
  Withdrawal { validity_override: ledger_state.WithdrawalValidity }
  TxOrder { validity_override: ledger_state.MidgardTxValidity }
}

pub type OperatorStatus {
  ActiveOperator
  RetiredOperator
}

pub type MintRedeemer {
  Spawn {
    settlement_id: ByteArray,
    output_index: Int,
    state_queue_merge_redeemer_index: Int,
    hub_ref_input_index: Int,
  }
  Conclude {
    settlement_id: ByteArray,
    removed_node_input_index: Int,
    anchor_node_input_index: Int,
  }
}

pub fn get_datum(
  reference_inputs: List<Input>,
  settlement_policy_id: PolicyId,
  settlement_ref_input_index: Int,
) -> Datum {
  expect Input {
    output: Output { datum: InlineDatum(settlement_datum_data), .. },
    ..
  } =
    reference_inputs
      |> utils.get_authentic_input_with_policy_at(
          settlement_policy_id,
          settlement_ref_input_index,
        )

  expect parsed_settlement_datum: Datum = settlement_datum_data

  parsed_settlement_datum
}
