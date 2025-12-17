use aiken/collection/list
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray.{drop}
use aiken_design_patterns/linked_list/unordered as unordered
use aiken_design_patterns/singular_utxo_indexer
use aiken_design_patterns/tx_level_minter
use cardano/address.{Script, VerificationKey}
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Redeemer, ScriptPurpose,
  Transaction,
}
use midgard/common/utils.{
  get_authentic_input_datum_with_nft_at,
  get_authentic_output_datum_at_credential_with_nft_at, get_redeemer_at,
  get_single_asset_from_value, get_single_asset_from_value_apart_from_ada,
  get_singleton_asset_with_policy, has_signed, plutarch_phas,
}
use midgard/computation_thread as ct
use midgard/ledger_state.{Header}
use midgard/state_queue

pub type Proof =
  ByteArray

pub type MembershipProofInputs {
  tx_root: ByteArray,
  tx_hash: ByteArray,
  tx_info_hash: ByteArray,
  tx_to_root_proof_path: Proof,
}

pub type ExclusionProofInputTx {
  tx_root: ByteArray,
  invalid_input_tx_hash: ByteArray,
  invalid_input_info_hash: ByteArray,
  input_to_root_proof_path: Proof,
}

pub type ExclusionProofUtxo {
  utxo_root: ByteArray,
  invalid_utxo: ByteArray,
  invalid_utxo_info_hash: ByteArray,
  utxo_to_root_proof_path: Proof,
}

pub type InclusionProofTx {
  current_block_txs_root: ByteArray,
  invalid_tx: ByteArray,
  invalid_tx_info_hash: ByteArray,
  tx_to_root_proof_path: Proof,
}

// Last step datum for fraud proofs
pub type FraudProofDatum {
  fraud_prover: ByteArray,
}

// ------------ ABSTRACTIONS FOR ALL FRAUD PROOF STEPS ------------
// Each step in a computation thread must in the continue redeemer:
// Parse computation state from datum (previous step's output datum)
// Receive arguments from current steps redeemer
// Advance the computation by step, producing a new state
// Suspend computation by serializing new state into datum for next step and pass it on

// -------------- REDEEMER: CONTINUE -----------------
// (1) Reference a state queue block with a block hash matching the last 28 bytes of the computation thread token name
// (2) Delegate compute to the merkleized validator to verify the inclusion of tx1 in the tx_root
// (3) The fraud prover field must match between the output state and the input datum
// (4) Verify the state transition from the previous state to the new state
// (5) The computation thread token must be sent to the next step's spending validator
// (6) Transaction must not mint or burn any other tokens

// Relevant to (1): tx_root provided by fraud prover in redeemer matches block_hash from the state queue block from the reference input
pub fn get_tx_root_validate_block_hash(
  tx: Transaction,
  state_queue_node_ref_input_index: Int,
  ct_token_asset_name: ByteArray,
  state_queue_policy: ByteArray,
) -> ByteArray {
  // Reference input with block_hash 
  let
    parsed_state_queue_datum,
    block_hash,
  <-
    state_queue.get_block_datum(
      tx.reference_inputs,
      state_queue_policy,
      state_queue_node_ref_input_index,
    )

  // Get CT token and check match with the block hash
  expect drop(ct_token_asset_name, n: 4) == block_hash

  parsed_state_queue_datum.transactions_root
}

// Relevant to (1): tx_root provided by fraud prover in redeemer matches block_hash from the state queue block from the reference input
pub fn get_prev_utxo_root(
  tx: Transaction,
  state_queue_node_ref_input_index: Int,
) -> ByteArray {
  // Reference input with block_hash 
  expect Some(state_queue_node_input) =
    list.at(tx.reference_inputs, state_queue_node_ref_input_index)

  // Extract tx_root via the state queue block hash
  expect InlineDatum(state_queue_datum_data) =
    state_queue_node_input.output.datum
  expect state_queue_node_datum: unordered.NodeDatum = state_queue_datum_data
  expect parsed_state_queue_datum: Header = state_queue_node_datum.data
  let extracted_prev_utxo_root = parsed_state_queue_datum.prev_utxos_root

  extracted_prev_utxo_root
}

// tx_root, merkle root of transactions in the state queue block
// tx_hash, hash of the transaction that is being proven to be in tx_root
// proof_path: minimum set of hashes needed to reconstruct the path from the tx to the root

// Relevant to (2)
pub fn validate_membership_proof(
  tx: Transaction,
  plutarch_midgard_merkle_validator: ScriptHash,
  proof_inputs: MembershipProofInputs,
) -> Bool {
  // expect
  //   merkelized_validator.delegated_validation(
  //     function_input: MembershipProofInputs {
  //       tx_root: proof_inputs.tx_root,
  //       tx_hash: proof_inputs.tx_hash,
  //       tx_info_hash: proof_inputs.tx_info_hash,
  //       tx_to_root_proof_path: proof_inputs.tx_to_root_proof_path,
  //     },
  //     staking_validator: plutarch_midgard_merkle_validator,
  //     redeemers: tx.redeemers,
  //     input_data_coercer: fn(withdraw_redeemer: Data) {
  //       expect coerced: MembershipProofInputs = withdraw_redeemer
  //       coerced
  //     },
  //   )
  True
}

// TODO
pub fn validate_membership_proof_2(
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  withdraw_redeemer_index: Int,
  tx_root: ByteArray,
  tx_hash: ByteArray,
  tx_info_hash: ByteArray,
  tx_to_root_proof: mpf.Proof,
) -> Bool {
  plutarch_phas(
    mpf_root: tx_root,
    key: tx_hash,
    value: tx_info_hash,
    membership_proof: tx_to_root_proof,
    redeemers: redeemers,
    withdraw_redeemer_index: withdraw_redeemer_index,
  )
}

// Relevant to (2)
pub fn validate_exclusion_proof(
  tx: Transaction,
  plutarch_midgard_merkle_validator: ScriptHash,
  proof_input: ExclusionProofInputTx,
) -> Bool {
  // expect
  //   merkelized_validator.delegated_validation(
  //     function_input: ExclusionProofInputTx {
  //       tx_root: proof_input.tx_root,
  //       invalid_input_tx_hash: proof_input.invalid_input_tx_hash,
  //       invalid_input_info_hash: proof_input.invalid_input_info_hash,
  //       input_to_root_proof_path: proof_input.input_to_root_proof_path,
  //     },
  //     staking_validator: plutarch_midgard_merkle_validator,
  //     input_data_coercer: fn(withdraw_redeemer: Data) {
  //       expect coerced: ExclusionProofInputTx = withdraw_redeemer
  //       coerced
  //     },
  //     redeemers: tx.redeemers,
  //   )
  True
}

// Relevant to (2)
pub fn validate_exclusion_utxo_proof(
  tx: Transaction,
  plutarch_midgard_merkle_validator: ScriptHash,
  proof_input: ExclusionProofUtxo,
) -> Bool {
  // expect
  //   merkelized_validator.delegated_validation(
  //     function_input: ExclusionProofUtxo {
  //       utxo_root: proof_input.utxo_root,
  //       invalid_utxo: proof_input.invalid_utxo,
  //       invalid_utxo_info_hash: proof_input.invalid_utxo_info_hash,
  //       utxo_to_root_proof_path: proof_input.utxo_to_root_proof_path,
  //     },
  //     staking_validator: plutarch_midgard_merkle_validator,
  //     input_data_coercer: fn(withdraw_redeemer: Data) {
  //       expect coerced: ExclusionProofUtxo = withdraw_redeemer
  //       coerced
  //     },
  //     redeemers: tx.redeemers,
  //   )
  True
}

// Common validation for any step's output state (except final step)
// Relevant to (3), (4), (5), (6)
pub fn validate_step_output(
  outputs: List<Output>,
  output_index: Int,
  next_validator_hash: ScriptHash,
  ct_token_policy_id: PolicyId,
  ct_token_asset_name: ByteArray,
  step_datum: ct.StepDatum,
  expected_output_datum: ct.StepDatum,
) -> Bool {
  // (5) Validate output goes to next validator
  // (6) Validate CT token forwarding and no other minting/burning
  let output_datum =
    get_authentic_output_datum_at_credential_with_nft_at(
      outputs,
      Script(next_validator_hash),
      ct_token_policy_id,
      ct_token_asset_name,
      output_index,
    )

  // (3) Validate fraud prover field matches
  expect parsed_output_datum: ct.StepDatum = output_datum
  expect parsed_output_datum.fraud_prover == step_datum.fraud_prover

  // (4) Validate state transition
  expect parsed_output_datum.data == expected_output_datum.data

  True
}

// -------------- REDEEMER: CONTINUE (FINAL STEP) -----------------
// Validations for all final step output states

// (1) Mint the fraud token
// (2) Burn computation thread token with Success redeemer
// (3) Output datum type must be StepDatum(Void)
// (4) The fraud_prover field must match between output datum and input datum
// (5) Transaction must not mint or burn any other tokens
pub fn validate_final_step_output(
  tx: Transaction,
  index_output: Int,
  fraud_proof_token_policy: PolicyId,
  ct_token_asset_name: ByteArray,
  step_datum: ct.StepDatum,
  computation_thread_redeemer_index: Int,
) -> Bool {
  // (3, 4) Validate output state is Void and fraud prover matches
  expect Some(output) = list.at(tx.outputs, index_output)
  expect InlineDatum(output_datum) = output.datum
  expect parsed_output_datum: FraudProofDatum = output_datum
  let expected_output_datum =
    FraudProofDatum { fraud_prover: step_datum.fraud_prover }
  expect expected_output_datum == parsed_output_datum

  // (1, 5) Validate fraud proof token minting and no other minting
  expect
    Pair(ct_token_asset_name, 1) == get_singleton_asset_with_policy(
      tx.mint,
      fraud_proof_token_policy,
    )

  // (2) Validate computation thread token burn with Success redeemer
  expect Some(computation_thread_redeemer_pair) =
    list.at(tx.redeemers, computation_thread_redeemer_index)
  expect ct.Success { token_to_burn_asset_name } =
    computation_thread_redeemer_pair.2nd
  expect token_to_burn_asset_name == ct_token_asset_name

  True
}

pub fn cancel(
  computation_thread_token_policy_id: PolicyId,
  input_index: Int,
  mint_redeemer_index: Int,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  extra_signatories: List<VerificationKeyHash>,
) -> Bool {
  // 1. Validate the `Cancel` redeemer from the computation thread policy is
  //    invoked (this in turn validates the burning of the specified computation
  //    thread token).
  let mint_redeemer_data =
    get_redeemer_at(
      redeemers: redeemers,
      expected_purpose: Mint(computation_thread_token_policy_id),
      redeemer_index: mint_redeemer_index,
    )
  expect ct.Cancel { token_to_burn_asset_name: redeemer_burning_asset_name } =
    mint_redeemer_data

  // 2. Transaction must be signed by the fraud prover.
  let step_datum_data =
    get_authentic_input_datum_with_nft_at(
      inputs: inputs,
      nft_policy_id: computation_thread_token_policy_id,
      nft_asset_name: redeemer_burning_asset_name,
      input_index: input_index,
    )
  expect ct.StepDatum { fraud_prover, .. } = step_datum_data
  expect fraud_prover |> has_signed(extra_signatories)

  // Done.
  True
}

// ## Helper Functions

/// Get computation thread token from input
/// This is used by all redeemers to identify the computation thread token passed
/// in from the previous step
pub fn get_singleton_ct_token(
  own_input: Input,
  ct_token_policy_id: PolicyId,
) -> ByteArray {
  expect Pair(ct_token_asset_name, 1) =
    get_singleton_asset_with_policy(own_input.output.value, ct_token_policy_id)
  ct_token_asset_name
}

// Validate output goes to fraud prover address
pub fn validate_output_to_fraud_prover(
  output_to_fraud_prover: Output,
  prover_hash: ByteArray,
) -> Bool {
  when output_to_fraud_prover.address.payment_credential is {
    VerificationKey(hash) -> hash == prover_hash
    _ -> False
  }
}
