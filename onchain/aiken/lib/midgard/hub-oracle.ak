use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/utils

pub const asset_name: AssetName = "MIDGARD_HUB_ORACLE"

pub type Datum {
  registered_operators: PolicyId,
  active_operators: PolicyId,
  retired_operators: PolicyId,
  scheduler: PolicyId,
  state_queue: PolicyId,
  fraud_proof_catalogue: PolicyId,
  fraud_proof: PolicyId,
  deposit: PolicyId,
  withdrawal: PolicyId,
  tx_order: PolicyId,
  settlement: PolicyId,
  payout: PolicyId,
  registered_operators_addr: Address,
  active_operators_addr: Address,
  retired_operators_addr: Address,
  scheduler_addr: Address,
  state_queue_addr: Address,
  fraud_proof_catalogue_addr: Address,
  fraud_proof_addr: Address,
  deposit_addr: Address,
  withdrawal_addr: Address,
  tx_order_addr: Address,
  settlement_addr: Address,
  reserve_addr: Address,
  payout_addr: Address,
  reserve_observer: ScriptHash,
}

/// The assumption here is that the UTxO with the datum is authenticated with an
/// NFT which its policy is the same as the address's payment script hash.
pub fn get_datum(
  reference_inputs: List<Input>,
  hub_oracle_script_hash: ScriptHash,
  hub_ref_input_index: Int,
) -> Datum {
  expect Input { output: Output { datum: InlineDatum(hub_datum_data), .. }, .. } =
    utils.get_authentic_input_of(
      reference_inputs,
      hub_oracle_script_hash,
      asset_name,
      hub_ref_input_index,
    )
  expect hub_datum: Datum = hub_datum_data
  hub_datum
}

/// Authenticates the hub oracle UTxO (same guarantees as
/// [`get_datum`](#get_datum)) but returns the datum's raw constructor fields
/// **without** structurally validating each one. This lets callers that only
/// need a handful of the 26 fields avoid the cost of parsing all of them ---
/// 14 of which are `Address` records with nested payment/stake credentials.
///
/// Callers extract only the fields they use via the `*_of` accessors below,
/// each of which validates just that single field. This is sound under the
/// same trust model already relied upon elsewhere (e.g.
/// `unsafe_event_to_key_value_pair`): the hub UTxO is NFT-authenticated and
/// its datum shape is fixed at genesis.
///
/// > [!NOTE]
/// > The accessor indices below are positionally coupled to the field order of
/// > [`Datum`](#Datum). Keep them in sync if the datum is ever reordered.
pub fn get_datum_fields(
  reference_inputs: List<Input>,
  hub_oracle_script_hash: ScriptHash,
  hub_ref_input_index: Int,
) -> List<Data> {
  expect Input { output: Output { datum: InlineDatum(hub_datum_data), .. }, .. } =
    utils.get_authentic_input_of(
      reference_inputs,
      hub_oracle_script_hash,
      asset_name,
      hub_ref_input_index,
    )
  builtin.unconstr_fields(hub_datum_data)
}

fn policy_field_at(fields: List<Data>, index: Int) -> PolicyId {
  expect Some(field) = fields |> list.at(index)
  builtin.un_b_data(field)
}

fn address_field_at(fields: List<Data>, index: Int) -> Address {
  expect Some(field) = fields |> list.at(index)
  expect address: Address = field
  address
}

// Policy ID accessors (see `Datum` field order).
pub fn scheduler_of(fields: List<Data>) -> PolicyId {
  policy_field_at(fields, 3)
}

pub fn state_queue_of(fields: List<Data>) -> PolicyId {
  policy_field_at(fields, 4)
}

pub fn deposit_of(fields: List<Data>) -> PolicyId {
  policy_field_at(fields, 7)
}

pub fn withdrawal_of(fields: List<Data>) -> PolicyId {
  policy_field_at(fields, 8)
}

pub fn tx_order_of(fields: List<Data>) -> PolicyId {
  policy_field_at(fields, 9)
}

pub fn settlement_of(fields: List<Data>) -> PolicyId {
  policy_field_at(fields, 10)
}

pub fn payout_of(fields: List<Data>) -> PolicyId {
  policy_field_at(fields, 11)
}

// Address accessors (see `Datum` field order).
pub fn deposit_addr_of(fields: List<Data>) -> Address {
  address_field_at(fields, 19)
}

pub fn withdrawal_addr_of(fields: List<Data>) -> Address {
  address_field_at(fields, 20)
}

pub fn tx_order_addr_of(fields: List<Data>) -> Address {
  address_field_at(fields, 21)
}

pub fn reserve_addr_of(fields: List<Data>) -> Address {
  address_field_at(fields, 23)
}

pub fn payout_addr_of(fields: List<Data>) -> Address {
  address_field_at(fields, 24)
}
