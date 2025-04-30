use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/interval.{Finite}
use aiken/merkle_patricia_forestry as mpf
use aiken_design_patterns/singular_utxo_indexer
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId, Value}
use cardano/transaction.{
  Datum as CardanoDatum, InlineDatum, Input, Mint, Output, OutputReference,
  Transaction,
}
use env
use midgard/common/parameter_validation
use midgard/common/types.{MerkleRoot, PosixTime}
use midgard/common/utils.{plutarch_pexcludes, plutarch_phas}
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement_queue
use midgard/user_events/witness.{
  validate_witness_redeemer, witness_script_prefix,
}

// ## Types

pub type Datum<event_type> {
  event: event_type,
  inclusion_time: PosixTime,
  witness: ScriptHash,
  refund_address: Address,
  refund_datum: CardanoDatum,
}

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  mint_redeemer_index: Int,
  purpose: SpendPurpose,
}

pub type SpendPurpose {
  ConcludeEvent { membership_proof: mpf.Proof }
  Refund { refund_approach: RefundApproach }
}

pub type RefundApproach {
  SkippedEvent { non_membership_proof: mpf.Proof }
  EventInSettlementGap { settlement_link_ref_input_index: Int }
}

pub type MintRedeemer {
  AuthenticateEvent {
    nonce_input_index: Int,
    event_output_index: Int,
    hub_ref_input_index: Int,
    witness_registration_redeemer_index: Int,
  }
  BurnEventNFT {
    nonce_asset_name: AssetName,
    witness_unregistration_redeemer_index: Int,
  }
}

// ## Utilities & Helpers

pub fn out_ref_to_nonce(out_ref: OutputReference) -> AssetName {
  cbor.serialise(out_ref) |> blake2b_256
}

// ## Validation Abstractions

pub fn validate_spend(
  hub_oracle: ScriptHash,
  datum: Option<Datum<Data>>,
  redeemer: SpendRedeemer,
  own_out_ref: OutputReference,
  tx: Transaction,
  hub_datum_resolver: fn(hub.Datum, fn(PolicyId, PolicyId, Address) -> Bool) ->
    Bool,
  settlement_node_data_resolver: fn(
    settlement_queue.NodeData,
    fn(MerkleRoot, PosixTime, PosixTime) -> Bool,
  ) ->
    Bool,
  event_conclusion_output_datum_validator: fn(CardanoDatum) -> Bool,
) {
  let Transaction { reference_inputs, redeemers, .. } = tx

  let SpendRedeemer {
    input_index,
    output_index,
    hub_ref_input_index,
    settlement_ref_input_index,
    mint_redeemer_index,
    purpose,
  } = redeemer

  let hub_datum =
    hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

  let
    event_policy_id,
    settlement_queue_policy_id,
    destination_address,
  <- hub_datum_resolver(hub_datum)

  let parsed_settlement_node_datum =
    settlement_queue.get_datum(
      reference_inputs,
      settlement_queue_policy_id,
      settlement_ref_input_index,
    )

  expect settlement_node_data: settlement_queue.NodeData =
    parsed_settlement_node_datum.data

  let
    events_root,
    settlement_start_time,
    settlement_end_time,
  <- settlement_node_data_resolver(settlement_node_data)

  expect Some(event_datum) = datum

  // Mint logic is responsible for validating the `witness` field. And `event`
  // is only relevant as `Data` here as it's only used for membership and non-
  // membership proofs.
  let Datum { event, inclusion_time, refund_address, refund_datum, .. }: Datum<
    Data,
  > = event_datum

  let event_id_info_pair = ledger_state.unsafe_event_to_key_value_pair(event)

  expect BurnEventNFT { nonce_asset_name, .. }: MintRedeemer =
    utils.get_redeemer_at(redeemers, Mint(event_policy_id), mint_redeemer_index)

  singular_utxo_indexer.spend(
    fn(_in_ix, input, spend_purpose, _out_ix, output) {
      let Input { output: Output { value: own_value, .. }, .. } = input

      // 1. The produced UTxO must not have any scripts attached to it.
      expect Output {
        address: output_address,
        value: output_value,
        datum: output_datum,
        reference_script: None,
      } = output

      // 2. Output value must be the same as the input value, except for the
      //    deposit NFT. So any UTxO spent from instances of this contract must
      //    have their NFT burnt and all their tokens transferred.
      //    TODO: Fees must be deduced whenever we have a clear plan for them
      //          (for `TransferToReserve` endpoint).
      expect
        output_value == (
          own_value |> assets.add(event_policy_id, nonce_asset_name, -1)
        )

      when spend_purpose is {
        ConcludeEvent { membership_proof } -> and {
            // 3. The referenced settlement node's event tree must contain the
            //    the subject event. No need to check for the inclusion time of
            //    the event.
            plutarch_phas(
              events_root,
              event_id_info_pair.1st,
              event_id_info_pair.2nd,
              membership_proof,
              redeemers,
            ),
            // 4. Output address must match the address extracted using
            //    `hub_datum_resolver`.
            output_address == destination_address,
            // 5. Custom validation on output datum must pass.
            event_conclusion_output_datum_validator(output_datum),
          }
        Refund { refund_approach } -> {
          expect and {
              // 3. The specified address must be the recepient.
              output_address == refund_address,
              // 4. The whole value must get refunded (excluding the NFT).
              output_value == (
                own_value
                  |> assets.add(event_policy_id, nonce_asset_name, -1)
              ),
              // 5. The attached datum must be as specified.
              output_datum == refund_datum,
            }

          when refund_approach is {
            SkippedEvent { non_membership_proof } -> and {
                // 7. The deposit event must be absent from the deposit tree.
                plutarch_pexcludes(
                  events_root,
                  event_id_info_pair.1st,
                  non_membership_proof,
                  redeemers,
                ),
                // 8. Inclusion time of the deposit subject to this refund
                //    logic must fall within the time range of the specified
                //    settlement node.
                //    TODO: Should any of the comparisons be exclusive?
                inclusion_time <= settlement_end_time,
                inclusion_time >= settlement_start_time,
              }
            EventInSettlementGap { settlement_link_ref_input_index } -> {
              // 6. Grab the immediate next settlement node, and ensure
              //    correct linkage (authenticity of the reference UTxO is
              //    checked with `settlement_queue.get_datum`).
              let parsed_settlement_link_datum =
                settlement_queue.get_datum(
                  reference_inputs,
                  settlement_queue_policy_id,
                  settlement_link_ref_input_index,
                )
              expect
                parsed_settlement_node_datum.link == parsed_settlement_link_datum.key

              // 7. Inclusion time of the deposit subject to this refund logic
              //    must fall within the time gap between the two settlement
              //    nodes.
              //    TODO: Should any of the comparisons be inclusive?
              expect settlement_queue.NodeData {
                start_time: settlement_link_start_time,
                ..
              } = parsed_settlement_link_datum.data
              and {
                inclusion_time > settlement_end_time,
                inclusion_time < settlement_link_start_time,
              }
            }
          }
        }
      }
    },
    purpose,
    input_index,
    output_index,
    own_out_ref,
    tx,
  )
}

pub fn validate_mint(
  hub_oracle: ScriptHash,
  event_address_getter: fn(hub.Datum) -> Address,
  redeemer: MintRedeemer,
  own_policy: PolicyId,
  tx: Transaction,
  event_value_validator: fn(AssetName, Value) -> Bool,
) {
  when redeemer is {
    AuthenticateEvent {
      nonce_input_index,
      event_output_index,
      hub_ref_input_index,
      witness_registration_redeemer_index,
    } -> {
      let Transaction {
        inputs,
        outputs,
        reference_inputs,
        validity_range,
        mint,
        redeemers,
        ..
      } = tx

      // Grab validity range's upper bound.
      expect Finite(valid_to) = validity_range.upper_bound.bound_type

      // Grab output reference of the specified nonce input.
      expect Some(Input { output_reference: nonce_input_out_ref, .. }) =
        inputs |> list.at(nonce_input_index)

      // In order to apply the parameterized witness script, we hash the
      // serialized nonce output reference to get the 32 bytes long nonce.
      let nonce = out_ref_to_nonce(nonce_input_out_ref)

      // 1. This event NFT must be minted with a quantity of 1.
      expect assets.quantity_of(mint, own_policy, nonce) == 1

      // Script hash of the witness script, required to be registered in this
      // transaction.
      let expected_witness_script_hash =
        parameter_validation.apply_prehashed_param(
          version: 3,
          prefix: witness_script_prefix,
          param: nonce,
        )

      // 2. Validate this script hash is being registered.
      // 3. Validate the policy ID passed in its redeemer is correct.
      expect
        validate_witness_redeemer(
          expected_witness_script_hash: expected_witness_script_hash,
          witness_redeemer_index: witness_registration_redeemer_index,
          for_registration: True,
          event_policy_id: own_policy,
          redeemers: redeemers,
        )

      // Event's produced UTxO, with an inline datum and no reference script
      // attached.
      expect Some(Output {
        address: output_event_address,
        value: output_event_value,
        datum: InlineDatum(output_event_datum_data),
        reference_script: None,
      }) = outputs |> list.at(event_output_index)

      // Extracting common data shared between all event datums.
      expect Datum {
        event,
        inclusion_time: output_event_inclusion_time,
        witness: output_event_witness,
        ..
      }: Datum<Data> = output_event_datum_data

      let event_id = ledger_state.unsafe_event_to_id_data(event)

      let nonce_input_out_ref_data: Data = nonce_input_out_ref

      // 4. Event's ID must be the same as the specified spent input.
      expect event_id == nonce_input_out_ref_data

      // Grab hub oracle datum.
      let hub_datum =
        hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

      let event_address = event_address_getter(hub_datum)

      and {
        // 5. Event UTxO must be produced at the provided script's address from
        //    hub oracle.
        output_event_address == event_address,
        // 6. Specified inclusion time of the event must be some time in the
        //    future.
        output_event_inclusion_time == valid_to + env.event_wait_duration,
        // 7. Recorded witness must be correct.
        output_event_witness == expected_witness_script_hash,
        // 8. The provided validation function must pass.
        event_value_validator(nonce, output_event_value),
      }
    }
    BurnEventNFT { nonce_asset_name, witness_unregistration_redeemer_index } -> {
      let Transaction { mint, redeemers, .. } = tx

      let expected_witness_script_hash =
        parameter_validation.apply_prehashed_param(
          version: 3,
          prefix: witness_script_prefix,
          param: nonce_asset_name,
        )

      and {
        // 1. The specified NFT must be burnt.
        assets.quantity_of(mint, own_policy, nonce_asset_name) == -1,
        // 2. Witness staking script corresponding to the NFT must be getting
        //    unregistered.
        // 3. `PublishRedeemer` for unregistration must be equal to
        //    `own_policy`
        validate_witness_redeemer(
          expected_witness_script_hash: expected_witness_script_hash,
          witness_redeemer_index: witness_unregistration_redeemer_index,
          for_registration: False,
          event_policy_id: own_policy,
          redeemers: redeemers,
        ),
      }
    }
  }
}
