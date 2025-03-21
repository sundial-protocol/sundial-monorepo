use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/linked_list/ordered
use aiken_design_patterns/linked_list/unordered.{
  Key, NodeDatum, get_node_inputs, get_node_outputs, list_state_transition,
}
use aiken_design_patterns/merkelized_validator
use aiken_design_patterns/singular_utxo_indexer
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Spend, Transaction,
}
use midgard/active_operators
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/scheduler.{Datum as SchedulerDatum, scheduler_asset_name}
use midgard/settlement_queue.{
  AppendSettlementNode, AttachResolutionClaim, Datum, Deinit,
  DisproveResolutionClaim, Init, ListStateTransition, MintRedeemer, NodeData,
  RemoveSettlementNode, ResolutionClaim, SpendRedeemer,
  settlement_resolution_duration,
}
use midgard/state_queue
use midgard/user_events

validator spend(
  hub_oracle: ScriptHash,
  settlement_policy_id: ScriptHash,
  plutarch_phas_validator_hash: ScriptHash,
) {
  spend(
    m_datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(node_datum) = m_datum

    expect NodeData { deposits_root, withdrawals_root, resolution_claim, .. } as input_node_data =
      node_datum.data

    when redeemer is {
      ListStateTransition ->
        list_state_transition(assets.tokens(tx.mint, settlement_policy_id))
      AttachResolutionClaim {
        node_input_index,
        node_output_index,
        hub_ref_input_index,
        active_operators_node_input_index,
        active_operators_redeemer_index,
        operator,
        scheduler_ref_input_index,
      } -> {
        // 1. The spent input must be a settlement node without a resolution claim.
        expect resolution_claim == None

        // 2. The spent input must be reproduced as a settlement node with a resolution claim.
        let Output { address: input_address, value: input_value, .. } =
          utils.get_own_input_at(tx.inputs, own_out_ref, node_input_index)
        //
        expect Some(Output {
          address: cont_output_address,
          value: cont_output_value,
          datum: InlineDatum(cont_output_datum),
          ..
        }) = tx.outputs |> list.at(node_output_index)
        expect NodeData {
          resolution_claim: Some(ResolutionClaim {
            resolution_time,
            operator: resolution_operator,
          }) as output_resolution_claim,
          ..
        } as output_node_data = cont_output_datum
        //
        expect input_address == cont_output_address
        expect input_value == cont_output_value
        // TODO: inefficient but implicitly safe
        expect
          output_node_data == NodeData {
            ..input_node_data,
            resolution_claim: output_resolution_claim,
          }

        // 3. The transaction must be signed by the resolution claim's operator.
        expect operator == resolution_operator
        expect operator |> utils.has_signed(tx.extra_signatories)

        // 4. The transaction must include the Midgard hub oracle NFT in a reference input.
        // 5. hub_oracle.{active_operators, scheduler}
        let hub.Datum { active_operators_addr, scheduler, .. } =
          hub.get_datum(tx.reference_inputs, hub_oracle, hub_ref_input_index)

        // 6. The transaction must include an input (`operator_node`), spent via the Update Bond
        //    Hold New Settlement redeemer, of an `active_operators` node with a key matching
        //    the resolution claim's operator.
        expect Some(
          Pair(Spend(active_operators_spend_purpose_out_ref),
          active_operators_spend_redeemer_data),
        ) = list.at(tx.redeemers, active_operators_redeemer_index)
        //
        expect active_operators.UpdateBondHoldNewSettlement {
          new_bond_unlock_time,
          ..
        }: active_operators.SpendRedeemer = active_operators_spend_redeemer_data
        //
        expect Some(Input {
          output: Output { address: active_operators_node_address, .. },
          output_reference: active_operators_out_ref,
        }) = list.at(tx.inputs, active_operators_node_input_index)
        //
        expect and {
            active_operators_out_ref == active_operators_spend_purpose_out_ref,
            active_operators_node_address == active_operators_addr,
          }

        // 7. The `bond_unlock_time` of the `operator_node` must match the `resolution_time`
        //    of the resolution claim.
        expect resolution_time == new_bond_unlock_time

        // 8. The transaction must include the `scheduler` utxo as a reference input,
        //    indicating that the current operator matches the resolution claim's operator.
        expect Some(scheduler_ref_input) =
          list.at(tx.reference_inputs, scheduler_ref_input_index)
        expect InlineDatum(scheduler_state_datum) =
          scheduler_ref_input.output.datum
        expect parsed_scheduler_state_datum: SchedulerDatum =
          scheduler_state_datum
        expect
          Pair(scheduler_asset_name, 1) == utils.get_singleton_asset_with_policy(
            scheduler_ref_input.output.value,
            scheduler,
          )
        expect operator == parsed_scheduler_state_datum.operator

        // Done. 
        True
      }
      _ -> False
    }
  }

  else(_) {
    fail
  }
}

validator mint(hub_oracle: ScriptHash) {
  mint(redeemer: MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    let Transaction {
      inputs,
      reference_inputs,
      outputs,
      mint,
      redeemers,
      extra_signatories,
      validity_range,
      ..
    } = tx

    let node_in_outputs = get_node_inputs(inputs, own_policy)
    let node_outputs = get_node_outputs(outputs, own_policy)
    let node_mint = assets.tokens(mint, own_policy)

    when redeemer is {
      Init -> and {
          assets.quantity_of(mint, hub_oracle, hub.asset_name) == 1,
          unordered.init(node_outputs, node_mint, own_policy),
        }
      Deinit -> and {
          assets.quantity_of(mint, hub_oracle, hub.asset_name) == -1,
          unordered.deinit(node_in_outputs, node_mint, own_policy),
        }
      AppendSettlementNode {
        anchor_node_output_index,
        node_output_index,
        state_queue_merge_redeemer_index,
        hub_ref_input_index,
      } -> {
        let hub.Datum { state_queue, state_queue_addr, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // 1. Ensure merge endpoint of state queue is present in the tx.
        expect Some(state_queue_redeemer_pair) =
          redeemers |> list.at(state_queue_merge_redeemer_index)
        expect state_queue_redeemer_pair.1st == Mint(state_queue)
        expect state_queue.MergeToConfirmedState {
          header_node_input_index,
          confirmed_state_node_output_index,
          ..
        }: state_queue.MintRedeemer = state_queue_redeemer_pair.2nd

        // 2. Grab the header hash of the new confirmed state to use as the
        //    asset name of the settlement authentication NFTs.
        expect Some(Output {
          datum: InlineDatum(confirmed_state_datum_data),
          ..
        }) = outputs |> list.at(confirmed_state_node_output_index)
        expect ledger_state.ConfirmedState {
          header_hash: confirmed_state_header_hash,
          ..
        } = confirmed_state_datum_data

        // 3. Validate appending to the end of settlement queue.
        expect
          unordered.append_unsafe(
            Key(confirmed_state_header_hash),
            node_output_index,
            anchor_node_output_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          )

        // 4. Fetch the state commitment UTxO that's being merged into the
        //    confirmed state.
        expect Input {
          output: Output { datum: InlineDatum(block_datum_data), .. },
          ..
        } =
          utils.get_authentic_input_with_policy_at_address(
            inputs,
            state_queue_addr,
            state_queue,
            state_queue.block_asset_name,
            header_node_input_index,
          )
        expect block_datum: state_queue.Datum = block_datum_data
        expect ledger_state.Header {
          deposits_root,
          withdrawals_root,
          start_time,
          end_time,
          ..
        } = block_datum.data

        // 5. Validate the produced output datum.
        expect Some(Output {
          datum: InlineDatum(settlement_output_datum_data),
          ..
        }) = list.at(node_outputs, node_output_index)
        expect parsed_settlement_output_datum: Datum =
          settlement_output_datum_data
        let expected_produced_node_data: Data =
          NodeData {
            deposits_root,
            withdrawals_root,
            start_time,
            end_time,
            resolution_claim: None,
          }
        parsed_settlement_output_datum.data == expected_produced_node_data
      }
      RemoveSettlementNode {
        key_to_remove,
        removed_node_input_index,
        anchor_node_input_index,
      } -> {
        expect Finite(valid_from) = validity_range.lower_bound.bound_type
        expect Some(Output { datum: InlineDatum(removed_node_datum_data), .. }) =
          node_in_outputs |> list.at(removed_node_input_index)
        expect parsed_removed_node_datum: Datum = removed_node_datum_data
        expect NodeData {
          resolution_claim: Some(ResolutionClaim {
            resolution_time: unwrapped_time_of_claimed_as_resolved,
            operator: operator_vkey,
          }),
          ..
        }: NodeData = parsed_removed_node_datum.data
        and {
          unordered.remove(
            Key(key_to_remove),
            removed_node_input_index,
            anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          operator_vkey |> utils.has_signed(extra_signatories),
          unwrapped_time_of_claimed_as_resolved + settlement_resolution_duration <= valid_from,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
