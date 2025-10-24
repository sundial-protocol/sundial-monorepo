use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/interval.{Finite, Interval}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Spend, Transaction,
}
use midgard/active_operators
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/registered_operators
use midgard/retired_operators
use midgard/scheduler.{Datum as SchedulerDatum, scheduler_asset_name}
use midgard/settlement.{
  ActiveOperator, AttachResolutionClaim, Conclude, Datum,
  DisproveResolutionClaim, MintRedeemer, ResolutionClaim, RetiredOperator, Spawn,
  SpendRedeemer,
}
use midgard/state_queue

validator spend(hub_oracle: ScriptHash, settlement_policy_id: ScriptHash) {
  spend(
    m_datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(datum) = m_datum

    let Datum {
      deposits_root,
      withdrawals_root,
      transactions_root,
      resolution_claim,
      ..
    } = datum

    when redeemer is {
      AttachResolutionClaim {
        settlement_input_index,
        settlement_output_index,
        hub_ref_input_index,
        active_operators_node_input_index,
        active_operators_redeemer_index,
        operator,
        scheduler_ref_input_index,
      } -> {
        let Transaction {
          inputs,
          reference_inputs,
          output,
          extra_signatories,
          redeemers,
          ..
        } = tx

        // 1. The spent input must not have a resolution claim.
        expect resolution_claim == None

        // 2. The spent input must be reproduced as a settlement utxo with a
        //    resolution claim.
        let Output { address: input_address, value: input_value, .. } =
          utils.get_own_input_at(inputs, own_out_ref, settlement_input_index)
        //
        expect Some(Output {
          address: cont_output_address,
          value: cont_output_value,
          datum: InlineDatum(cont_output_datum_data),
          reference_script: None,
        }) = outputs |> list.at(settlement_output_index)
        //
        expect Datum {
          resolution_claim: Some(ResolutionClaim {
            resolution_time,
            operator: resolution_operator,
          }) as output_resolution_claim,
          ..
        } as cont_output_datum = cont_output_datum_data
        //
        expect input_address == cont_output_address
        expect input_value == cont_output_value
        // TODO: inefficient but implicitly safe
        expect
          cont_output_datum == Datum {
            ..datum,
            resolution_claim: output_resolution_claim,
          }

        // 3. The transaction must be signed by the resolution claim's operator.
        expect operator == resolution_operator
        expect operator |> utils.has_signed(extra_signatories)

        // 4. The transaction must include the Midgard hub oracle NFT in a
        //    reference input.
        // 5. hub_oracle.{active_operators, scheduler}
        let hub.Datum { active_operators_addr, scheduler, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // 6. The transaction must include an input (`operator_node`), spent via
        //    the Update Bond Hold New Settlement redeemer, of an
        //    `active_operators` node with a key matching the resolution claim's
        //    operator.
        let active_operators_spend_redeemer_data =
          redeemers
            |> utils.get_redeemer_at(
                Spend(active_operators_out_ref),
                active_operators_redeemer_index,
              )
        //
        expect active_operators.UpdateBondHoldNewSettlement {
          new_bond_unlock_time,
          ..
        }: active_operators.SpendRedeemer = active_operators_spend_redeemer_data
        //
        expect Some(Input {
          output: Output { address: active_operators_node_address, .. },
          output_reference: active_operators_out_ref,
        }) = list.at(inputs, active_operators_node_input_index)
        //
        expect active_operators_node_address == active_operators_addr

        // 7. The `bond_unlock_time` of the `operator_node` must match the
        //    `resolution_time` of the resolution claim, which lies in the
        //    future by `maturity_duration` milliseconds.
        expect resolution_time == new_bond_unlock_time

        // 8. The transaction must include the `scheduler` utxo as a reference
        //    input, indicating that the current operator matches the resolution
        //    claim's operator.
        expect Input {
          output: Output { datum: InlineDatum(scheduler_state_datum), .. },
          ..
        } =
          utils.get_authentic_input_with_nft_at(
            reference_inputs,
            scheduler,
            scheduler_asset_name,
            scheduler_ref_input_index,
          )
        expect parsed_scheduler_state_datum: SchedulerDatum =
          scheduler_state_datum
        expect operator == parsed_scheduler_state_datum.operator

        // Done. 
        True
      }
      DisproveResolutionClaim {
        settlement_input_index,
        settlement_output_index,
        hub_ref_input_index,
        operators_redeemer_index,
        operator,
        operator_status,
        unresolved_event_ref_input_index,
        unresolved_event_asset_name,
        event_type,
        membership_proof,
      } -> {
        let Transaction {
          inputs,
          reference_inputs,
          output,
          validity_range,
          redeemers,
          ..
        } = tx

        // 1. The spent input must have a resolution claim.
        expect Some(ResolutionClaim {
          resolution_time,
          operator: resolution_operator,
        }) = resolution_claim
        expect operator == resolution_operator

        // 2. The spent input must be reproduced as a settlement utxo without a
        //    resolution claim.
        let Output { address: input_address, value: input_value, .. } =
          utils.get_own_input_at(inputs, own_out_ref, settlement_input_index)
        //
        expect Some(Output {
          address: cont_output_address,
          value: cont_output_value,
          datum: InlineDatum(cont_output_datum_data),
          reference_script: None,
        }) = outputs |> list.at(settlement_output_index)
        //
        expect input_address == cont_output_address
        expect input_value == cont_output_value
        // TODO: inefficient but implicitly safe
        expect
          cont_output_datum_data == Datum { ..datum, resolution_claim: None }

        // 3. The transaction must include the Midgard hub oracle NFT in a
        //    reference input.
        // 4. hub_oracle.{active_operators, deposit, withdrawal}
        let hub.Datum {
          active_operators,
          deposit: deposit_script_hash,
          withdrawal: withdrawal_script_hash,
          tx_order: tx_order_script_hash,
          retired_operators,
          ..
        } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // 5. The transaction must include either a deposit, a withdrawal order,
        //    or an L2 transaction order as a reference input. The validity of
        //    withdrawal or transaction order in the corresponding tree root can
        //    be different than the one coming from the reference input.
        //    Inclusion of the event in the tree is checked using the validity
        //    provided via the redeemer.
        expect
          settlement.valid_event_inclusion(
            event_type,
            deposit_script_hash,
            withdrawal_script_hash,
            tx_order_script_hash,
            deposits_root,
            withdrawals_root,
            transactions_root,
            membership_proof,
            unresolved_event_asset_name,
            unresolved_event_ref_input_index,
            reference_inputs,
            redeemers,
          )

        // 6. The transaction's time-validity upper bound must be earlier than
        //    the resolution claim's resolution time.
        expect Finite(valid_to) = validity_range.upper_bound.bound_type
        expect valid_to < resolution_time

        // 7. Let `operator_status` be a redeemer argument indicating whether
        //    `operator` is active or retired.
        when operator_status is {
          ActiveOperator -> {
            // 8. The transaction must remove a node from the active operators list
            //    via the Remove Operator Bad Settlement redeemer.
            let active_operators_mint_redeemer_data =
              utils.get_redeemer_at(
                redeemers,
                Mint(active_operators),
                operators_redeemer_index,
              )
            //
            expect active_operators.RemoveOperatorBadSettlement {
              slashed_active_operator_key,
              ..
            }: active_operators.MintRedeemer =
              active_operators_mint_redeemer_data
            //
            expect slashed_active_operator_key == operator
            //
            True
          }
          RetiredOperator -> {
            // 9. The transaction must remove a node from the retired operators
            //    list via the Remove Operator Bad Settlement redeemer.
            let retired_operators_mint_redeemer_data =
              redeemers
                |> utils.get_redeemer_at(
                    Mint(retired_operators),
                    operators_redeemer_index,
                  )
            //
            expect retired_operators.RemoveOperatorBadSettlement {
              slashed_retired_operator_key,
              ..
            }: retired_operators.MintRedeemer =
              retired_operators_mint_redeemer_data
            //
            expect slashed_retired_operator_key == operator
            //
            True
          }
        }
      }
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

    when redeemer is {
      Spawn {
        settlement_id,
        output_index,
        state_queue_merge_redeemer_index,
        hub_ref_input_index,
      } -> {
        // 1. The transaction must include the Midgard hub oracle NFT in a
        //    reference input.
        // 2. hub_oracle.{state_queue}
        let hub.Datum { state_queue, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // 3. The transaction must Remove a `state_queue` node via the Merge To
        //    Confirmed State redeemer. Let `merged_block` be the block being
        //    merged to the confirmed state, and let `header_hash` be its
        //    header-hash key.
        //
        // Note: we trust the redeemer arguments provided to state_queue
        // because we expect it to check them.
        let state_queue_redeemer_data =
          redeemers
            |> utils.get_redeemer_at(
                Mint(state_queue),
                state_queue_merge_redeemer_index,
              )
        //
        expect state_queue.MergeToConfirmedState {
          header_node_key,
          header_node_input_index,
          ..
        }: state_queue.MintRedeemer = state_queue_redeemer_data
        // We verify this redeemer argument so that other scripts can use it:
        expect header_node_key == settlement_id
        //
        expect Some(merged_block) = list.at(inputs, header_node_input_index)
        expect Input {
          output: Output { datum: InlineDatum(block_datum_data), .. },
          ..
        } = merged_block
        //
        expect block_datum: state_queue.Datum = block_datum_data
        expect ledger_state.Header {
          deposits_root,
          withdrawals_root,
          transactions_root,
          start_time,
          end_time,
          ..
        } = block_datum.data

        // 4. The transaction must produce a settlement utxo with an nft which
        //    its asset name matches `header_hash`. Let `new_settlement` be the
        //    new settlement utxo.
        expect Some(Output {
          datum: InlineDatum(settlement_output_datum_data),
          ..
        }) = list.at(outputs, output_index)

        // 5. `merged_block` and `new_settlement` must match on all these
        //    fields: `deposits_root`, `withdrawals_root`, `txs_root`.
        // 6. The `resolution_claim` of `new_settlement` must be empty.
        expect parsed_settlement_output_datum: Datum =
          settlement_output_datum_data
        let expected_produced_settlement_datum: Data =
          Datum {
            deposits_root,
            withdrawals_root,
            transactions_root,
            resolution_claim: None,
          }
        expect
          parsed_settlement_output_datum.data == expected_produced_settlement_datum

        // Done.
        True
      }
      Conclude {
        settlement_id,
        removed_node_input_index,
        anchor_node_input_index,
      } -> {
        // 1. The transaction must Remove a node from the `settlement`. Let
        //    `removed_settlement_node` be that node.
        expect
          unordered.remove(
            Key(settlement_id),
            removed_node_input_index,
            anchor_node_input_index,
            node_in_outputs,
            outputs,
          )
        //
        expect Some(Output { datum: InlineDatum(removed_node_datum_data), .. }) =
          node_in_outputs |> list.at(removed_node_input_index)

        // 2. The `resolution_claim` of `removed_settlement_node` must not be empty.
        expect parsed_removed_node_datum: Datum = removed_node_datum_data
        expect NodeData {
          resolution_claim: Some(ResolutionClaim {
            resolution_time: unwrapped_time_of_claimed_as_resolved,
            operator: operator_vkey,
          }),
          ..
        }: NodeData = parsed_removed_node_datum.data

        // 3. The transaction must be signed by the `operator` of `resolution_claim`.
        expect operator_vkey |> utils.has_signed(extra_signatories)

        // 4. The transaction's time-validity lower bound must match or exceed the
        //    `resolution_time` of the `resolution_claim`.
        expect Finite(valid_from) = validity_range.lower_bound.bound_type
        expect unwrapped_time_of_claimed_as_resolved <= valid_from

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
