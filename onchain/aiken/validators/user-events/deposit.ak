use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/singular_utxo_indexer
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Transaction,
}
use env
use midgard/common/parameter_validation
use midgard/common/utils.{plutarch_pexcludes, plutarch_phas}
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement_queue
use midgard/user_events
use midgard/user_events/deposit.{
  AuthenticateDeposit, BurnDepositNFT, Datum, DepositInSettlementGap,
  MintRedeemer, Refund, SkippedDeposit, SpendRedeemer, TransferToReserve,
}
use midgard/user_events/witness.{
  validate_witness_redeemer, witness_script_prefix,
}

validator spend(hub_oracle: ScriptHash) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
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

    let hub.Datum {
      deposit: deposit_policy_id,
      settlement_queue: settlement_queue_policy_id,
      reserve_addr,
      ..
    } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

    let parsed_settlement_node_datum =
      settlement_queue.get_datum(
        reference_inputs,
        settlement_queue_policy_id,
        settlement_ref_input_index,
      )

    expect settlement_queue.NodeData {
      deposits_root,
      start_time: settlement_start_time,
      end_time: settlement_end_time,
      ..
    } = parsed_settlement_node_datum.data

    expect Some(deposit_datum) = datum
    let user_events.Datum {
      event: ledger_state.DepositEvent {
        id: event_nonce_out_ref,
        info: event_info,
      },
      inclusion_time,
      refund_address,
      refund_datum,
      ..
    } = deposit_datum

    expect BurnDepositNFT { l2_id: burn_asset_name, .. }: MintRedeemer =
      utils.get_redeemer_at(
        redeemers,
        Mint(deposit_policy_id),
        mint_redeemer_index,
      )

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
        //    deposit NFT.
        //    TODO: Reserve contract might end up requiring an NFT.
        //    TODO: Fees must be deduced whenever we have a clear plan for them
        //          (for `TransferToReserve` endpoint).
        expect
          output_value == (
            own_value |> assets.add(deposit_policy_id, burn_asset_name, -1)
          )

        when spend_purpose is {
          TransferToReserve { membership_proof } -> {
            // Downcast values already extracted from the datum.
            let event_id_data: Data = event_nonce_out_ref
            let event_info_data: Data = event_info

            and {
              // 3. The referenced settlement node's deposit tree must contain
              //    the subject deposit event. No need to check for the
              //    inclusion time of the deposit.
              plutarch_phas(
                deposits_root,
                event_id_data,
                event_info_data,
                membership_proof,
                redeemers,
              ),
              // 4. Assert no datum is attached to the produced UTxO.
              //    TODO: This might change depending on implementation of
              //          reserve.
              output_datum == NoDatum,
              // 5. Deposit must go to the reserve.
              output_address == reserve_addr,
            }
          }
          Refund { refund_approach } -> {
            expect and {
                // 3. The specified address must be the recepient.
                output_address == refund_address,
                // 4. The whole value must get refunded (excluding the NFT).
                output_value == (
                  own_value
                    |> assets.add(deposit_policy_id, burn_asset_name, -1)
                ),
                // 5. The attached datum must be as specified.
                output_datum == refund_datum,
              }

            when refund_approach is {
              SkippedDeposit { non_membership_proof } -> {
                // Downcast the key, which is already extracted from the datum.
                let event_id_data: Data = event_nonce_out_ref

                and {
                  // 7. The deposit event must be absent from the deposit tree.
                  plutarch_pexcludes(
                    deposits_root,
                    event_id_data,
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
              }
              DepositInSettlementGap { settlement_link_ref_input_index } -> {
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

  else(_) {
    fail
  }
}

validator mint(hub_oracle: ScriptHash) {
  mint(
    redeemer: user_events.MintRedeemer,
    own_policy: PolicyId,
    tx: Transaction,
  ) {
    // 1. Perform numerous validations and grant access to:
    //    - Serialized nonce based on the output reference of a specified input
    //      UTxO
    //    - The output value of the UTxO at the deposit address
    //    - Data encoded `DepositInfo` (which we are ignoring here, implicitly
    //      relying on user signature for its correctness)
    let
      l2_id,
      output_deposit_value,
      _output_deposit_event_info_data,
    <-
      user_events.validate_mint(
        hub_oracle,
        fn(hub_datum) {
          let hub.Datum { deposit_addr, .. } = hub_datum
          deposit_addr
        },
        redeemer,
        own_policy,
        tx,
      )

    // 2. Going through all the tokens of the produced UTxO and counting its
    //    tokens, while also validating that it only contains one valid
    //    authentication NFT.
    let
      output_deposit_nft_is_valid,
      total_token_count,
    <-
      list.foldl2(
        output_deposit_value |> assets.to_dict |> dict.to_pairs,
        False,
        0,
        fn(policy_tokens_pair, beacon_found, token_count_so_far, return) {
          if !beacon_found && policy_tokens_pair.1st == own_policy {
            expect [output_deposit_nft_tn_qty] =
              policy_tokens_pair.2nd |> dict.to_pairs
            expect output_deposit_nft_tn_qty == Pair(l2_id, 1)
            return(True, token_count_so_far)
          } else {
            let policy_token_count = dict.size(policy_tokens_pair.2nd)
            return(beacon_found, token_count_so_far + policy_token_count)
          }
        },
      )

    and {
      // 3. Deposit NFT must be included in the produced UTxO.
      output_deposit_nft_is_valid,
      // 4. Total token count in the deposit UTxO (including ADA) must not
      //    exceed the relevant protocol parameter.
      total_token_count <= env.max_tokens_allowed_in_deposits,
    }
  }

  else(_) {
    fail
  }
}
