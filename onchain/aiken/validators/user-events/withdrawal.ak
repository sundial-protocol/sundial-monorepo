use aiken/crypto.{ScriptHash}
use aiken_design_patterns/singular_utxo_indexer
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  Input, Mint, Output, OutputReference, Transaction, Withdraw,
}
use midgard/common/utils.{plutarch_phas}
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/reserve
use midgard/settlement_queue
use midgard/user_events
use midgard/user_events/withdrawal.{
  Conclude, Datum, Refund, SpendPurpose, SpendRedeemer,
}

validator spend(hub_oracle: ScriptHash) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { reference_inputs, redeemers, .. } = tx

    let user_events.SpendRedeemer {
      input_index,
      output_index,
      hub_ref_input_index,
      settlement_ref_input_index,
      mint_redeemer_index,
      purpose,
    } = redeemer

    // TODO: We can also use `builtin.un_constr_data` to manually match on
    //       different constructor indices.
    expect purpose: SpendPurpose = purpose

    let hub.Datum {
      withdrawal: withdrawal_policy_id,
      settlement_queue: settlement_queue_policy_id,
      reserve_observer,
      ..
    } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

    let parsed_settlement_node_datum =
      settlement_queue.get_datum(
        reference_inputs,
        settlement_queue_policy_id,
        settlement_ref_input_index,
      )

    expect settlement_queue.NodeData {
      withdrawals_root,
      start_time: settlement_start_time,
      end_time: settlement_end_time,
      ..
    } = parsed_settlement_node_datum.data

    expect Some(withdrawal_datum) = datum
    let user_events.Datum {
      event: withdrawal_event,
      inclusion_time,
      refund_address,
      refund_datum,
      ..
    } = withdrawal_datum

    expect user_events.BurnEventNFT { nonce_asset_name: burn_asset_name, .. }: user_events.MintRedeemer =
      utils.get_redeemer_at(
        redeemers,
        Mint(withdrawal_policy_id),
        mint_redeemer_index,
      )

    singular_utxo_indexer.spend(
      fn(_in_ix, input, spend_purpose, _out_ix, output) {
        let Input { output: Output { value: own_value, .. }, .. } = input

        let Output {
          address: output_address,
          value: output_value,
          datum: output_datum,
          ..
        } = output

        // 1. Output value must be the same as the input value, except for the
        //    withdrawal NFT.
        //    TODO: Fees must be deduced whenever we have a clear plan for them
        //          (for `TransferToReserve` endpoint).
        expect
          output_value == (
            own_value |> assets.add(withdrawal_policy_id, burn_asset_name, -1)
          )

        when spend_purpose is {
          Conclude { membership_proof, reserve_redeemer_index } -> {
            let Pair(withdrawal_event_id, withdrawal_event_info) =
              ledger_state.unsafe_event_to_key_value_pair(withdrawal_event)

            // 3. The referenced settlement node's withdrawal tree must contain
            //    the subject withdrawal event. No need to check for the
            //    inclusion time of the withdrawal.
            expect
              plutarch_phas(
                withdrawals_root,
                withdrawal_event_id,
                withdrawal_event_info,
                membership_proof,
                redeemers,
              )

            expect ledger_state.WithdrawalInfo {
              l2_value,
              l1_address,
              l1_datum,
              ..
            } = withdrawal_event_info

            expect and {
                // 4. Output value must contain all the value from L2.
                //    TODO: No fees taken into account yet.
                assets.match(
                  output_value,
                  l2_value,
                  fn(value_ada, tokens_ada) { value_ada == tokens_ada },
                ),
                // 5. Assert specified datum is attached.
                output_datum == l1_datum,
                // 6. Withdrawal must go to the specified address.
                output_address == l1_address,
              }

            // 7. The observer script of reserve contract must be present in the
            //    transaction (withdraw-0 trick), with a matching redeemer.
            expect reserve.ValidateWithdrawals { withdrawal_input_index, .. }: reserve.WithdrawRedeemer =
              utils.get_redeemer_at(
                redeemers,
                Withdraw(Script(reserve_observer)),
                reserve_redeemer_index,
              )

            // 8. The withdrawal input index from reserve's observer script
            //    (i.e. withdraw-0 trick) must mach the input index for this
            //    spend validation.
            withdrawal_input_index == input_index
          }
          Refund { refund_approach } ->
            user_events.validate_refund(
              own_value,
              withdrawal_policy_id,
              burn_asset_name,
              output_address,
              output_value,
              output_datum,
              refund_address,
              refund_datum,
              refund_approach,
              withdrawals_root,
              ledger_state.unsafe_event_to_id_data(withdrawal_event),
              settlement_queue_policy_id,
              parsed_settlement_node_datum.link,
              settlement_start_time,
              settlement_end_time,
              inclusion_time,
              reference_inputs,
              redeemers,
            )
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
    //    - Serialized and hashed nonce based on the output reference of a
    //      specified input UTxO
    //    - The output value of the UTxO at the withdrawal address
    let
      l1_id,
      output_withdrawal_order_value,
    <-
      user_events.validate_mint(
        hub_oracle,
        fn(hub_datum) {
          let hub.Datum { withdrawal_addr, .. } = hub_datum
          withdrawal_addr
        },
        redeemer,
        own_policy,
        tx,
      )

    let output_withdrawal_order_nft_triplet =
      utils.get_single_asset_from_value_apart_from_ada(
        output_withdrawal_order_value,
      )

    // 2. The output value of the UTxO at the withdrawal address must contain
    //    only ADA and the authentication NFT.
    output_withdrawal_order_nft_triplet == (own_policy, l1_id, 1)
  }

  else(_) {
    fail
  }
}
