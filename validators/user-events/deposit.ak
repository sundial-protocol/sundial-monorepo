use aiken/collection/list
use aiken/crypto.{Blake2b_256, Hash, ScriptHash}
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/linked_list/unordered
use cardano/assets.{PolicyId}
use cardano/certificate.{Certificate, RegisterCredential, UnregisterCredential}
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Transaction,
}
use midgard/common/parameter_validation
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/settlement_queue
use midgard/user_events
use midgard/user_events/deposit.{
  AuthenticateDeposit, BurnDepositNFT, Datum, DepositInTimeGap, Event,
  FundWithdrawal, Info, MintRedeemer, Refund, SkippedDeposit, SpendRedeemer,
  TransferToReserve, WitnessRedeemer, nonce_out_ref_to_l2_id,
  validate_witness_redeemer, witness_script_prefix,
}

validator spend(hub_oracle: ScriptHash) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, outputs, reference_inputs, mint, redeemers, .. } =
      tx

    let SpendRedeemer {
      input_index,
      hub_ref_input_index,
      settlement_ref_input_index,
      settlement_node_asset_name,
      mint_redeemer_index,
      merkle_proof,
      purpose,
    } = redeemer

    let hub.Datum {
      deposit: deposit_policy_id,
      settlement_queue: settlement_queue_policy_id,
      settlement_queue_addr,
      reserve_addr,
      ..
    } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

    let parsed_settlement_node_datum =
      settlement_queue.get_datum(
        reference_inputs,
        settlement_queue_addr,
        settlement_queue_policy_id,
        settlement_node_asset_name,
        settlement_ref_input_index,
      )

    expect settlement_queue.NodeData {
      deposits_root,
      start_time: settlement_start_time,
      end_time: settlement_end_time,
      ..
    } = parsed_settlement_node_datum.data

    expect Some(deposit_datum) = datum
    let Datum {
      event: user_events.Event {
        id: event_nonce_out_ref,
        info: Info { tokens: deposited_tokens, .. } as event_info,
      },
      inclusion_time,
      refund_address,
      refund_datum,
      ..
    } = deposit_datum

    let expected_l2_id = nonce_out_ref_to_l2_id(event_nonce_out_ref)

    let Output { value: own_value, .. } =
      utils.get_own_input_at(inputs, own_out_ref, input_index)

    expect BurnDepositNFT { l2_id: burn_asset_name, .. }: MintRedeemer =
      utils.get_redeemer_at(
        redeemers,
        Mint(deposit_policy_id),
        mint_redeemer_index,
      )

    expect burn_asset_name == expected_l2_id

    expect
      utils.authentic_value_has_tokens(
        own_value,
        deposit_policy_id,
        burn_asset_name,
        deposited_tokens,
      )

    when purpose is {
      FundWithdrawal -> todo
      TransferToReserve { output_index } -> {
        // 1. Assert no datum and no reference script attached to the produced
        //    UTxO.
        expect Some(Output {
          address: output_address,
          value: output_value,
          datum: NoDatum,
          reference_script: None,
        }) = outputs |> list.at(output_index)

        expect and {
            // 2. Deposit must go to the reserve.
            output_address == reserve_addr,
            // 3. UTxO at reserve must only contain ADA.
            output_value == assets.from_lovelace(assets.lovelace_of(own_value)),
          }

        // We already expect the deposit NFT to be burnt, so we remove it from
        // `mint`, and also negate the result to step closer to what tokens are
        // included in the deposit UTxO.
        let negated_mint_without_deposit_nft =
          mint
            |> assets.add(deposit_policy_id, burn_asset_name, 1)
            |> assets.negate

        // 4. Only the deposit NFT, and the deposited tokens must be burnt in
        //    this transaction.
        assets.match(
          negated_mint_without_deposit_nft,
          deposited_tokens,
          fn(_, tokens_ada) { tokens_ada == 0 },
        )
      }
      Refund { output_index, refund_approach } -> {
        expect Some(Output {
          address: output_address,
          value: output_value,
          datum: output_datum,
          reference_script: None,
        }) = outputs |> list.at(output_index)

        expect and {
            // 1. The specified address must be the recepient.
            output_address == refund_address,
            // 2. The whole value must go refunded.
            output_value == own_value,
            // 3. The attached datum must be as specified.
            output_datum == refund_datum,
          }
        when refund_approach is {
          SkippedDeposit { insertion_root } -> {
            // Downcast values already extracted from the datum.
            let event_id_data: Data = event_nonce_out_ref
            let event_info_data: Data = event_info

            // Find the deposit root after this event has been inserted into the
            // settlement node's deposit tree.
            let root_after_insertion =
              utils.plutarch_pinsert(
                deposits_root,
                event_id_data,
                event_info_data,
                merkle_proof,
                redeemers,
              )

            and {
              // 1. It must be proven that this event can successfully be inserted
              //    into the existing tree of the settlement node (i.e. the event
              //    doesn't exist in the tree).
              root_after_insertion == insertion_root,
              // 2. Inclusion time of the deposit subject to this refund logic must
              //    fall within the time range of the specified settlement node.
              //    TODO: Should any of the comparisons be exclusive?
              inclusion_time <= settlement_end_time,
              inclusion_time >= settlement_start_time,
            }
          }
          DepositInTimeGap { settlement_link_ref_input_index } -> {
            // 1. Grab the immediate next settlement node, and ensure correct
            //    linkage (authenticity of the reference UTxO is checked with
            //    `settlement_queue.get_datum`).
            let parsed_settlement_link_datum =
              settlement_queue.get_datum(
                reference_inputs,
                settlement_queue_addr,
                settlement_queue_policy_id,
                unordered.serialize_key(parsed_settlement_node_datum.key),
                settlement_link_ref_input_index,
              )
            expect
              parsed_settlement_node_datum.link == parsed_settlement_link_datum.key

            // 2. Inclusion time of the deposit subject to this refund logic must
            //    fall within the time gap between the two settlement nodes.
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
  }

  else(_) {
    fail
  }
}

validator mint(hub_oracle: ScriptHash) {
  mint(redeemer: MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    when redeemer is {
      AuthenticateDeposit {
        nonce_input_index,
        deposit_output_index,
        hub_ref_input_index,
        witness_registration_redeemer_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          validity_range,
          redeemers,
          ..
        } = tx

        // Grab hub oracle datum.
        let hub.Datum { deposit_addr, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // Grab validity range's upper bound.
        expect Finite(valid_to) = validity_range.upper_bound.bound_type

        // Grab output reference of the specified nonce input.
        expect Some(Input { output_reference: nonce_input_out_ref, .. }) =
          inputs |> list.at(nonce_input_index)

        // The deposit will be represented on L2 as a UTxO, where its `tx_hash`
        // is Blake2b256 hash of the serialised nonce input output reference,
        // and its output index is 0.
        let l2_id = nonce_out_ref_to_l2_id(nonce_input_out_ref)

        // Script hash of the witness script, required to be registered in this
        // transaction.
        let expected_witness_script_hash =
          parameter_validation.apply_prehashed_param(
            version: 3,
            prefix: witness_script_prefix,
            param: l2_id,
          )

        // 1. Validate this script hash is being registered.
        // 2. Validate the policy ID passed in its redeemer is the same as this
        //    deposit policy ID.
        expect
          validate_witness_redeemer(
            expected_witness_script_hash: expected_witness_script_hash,
            witness_redeemer_index: witness_registration_redeemer_index,
            for_registration: True,
            event_policy_id: own_policy,
            l2_id: l2_id,
            redeemers: redeemers,
          )

        // Deposit event's produced UTxO.
        expect Some(Output {
          address: output_deposit_address,
          value: output_deposit_value,
          datum: InlineDatum(output_deposit_datum_data),
          reference_script: None,
        }) = outputs |> list.at(deposit_output_index)

        // Extracting L1 relevant data from the produced deposit datum. Ignored
        // values are user-related and approved simply by depositor's signature.
        expect Datum {
          event: user_events.Event {
            id: output_deposit_id,
            info: Info { tokens: output_deposit_tokens, .. },
          },
          inclusion_time: output_deposit_inclusion_time,
          witness: output_deposit_witness,
          ..
        }: Datum = output_deposit_datum_data

        and {
          // 3. Deposit UTxO must be produced at the deposit script's address
          //    from hub oracle.
          output_deposit_address == deposit_addr,
          // 4. Specified list of tokens in the deposit event must be included
          //    in the value of the deposit UTxO (along with its NFT). The only
          //    validation on ADA is that `output_deposit_tokens` must have 0
          //    of it (it still can contain `("", "", 0)`, TODO?).
          utils.authentic_value_has_tokens(
            output_deposit_value,
            own_policy,
            l2_id,
            output_deposit_tokens,
          ),
          // 5. ID of the deposit event must match the specified nonce input's
          //    output reference.
          output_deposit_id == nonce_input_out_ref,
          // 6. Specified inclusion time of the event must be some time in the
          //    future.
          output_deposit_inclusion_time == valid_to + user_events.wait_duration,
          // 7. Recorded witness must be correct.
          output_deposit_witness == expected_witness_script_hash,
        }
      }
      BurnDepositNFT { l2_id, witness_unregistration_redeemer_index } -> {
        let Transaction { mint, redeemers, .. } = tx

        let expected_witness_script_hash =
          parameter_validation.apply_prehashed_param(
            version: 3,
            prefix: witness_script_prefix,
            param: l2_id,
          )

        and {
          // 1. The specified NFT must be burnt.
          assets.quantity_of(mint, own_policy, l2_id) == -1,
          // 2. Witness staking script corresponding to the NFT must be getting
          //    unregistered.
          validate_witness_redeemer(
            expected_witness_script_hash: expected_witness_script_hash,
            witness_redeemer_index: witness_unregistration_redeemer_index,
            for_registration: False,
            event_policy_id: own_policy,
            l2_id: l2_id,
            redeemers: redeemers,
          ),
        }
      }
    }
  }

  else(_) {
    fail
  }
}

validator witness(nonce: Hash<Blake2b_256, ByteArray>) {
  publish(redeemer: WitnessRedeemer, certificate: Certificate, tx: Transaction) {
    // Note that we're ignoring the `nonce`. However, it still affects the
    // resulting script hash since the wrapper function has been applied to it.
    parameter_validation.prehashed_param_no_datum_wrapper(
      validator_function: fn(
        _nonce: Hash<Blake2b_256, ByteArray>,
        witness_redeemer: WitnessRedeemer,
        cert: Certificate,
        tx: Transaction,
      ) -> Bool {
        let WitnessRedeemer { deposit_minting_policy, l2_id } = witness_redeemer

        let mint_qty =
          tx.mint |> assets.quantity_of(deposit_minting_policy, l2_id)

        when cert is {
          RegisterCredential { .. } -> mint_qty == 1
          UnregisterCredential { .. } -> mint_qty == -1
          _ -> False
        }
      },
      param: nonce,
      redeemer: redeemer,
      variable_arg: certificate,
      tx: tx,
    )
  }

  else(_) {
    fail
  }
}
