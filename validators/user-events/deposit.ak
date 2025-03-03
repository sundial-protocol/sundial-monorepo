use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{Blake2b_256, Hash, ScriptHash}
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/linked_list/unordered
use cardano/address.{Address, Credential, Script}
use cardano/assets.{PolicyId}
use cardano/certificate.{Certificate, RegisterCredential, UnregisterCredential}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Publish, Redeemer,
  Transaction,
}
use midgard/common/parameter_validation
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/settlement_queue
use midgard/user_events
use midgard/user_events/deposit.{
  AuthenticateDeposit, BurnDepositNFT, Datum, Event, FundWithdrawal, Info,
  MintRedeemer, RefundOfDepositInTimeGap, RefundOfSkippedDeposit, SpendRedeemer,
  TransferToReserve, nonce_out_ref_to_l2_id,
}

validator spend(hub_oracle: ScriptHash) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, reference_inputs, redeemers, .. } = tx

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

    expect Some(deposit_datum) = datum
    let Datum {
      event: user_events.Event {
        id: event_nonce_out_ref,
        info: Info { tokens: deposited_tokens, .. } as event_info,
      },
      inclusion_time,
      witness,
      refund_address,
      refund_datum,
    } = deposit_datum

    let expected_l2_id = nonce_out_ref_to_l2_id(event_nonce_out_ref)

    let Output { value: own_value, .. } =
      utils.get_own_input_at(inputs, own_out_ref, input_index)

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

    expect BurnDepositNFT { l2_id: burn_asset_name }: MintRedeemer =
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
      TransferToReserve -> todo
      // TODO: Check unregistration of witness staking credential for refund
      //       endpoints.
      RefundOfSkippedDeposit { insertion_root } -> {
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
      RefundOfDepositInTimeGap { settlement_link_ref_input_index } -> {
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

  else(_) {
    fail
  }
}

/// Witness script's prefix from its single CBOR encoded applied bytearray.
/// TODO: This should probably be moved to `env`.
const witness_script_prefix: ByteArray =
  #"59016101010033232323232323223225333004323232323253330093370e900318051baa00113232323232323253330103370e900000209998081801998011bab30013012375401400a941288a99980819b87480080104ccc040c00ccc008dd5980098091baa00a0054a09445281180a980b180b180b180b0009119299980899b8748008c048dd50008a5eb7bdb1804dd5980b18099baa001323300100100322533301500114c103d87a800013233322253330163372200e0062a66602c66e3c01c00c4cdd2a4000660346e980092f5c02980103d87a8000133006006001375c60280026eacc054004c064008c05c00494ccc0440045288a50300d3754602260240066eb8010dd6980780098059baa00116300d300e003300c002300b002300b0013006375400229309b2b1bae0015734aae7555cf2ab9f5740ae855d12601225820"

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

        expect and {
            // 1. Deposit UTxO must be produced at the deposit script's address
            //    from hub oracle.
            output_deposit_address == deposit_addr,
            // 2. Specified list of tokens in the deposit event must be included
            //    in the value of the deposit UTxO (along with its NFT). No
            //    validation on ADA (TODO?).
            utils.authentic_value_has_tokens(
              output_deposit_value,
              own_policy,
              l2_id,
              output_deposit_tokens,
            ),
            // 3. ID of the deposit event must match the specified nonce input's
            //    output reference.
            output_deposit_id == nonce_input_out_ref,
            // 4. Specified inclusion time of the event must be some time in the
            //    future.
            output_deposit_inclusion_time == valid_to + user_events.wait_duration,
            // 5. Recorded witness must be correct.
            output_deposit_witness == expected_witness_script_hash,
          }

        // Grab redeemer pair of witness script registration.
        expect Some(witness_registration_redeemer_pair) =
          redeemers |> list.at(witness_registration_redeemer_index)

        // Grab the included script hash (no validation on `certificate` index,
        // TODO?).
        expect Publish {
          certificate: RegisterCredential {
            credential: Script(witness_registration_script_hash),
            ..
          },
          ..
        } = witness_registration_redeemer_pair.1st

        // Type cast its redeemer to `PolicyId`.
        expect witness_registration_redeemer: PolicyId =
          witness_registration_redeemer_pair.2nd

        and {
          // 6. Registered script must match the expected witness.
          witness_registration_script_hash == expected_witness_script_hash,
          // 7. Deposit policy ID must be the registration redeemer.
          witness_registration_redeemer == own_policy,
        }
      }
      BurnDepositNFT { l2_id } ->
        assets.quantity_of(tx.mint, own_policy, l2_id) == -1
    }
  }

  else(_) {
    fail
  }
}

validator witness(nonce: Hash<Blake2b_256, ByteArray>) {
  publish(redeemer: PolicyId, certificate: Certificate, tx: Transaction) {
    parameter_validation.prehashed_param_no_datum_wrapper(
      validator_function: fn(
        _nonce: Hash<Blake2b_256, ByteArray>,
        deposit_minting_policy: PolicyId,
        cert: Certificate,
        tx: Transaction,
      ) -> Bool {
        when cert is {
          RegisterCredential { .. } | UnregisterCredential { .. } ->
            !(tx.mint
              |> assets.tokens(deposit_minting_policy)
              |> dict.is_empty)
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
