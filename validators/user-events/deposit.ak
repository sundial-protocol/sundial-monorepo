use aiken/cbor
use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{Blake2b_224, Hash, ScriptHash, blake2b_224}
use aiken_design_patterns/merkelized_validator.{delegated_validation}
use aiken_design_patterns/parameter_validation as pv
use cardano/address.{Address, Credential}
use cardano/assets.{PolicyId}
use cardano/certificate.{Certificate, RegisterCredential, UnregisterCredential}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Redeemer, Transaction,
}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/user_events
use midgard/user_events/deposit.{
  AuthenticateDeposit, BurnDepositNFT, Datum, Event, MintRedeemer, SpendRedeemer, Info,
}

validator spend(
  hub_oracle: ScriptHash,
  reserve: Address,
  plutarch_phas_validator_hash: ScriptHash,
  deposit_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, reference_inputs, .. } = tx
    expect Some(deposit_datum) = datum
    let Datum { event, inclusion_time, witness, refund_address, refund_datum } =
      deposit_datum
    let SpendRedeemer {
      input_index,
      hub_ref_input_index,
      settlement_ref_input_index,
      settlement_node_asset_name,
      membership_proof,
      purpose,
    } = redeemer
    let hub.Datum {
      settlement_queue: settlement_queue_policy_id,
      settlement_queue_addr,
      reserve_addr,
      ..
    } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)
    let Input { .. } =
      utils.get_authentic_input_with_policy_at_address(
        reference_inputs,
        settlement_queue_addr,
        settlement_queue_policy_id,
        settlement_node_asset_name,
        settlement_ref_input_index,
      )
    let Output { .. } = utils.get_own_input_at(inputs, own_out_ref, input_index)
    todo
  }

  else(_) {
    fail
  }
}

validator mint(
  hub_oracle: ScriptHash,
  unapplied_witness_script_prefix: ByteArray,
) {
  mint(redeemer: MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    let Transaction { inputs, outputs, reference_inputs, .. } = tx
    when redeemer is {
      AuthenticateDeposit {
        nonce_input_index,
        deposit_output_index,
        hub_ref_input_index,
      } -> {
        // Grab hub oracle datum.
        let hub.Datum { deposit_addr, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        expect Some(Input { output_reference: nonce, .. }) =
          inputs |> list.at(nonce_input_index)

        let expected_witness_script_hash =
          pv.apply_prehashed_param_2(
            version: 3,
            prefix: unapplied_witness_script_prefix,
            param_0: own_policy,
            param_1: cbor.serialise(nonce) |> blake2b_224,
          )

        expect Some(Output {
          address: output_deposit_address,
          value: output_deposit_value,
          datum: InlineDatum(output_deposit_datum_data),
          reference_script: None,
        }) = outputs |> list.at(deposit_output_index)

        expect Datum {
          event: user_events.Event {
            id: output_deposit_id,
            info: Info { tokens: output_deposit_tokens, .. },
          },
          inclusion_time: output_deposit_inclusion_time,
          witness: output_deposit_witness,
          ..
        }: Datum = output_deposit_datum_data

        todo
      }
      BurnDepositNFT -> todo
    }
  }

  else(_) {
    fail
  }
}

validator witness(
  deposit_minting_policy: PolicyId,
  nonce: Hash<Blake2b_224, ByteArray>,
) {
  publish(redeemer: Redeemer, certificate: Certificate, tx: Transaction) {
    pv.prehashed_param_no_datum_wrapper_2(
      validator_function: fn(
        deposit_minting_policy: PolicyId,
        nonce: Hash<Blake2b_224, ByteArray>,
        _redeemer: Redeemer,
        cert: Certificate,
        tx: Transaction,
      ) -> Bool {
        when cert is {
          RegisterCredential { .. } | UnregisterCredential { .. } -> {
            let deposit_mints_is_empty =
              tx.mint
                |> assets.tokens(deposit_minting_policy)
                |> dict.is_empty

            and {
              !deposit_mints_is_empty,
              nonce == nonce,
            }
          }
          _ -> False
        }
      },
      parameter_0: deposit_minting_policy,
      parameter_1: nonce,
      redeemer: redeemer,
      variable_arg: certificate,
      tx: tx,
    )
  }

  else(_) {
    fail
  }
}
