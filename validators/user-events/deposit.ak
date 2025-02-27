use aiken/cbor
use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{Blake2b_224, Blake2b_256, Hash, ScriptHash, blake2b_224}
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/merkelized_validator.{delegated_validation}
use cardano/address.{Address, Credential}
use cardano/assets.{PolicyId}
use cardano/certificate.{Certificate, RegisterCredential, UnregisterCredential}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Redeemer, Transaction,
}
use midgard/common/parameter_validation
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/user_events
use midgard/user_events/deposit.{
  AuthenticateDeposit, BurnDepositNFT, Datum, Event, Info, MintRedeemer,
  SpendRedeemer,
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


/// Witness script's prefix from its single CBOR encoded bytearray.
const unapplied_witness_script_prefix: ByteArray =
  #"59019b0101003332323232323232232232253330063232323232533300b3370e900318061baa001132323232323253330113370e90000020a9998089998089801998011bab30013013375401201e94128899b8f00d00d14a02a66602266e1d2002004153330113330113003330023756600260266ea802403d2825113371e01a01a29405281180b180b980b980b980b8009119299980919b8748008c04cdd50008a5eb7bdb1804dd5980b980a1baa001323300100100322533301600114c0103d87a800013233322253330173372200e0062a66602e66e3c01c00c4cdd2a4000660366e980092f5c02980103d87a8000133006006001375c602a0026eacc058004c068008c06000494ccc0480045288a50300e3754602460260046eb4c044004c034dd50008b1807980800198070011806801180680098041baa00114984d958dd70009bae0015734aae7555cf2ab9f5740ae855d126011e581c"

validator mint(
  hub_oracle: ScriptHash,
) {
  mint(redeemer: MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    when redeemer is {
      AuthenticateDeposit {
        nonce_input_index,
        deposit_output_index,
        hub_ref_input_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          validity_range,
          ..
        } = tx

        // Grab hub oracle datum.
        let hub.Datum { deposit_addr, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        expect Finite(valid_to) = validity_range.upper_bound.bound_type

        expect Some(Input { output_reference: nonce, .. }) =
          inputs |> list.at(nonce_input_index)

        let nonce_hash = cbor.serialise(nonce) |> blake2b_224
        let expected_witness_script_hash =
          parameter_validation.apply_prehashed_param_2(
            version: 3,
            prefix: unapplied_witness_script_prefix,
            param_0: own_policy,
            param_1: nonce_hash,
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

        expect and {
            output_deposit_address == deposit_addr,
            utils.authentic_value_has_tokens(
              output_deposit_value,
              own_policy,
              nonce_hash,
              output_deposit_tokens,
            ),
          }

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
  nonce: Hash<Blake2b_256, ByteArray>,
) {
  publish(redeemer: Redeemer, certificate: Certificate, tx: Transaction) {
    parameter_validation.prehashed_param_no_datum_wrapper_2(
      validator_function: fn(
        deposit_minting_policy: PolicyId,
        nonce: Hash<Blake2b_256, ByteArray>,
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
