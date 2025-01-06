use aiken/cbor
use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{Blake2b_224, Hash, ScriptHash, blake2b_224}
use aiken_design_patterns/merkelized_validator.{delegated_validation}
use aiken_design_patterns/parameter_validation as pv
use cardano/address.{Address, Credential}
use cardano/assets.{PolicyId}
use cardano/certificate.{Certificate, RegisterCredential, UnregisterCredential}
use cardano/transaction.{Input, OutputReference, Redeemer, Transaction}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/user_events/deposit.{
  AuthenticateDeposit, BurnDepositNFT, Datum, Event, MintRedeemer,
  ObserverRedeemer, SpendRedeemer,
}

validator main(
  observer_script: ScriptHash,
  hub_oracle: ScriptHash,
  midgard_reserve: Address,
  mpf_validator: ScriptHash,
  unapplied_witness_script_prefix: ByteArray,
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
      hub_ref_input_index,
      mpf_ref_input_index,
      membership_proof,
      purpose,
    } = redeemer
    let hub.Datum { settlement, reserve_addr, .. } =
      hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)
    let Input { .. } =
      utils.get_authentic_input_of_at(
        reference_inputs,
        settlement,
        #"",
        mpf_ref_input_index,
      )
    todo
  }

  mint(redeemer: MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    when redeemer is {
      AuthenticateDeposit { nonce } -> {
        let expected_witness_script_hash =
          pv.apply_prehashed_param_2(
            version: 3,
            prefix: unapplied_witness_script_prefix,
            param_0: own_policy,
            param_1: cbor.serialise(nonce) |> blake2b_224,
          )
        todo
      }
      BurnDepositNFT -> todo
    }
  }

  else(_) {
    fail
  }
}

validator observer(
  state_queue: ScriptHash,
  midgard_reserve: Address,
  unapplied_witness_script_prefix: ByteArray,
) {
  withdraw(redeemer: ObserverRedeemer, _own_cred: Credential, tx: Transaction) {
    todo
  }

  publish(_redeemer: Redeemer, certificate: Certificate, _tx: Transaction) {
    when certificate is {
      // No constraints on registration
      RegisterCredential { .. } -> True
      // No other certificates allowed
      _ -> False
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
        _nonce: Hash<Blake2b_224, ByteArray>,
        _redeemer: Redeemer,
        cert: Certificate,
        tx: Transaction,
      ) -> Bool {
        when cert is {
          RegisterCredential { .. } | UnregisterCredential { .. } ->
            when
              tx.mint |> assets.tokens(deposit_minting_policy) |> dict.to_pairs
            is {
              [] -> False
              _ -> True
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
