use aiken/collection/list
use aiken/collection/pairs
use aiken/crypto.{ScriptHash}
use aiken_design_patterns/singular_utxo_indexer
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/settlement/deposit.{
  BurnNFT, Datum, MintNFT, MintRedeemer, SpendRedeemer,
}
use midgard/state_queue

validator main(hub_oracle: ScriptHash, mpf_validator: ScriptHash) {
  spend(
    m_datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, reference_inputs, outputs, .. } = tx
    expect Some(datum) = m_datum
    let Datum { mpf_root, processed, publisher_key } = datum
    let SpendRedeemer {
      input_index,
      output_index,
      hub_ref_input_index,
      purpose,
    } = redeemer
    let hub.Datum { deposit_settlement, .. } =
      hub.get_datum(tx.reference_inputs, hub_oracle, hub_ref_input_index)
    singular_utxo_indexer.spend(
      fn(in_ix, input, spend_purpose, out_ix, output) {
        // Prevent double satisfaction.
        expect in_ix == out_ix
        todo
      },
      purpose,
      input_index,
      output_index,
      own_out_ref,
      tx,
    )
  }

  mint(redeemer: MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    when redeemer is {
      MintNFT { state_queue_input_index, hub_ref_input_index } -> {
        let Transaction { inputs, reference_inputs, outputs, redeemers, .. } =
          tx
        let hub.Datum { state_queue, state_queue_addr, deposit_settlement, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // Ensure merge endpoint of state queue is present in the tx.
        expect Some(state_queue_redeemer_data) =
          redeemers |> pairs.get_first(Mint(state_queue))
        expect state_queue.Merge { .. } = state_queue_redeemer_data

        // Fetch the state commitment UTxO that's being spent in order to be
        // merged into the canonical state.
        expect Input {
          output: Output { datum: InlineDatum(block_datum_data), .. },
          ..
        } =
          utils.get_authentic_input_with_policy_at_address(
            inputs,
            state_queue_addr,
            state_queue,
            state_queue.block_token_name,
            state_queue_input_index,
          )
        expect state_queue.Datum { publisher_key, .. } =
          state_commitment_datum_data

        // Only 1 UTxO must be produced at own script address. Staking part is
        // not validated (TODO?).
        expect [
          Output {
            value: produced_value,
            datum: InlineDatum(produced_datum_data),
            reference_script: None,
            ..
          },
        ] =
          outputs
            |> list.filter(
                fn(o) {
                  when o.address.payment_credential is {
                    Script(script_hash) -> script_hash == own_policy
                    _ -> False
                  }
                },
              )
        todo
      }
      BurnNFT -> {
        expect [Input { .. }] =
          tx.inputs
            |> list.filter(
                fn(i) {
                  when i.output.address.payment_credential is {
                    Script(script_hash) -> script_hash == own_policy
                    _ -> False
                  }
                },
              )
        todo
      }
    }
  }

  else(_) {
    fail
  }
}
