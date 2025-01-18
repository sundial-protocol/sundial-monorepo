use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/collection/pairs
use aiken/crypto.{ScriptHash}
use aiken_design_patterns/merkelized_validator
use aiken_design_patterns/singular_utxo_indexer
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement.{
  BurnNFT, ClaimAsProcessed, ConcludeSettlement, Datum, MintNFTs, MintRedeemer,
  RevokeProcessedFlag, SpendRedeemer, UpdateProcessedFlag,
}
use midgard/state_queue
use midgard/user_events

validator main(
  hub_oracle: ScriptHash,
  plutarch_phas_validator_hash: ScriptHash,
) {
  spend(
    m_datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(datum) = m_datum
    let Datum {
      mpf_root,
      time_of_claimed_as_processed,
      operator_vkey,
      ..
    } = datum

    when redeemer is {
      UpdateProcessedFlag { input_index, output_index, hub_ref_input_index, purpose } -> {
        let Transaction {
          reference_inputs,
          redeemers,
          extra_signatories,
          validity_range,
          ..
        } = tx
        let hub.Datum { deposit: deposit_script_hash, withdrawal: withdrawal_script_hash, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        singular_utxo_indexer.spend(
          fn(in_ix, input, spend_purpose, out_ix, output) {
            // 1. Prevent double satisfaction.
            expect in_ix == out_ix

            let Input {
              output: Output { address: input_address, value: input_value, .. },
              ..
            } = input

            expect Output {
              address: cont_output_address,
              value: cont_output_value,
              datum: InlineDatum(cont_output_datum_data),
              ..
            } = output

            // 2. Make sure the UTxO is reproduced at own address.
            expect input_address == cont_output_address

            // 3. Spent value must be reproduced unchanged.
            expect input_value == cont_output_value

            when spend_purpose is {
              ClaimAsProcessed -> {
                // 4. Operator must have signed this claim.
                expect operator_vkey |> utils.has_signed(extra_signatories)

                // 5. The UTxO must not have been claimed as processed prior to this
                //    transaction.
                expect time_of_claimed_as_processed == None

                // 6. The only change in the updated time must be the addition of
                //    current time as the moment of claim.
                let expected_cont_output_datum_data: Data =
                  Datum {
                    ..datum,
                    time_of_claimed_as_processed: Some(
                      utils.get_current_time(validity_range),
                    ),
                  }
                cont_output_datum_data == expected_cont_output_datum_data
              }
              RevokeProcessedFlag {
                unprocessed_event_ref_input_index,
                unprocessed_event_asset_name,
                event_type,
                membership_proof,
              } -> {
                // 2. Find the specified (via its token name) event UTxO as a ref.
                //    input at given index. Its NFT must have the same policy as its
                //    address script hash.
                expect Input {
                  output: Output { datum: InlineDatum(event_datum_data), .. },
                  ..
                } =
                  utils.get_authentic_input_of(
                    reference_inputs,
                    when event_type is {
                      settlement.Deposit -> deposit_script_hash
                      settlement.Withdrawal -> withdrawal_script_hash
                    },
                    unprocessed_event_asset_name,
                    unprocessed_event_ref_input_index,
                  )

                // Unsafe coercion is fine here as we have already validated the
                // authenticity of the UTxO.
                let Pair(event_key, event_value) =
                  user_events.unsafe_to_key_value_pair(event_datum_data)

                // 3. The specified deposit event must be present in the stored tree
                //    of the settlement datum that's been flagged as fully processed
                //    by its operator.
                expect
                  merkelized_validator.generic_delegated_validation(
                    staking_validator: plutarch_phas_validator_hash,
                    withdraw_redeemer_validator: fn(
                      membership_validator_redeemer: Data,
                    ) -> Bool {
                      expect [
                        input_root_data, input_key_data, input_value_data,
                        input_proof_data,
                        ..
                      ] = builtin.unconstr_fields(membership_validator_redeemer)
                      let mpf_root_data: Data = mpf_root
                      let proof_data: Data = membership_proof
                      and {
                        mpf_root_data == input_root_data,
                        event_key == input_key_data,
                        event_value == input_value_data,
                        proof_data == input_proof_data,
                      }
                    },
                    redeemers: redeemers,
                  )

                // 6. The only change in the datum should be removal of the claim.
                let expected_cont_output_datum_data: Data =
                  Datum { ..datum, time_of_claimed_as_processed: None }
                cont_output_datum_data == expected_cont_output_datum_data
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
      ConcludeSettlement -> todo
    }

  }

  mint(redeemer: MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    when redeemer is {
      MintNFTs {
        state_queue_input_index,
        hub_ref_input_index,
        confirmed_state_output_index,
      } -> {
        let Transaction {
          inputs,
          reference_inputs,
          outputs,
          mint,
          redeemers,
          ..
        } = tx
        let hub.Datum { state_queue, state_queue_addr, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // Ensure merge endpoint of state queue is present in the tx.
        expect Some(state_queue_redeemer_data) =
          redeemers |> pairs.get_first(Mint(state_queue))
        expect state_queue.MergeToConfirmedState: state_queue.MintRedeemer =
          state_queue_redeemer_data

        // Grab the header hash of the new confirmed state to use as the asset
        // name of the settlement authentication NFTs.
        expect Some(Output {
          datum: InlineDatum(confirmed_state_datum_data),
          ..
        }) = outputs |> list.at(confirmed_state_output_index)
        expect ledger_state.ConfirmedState {
          header_hash: confirmed_state_header_hash,
          ..
        } = confirmed_state_datum_data

        // Only 2 UTxOs must be produced at own script address. It is expected
        // that the deposits UTxO is produced at the smaller output index.
        // Staking parts are not validated (TODO?).
        expect [
          Output {
            value: produced_deposits_value,
            datum: InlineDatum(produced_deposits_datum_data),
            reference_script: None,
            ..
          },
          Output {
            value: produced_withdrawals_value,
            datum: InlineDatum(produced_withdrawals_datum_data),
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

        // Validate produced UTxOs contain authentication NFTs.
        let produced_deposits_nft_triplet =
          utils.get_single_asset_from_value_apart_from_ada(
            produced_deposits_value,
          )
        let produced_withdrawals_nft_triplet =
          utils.get_single_asset_from_value_apart_from_ada(
            produced_withdrawals_value,
          )
        expect and {
            produced_deposits_nft_triplet == (
              own_policy,
              confirmed_state_header_hash,
              1,
            ),
            produced_withdrawals_nft_triplet == produced_deposits_nft_triplet,
          }

        // Ensure the two NFTs are minted.
        expect [Pair(minted_name, minted_qty)] =
          mint |> assets.tokens(own_policy) |> dict.to_pairs
        expect and {
            minted_name == confirmed_state_header_hash,
            minted_qty == 2,
          }

        // Fetch the state commitment UTxO that's being spent in order to be
        // merged into the confirmed state.
        expect Input {
          output: Output { datum: InlineDatum(block_datum_data), .. },
          ..
        } =
          utils.get_authentic_input_with_policy_at_address(
            inputs,
            state_queue_addr,
            state_queue,
            state_queue.block_asset_name,
            state_queue_input_index,
          )
        expect block_datum: state_queue.Datum = block_datum_data
        expect ledger_state.Header {
          operator_vkey,
          deposits_root,
          withdrawals_root,
          ..
        } = block_datum.data

        // Validate the produced output datums.
        let expected_produced_deposits_datum_data: Data =
          Datum {
            mpf_root: deposits_root,
            time_of_claimed_as_processed: None,
            operator_vkey,
            tree_content: settlement.Deposit,
          }
        let expected_produced_withdrawals_datum_data: Data =
          Datum {
            mpf_root: withdrawals_root,
            time_of_claimed_as_processed: None,
            operator_vkey,
            tree_content: settlement.Withdrawal,
          }
        and {
          produced_deposits_datum_data == expected_produced_deposits_datum_data,
          produced_withdrawals_datum_data == expected_produced_withdrawals_datum_data,
        }
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
