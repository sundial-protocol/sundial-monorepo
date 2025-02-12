use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/collection/pairs
use aiken/crypto.{ScriptHash}
use aiken_design_patterns/linked_list/ordered.{Key, NodeDatum}
use aiken_design_patterns/merkelized_validator
use aiken_design_patterns/singular_utxo_indexer
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Spend, Transaction,
}
use midgard/active_operators
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement.{
  BurnNFT, ClaimAsResolved, ConcludeSettlement, Datum, MintNFTs, MintRedeemer,
  RevokeResolvedFlag, SpendRedeemer, UpdateResolvedFlag,
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
    let Datum { mpf_root, time_of_claimed_as_resolved, operator, .. } = datum

    when redeemer is {
      UpdateResolvedFlag {
        input_index,
        output_index,
        hub_ref_input_index,
        active_operators_node_input_index,
        active_operators_redeemer_index,
        purpose,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          redeemers,
          extra_signatories,
          ..
        } = tx
        let hub.Datum {
          deposit: deposit_script_hash,
          withdrawal: withdrawal_script_hash,
          active_operators_addr,
          active_operators: active_operators_policy_id,
          ..
        } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // Using the singular one-to-one indexer design pattern.
        singular_utxo_indexer.spend(
          fn(in_ix, input, spend_purpose, out_ix, output) {
            // 1. Prevent double satisfaction (TODO: is this needed?).
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

            // 3. Spent value must be reproduced unchanged. TODO: This is
            // relying on some additional ADA being available (i.e. to be paid
            // to the active operator).
            expect input_value == cont_output_value

            // 4. The UTxO must not have been subject to fraudulent activity
            //    before.
            expect Some(operator_vkey) = operator

            when spend_purpose is {
              ClaimAsResolved -> {
                // 5. Operator must have signed this claim.
                expect operator_vkey |> utils.has_signed(extra_signatories)

                // 6. The UTxO must not have been claimed as resolved prior to
                //    this transaction.
                expect time_of_claimed_as_resolved == None

                // 7. Expect a spend redeemer at the specified redeemer index.
                expect Some(
                  Pair(Spend(active_operators_spend_purpose_out_ref),
                  active_operators_spend_redeemer_data),
                ) = list.at(redeemers, active_operators_redeemer_index)

                // 8. Found redeemer data must have the same structure as active
                //    operator's update commitment time redeemer.
                expect active_operators.UpdateCommitmentTime {
                  active_node_output_index,
                  ..
                }: active_operators.SpendRedeemer =
                  active_operators_spend_redeemer_data

                // Grab the active operators contract's input UTxO to validate
                // its address, and also to make sure the output reference here
                // matches the one from the `redeemers`.
                expect Some(Input {
                  output: Output { address: active_operators_node_address, .. },
                  output_reference: active_operators_out_ref,
                }) = list.at(inputs, active_operators_node_input_index)

                // 9. The picked active operators contract redeemer must point
                //    to the same input found based on the settlement's
                //    redeemer.
                expect
                  active_operators_out_ref == active_operators_spend_purpose_out_ref

                // 10. Provided input index of an active operator node must come
                //     from the address found from the hub oracle.
                expect active_operators_node_address == active_operators_addr

                // Grab the continued output of the active operators contract.
                expect Some(Output {
                  datum: InlineDatum(cont_active_operators_node_datum_data),
                  ..
                }) = list.at(outputs, active_node_output_index)

                // TODO: Update the `UpdateCommitmentTime` redeemer to carry
                // this key, instead of reading it from the output datum.
                expect NodeDatum {
                  key: Key(active_operators_node_key),
                  data: active_operators_data,
                  ..
                }: NodeDatum = cont_active_operators_node_datum_data

                // 11. The key of the node must be the same as current
                //     settlement's recorded operator validation key hash.
                expect active_operators_node_key == operator_vkey

                // 12. Active operator's updated datum is expected to record the
                //     new commitment time.
                expect active_operators.Datum{commitment_time: Some(
                  new_commitment_time,
                )}: active_operators.Datum = active_operators_data

                // 13. The only change in the updated time must be the addition
                //     of the new commitment time as the moment of claim.
                let expected_cont_output_datum_data: Data =
                  Datum {
                    ..datum,
                    time_of_claimed_as_resolved: Some(new_commitment_time),
                  }
                cont_output_datum_data == expected_cont_output_datum_data
              }
              RevokeResolvedFlag {
                unresolved_event_ref_input_index,
                unresolved_event_asset_name,
                event_type,
                membership_proof,
              } -> {
                // 2. Find the specified (via its token name) event UTxO as a
                //    ref. input at given index. Its NFT must have the same
                //    policy as its address script hash.
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
                    unresolved_event_asset_name,
                    unresolved_event_ref_input_index,
                  )

                // Unsafe coercion is fine here as we have already validated the
                // authenticity of the UTxO.
                let Pair(event_key, event_value) =
                  user_events.unsafe_to_key_value_pair(event_datum_data)

                // 3. The specified deposit event must be present in the stored
                //    tree of the settlement datum that's been flagged as fully
                //    resolved by its operator.
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

                // 4. Expect a mint redeemer at the specified redeemer index.
                expect Some(
                  Pair(Mint(active_operators_mint_purpose_policy_id),
                  active_operators_mint_redeemer_data),
                ) = list.at(redeemers, active_operators_redeemer_index)

                // 5. The policy ID found via the redeemer must match the one
                //    found in the hub oracle.
                expect
                  active_operators_mint_purpose_policy_id == active_operators_policy_id

                // 6. Found redeemer data must have the same structure as active
                //    operator's bond slash redeemer.
                expect active_operators.RemoveOperatorSlashBond {
                  slashed_active_operator_key,
                  ..
                }: active_operators.MintRedeemer =
                  active_operators_mint_redeemer_data

                // 7. Operator vkey found in the slash bond redeemer must match
                //    the one stored in the spending settlement UTxO.
                expect slashed_active_operator_key == operator_vkey

                // 8. The only change in the datum should be removal of the
                //    claim.
                let expected_cont_output_datum_data: Data =
                  Datum { ..datum, time_of_claimed_as_resolved: None }
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
      ConcludeSettlement { input_index } -> {
        let Transaction { inputs, mint, .. } = tx

        // Grab own script hash.
        expect Output {
          address: Address { payment_credential: Script(own_hash), .. },
          value: own_value,
          ..
        } = utils.get_own_input_at(inputs, own_out_ref, input_index)

        // 1. Only one NFT is expected in the spending UTxO.
        let (own_nft_policy_id, own_nft_asset_name, _own_nft_qty) =
          utils.get_single_asset_from_value_apart_from_ada(own_value)

        // 2. The NFT must have the same policy ID as the spending script.
        expect own_nft_policy_id == own_hash

        // TODO: Is this needed?
        // expect _own_nft_qty == 1
        // 3. The spending NFT must be burnt. Since there are only 2 endpoints
        //    available in the minting logic, one of which expects the mint
        //    quantity to be 2, this automatically validates the other endpoint
        //    is invoked (i.e. the burning endpoint).
        expect [Pair(burnt_asset_name, burn_qty)] =
          mint |> assets.tokens(own_hash) |> dict.to_pairs
        and {
          burnt_asset_name == own_nft_asset_name,
          burn_qty == -1,
        }
      }
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

        // 1. Ensure merge endpoint of state queue is present in the tx.
        expect Some(state_queue_redeemer_data) =
          redeemers |> pairs.get_first(Mint(state_queue))
        expect state_queue.MergeToConfirmedState: state_queue.MintRedeemer =
          state_queue_redeemer_data

        // 2. Grab the header hash of the new confirmed state to use as the
        //    asset name of the settlement authentication NFTs.
        expect Some(Output {
          datum: InlineDatum(confirmed_state_datum_data),
          ..
        }) = outputs |> list.at(confirmed_state_output_index)
        expect ledger_state.ConfirmedState {
          header_hash: confirmed_state_header_hash,
          ..
        } = confirmed_state_datum_data

        // 3. Only 2 UTxOs must be produced at own script address. It is
        //    expected that the deposits UTxO is produced at the smaller output
        //    index. Staking parts are not validated (TODO?).
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

        // 4. Validate produced UTxOs contain authentication NFTs.
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

        // 5. Ensure the two NFTs are minted.
        expect [Pair(minted_name, minted_qty)] =
          mint |> assets.tokens(own_policy) |> dict.to_pairs
        expect and {
            minted_name == confirmed_state_header_hash,
            minted_qty == 2,
          }

        // 6. Fetch the state commitment UTxO that's being spent in order to be
        //    merged into the confirmed state.
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

        // 7. Validate the produced output datums.
        let expected_produced_deposits_datum_data: Data =
          Datum {
            mpf_root: deposits_root,
            time_of_claimed_as_resolved: None,
            operator: Some(operator_vkey),
            tree_content: settlement.Deposit,
          }
        let expected_produced_withdrawals_datum_data: Data =
          Datum {
            mpf_root: withdrawals_root,
            time_of_claimed_as_resolved: None,
            operator: Some(operator_vkey),
            tree_content: settlement.Withdrawal,
          }
        and {
          produced_deposits_datum_data == expected_produced_deposits_datum_data,
          produced_withdrawals_datum_data == expected_produced_withdrawals_datum_data,
        }
      }
      // TODO: Is validation on the state queue redeemer needed? I.e. are
      // there any scenarios for spending the confirmed state other than the
      // merging tx?
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
