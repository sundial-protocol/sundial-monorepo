use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{NoDatum, OutputReference, Transaction}
use env
use midgard/hub_oracle as hub
use midgard/settlement_queue
use midgard/user_events
use midgard/user_events/deposit.{Datum}

validator spend(hub_oracle: ScriptHash) {
  spend(
    datum: Option<user_events.Datum<Data>>,
    redeemer: user_events.SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    user_events.validate_spend(
      hub_oracle: hub_oracle,
      datum: datum,
      redeemer: redeemer,
      own_out_ref: own_out_ref,
      tx: tx,
      hub_datum_resolver: fn(hub_datum, return) -> Bool {
        let hub.Datum { deposit, settlement_queue, reserve_addr, .. } =
          hub_datum
        return(deposit, settlement_queue, reserve_addr)
      },
      settlement_node_data_resolver: fn(settlement_queue_node_data, return) -> Bool {
        let settlement_queue.NodeData {
          deposits_root,
          start_time: settlement_start_time,
          end_time: settlement_end_time,
          ..
        } = settlement_queue_node_data
        return(deposits_root, settlement_start_time, settlement_end_time)
      },
      event_conclusion_output_datum_validator: fn(output_datum) -> Bool {
        output_datum == NoDatum
      },
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
    //    - The output value of the UTxO at the deposit address
    let
      l2_id,
      output_deposit_value,
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
