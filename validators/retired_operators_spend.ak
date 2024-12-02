use cardano/assets.{tokens}
use cardano/transaction.{OutputReference, Transaction}
use linked_list/list as linked_list

validator retired_operators(retired_operators_mint_script_hash: ByteArray) {
  spend(
    _datumOpt: Option<Data>,
    _redeemer: Data,
    _input: OutputReference,
    self: Transaction,
  ) {
    linked_list.list_state_transition(
      tokens(self.mint, retired_operators_mint_script_hash),
    )
  }

  else(_) {
    fail
  }
}
