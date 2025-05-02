pub type SpendRedeemer {
  FundWithdrawal
}

pub type WithdrawRedeemer {
  ValidateWithdrawals {
    withdrawal_input_index: Int,
    reserve_input_indices: List<Int>,
  }
}
