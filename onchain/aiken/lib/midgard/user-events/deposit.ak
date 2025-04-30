use midgard/ledger_state
use midgard/user_events

pub type Datum =
  user_events.Datum<ledger_state.DepositEvent>
