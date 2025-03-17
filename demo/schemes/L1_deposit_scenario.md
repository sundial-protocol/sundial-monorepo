```mermaid

sequenceDiagram

  Actor User
  participant Midgard
  participant StateQueue
  User ->> Midgard: Submit L1 Deposit Tx<br>(spend l1_nonce)
  Midgard ->> StateQueue: Register staking script with l1_nonce<br>Mint auth token + Deposit datum<br>Send funds + token + address to deposit address
  Note over Midgard,StateQueue: DepositDatum ≔<br>event : DepositEvent,<br>inclusion time : PosixTime,<br>witness : ScriptHash,<br>refund address : Address,<br>refund datum : Option (Data)
  StateQueue ->> StateQueue: Check deposit inclusion in block header
  alt Fraud proof detected
    StateQueue ->> StateQueue: Remove bad header<br>Re-include deposit events
  else No fraud proof
    StateQueue ->> StateQueue: Deposit included eventually
  end
  alt In settlement queue
    StateQueue ->> Midgard: Absorb deposit to reserver/withdrawals
  else Not in settlement queue
    StateQueue ->> User: Refund deposit to refund_address
  end
  Midgard ->> Midgard: Deregister staking credential
```