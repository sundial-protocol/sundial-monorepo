module Validators.Membership (membershipStakeValidator) where

import Plutarch.Core.Integrity (pisRewardingScript)
import Plutarch.Core.ValidationLogic (pvalidateConditions)
import Plutarch.LedgerApi.V3 (
  PScriptContext (
    PScriptContext,
    pscriptContext'redeemer,
    pscriptContext'scriptInfo
  ),
 )
import Plutarch.MerkleTree.PatriciaForestry (phas)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Types.Membership (PMerkleMembershipRedeemer (..))

-- | 'Withdraw zero' validator to ensure the merkle tree contains a specific entry.
membershipStakeValidator :: ClosedTerm (PScriptContext :--> PUnit)
membershipStakeValidator = plam $ \ctx -> P.do
  PScriptContext {pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  PMerkleMembershipRedeemer
    { pmmInputRoot
    , pmmInputKey
    , pmmInputValue
    , pmmInputProof
    } <-
    pmatch $
      pfromData $
        -- Q (Chase): Should we perhaps use PTryFrom here?
        punsafeCoerce @(PAsData PMerkleMembershipRedeemer) (pto pscriptContext'redeemer)
  let validateMembership =
        phas
          # pfromData pmmInputRoot
          # pfromData pmmInputKey
          # pfromData pmmInputValue
          # pfromData pmmInputProof
  pvalidateConditions
    [ pisRewardingScript (pdata pscriptContext'scriptInfo)
    , validateMembership
    ]
