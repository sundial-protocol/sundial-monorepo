{-# LANGUAGE OverloadedStrings #-}

module Testing.Budget (tests) where

import Codec.Serialise (deserialise)
import Data.ByteString.Base16.Lazy qualified as BS16
import Data.ByteString.Lazy.Char8 qualified as BS8
import Plutarch.Internal.Term
import Plutarch.LedgerApi.V3 (scriptHash)
import Plutarch.MerkleTree.PatriciaForestry (MerklePatriciaForestry (..), ProofStep)
import Plutarch.Prelude
import PlutusCore.Data qualified as PD
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
import PlutusLedgerApi.V3 hiding (POSIXTime)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import PlutusTx.IsData qualified as PlutusTx
import Profile (getExUnits)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Testing.ScriptContextBuilder (buildScriptContext, withRewardingScript)
import Types.Membership (MerkleMembershipRedeemer (..), MerkleNonMembershipRedeemer (..))
import Validators.Membership (membershipStakeValidator, nonMembershipStakeValidator)

-- | Component-wise budget ceiling check.
-- Both CPU steps and memory units must be within their respective limits.
-- Note: 'ExBudget' derives a lexicographic 'Ord', so '(<= )' on 'ExBudget'
-- does not enforce both components simultaneously; this helper is explicit.
withinBudget :: ExBudget -> ExBudget -> Bool
withinBudget actual maxBudget =
    exBudgetCPU actual <= exBudgetCPU maxBudget
        && exBudgetMemory actual <= exBudgetMemory maxBudget

-- | Assert that a fully-applied closed term evaluates within a budget ceiling.
-- The actual budget is included in the failure message for easy tuning.
budgetTest :: String -> ClosedTerm a -> ExBudget -> TestTree
budgetTest name term maxBudget = testCase name $ do
    let budget = getExUnits term
    assertBool
        ( "execution budget "
            <> show budget
            <> " exceeds ceiling "
            <> show maxBudget
        )
        (withinBudget budget maxBudget)

-- | Conservative per-validator budget ceiling.
-- Set to 50 % of Cardano mainnet per-transaction protocol limits:
--   CPU    : 5_000_000_000 steps  (mainnet limit: 10_000_000_000)
--   Memory : 7_000_000 units      (mainnet limit: 14_000_000)
-- Tighten these values once you have baseline measurements.
validatorBudgetCeiling :: ExBudget
validatorBudgetCeiling =
    ExBudget
        { exBudgetCPU = 5_000_000_000
        , exBudgetMemory = 7_000_000
        }

tests :: TestTree
tests =
    testGroup
        "Execution Budget Tests"
        [ budgetTest
            "membershipStakeValidator / simple membership (key=deadbeef)"
            ( membershipStakeValidator #$ pconstant . buildScriptContext $
                withRewardingScript
                    (toBuiltinData simpleMembershipRedeemer1)
                    membershipValidatorCredential
                    0
            )
            validatorBudgetCeiling
        , budgetTest
            "membershipStakeValidator / simple membership (key=cafe)"
            ( membershipStakeValidator #$ pconstant . buildScriptContext $
                withRewardingScript
                    (toBuiltinData simpleMembershipRedeemer2)
                    membershipValidatorCredential
                    0
            )
            validatorBudgetCeiling
        , budgetTest
            "nonMembershipStakeValidator / simple non-membership (key=cafe)"
            ( nonMembershipStakeValidator #$ pconstant . buildScriptContext $
                withRewardingScript
                    (toBuiltinData simpleNonMembershipRedeemer)
                    nonMembershipValidatorCredential
                    0
            )
            validatorBudgetCeiling
        ]

-- ---------------------------------------------------------------------------
-- Validator credentials (needed to build the rewarding-script context)
-- ---------------------------------------------------------------------------

membershipValidatorCredential :: Credential
membershipValidatorCredential =
    ScriptCredential
        . scriptHash
        . either (error . show) id
        $ compile (Tracing LogInfo DoTracing) membershipStakeValidator

nonMembershipValidatorCredential :: Credential
nonMembershipValidatorCredential =
    ScriptCredential
        . scriptHash
        . either (error . show) id
        $ compile (Tracing LogInfo DoTracing) nonMembershipStakeValidator

-- ---------------------------------------------------------------------------
-- Fixture data (mirrors Testing.MembershipValidator)
-- ---------------------------------------------------------------------------

simpleMembershipRedeemer1 :: MerkleMembershipRedeemer
simpleMembershipRedeemer1 =
    MerkleMembershipRedeemer
        { mmInputRoot = MerklePatriciaForestry $ stringToBuiltinByteStringHex "90ea40c96e91b5d6c4518a56e468e20ed0ecfee07c4d300503198dfc7173bc9d"
        , mmInputKey = stringToBuiltinByteStringHex "deadbeef"
        , mmInputValue = stringToBuiltinByteStringHex "face"
        , mmInputProof = parseProofCBOR "9fd87b9f0058204e400278c29c37ee640391dfb9792390a8ac9adb6200ed47c725a86099a8586c582028b77fdfb12e58d34edf655736e1d62414635099c5a9e1ac49e52cbb89ae3100ffff"
        }

simpleMembershipRedeemer2 :: MerkleMembershipRedeemer
simpleMembershipRedeemer2 =
    MerkleMembershipRedeemer
        { mmInputRoot = MerklePatriciaForestry $ stringToBuiltinByteStringHex "90ea40c96e91b5d6c4518a56e468e20ed0ecfee07c4d300503198dfc7173bc9d"
        , mmInputKey = stringToBuiltinByteStringHex "cafe"
        , mmInputValue = stringToBuiltinByteStringHex "decaf"
        , mmInputProof = parseProofCBOR "9fd87b9f005820f3e925002fed7cc0ded46842569eb5c90c910c091d8d04a1bdf96e0db719fd915820888938a89cccc775894c0a433c2f7d7bfee5a7d64ac5563c22ca54b8a0cc4644ffff"
        }

simpleNonMembershipRedeemer :: MerkleNonMembershipRedeemer
simpleNonMembershipRedeemer =
    MerkleNonMembershipRedeemer
        { mnmInputRoot = MerklePatriciaForestry $ stringToBuiltinByteStringHex "a821131f1d697b5f9203b2d95ac8c54ecde0135301ec32e31a8c1255b7c65a7f"
        , mnmInputKey = stringToBuiltinByteStringHex "cafe"
        , mnmInputProof = parseProofCBOR "9fd87b9f005820f3e925002fed7cc0ded46842569eb5c90c910c091d8d04a1bdf96e0db719fd915820888938a89cccc775894c0a433c2f7d7bfee5a7d64ac5563c22ca54b8a0cc4644ffff"
        }

-- | Decode a hex-encoded CBOR proof into a list of 'ProofStep's.
parseProofCBOR :: String -> [ProofStep]
parseProofCBOR =
    PlutusTx.unsafeFromData
        . deserialise @PD.Data
        . either error id
        . BS16.decode
        . BS8.pack
