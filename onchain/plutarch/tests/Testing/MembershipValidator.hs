{-# LANGUAGE OverloadedStrings #-}

module Testing.MembershipValidator (tests) where

import Test.Tasty
import Test.Tasty.HUnit

-- import Midgard.Utils (pand'List, pintToByteString)
import Testing.Eval (psucceeds)

-- import Midgard.Crypto (pethereumPubKeyToPubKeyHash, pcompressPublicKey)

import Codec.Serialise (deserialise)
import Data.ByteString.Base16.Lazy qualified as BS16
import Data.ByteString.Lazy.Char8 qualified as BS8
import Plutarch.Internal.Term
import Plutarch.LedgerApi.V3 (scriptHash)
import Plutarch.MerkleTree.PatriciaForestry (MerklePatriciaForestry (..), ProofStep)
import Plutarch.Prelude
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 hiding (POSIXTime)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import PlutusTx.IsData qualified as PlutusTx
import Testing.ScriptContextBuilder (buildScriptContext, withRewardingScript)
import Types.Membership (MerkleMembershipRedeemer (..))
import Validators.Membership (membershipStakeValidator)

tests :: TestTree
tests =
  testGroup
    "(Non)Membership Validator Tests"
    [ testCase "Simple Membership" $ do
        psucceeds $
          membershipStakeValidator #$ pconstant . buildScriptContext $
            withRewardingScript (toBuiltinData simpleMembershipRedeemer) membershipValidatorCredential 0
    ]

-- Note: Should always use the same compile config as 'psucceeds' above.
membershipValidatorCredential :: Credential
membershipValidatorCredential =
  ScriptCredential
    . scriptHash
    . either (error . show) id
    $ compile (Tracing LogInfo DoTracing) membershipStakeValidator

-- Example Merkle Tree generated via mpf-cli
-- N.B: Keys and values are in hex.
-- { "deadbeef": "face", "cafe": "decaf" }
-- root_hash: 90ea40c96e91b5d6c4518a56e468e20ed0ecfee07c4d300503198dfc7173bc9d
-- proofCBOR: 9fd87b9f0058204e400278c29c37ee640391dfb9792390a8ac9adb6200ed47c725a86099a8586c582028b77fdfb12e58d34edf655736e1d62414635099c5a9e1ac49e52cbb89ae3100ffff
-- TODO (chase): In the future, we should try to use Haskell for the generation of these.
simpleMembershipRedeemer :: MerkleMembershipRedeemer
simpleMembershipRedeemer =
  MerkleMembershipRedeemer
    { mmInputRoot = MerklePatriciaForestry $ stringToBuiltinByteStringHex "90ea40c96e91b5d6c4518a56e468e20ed0ecfee07c4d300503198dfc7173bc9d"
    , mmInputKey = stringToBuiltinByteStringHex "deadbeef"
    , mmInputValue = stringToBuiltinByteStringHex "face"
    , mmInputProof = parseProofCBOR "9fd87b9f0058204e400278c29c37ee640391dfb9792390a8ac9adb6200ed47c725a86099a8586c582028b77fdfb12e58d34edf655736e1d62414635099c5a9e1ac49e52cbb89ae3100ffff"
    }

-- | Given a hex string representing a CBOR encoded proof, parse it into the corresponding list of ProofSteps.
parseProofCBOR :: String -> [ProofStep]
parseProofCBOR =
  PlutusTx.unsafeFromData
    . deserialise @PD.Data
    . either error id
    . BS16.decode
    . BS8.pack
