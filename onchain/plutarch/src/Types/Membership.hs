module Types.Membership (
  PMerkleMembershipRedeemer (..),
  MerkleMembershipRedeemer (..),
  ProofStep (..),
  Neighbor (..),
) where

import Control.Monad (guard)
import Data.List (uncons)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.MerkleTree.PatriciaForestry (
  MerklePatriciaForestry,
  Neighbor (..),
  PMerklePatriciaForestry,
  PProof,
  ProofStep (..),
 )
import Plutarch.Prelude
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (
  BuiltinByteString,
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  builtinDataToData,
  dataToBuiltinData,
 )
import PlutusTx qualified

-- >>> PlutusTx.toBuiltinData $ MerklePatriciaForestry "deadbeef"
-- B "deadbeef"

data MerkleMembershipRedeemer = MerkleMembershipRedeemer
  { inputRoot :: MerklePatriciaForestry
  , inputKey :: BuiltinByteString
  , inputValue :: BuiltinByteString
  , inputProof :: [ProofStep]
  }
  deriving stock (Show, Eq, Generic)

instance PlutusTx.ToData MerkleMembershipRedeemer where
  toBuiltinData MerkleMembershipRedeemer {inputRoot, inputKey, inputValue, inputProof} =
    dataToBuiltinData $
      PD.List
        [ PlutusTx.toData inputRoot
        , PlutusTx.toData inputKey
        , PlutusTx.toData inputValue
        , PlutusTx.toData inputProof
        ]

instance PlutusTx.FromData MerkleMembershipRedeemer where
  fromBuiltinData :: PlutusTx.BuiltinData -> Maybe MerkleMembershipRedeemer
  fromBuiltinData (builtinDataToData -> PD.List dataList) = do
    (rawInputRoot, rest) <- uncons dataList
    inputRoot <- PlutusTx.fromData rawInputRoot
    (rawInputKey, rest) <- uncons rest
    inputKey <- PlutusTx.fromData rawInputKey
    (rawInputValue, rest) <- uncons rest
    inputValue <- PlutusTx.fromData rawInputValue
    (rawInputProof, rest) <- uncons rest
    inputProof <- PlutusTx.fromData rawInputProof
    guard $ null rest
    pure $ MerkleMembershipRedeemer {inputRoot, inputKey, inputValue, inputProof}
  fromBuiltinData _ = Nothing

data PMerkleMembershipRedeemer (s :: S) = PMerkleMembershipRedeemer
  { pmmInputRoot :: Term s (PAsData PMerklePatriciaForestry)
  , pmmInputKey :: Term s (PAsData PByteString)
  , pmmInputValue :: Term s (PAsData PByteString)
  , pmmInputProof :: Term s (PAsData PProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow, PEq)
  deriving (PlutusType) via (DeriveAsDataRec PMerkleMembershipRedeemer)

deriving via
  DeriveDataPLiftable (PAsData PMerkleMembershipRedeemer) MerkleMembershipRedeemer
  instance
    PLiftable PMerkleMembershipRedeemer
