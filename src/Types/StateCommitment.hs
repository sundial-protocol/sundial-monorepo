{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}


module Types.StateCommitment where 

import Plutarch.Prelude
import Plutarch.DataRepr (DerivePConstantViaData(DerivePConstantViaData), PDataFields)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import qualified PlutusTx
import MerkleTree.MerklePatriciaForestry (PMerklePatriciaForestry, MerklePatriciaForestry)
import PlutusLedgerApi.V2 (POSIXTime, BuiltinByteString, ScriptPurpose)
import Plutarch.LedgerApi.V3 (
  PScriptHash,
  PRedeemer,
  PDatum,
  PPubKeyHash(..),
  PPosixTime(..),
  PScriptPurpose,
  PTxId, 
  PTxOutRef, 
  PTxOut, 
  PDatumHash,
  PTxInInfo
  )
import Plutarch.Crypto (pblake2b_256)
import Plutarch.Builtin (pserialiseData, pforgetData)
import Plutarch.Prelude
import MerkleTree.MerklePatriciaForestry
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Interval qualified as Interval
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V3 (TxInInfo)
import PlutusLedgerApi.V3.Tx qualified as V3
import PlutusTx.AssocMap (Map)

data Block = Block
  { txRoot :: MerklePatriciaForestry
  , depositEventRoot :: MerklePatriciaForestry
  , withdrawalEventRoot :: MerklePatriciaForestry
  , startTimestamp :: POSIXTime
  , endTimestamp :: POSIXTime
  , oldStateRoot :: MerklePatriciaForestry
  , newStateRoot :: MerklePatriciaForestry
  , transactionCount :: Integer
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''Block

data PBlock (s :: S)
  = PBlock
      ( Term
          s
          ( PDataRecord
              '[ "txRoot" ':= PMerklePatriciaForestry
               , "depositEventRoot" ':= PMerklePatriciaForestry
               , "withdrawalEventRoot" ':= PMerklePatriciaForestry
               , "startTimestamp" ':= PPosixTime
               , "endTimestamp" ':= PPosixTime
               , "oldStateRoot" ':= PMerklePatriciaForestry
               , "newStateRoot" ':= PMerklePatriciaForestry
               , "transactionCount" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PBlock where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PBlock where
  type PLifted PBlock = Block

deriving via
  (DerivePConstantViaData Block PBlock)
  instance
    (PConstantDecl Block)

-- StateCommitment type
data StateCommitment = StateCommitment
  { publisher :: BuiltinByteString
  , block :: Block
  , oldUtxoRoot :: MerklePatriciaForestry
  , newUtxoRoot :: MerklePatriciaForestry
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''StateCommitment

data PStateCommitment (s :: S)
  = PStateCommitment
      ( Term
          s
          ( PDataRecord
              '[ "publisher" ':= PPubKeyHash
               , "block" ':= PBlock
               , "oldUtxoRoot" ':= PMerklePatriciaForestry
               , "newUtxoRoot" ':= PMerklePatriciaForestry
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PStateCommitment where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PStateCommitment where
  type PLifted PStateCommitment = StateCommitment

deriving via
  (DerivePConstantViaData StateCommitment PStateCommitment)
  instance
    (PConstantDecl StateCommitment)

data MidgardScriptInfo
  = MintingScript V2.CurrencySymbol
  | SpendingScript V2.ScriptHash V2.TxOutRef
  | ObservingScript V2.ScriptHash
  deriving stock (Generic)

PlutusTx.unstableMakeIsData ''MidgardScriptInfo

data PMidgardScriptInfo (s :: S)
  = PMintingScript (Term s (PDataRecord '["_0" ':= Value.PCurrencySymbol]))
  | PSpendingScript (Term s (PDataRecord '["_0" ':= PScriptHash, "_1" ':= PTxOutRef]))
  | PObservingScript (Term s (PDataRecord '["_0" ':= PScriptHash]))
  deriving stock (Generic)
  deriving anyclass
    ( PlutusType
    , PIsData
    , PEq
    , PShow
    )

instance DerivePlutusType PMidgardScriptInfo where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PMidgardScriptInfo where
  type PLifted _ = MidgardScriptInfo

deriving via
  (DerivePConstantViaData MidgardScriptInfo PMidgardScriptInfo)
  instance
    PConstantDecl MidgardScriptInfo

data MidgardTxInfo = MidgardTxInfo
  { mtxInfoInputs                :: [TxInInfo]
  , mtxInfoInputsLength          :: Integer
  , mtxInfoReferenceInputs       :: [TxInInfo]
  , mtxInfoReferenceInputsLength :: Integer 
  , mtxInfoOutputs               :: [V2.TxOut]
  , mtxInfoOutputsLength         :: Integer 
  , mtxInfoFee                   :: V2.Lovelace
  , mtxInfoMint                  :: V2.Value
  , mtxInfoObservers             :: [V2.ScriptHash]
  , mtxInfoValidRange            :: V2.POSIXTimeRange
  , mtxInfoSignatories           :: [V2.PubKeyHash]
  , mtxInfoRedeemers             :: Map ScriptPurpose V2.Redeemer
  , mtxInfoData                  :: Map V2.DatumHash V2.Datum
  , mtxInfoId                    :: V3.TxId
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''MidgardTxInfo

-- TODO: should we have two transaction formats? txInfo and serializedTx?
newtype PMidgardTxInfo (s :: S)
  = PMidgardTxInfo
      ( Term
          s
          ( PDataRecord
              '[ "inputs" ':= PBuiltinList (PAsData PTxInInfo)
               -- total number of inputs
               , "inputsCount" ':= PInteger 
               , "referenceInputs" ':= PBuiltinList (PAsData PTxInInfo)
               -- total number of reference inputs
               , "referenceInputsLength" ':= PInteger 
               , "outputs" ':= PBuiltinList (PAsData PTxOut)
               -- total number of outputs 
               , "outputsLength" ':= PInteger
               -- the transaction fee
               , "fee" ':= Value.PLovelace
               -- value minted by the transaction
               , "mint" ':= Value.PValue 'AssocMap.Sorted 'Value.NoGuarantees 
               -- count of unique tokens minted by the transaction
               , "mintCount" ':= PInteger 
               -- Script hashes of all observer scripts executing in the transaction
               , "observers" ':= PBuiltinList (PAsData PScriptHash)
               , "validRange" ':= Interval.PInterval PPosixTime
               , "signatories" ':= PBuiltinList (PAsData PPubKeyHash)
               , "redeemers" ':= PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)) 
               , "data" ':= PBuiltinList (PBuiltinPair (PAsData PDatumHash) (PAsData PDatum))
               -- hash of the pending transaction
               -- TODO:
               -- probably have to change this to script data integrity hash or something
               , "id" ':= PTxId
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass
    ( PlutusType
    , PIsData
    , PDataFields
    , PEq
    )

instance DerivePlutusType PMidgardTxInfo where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PMidgardTxInfo where
  type PLifted _ = MidgardTxInfo

deriving via
  (DerivePConstantViaData MidgardTxInfo PMidgardTxInfo)
  instance
    PConstantDecl MidgardTxInfo

data PMidgardContext (s :: S) = 
  PMidgardContext 
    ( Term
        s
        ( PDataRecord
            '[ "txInfo" ':= PMidgardTxInfo
             , "redeemer" ':= PRedeemer
             , "scriptInfo" ':= PMidgardScriptInfo
             ]
        )
    )
  deriving stock Generic 
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PMidgardContext where
  type DPTStrat _ = PlutusTypeData


-- Test whether a transaction hash is present in the given tx_root.
-- Returns `False` when the element isn't in the tree.
phasTransactionHash :: Term s (PMerklePatriciaForestry :--> PByteString :--> PProof :--> PBool)
phasTransactionHash = phoistAcyclic $ plam $ \txroot txhash proof -> 
  phas # txroot # txhash # txhash # proof 


ptxHashMatches :: Term s (PByteString :--> PAsData PMidgardTxInfo :--> PBool)
ptxHashMatches = phoistAcyclic $ plam $ \hash transaction -> 
  hash #== pblake2b_256 # (pserialiseData # (pforgetData transaction))
 