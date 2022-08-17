{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Project01.MoneyTransfer where
 
import Control.Monad (void)
import Data.ByteString.Char8 qualified as C
import Data.Text                  (Text)
import Data.Void
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Ledger (Address, Datum (Datum), ScriptContext, Validator, Value, PaymentPubKeyHash, getCardanoTxId)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (ChainIndexTxOut (..))
import Ledger.Typed.Scripts qualified as Scripts
import Playground.Contract
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude hiding (pure, (<$>), Semigroup (..))
import Prelude (Semigroup (..))
import           Text.Printf          (printf)

data MoneyTransferDatum = MTDatum
    { beneficiary :: PaymentPubKeyHash
    , value :: Integer
    , secret :: String
    } deriving Show

PlutusTx.unstableMakeIsData ''MTDatum

newtype HashedString = HS BuiltinByteString
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift  ''HashedString

newtype ClearString = CS BuiltinByteString
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''ClearString

{-# INLINABLE validateAnswer #-}
validateAnswer :: HashedString -> ClearString -> ScriptContext -> Bool
validateAnswer hs cs _ = isValidAnswer hs cs
 
{-# INLINABLE isGoodAnswer #-}
isValidAnswer :: HashedString -> ClearString -> Bool
isValidAnswer (HS secret) (CS answer') = secret == sha2_256 answer'
clearString :: Haskell.String -> ClearString
clearString = CS . toBuiltin . C.pack

data Transfer
instance Scripts.ValidatorTypes Transfer where
  type instance DatumType Transfer = MTDatum
  type instance RedeemerType Transfer = ClearString

{-# INLINABLE mkValidator  #-}
mkValidator :: Datum -> Bool -> ScriptContext -> Bool
mkValidator dat redeemer ctx =
  traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
  isValidAnswer 
    where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      signedByBeneficiary :: Bool
      signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary dat

typedValidator :: Scripts.TypedValidator Transfer
typedValidator = Scripts.mkTypedValidator @Transfer
       $$(PlutusTx.compile[|| mkValidator ||])
       $$(PlutusTx.compile [|| wrap ||])
   where
     wrap = Scripts.wrapValidator @TransferDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

      

        
