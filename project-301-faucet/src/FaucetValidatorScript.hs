{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}


module FaucetValidatorScript where

import              Control.Monad             hiding (fmap)
import              Data.Aeson           (ToJSON, FromJSON)
import              Data.Map                  as Map
import              Data.Text                 (Text)
import              Data.Void                 (Void)
import              GHC.Generics              (Generic)
import              Plutus.Contract
import              PlutusTx                  (Data (..))
import qualified    PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)
import              Ledger              hiding (singleton)
import              Ledger.Constraints   (TxConstraints)
import qualified    Ledger.Constraints   as Constraints
import              Ledger.Typed.Scripts as Scripts
import              Ledger.Ada
import              Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import              Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import              Playground.Types     (KnownCurrency (..))
import              Schema                    (ToSchema)
import              Ledger.Address
import              Ledger.Value        as Value
import              Prelude             (Show (..))
import qualified    Prelude                   as Pr

-- Simple Faucet validator script

-- Usage:
-- Expect one utxo at each contract.
-- Take that utxo as input
-- Create a new utxo with the "change" as output

-- This contract provides an example of using Validator Parameters.

-- For now, the Datum and Redeemer are not used in contract logic
-- Transactions will still have to match the type Integer for Datum and Redeemer
-- The context matters: we want to see that the PPBLSummer2022 token is in the transaction inputs and outputs.

data FaucetDatum = FaucetDatum
  { accessTokenSymbol   :: !CurrencySymbol
  , accessTokenName     :: !TokenName
  , faucetTokenSymbol   :: !CurrencySymbol
  , faucetTokenName     :: !TokenName
  , withdrawalAmount    :: !Integer
  } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''FaucetDatum

{-# INLINABLE mkValidator #-}
mkValidator :: FaucetDatum -> () -> ScriptContext -> Bool
mkValidator faucet _ ctx =   traceIfFalse "Input needs PPBLSummer2022 token"    inputHasAccessToken &&
                            traceIfFalse "PPBLSummer2022 token must return to sender" outputHasAccessToken &&
                            traceIfFalse "Faucet token must be distributed to sender" outputHasFaucetToken &&
                            traceIfFalse "Must return remaining tokens to contract"   faucetContractGetsRemainingTokens &&
                            traceIfFalse "Do we need to check datum"                  checkDatumIsOk
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    receiverPkh :: PubKeyHash
    receiverPkh = head $ txInfoSignatories info

    allTokens :: [CurrencySymbol]
    allTokens = symbols $ valueSpent info

    inputHasAccessToken :: Bool
    inputHasAccessToken = (accessTokenSymbol faucet) `elem` allTokens

    valueToReceiver :: Value
    valueToReceiver = valuePaidTo info receiverPkh

    outputHasAccessToken :: Bool
    outputHasAccessToken = (valueOf valueToReceiver (accessTokenSymbol faucet) (accessTokenName faucet)) >= 1

    outputHasFaucetToken :: Bool
    outputHasFaucetToken = (valueOf valueToReceiver (faucetTokenSymbol faucet) (faucetTokenName faucet)) >= (withdrawalAmount faucet)

    -- The UTXO input from Faucet
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "faucet input missing"
        Just i  -> txInInfoResolved i

    -- The UTXO output back to Faucet
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o -- There must be exactly ONE output UTXO
        _   -> traceError "expected exactly one faucet output"

    faucetInputValue :: Value
    faucetInputValue = txOutValue ownInput

    faucetOutputValue :: Value
    faucetOutputValue = txOutValue ownOutput

    faucetContractGetsRemainingTokens :: Bool
    faucetContractGetsRemainingTokens = (valueOf faucetInputValue (faucetTokenSymbol faucet) (faucetTokenName faucet)) - (withdrawalAmount faucet) <= (valueOf faucetOutputValue (faucetTokenSymbol faucet) (faucetTokenName faucet))

    checkDatumIsOk :: Bool
    checkDatumIsOk = True

data Faucet
instance Scripts.ValidatorTypes Faucet where
    type instance DatumType Faucet = FaucetDatum
    type instance RedeemerType Faucet = () 

-- instance ValidatorTypes FaucetTypes where
--     type DatumType FaucetTypes = Integer
--     type RedeemerType FaucetTypes = Integer

typedValidator :: Scripts.TypedValidator Faucet
typedValidator = Scripts.mkTypedValidator @Faucet
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @FaucetDatum @()


validator :: Validator
validator = Scripts.validatorScript typedValidator

