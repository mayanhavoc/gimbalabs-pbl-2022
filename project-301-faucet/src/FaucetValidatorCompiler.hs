{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module FaucetValidatorCompiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Ledger
import qualified FaucetValidatorScriptWithPkh (validator, FaucetParams (..))



writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

-- This is a comment in Haskell
-- 1. Change the name of the output file to ppbl-faucet-yourTokenName.plutus
-- 2. Change the faucetTokenSymbol and faucetTokenName to match the token that you want to distribute
-- 3. Change the withdrawalAmount to whatever amount you want people to able to claim in one transaction
-- 4. Compile your script
-- 5. Recommended: take a screenshot of this file so that you can remember the parameters you used

-- TokenSymbol == Token's policyid

writeFaucetScript :: IO (Either (FileError ()) ())
writeFaucetScript = writeValidator "output/ppbl-faucet-micochango.plutus" $ FaucetValidatorScriptWithPkh.validator $ FaucetValidatorScriptWithPkh.FaucetParams
    {
      FaucetValidatorScriptWithPkh.accessTokenSymbol     = "1309921891e459c7e9acb338d5dae18f98d1c2f55c1852cd5cf341f9"
    , FaucetValidatorScriptWithPkh.accessTokenName       = "PPBLSummer2022"
    , FaucetValidatorScriptWithPkh.faucetTokenSymbol     = "5e33da121bfacab420c3af5443ef68b31cfcf6737069c657907bde6c"
    , FaucetValidatorScriptWithPkh.faucetTokenName       = "micochango"
    , FaucetValidatorScriptWithPkh.withdrawalAmount      = 100
    }
