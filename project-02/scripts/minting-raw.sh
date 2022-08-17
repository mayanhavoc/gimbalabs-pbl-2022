#! /bin/bash

# $TESTNET is an environment variable defined in the shell config
# TXIN is the UTxO that will be spent in order to mint the tokens.
# Results from querying the $MINTINGWALLET and selecting a UTXO that can cover the minting cost  
#                            TxHash                                 TxIx        Amount
# --------------------------------------------------------------------------------------
# 6a0ac8b791aff0c0e6ef96689bc58747fd9d7e3138d86b38f851420ebe54ae81     0        1000000000 lovelace + TxOutDatumNone
TXIN=6a0ac8b791aff0c0e6ef96689bc58747fd9d7e3138d86b38f851420ebe54ae81#0 

# The command to build a verification and signing key is 
# cardano-cli address key-gen \
#   --verification-key-file payment.vkey \
#   --signing-key-file payment.skey

# The $MINTINGWALLET is the payment address built from the payment.vkey and payment.skey
MINTINGWALLET=addr_test1vza84v5z84mmveyyyvs6er0wa04qzdakr8hz4evrq8vvfmshsd9yk

# The $POLICYSCRIPT's key hash is derived from the command `cardano-cli address key-hash --payment-verification-key-file payment.vkey`  where the payment.vkey comes from the $MINTINGWALLET
POLICYSCRIPT=../native-scripts/my-first-policy.script

# The $POLICYID is the output of the command 
# cardano-cli transaction policyid --script-file my-first-policy.script
# Where 'my-first-policy.script' is the path to the $MINTINGWALLET file created to hold the key hash and type of signature of said policy
POLICYID=d89fd37a96c43bb5566550785c2c59f69ad15266b7682bc9bac44edc

# The $TOKENNAME CANNOT be a string, it must be converted into a hexadecimal value
# ref. https://string-functions.com/string-hex.aspx
TOKENNAME=63696c616e74726f

cardano-cli transaction build \
--babbage-era \
--testnet-magic $TESTNET \
--tx-in $TXIN \
--tx-out $MINTINGWALLET+"1500000 + 100 $POLICYID.$TOKENNAME" \
--mint "100 $POLICYID.$TOKENNAME" \
--mint-script-file $POLICYSCRIPT \
--change-address $MINTINGWALLET \
--protocol-params-file ../protocol.json \
--out-file ../transactions/mint-native-assets.raw