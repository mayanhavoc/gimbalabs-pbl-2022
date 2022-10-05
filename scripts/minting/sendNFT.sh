#! /bin/bash

TXIN1=
TXIN2=
POLICYID=
ASSETNAME=
MINTERADDRESS=
MINTERKEY=
ETERNL=
TESTNET=

cardano-cli transaction build \
--babbage-era \
--testnet-magic $TESTNET \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $ETERNL+"2000000 + 1 $POLICYID.$ASSETNAME" \
--change-address $MINTERADDRESS \
--out-file tx.draft

cardano-cli transaction sign \
--tx-body-file tx.draft \
--signing-key-file $MINTERKEY \
--testnet-magic $TESTNET \
--out-file tx.signed 

cardano-cli transaction submit \
--testnet-magic $TESTNET \
--tx-file tx.signed 
