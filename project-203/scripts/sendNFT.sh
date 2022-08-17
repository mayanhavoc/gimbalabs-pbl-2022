#! /bin/bash

TXIN1="ac06369da6ddcdeefc1c1d3da89655c02e75b426f55e71b876857d43b0977a1e#1"
TXIN2="ac06369da6ddcdeefc1c1d3da89655c02e75b426f55e71b876857d43b0977a1e#0"
POLICYID="d89fd37a96c43bb5566550785c2c59f69ad15266b7682bc9bac44edc"
ASSETNAME="47696d62616c6162734e4654"
MINTERADDRESS="addr_test1vza84v5z84mmveyyyvs6er0wa04qzdakr8hz4evrq8vvfmshsd9yk"
MINTERKEY="$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-203/tWallets/tWallet01/payment.skey"
ETERNL="addr_test1qr8445rsl2cyp9z8rf27et7n9usezfjjeeu09czlftg29vg48qx7a872uhehg8vvzysqysfkhewul0flashf9wf0r85sd749lp"

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
