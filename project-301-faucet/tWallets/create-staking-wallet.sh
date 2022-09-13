#! /bin/bash

WALLETDIRECTORY=$1
mkdir $WALLETDIRECTORY
cd $WALLETDIRECTORY

cardano-cli address key-gen \
    --verification-key-file payment.vkey \
    --signing-key-file payment.skey

cardano-cli stake-address key-gen \
    --verification-key-file stake.vkey \
    --signing-key-file stake.skey 

cardano-cli address build \
    --payment-verification-key-file payment.vkey \
    --stake-verification-key-file stake.vkey \
    --out-file payment.addr \
    --testnet-magic 1097911063