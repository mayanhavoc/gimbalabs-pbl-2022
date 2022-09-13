#! /bin/bash

WALLETDIRECTORY=$1
mkdir $WALLETDIRECTORY
cd $WALLETDIRECTORY

echo
echo "Building payment keys..."
cardano-cli address key-gen \
    --verification-key-file payment.vkey \
    --signing-key-file payment.skey

echo
echo "Building staking keys..."
cardano-cli stake-address key-gen \
    --verification-key-file stake.vkey \
    --signing-key-file stake.skey 

echo
echo "Building payment address..."
cardano-cli address build \
    --payment-verification-key-file payment.vkey \
    --stake-verification-key-file stake.vkey \
    --out-file payment.addr \
    --testnet-magic 1