#! /bin/bash

PAYMENT_SCRIPT_FILE=../../output/ppbl-faucet-micochango.plutus

cardano-cli address build \
--payment-script-file $PAYMENT_SCRIPT_FILE \
--testnet-magic 1 \
--out-file ppbl-faucet-micochango.addr