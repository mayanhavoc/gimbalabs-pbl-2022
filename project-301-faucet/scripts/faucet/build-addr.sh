#! /bin/bash

PAYMENT_SCRIPT_FILE=../../shared-script/ppbl-pre-prod-faucet-tgimbal-pkh.plutus

cardano-cli address build \
--payment-script-file $PAYMENT_SCRIPT_FILE \
--testnet-magic 1 \
--out-file ppbl-faucet-integer.addr