#! /bin/bash

# The $MINTERKEY is the path to the payment.skey file of the $MINTINGWALLET
MINTERKEY=../tWallet01/payment.skey

cardano-cli transaction sign \
--signing-key-file $MINTERKEY \
--testnet-magic $TESTNET \
--tx-body-file ../transactions/mint-native-assets.raw \
--out-file ../transactions/mint-native-assets.signed
