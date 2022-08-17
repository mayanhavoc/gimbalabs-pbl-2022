#! /bin/bash

cardano-cli transaction submit \
--tx-file ../transactions/mint-native-assets.signed \
--testnet-magic $TESTNET

