#! /bin/bash

SIGNED_TX_FILE_PATH=$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/transactions/tx.signed

cardano-cli transaction submit \
--testnet-magic 1 \
--tx-file $SIGNED_TX_FILE_PATH