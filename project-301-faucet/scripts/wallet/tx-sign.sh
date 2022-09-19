#! /bin/bash

TX_DRAFT_FILE_PATH=$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/transactions/tx.draft
PAYMENT_SKEY_PATH=$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/tWallets/minterWallet/payment.skey
TESTNET=1
OUTFILE_PATH=$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/transactions/tx.signed

cardano-cli transaction sign \
--tx-body-file $TX_DRAFT_FILE_PATH \
--signing-key-file $PAYMENT_SKEY_PATH \
--testnet-magic $TESTNET \
--out-file $OUTFILE_PATH