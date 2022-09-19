#! /bin/bash


TXIN=17a8d8b04072be3153427fbfcaaa963ae62f884a66c50795f29b4a9c3eea3c84#1
ADDR=addr_test1qqrzd8t2aljh5agrnms0cesre2e8cvtsg9jg5d3snjmf9mf3xn7jydpgqqwc9ek4hdkqm2xw56mkmzr02de0jjkuemqsr9mger
LOVELACE=12831991
OUTFILE_PATH=$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/transactions/tx.draft

cardano-cli transaction build \
--alonzo-era \
--testnet-magic 1 \
--tx-in $TXIN \
--tx-out $ADDR+$LOVELACE \
--change-address $ADDR \
--out-file $OUTFILE_PATH