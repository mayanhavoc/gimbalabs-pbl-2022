#! /bin/bash


TXIN=69d6ffdb8584f33b7563d6d7fb248608154ebf3b14abc0ef0870510942157926#0
ADDR=addr_test1qq3r5yg253j0h4vnurs6ddd2rvgzhqljhn9mqmegarj3rs9reqs2pmc4q9mzzdln6hx3xyyfarkvtd7hnh65fuvms0cs07wg0m
LOVELACE=1779663982
OUTFILE_PATH=$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/transactions/tx.draft

cardano-cli transaction build \
--alonzo-era \
--testnet-magic 1 \
--tx-in $TXIN \
--tx-out $ADDR+$LOVELACE \
--change-address $ADDR \
--out-file $OUTFILE_PATH