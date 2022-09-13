#! /bin/bash

NETWORK_ID=
PAYMENT_ADDR=

echo
echo "UTxOs available:"
cardano-cli query utxo \
            $NETWORK_ID \
            --address $PAYMENT_ADDR