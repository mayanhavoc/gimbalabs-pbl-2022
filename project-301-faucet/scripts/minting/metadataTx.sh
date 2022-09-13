#! /bin/bash/

TXIN=
SENDERADDRESS=
SENDERKEY=
METADATA_JSON_FILE=
TESTNET=

cardano-cli transaction build \
--babbage-era \
--testnet-magic 1097911063 \
--tx-in $TXIN \
--change-address $SENDERADDRESS \
--metadata-json-file $METADATA_JSON_FILE \
--protocol-params-file ../../project-02/protocol.json \
--out-file tx-with-metadata.raw

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1097911063 \
--tx-body-file tx-with-metadata.raw \
--out-file tx-with-metadata.signed

cardano-cli transaction submit \
--tx-file tx-with-metadata.signed \
--testnet-magic $TESTNET