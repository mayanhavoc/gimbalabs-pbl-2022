#! /bin/bash/

TXIN=2e0a482e9ca794782a48597a7430e8f164164d163205a023bba349a1c4c9cd0b#0
SENDERADDRESS=addr_test1vza84v5z84mmveyyyvs6er0wa04qzdakr8hz4evrq8vvfmshsd9yk
SENDERKEY="../../project-02/tWallets/tWallet01/payment.skey"
METADATA_JSON_FILE="/Users/macadmin/Documents/plutus-gimbalabs-ppbl-course-02/project-203/metadata/simple-metadata.json"

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
--testnet-magic 1097911063