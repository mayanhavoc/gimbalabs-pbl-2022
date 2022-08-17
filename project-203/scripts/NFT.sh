#! /bin/bash

# Remember that $ASSETNAME must be a Hex string

TXIN="c1dad807d95a03c9f25751076053786d6c038baf5e59877e20db8c955e514b53#0"
MINTERADDRESS="addr_test1vza84v5z84mmveyyyvs6er0wa04qzdakr8hz4evrq8vvfmshsd9yk"
MINTERKEY="$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-203/tWallets/tWallet01/payment.skey"
POLICYID="d89fd37a96c43bb5566550785c2c59f69ad15266b7682bc9bac44edc"
ASSETNAME="47696d62616c6162734e4654"
MINT_SCRIPT_FILE="$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-203/policies/mh-ppbl-nft-3.script"
METADATA_JSON_FILE="$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-203/metadata/nft.json"

cardano-cli transaction build \
--babbage-era \
--testnet-magic 1097911063 \
--tx-in $TXIN \
--tx-out $MINTERADDRESS+"2000000 + 1 $POLICYID.$ASSETNAME" \
--mint "1 $POLICYID.$ASSETNAME" \
--mint-script-file $MINT_SCRIPT_FILE \
--change-address $MINTERADDRESS \
--metadata-json-file $METADATA_JSON_FILE \
--protocol-params-file $HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-203/protocol.json \
--out-file mh-mint-ppbl-nft.raw

cardano-cli transaction sign \
--signing-key-file $MINTERKEY \
--testnet-magic $TESTNET \
--tx-body-file mh-mint-ppbl-nft.raw \
--out-file mh-mint-ppbl-nft.signed

cardano-cli transaction submit \
--tx-file mh-mint-ppbl-nft.signed \
--testnet-magic $TESTNET



