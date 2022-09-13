#! /bin/bash

# Remember that $ASSETNAME must be a Hex string

TXIN=e673346cd7114bc73218a797a1b8d8c7f4304e796da97196669db76142ad38e7
TXIX=0
TX_BODY_FILE=$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/transactions/nft-mint.raw
TX_FILE=$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/transactions/nft-mint.signed
MINTER=addr_test1qq3r5yg253j0h4vnurs6ddd2rvgzhqljhn9mqmegarj3rs9reqs2pmc4q9mzzdln6hx3xyyfarkvtd7hnh65fuvms0cs07wg0m
MINTERKEY=$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/tWallets/minterWallet/payment.skey
POLICYID=5e33da121bfacab420c3af5443ef68b31cfcf6737069c657907bde6c
ASSETNAME=6d69636f6368616e676f5f746f6b656e
MINT_SCRIPT_FILE=$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/policies/policy.script
METADATA_JSON_FILE=$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/metadata/nft.json
TESTNET=1

# cardano-cli transaction build \
# --alonzo-era \
# --testnet-magic $TESTNET \
# --tx-in $TXIN#$TXIX \
# --tx-out $MINTER+"2000000 + 100000 $POLICYID.$ASSETNAME" \
# --mint "100000 $POLICYID.$ASSETNAME" \
# --mint-script-file $MINT_SCRIPT_FILE \
# --change-address $MINTER \
# --metadata-json-file $METADATA_JSON_FILE \
# --protocol-params-file $HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/protocol.json \
# --out-file $HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/transactions/nft-mint.raw 

# cardano-cli transaction sign \
# --signing-key-file $MINTERKEY \
# --testnet-magic $TESTNET \
# --tx-body-file $TX_BODY_FILE \
# --out-file $HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/transactions/nft-mint.signed

cardano-cli transaction submit \
--tx-file $TX_FILE \
--testnet-magic $TESTNET



