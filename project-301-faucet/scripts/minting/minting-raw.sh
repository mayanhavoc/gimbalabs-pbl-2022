#! /bin/bash

# Minter address (no string quotes necessary)
MINTER=addr_test1qq3r5yg253j0h4vnurs6ddd2rvgzhqljhn9mqmegarj3rs9reqs2pmc4q9mzzdln6hx3xyyfarkvtd7hnh65fuvms0cs07wg0m

# Path to signing key
MINTERKEY=$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/tWallets/minterWallet/payment.skey

# More than one TXIN can be included to cover costs (collateral is not taken into account for covering tx costs)
TXIN=e673346cd7114bc73218a797a1b8d8c7f4304e796da97196669db76142ad38e7#0
# TXIN2=

COLLATERAL=b837d47eb199cf824d34a984ed0deaed09969581975ce863e52c9fd8d86aa653#0
POLICYID=a10aa40d0ec3fd4c8fa33a2910fb27941ae8b7ad2a5b0c30816f7a20
TOKENNAME=6d69636f6368616e676f20746f6b656e
MINTAMOUNT=1000
SCRIPTFILE="../../output/token-minting-script.plutus"
REDEEMERFILE="../../output/redeemer.json"
TESTNET=1


# Build transaction

cardano-cli transaction build \
--alonzo-era \
--testnet-magic $TESTNET \
--tx-in $TXIN \
--tx-out $MINTER+15000000+"$MINTAMOUNT $POLICYID.$TOKENNAME" \
--change-address $MINTER \
--mint "$MINTAMOUNT $POLICYID.$TOKENNAME" \
--mint-script-file $SCRIPTFILE \
--mint-redeemer-file $REDEEMERFILE \
--tx-in-collateral $COLLATERAL \
--protocol-params-file ../../protocol.json \
--out-file ../../transactions/mint-token-plutus.raw