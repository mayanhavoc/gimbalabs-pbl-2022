#! /bin/bash

SENDER=addr_test1qq3r5yg253j0h4vnurs6ddd2rvgzhqljhn9mqmegarj3rs9reqs2pmc4q9mzzdln6hx3xyyfarkvtd7hnh65fuvms0cs07wg0m
SENDER_KEY="$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/tWallets/minterWallet/payment.skey"

DATUMHASH=bf9bb0aa8595378e2c7a5ef9e89a8b908444861f7de56d5cb88191823b384129
COLLATERAL=17a8d8b04072be3153427fbfcaaa963ae62f884a66c50795f29b4a9c3eea3c84#0
PLUTUS_SCRIPT_FILE="$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/output/ppbl-faucet-micochango.plutus"

ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7467696d62616c"
AUTH_TOKEN="748ee66265a1853c6f068f86622e36b0dda8edfa69c689a7dd232c60.5050424c53756d6d657232303232"
AUTH_TOKEN_TXIN=eda7e643854ce0a0ab08facea48718569b9c6570d9363498aca15b28b73f72d5#0

CONTRACT_TXIN=31c76e8fede6ec13e829c562eb876bfa471433f0ce2725f1a2cd4a8224c51dd4#3
CONTRACTADDR=addr_test1wpenjjl2ea22r0vlcm9m3hy9heafwpt3grmty0qfx4r0nrglkg0pk

FEE_TXIN=b837d47eb199cf824d34a984ed0deaed09969581975ce863e52c9fd8d86aa653#1
REDEEMER_FILE="$HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/redeemer.json"
TOKENS_BACK_TO_CONTRACT=266990 

cardano-cli transaction build \
--alonzo-era \
--testnet-magic 1 \
--tx-in $AUTH_TOKEN_TXIN \
--tx-in $FEE_TXIN \
--tx-in $CONTRACT_TXIN \
--tx-in-script-file $PLUTUS_SCRIPT_FILE \
--tx-in-datum-value 1704 \
--tx-in-redeemer-file $REDEEMER_FILE \
--tx-in-collateral $COLLATERAL \
--tx-out $SENDER+"2000000 + 10 $ASSET" \
--tx-out $SENDER+"2000000 + 1 $AUTH_TOKEN" \
--tx-out $CONTRACTADDR+"2000000 + $TOKENS_BACK_TO_CONTRACT $ASSET" \
--tx-out-datum-hash $DATUMHASH \
--change-address $SENDER \
--protocol-params-file $HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/protocol.json \
--out-file $HOME/Documents/plutus-gimbalabs-ppbl-course-02/project-301-faucet/output/unlock.raw

