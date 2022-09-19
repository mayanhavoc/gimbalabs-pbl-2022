#! /bin/bash

TXIN1=8a3eb2fddf0ab7d7f9de3501ca1e6df46fa31724d5d7ad2efab00ee0b8653031#24
TXIN2=36852535cb7f478ead6902ff1ae3bcc1f62b86a810f063fe61e07653eb31d290#1
POLICYID=748ee66265a1853c6f068f86622e36b0dda8edfa69c689a7dd232c60
ASSETNAME=5050424c53756d6d657232303232
MINTERADDRESS=addr_test1qqrzd8t2aljh5agrnms0cesre2e8cvtsg9jg5d3snjmf9mf3xn7jydpgqqwc9ek4hdkqm2xw56mkmzr02de0jjkuemqsr9mger
MINTERKEY=
ETERNL=
TESTNET=

cardano-cli transaction build \
--alonzo-era \
--testnet-magic $TESTNET \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $ETERNL+"2000000 + 1 $POLICYID.$ASSETNAME" \
--change-address $MINTERADDRESS \
--out-file tx.draft

cardano-cli transaction sign \
--tx-body-file tx.draft \
--signing-key-file $MINTERKEY \
--testnet-magic $TESTNET \
--out-file tx.signed 

cardano-cli transaction submit \
--testnet-magic $TESTNET \
--tx-file tx.signed 
