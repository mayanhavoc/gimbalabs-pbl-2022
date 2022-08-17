## MINTING Transaction:

```Bash
cardano-cli transaction build \
--babbage-era \
--testnet-magic $TESTNET \
--tx-in $TXIN \
--tx-out $MINTINGWALLET+"1500000 + 100 $POLICYID.$TOKENNAME" \
--mint "100 $POLICYID.$TOKENNAME" \
--mint-script-file $POLICYSCRIPT \
--change-address $MINTINGWALLET \
--protocol-params-file <PATH>/protocol.json \
--out-file mint-native-assets.raw
```