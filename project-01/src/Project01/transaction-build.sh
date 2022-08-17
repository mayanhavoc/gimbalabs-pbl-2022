
cardano-cli transaction build \
	    --babbage-era \
	    --testnet-magic $TESTNET \
	    --tx-in $TXIN \
	    --tx-out $MYADDRESS+20000000\
	    --change-address $MYADDRESS\
	    --out-file tx.draft
