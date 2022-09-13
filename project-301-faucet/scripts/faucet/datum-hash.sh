#! /bin/bash

# --script-data-value accepts different parameters as input, one of them is an arbitrary number but you can also use JSON scripts for more complicated data (like datums)
DATA_VALUE=1618

cardano-cli transaction hash-script-data --script-data-value $DATA_VALUE