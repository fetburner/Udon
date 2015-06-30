#!/bin/bash
# execute sml-file by udon2js + node.js and smlnj

sh ./udon.sh $1
node $1.js
sml $1
