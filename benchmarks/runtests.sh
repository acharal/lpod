#!/bin/bash

echo "test,candidate,preferred,psmodels,lpod2asprin,ours"
for i in {01..17}; do
    testfile="./tests/test${i}/test${i}"
    ./runtest.sh $testfile $i
done
