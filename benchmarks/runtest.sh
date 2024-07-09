#!/bin/bash

if [[ -z "$1" ]]; then
    echo "Usage: ./runtest.sh test{n} n"
    exit 1
fi

testfile=$1

pstest=${testfile}ps.lpod
l2atest=${testfile}l2a.lp
ourtest=${testfile}.lpod
tempfile=${testfile}temp

# the lparse and psmodels executables must be placed in the same directory as this script
./lparse --inclusion-optimal $pstest > $tempfile 2> /dev/null
t0=$(date +%s%N)
./psmodels 0 $tempfile > /dev/null 2> /dev/null
t1=$(date +%s%N)
pstime=$(echo "scale=3; ($t1- $t0) / 1000000000" | bc)

# lpod2asprin files must be placed in the same directory as this script
python lpod2asprin.py -i $l2atest -type i > $tempfile 2>/dev/null
l2atime=$(grep "CPU Time" $tempfile | awk '{print $4}' | sed 's/s//')

swipl ../lpod.pl $ourtest > $testfile.lp
clingo $testfile.lp 0 > $tempfile 2>/dev/null
candidate=$(grep "Models" $tempfile | awk '{print $3}')
asprin $testfile.lp ../pref.lp 0 > $tempfile 2>/dev/null
ours=$(grep "CPU Time" $tempfile | awk '{print $4}' | sed 's/s//')
preferred=$(grep "Optimal" $tempfile | awk '{print $3}')

echo "$2,$candidate,$preferred,$pstime,$l2atime,$ours"
rm $tempfile $testfile.lp
