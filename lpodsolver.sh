#!/bin/bash

if [[ -z "$1" ]]; then
    echo "Usage: ./lpodsolver.sh example.lpod"
    exit 1
fi

name=$(basename $1)

swipl ./lpod.pl $1 > $name.lp
asprin $name.lp ./pref.lp 0

rm $name.lp
