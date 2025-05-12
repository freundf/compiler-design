#!/bin/bash

# This script is supposed to run your compiler
input="$1"
output="$2"

cabal run l1c -- "$input" temp.s
status=$?
if [ $status -ne 0 ]; then
  rm -f temp.s
  exit $status
fi

gcc temp.s -o "$output"
rm -f temp.s