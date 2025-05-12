#!/bin/bash

# This script is supposed to run your compiler
input="$1"
output="$2"

cabal run l1c -- "$input" temp.s
gcc temp.s -o "$output"

rm temp.s
