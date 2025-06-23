#!/bin/bash

# This script is supposed to run your compiler
input="$1"
output="$2"

cabal run l1c -- "$input" "$output"