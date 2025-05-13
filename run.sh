#!/bin/bash

# This script is supposed to run your compiler
input="$1"
input_dir=$(dirname "$input")
output="$2"
temp_file="$input_dir/temp.s"

cabal run l1c -- "$input" "$temp_file"
status=$?
if [ $status -ne 0 ]; then
  rm -f "$temp_file"
  exit $status
fi

gcc "$temp_file" -o "$output"
rm -f "$temp_file"