#!/usr/bin/env bash

# set -e makes the script exit if any command fails
# set -u makes the script exit if any unset variable is used
# set -o pipefail makes the script exit if any command in a pipeline fails
set -euo pipefail

for i in 1 5 10 20 40 80
do
    echo "---"
    echo "> message, $i (*2) followers"
    erl +S 4 -noshell -s benchmark_final followers $i -s init stop
    echo "---"
done