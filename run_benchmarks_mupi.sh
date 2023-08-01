#!/usr/bin/env bash

# set -e makes the script exit if any command fails
# set -u makes the script exit if any unset variable is used
# set -o pipefail makes the script exit if any command in a pipeline fails
set -euo pipefail

for i in 10000
do
    echo "---"
    echo "> message, $i mupi"
    erl +S 4 -noshell -s benchmark mupi $i -s init stop
    echo "---"
done
