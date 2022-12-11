#!/usr/bin/env bash

set -eu

flags=(
    -Wall
    -Wcompat
    -Werror
    -Widentities
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wmissing-export-lists
    -Wmonomorphism-restriction
    -Wpartial-fields
    -Wunused-packages
    -Wunused-type-patterns
)

hlint "$1"
ormolu -i "$1"
runghc "${flags[@]}" "$1" < "$2"
