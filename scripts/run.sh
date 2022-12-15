#!/usr/bin/env bash

set -eu

for x in bin build; do
    if [ ! -d "$WD/$x" ]; then
        mkdir "$WD/$x"
    fi
done

flags=(
    "-fdiagnostics-color=always"
    -O2
    "-outputdir $WD/build"
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
ghc "${flags[@]}" -o "$WD/bin/main" "$1"
"$WD/bin/main" < "$2"
