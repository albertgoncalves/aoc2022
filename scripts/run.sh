#!/usr/bin/env bash

set -eu

if [ ! -d "$WD/bin" ]; then
    mkdir "$WD/bin"
fi

flags=(
    "-fdiagnostics-color=always"
    -no-keep-hi-files
    -no-keep-o-files
    -O2
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
ormolu -i --no-cabal "$1"
ghc "${flags[@]}" -o "$WD/bin/main" "$1"
"$WD/bin/main" < "$2"
