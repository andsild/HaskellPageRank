#!/usr/bin/env bash

stack --nix runhaskell ./executables/CriterionToLatex.hs \
    && cat ./dist/parsedBenchmark.dat 

name="./dist/parsedBenchmark"
mkdir -p "./dist"
if [[ -e $name.dat ]] ; then
    i=0
    while [[ -e $name-$i.dat ]] ; do
        let i++
    done
    name=$name-$i
fi
cd -
cp -vi "./dist/parsedBenchmark.dat" "$name".dat
