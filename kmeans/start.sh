#!/bin/bash
# settings structure (the same as running cabal run and inserting parameters manualy)
# param1 filePath (eg. ./data/data.csv)
# param2 features list (eg. 0,3 -- this selects 1st and 4th feature)
# param3 number of clusters (eg. 5)

if [[ -z $1 ]];
then
    echo "Empty param ... accessing default"
    cat ./settings/s0.txt | cabal run
else
    echo "Accessing: $1"
    cat $1 | cabal run
fi