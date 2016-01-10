#!/bin/bash

celebs=""
people=""
places=""
weapons=""
debug=""
intermediate="data.txt"
output="world.txt"
nopy=""

if [ $# -eq 0 ]; then
    echo "Usage: ./run.sh <args>"
    echo " -c Number of celebrities"
    echo " -p Number of people"
    echo " -P Number of places"
    echo " -w Number of weapons"
    echo " -d Debug mode"
    echo " -i Specify intermediate filename"
    echo " -I Do not use intermediate file (incompatible with '-i')"
    echo " -o Specify output filename"
    echo " -n Do not get; only parse (incompatible with '-I')"
    exit
fi

while getopts 'c:p:P:w:di:Io:n' opt; do
    case "$opt" in
        c) # Celebrities
            celebs="-c $OPTARG"
            ;;
        p) # People
            people="-p $OPTARG"
            ;;
        P) # Places
            places="-P $OPTARG"
            ;;
        w) # Weapons
            weapons="-w $OPTARG"
            ;;
        d) # Debug Mode
            debug="-d"
            ;;
        i) # Intermediate filename
            intermediate="$OPTARG"
            ;;
        I) # Don't use intermediate file
            intermediate=""
            ;;
        o) # Output filename
            output="$OPTARG"
            ;;
        n) # Don't use the Internet; just parse the intermediate file
           # (Has no effect if -I is specified)
            nopy="true"
            ;;
    esac
done

pycommand="./py/get.py $celebs $people $places $weapons $debug"
plcommand="./perl/parse.pl"

if [ -n "$intermediate" ]; then
    if [ -z "$nopy" ]; then
        $pycommand >"./data/$intermediate"
    fi
    $plcommand <"./data/$intermediate" >"./data/$output"
else
    $pycommand | $plcommand >"./data/$output"
fi
