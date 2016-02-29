#!/bin/bash

celebs=""
people=""
places=""
weapons=""
debug=""

if [ $# -eq 0 ] || [ "$1" == "--help" ]; then
    echo "Usage: ./stage1.sh <args>"
    echo " -c Number of celebrities"
    echo " -p Number of people"
    echo " -P Number of places"
    echo " -w Number of weapons"
    echo " -d Debug mode"
    exit
fi

while getopts 'c:p:P:w:d' opt; do
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
    esac
done

./python/get.py $celebs $people $places $weapons $debug
