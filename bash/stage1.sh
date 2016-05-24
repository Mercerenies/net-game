#!/bin/bash

celebs=""
people=""
places=""
weapons=""
monsters=""
animals=""
debug=""

if [ $# -eq 0 ] || [ "$1" == "--help" ]; then
    echo "Usage: ./stage1.sh <args>"
    echo " -c Number of celebrities"
    echo " -p Number of people"
    echo " -P Number of places"
    echo " -w Number of weapons"
    echo " -m Number of monsters"
    echo " -a Number of animals"
    echo " -d Debug mode"
    exit
fi

while getopts 'c:p:P:w:m:a:d' opt; do
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
        m) # Monsters
            monsters="-m $OPTARG"
            ;;
        a) # Animals
            animals="-a $OPTARG"
            ;;
        d) # Debug Mode
            debug="-d"
            ;;
    esac
done

./python/get.py $celebs $people $places $weapons $monsters $animals $debug