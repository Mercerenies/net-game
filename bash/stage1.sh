#!/bin/bash

celebs=""
people=""
places=""
weapons=""
monsters=""
animals=""
foods=""
debug=""
rein=""

if [ $# -eq 0 ] || [ "$1" == "--help" ]; then
    echo "Usage: ./stage1.sh <args>"
    echo " -c Number of celebrities"
    echo " -p Number of people"
    echo " -P Number of places"
    echo " -w Number of weapons"
    echo " -m Number of monsters"
    echo " -a Number of animals"
    echo " -f Number of foods"
    echo " -d Debug mode"
    echo " -r Use the reinforcement learning engine (experimental)"
    exit
fi

while getopts 'c:p:P:w:m:a:f:dr' opt; do
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
        f) # Foods
            foods="-f $OPTARG"
            ;;
        d) # Debug Mode
            debug="-d"
            ;;
        r) # Reinforcement Engine
            rein="-r"
            ;;
    esac
done

./python/get.py $celebs $people $places $weapons $monsters $animals $foods $debug $rein
