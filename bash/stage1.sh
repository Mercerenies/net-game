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
upage=""
expr=""

if [ $# -eq 0 ] || [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./stage1.sh <args>"
    >&2 echo " -c Number of celebrities"
    >&2 echo " -p Number of people"
    >&2 echo " -P Number of places"
    >&2 echo " -w Number of weapons"
    >&2 echo " -m Number of monsters"
    >&2 echo " -a Number of animals"
    >&2 echo " -f Number of foods"
    >&2 echo " -d Debug level"
    >&2 echo " -r Use the reinforcement learning engine (experimental)"
    >&2 echo " -u Crawl only the specified page"
    >&2 echo " -e Perform the commands given in the expression"
    exit
fi

while getopts 'c:p:P:w:m:a:f:d:ru:e:' opt; do
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
            debug="-d $OPTARG"
            ;;
        r) # Reinforcement Engine
            rein="-r"
            ;;
        u) # Unit Page
            upage="-u $OPTARG"
            ;;
        e) # Expression
            expr="$OPTARG"
            ;;
    esac
done

./python/get.py $celebs $people $places $weapons $monsters $animals $foods $debug $rein $upage -e "$expr"
