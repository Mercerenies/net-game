#!/bin/bash

debug="-d 0"
debugl="0"
rein=""
timeout=""
small='-s'

if [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./client.sh [-d debug_level] [-r] [-t seconds] [-S]"
    >&2 echo " -d Set the global debug level"
    >&2 echo " -r Use the reinforcement learning engine"
    >&2 echo " -t Set the backend's timeout value, in seconds"
    >&2 echo " -S Disable 'small world' mode"
    exit
fi

while getopts 'd:rt:S' opt; do
    case "$opt" in
        d)
            debug="-d $OPTARG"
            debugl="$OPTARG"
            ;;
        r)
            rein="-r"
            ;;
        t)
            timeout="-t $OPTARG"
            ;;
        S)
            small=''
            ;;
    esac
done

echo "[]" | ./bash/stage3.sh $small $debug >./temp/alpha.txt # Alpha Stage
echo "[]" >./temp/excess.txt # Prepare an empty excess file

./bash/stage0.sh -p 9321 $debug $rein $timeout &
./bash/stage4.sh -C 9321 -f ./temp/alpha.txt
