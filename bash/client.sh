#!/bin/bash

debug="-d 0"
debugl="0"
rein=""
timeout=""

if [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./client.sh [-d debug_level] [-r] [-t seconds]"
    >&2 echo " -d Set the global debug level"
    >&2 echo " -r Use the reinforcement learning engine"
    >&2 echo " -t Set the backend's timeout value, in seconds"
    exit
fi

while getopts 'd:rt:' opt; do
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
    esac
done

echo "[]" | ./bash/stage3.sh "$debug" >./temp/alpha.txt # Alpha Stage
echo "[]" >./temp/excess.txt # Prepare an empty excess file

./bash/stage0.sh -p 9321 $debug $rein $timeout &
# TODO Pass debug level to Lisp (probably through the alpha file)
./bash/stage4.sh -C 9321 -f ./temp/alpha.txt
