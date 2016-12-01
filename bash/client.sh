#!/bin/bash

debug=""
debugl=""

if [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./client.sh [-d debug_level]"
    exit
fi

while getopts 'd:' opt; do
    case "$opt" in
        d)
            debug="-d $OPTARG"
            debugl="$OPTARG"
            ;;
    esac
done

echo "[]" | ./bash/stage3.sh "$debug" >./temp/alpha.txt # Alpha Stage
echo "[]" >./temp/excess.txt # Prepare an empty excess file

./bash/stage0.sh 9321 "$debugl" &
./bash/stage4.sh -C 9321 -f ./temp/alpha.txt # TODO Pass debug level to Lisp (probably through the alpha file)
