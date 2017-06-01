#!/bin/bash

debug=""
expr=""

if [ $# -eq 0 ] || [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./stage1.sh <args>"
    >&2 echo " -d Debug level"
    >&2 echo " -e Perform the commands given in the expression"
    exit
fi

while getopts 'd:e:' opt; do
    case "$opt" in
        d) # Debug Mode
            debug="-d $OPTARG"
            ;;
        e) # Expression
            expr="$OPTARG"
            ;;
    esac
done

./python/get.py $debug -e "$expr"
