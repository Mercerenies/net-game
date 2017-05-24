#!/bin/bash

# TODO At some point, I would like to move this to a new file (legacy_stage1.sh, for example)
#      and redirect the Lua scripts to use that file. The new stage1.sh will only support the
#      expression syntax, so master.sh will no longer use the old argument form either. Eventually,
#      I would like to switch Lua completely over to the expression syntax, but that may take some
#      effort.
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
