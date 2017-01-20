#!/bin/bash

if [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./stage0.sh <-p port> [args]"
    >&2 echo " -p (Required) The port number to connect to"
    >&2 echo " -r Use the reinforcement learning engine"
    >&2 echo " -t Use the specified timeout to prevent long pauses, in seconds"
    >&2 echo " -d Set the debug level"
    exit
fi

port=""
rein="0"
timeout="no"
debug="0"

while getopts 'p:rt:d:' opt; do
    case "$opt" in
        p)
            port="$OPTARG"
            ;;
        r)
            rein="1"
            ;;
        t)
            timeout="$OPTARG"
            ;;
        d)
            debug="$OPTARG"
            ;;
    esac
done

if [ -z "$port" ]; then
    >&2 echo "Usage: ./stage0.sh <-p port> [args]"
    >&2 echo "For detailed help with instructions, try \`./stage0.sh --help\`"
    exit 1
fi

lua ./lua/manage.lua "$port" "$debug" "$rein" "$timeout"
