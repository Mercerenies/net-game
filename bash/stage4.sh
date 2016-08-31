#!/bin/bash

if [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./stage4.sh [-l clisp] [-C port] [-f filename]"
    >&2 echo " -l Use the specified Common Lisp implementation"
    >&2 echo " -C Use the client system and connect to the specified port"
    >&2 echo " -f Use the specified world file"
    exit
fi

port=""
cmd=clisp
file=run
infile=""

while getopts 'l:C:f:' opt; do
    case "$opt" in
        l)
            cmd="$OPTARG"
            ;;
        C)
            port="$OPTARG"
            ;;
        f)
            infile="-file $OPTARG"
            ;;
    esac
done

if [ -n "$port" ]; then
    port="-port $port"
    file=client
fi

$cmd ./lisp/$file.lisp $infile
