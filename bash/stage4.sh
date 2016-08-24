#!/bin/bash

if [ "$1" == "--help" ]; then
    echo "Usage: ./stage4.sh [-l clisp] [-C port]"
    echo " -l Use the specified Common Lisp implementation"
    echo " -C Use the client system and connect to the specified port"
    exit
fi

port=""
cmd=clisp
file=run

while getopts 'l:C:' opt; do
    case "$opt" in
        l)
            cmd="$OPTARG"
            ;;
        C)
            port="$OPTARG"
            ;;
    esac
done

if [ -n "$port" ]; then
    port="-port $port"
    file=client
fi

$cmd ./lisp/$file.lisp
