#!/bin/bash

if [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./stage0.sh <port> <debug_level> [reinforcement]"
    exit
fi

if [ -z "$1" ] || [ -z "$2" ]; then
    >&2 echo "Usage: ./stage0.sh <port> <debug_level> [reinforcement]"
    exit 1
fi

rein="0"
if [ -n "$3" ]; then
    rein="$3"
fi

lua ./lua/manage.lua "$1" "$2" "$rein"
