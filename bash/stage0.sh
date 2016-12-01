#!/bin/bash

if [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./stage0.sh <port> <debug_level>"
    exit
fi

if [ -z "$1" ] || [ -z "$2" ]; then
    >&2 echo "Usage: ./stage0.sh <port> <debug_level>"
    exit 1
fi

# TODO The debug level argument is not used right now

lua ./lua/manage.lua "$1"
