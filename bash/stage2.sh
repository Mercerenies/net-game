#!/bin/bash

if [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./stage2.sh [debug_level]"
    exit
fi

if [ -n "$1" ]; then
    ./perl/parse.pl "$1"
else
    ./perl/parse.pl 0
fi
