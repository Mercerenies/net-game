#!/bin/bash

if [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./stage2.sh"
    exit
fi

./perl/parse.pl
