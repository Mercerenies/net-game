#!/bin/bash

if [ "$1" == "--help" ]; then
    echo "Usage: ./stage2.sh"
    exit
fi

./perl/parse.pl
