#!/bin/bash

if [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./stage0.sh <port>"
    exit
fi

if [ -z "$1" ]; then
    >&2 echo "Usage: ./stage0.sh <port>"
    exit 1
fi

lua ./lua/manage.lua "$1"
