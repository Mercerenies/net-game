#!/bin/bash

if [ "$1" == "--help" ]; then
    echo "Usage: ./stage3.sh"
    exit
fi

./ruby/runner.rb
