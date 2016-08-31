#!/bin/bash

if [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./stage3.sh [-D delta_file] [-0 old_alpha]"
    >&2 echo " -D Output delta information to the given file"
    >&2 echo " -0 Get old alpha information from the specified file (required if -D is given)"
    exit
fi

dfile=''
afile=''

while getopts 'D:0:' opt; do
    case "$opt" in
        D)
            dfile="$OPTARG"
            ;;
        0)
            afile="$OPTARG"
            ;;
    esac
done

if [ -n "$dfile" ] && [ -n "$afile" ]; then
    ./ruby/deltarunner.rb "$afile" "$dfile"
else
    ./ruby/runner.rb
fi
