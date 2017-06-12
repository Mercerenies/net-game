#!/bin/bash

if [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./stage3.sh [-D delta_file] [-0 old_alpha] [-E excess_file] [-d debug_level] [-s] [data_file]..."
    >&2 echo " -D Output delta information to the given file"
    >&2 echo " -0 Get old alpha information from the specified file"
    >&2 echo " -E Output extra, unused parse data to the given file"
    >&2 echo " -d Set the debug level to the value specified"
    >&2 echo " -s Enable small world mode; ignored if running in delta mode"
    >&2 echo " ( NOTE: If any of -D, -0, -E is supplied, all three should be supplied )"
    exit
fi

dfile=''
afile=''
efile=''
debug='0'
small="no"

while getopts 'D:0:E:d:s' opt; do
    case "$opt" in
        D)
            dfile="$OPTARG"
            ;;
        0)
            afile="$OPTARG"
            ;;
        E)
            efile="$OPTARG"
            ;;
        d)
            debug="$OPTARG"
            ;;
        s)
            small="yes"
            ;;
    esac
done

shift $((OPTIND - 1))

if [ -n "$dfile" ] && [ -n "$afile" ] && [ -n "$efile" ]; then
    ./ruby/deltarunner.rb "$debug" "$afile" "$dfile" "$efile" $*
else
    ./ruby/runner.rb "$debug" "$small" $*
fi
