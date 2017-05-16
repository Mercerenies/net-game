#!/bin/bash

if [ -z "$1" ] || [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./pretty.sh [-r] [file] [column]"
    >&2 echo "  -r Recognize the file format; do not write other output"
    >&2 echo "  column number is ignored in formats that are not column-based"
fi

# Recognize the format of the file
# Currently, JSON and two-column associative are recognized

recog=""
if [ "$1" == "-r" ]; then
    shift
    recog="yes"
fi

file="$1"
column="$2"
format='unknown'

if ! [ -e "$file" ]; then
    >&2 echo "error: $file does not exist or cannot be read"
    exit 1
fi

if python3 -m json.tool "$file" >/dev/null 2>/dev/null; then
    format='json'
elif [ $(sed -n '/\(\w\+ \)\+ *\( \w\+\)\+/ p' "$file" | wc -l) -eq $(wc -l <"$file") ]; then
    format='twocol'
fi

if [ -n "$recog" ]; then
    if [ "$format" == "unknown" ]; then
        >&2 echo "warning: $file format not recognized..."
        exit 1
    else
        echo "info: $file has format '$format'"
        exit 0
    fi
fi

case "$format" in
    json)
        python3 -m json.tool "$file"
        ;;
    twocol)
        if [ -n "$column" ]; then
            awk -F '  +' "{ print \$$column; }" "$file" | sort | uniq
        else
            cat "$file"
        fi
        ;;
    unknown)
        >&2 echo "warning: $file format not recognized..."
        cat "$file"
        exit 1
        ;;
    *)
        >&2 echo "error: Internal error at format ('$format'); please report this."
        exit 2
        ;;
esac
