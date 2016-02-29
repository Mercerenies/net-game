#!/bin/bash

if [ "$1" == "--help" ]; then
    echo "Usage: ./stage4.sh [clisp]"
    echo " (where the optional argument clisp is the name of the Common Lisp interpreter)"
    exit
fi

if [ $# -gt 0 ]; then
    cmd=$1
else
    cmd=clisp
fi

$cmd ./lisp/run.lisp
