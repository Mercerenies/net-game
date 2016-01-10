#!/bin/bash

cmd=clisp
if [ $# -gt 0 ]; then
    cmd=$1
fi

$cmd ./lisp/run.lisp
