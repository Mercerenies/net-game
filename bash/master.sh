#!/bin/bash

debug=""
debugl=""
expr=""
stage1=""
stage2=""
stage3=""
stage4=""
clisp="clisp"
small=""

prefix="net"

if [ $# -eq 0 ]; then
    >&2 echo "Usage: ./master.sh <args>"
    >&2 echo "For detailed help with instructions, try \`./master.sh --help\`"
    exit
fi

if [ $1 == "--help" ]; then
    >&2 echo "Usage: ./master.sh <args>"
    >&2 echo " -d Debug level"
    >&2 echo " -e Crawl using the given command(s)"
    >&2 echo " -1 Run Stage 1 (Python / Site Crawling)"
    >&2 echo " -2 Run Stage 2 (Perl / Page Parsing)"
    >&2 echo " -3 Run Stage 3 (Ruby / World Generation)"
    >&2 echo " -4 Run Stage 4 (Common Lisp / Gameplay)"
    >&2 echo " -l Use the given Common Lisp implementation"
    >&2 echo " -s Enable small world mode"
    >&2 echo " ** For detailed information about any of these flags, consult the"
    >&2 echo "    documentation for ./stageN.sh, where N is the relevant stage."
    exit
fi

while getopts 'd:1234l:e:s' opt; do
    case "$opt" in
        d) # Debug Mode
            debug="-d $OPTARG"
            debugl="$OPTARG"
            ;;
        1) # Stage 1
            stage1="./bash/stage1.sh"
            ;;
        2) # Stage 2
            stage2="./bash/stage2.sh"
            ;;
        3) # Stage 3
            stage3="./bash/stage3.sh"
            ;;
        4) # Stage 4
            stage4="./bash/stage4.sh"
            ;;
        l) # CLisp
            clisp="$OPTARG"
            ;;
        e) # Expression
            expr="$OPTARG"
            ;;
        s) # Small World
            small='-s'
            ;;
    esac
done

if [ -n "$stage1" ]; then
    stage1="$stage1 $debug $rein"
fi

if [ -n "$stage2" ]; then
    stage2="$stage2 $debugl"
fi

if [ -n "$stage4" ]; then
    stage4="$stage4 $clisp"
fi

if [ -n "$stage1" ]; then
    $stage1 -e "$expr" >"./temp/${prefix}1.txt"
fi
if [ -n "$stage2" ]; then
    $stage2 <"./temp/${prefix}1.txt" >"./temp/${prefix}2.txt"
fi
if [ -n "$stage3" ]; then
    $stage3 "$small" "$debug" <"./temp/${prefix}2.txt" >"./temp/system.txt"
fi
if [ -n "$stage4" ]; then
    $stage4
fi
