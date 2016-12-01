#!/bin/bash

celebs=""
people=""
places=""
weapons=""
monsters=""
animals=""
foods=""
debug=""
debugl=""
rein=""
stage1=""
stage2=""
stage3=""
stage4=""
clisp="clisp"

prefix="net"

if [ $# -eq 0 ]; then
    >&2 echo "Usage: ./master.sh <args>"
    >&2 echo "For detailed help with instructions, try \`./master.sh --help\`"
    exit
fi

if [ $1 == "--help" ]; then
    >&2 echo "Usage: ./master.sh <args>"
    >&2 echo " -c Number of celebrities"
    >&2 echo " -p Number of people"
    >&2 echo " -P Number of places"
    >&2 echo " -w Number of weapons"
    >&2 echo " -m Number of monsters"
    >&2 echo " -a Number of animals"
    >&2 echo " -f Number of foods"
    >&2 echo " -d Debug level"
    >&2 echo " -r Use the reinforcement learning engine (experimental)"
    >&2 echo " -1 Run Stage 1 (Python / Site Crawling)"
    >&2 echo " -2 Run Stage 2 (Perl / Page Parsing)"
    >&2 echo " -3 Run Stage 3 (Ruby / World Generation)"
    >&2 echo " -4 Run Stage 4 (Common Lisp / Gameplay)"
    >&2 echo " -l Use the given Common Lisp implementation"
    exit
fi

while getopts 'c:p:P:w:m:a:f:d:r1234l:' opt; do
    case "$opt" in
        c) # Celebrities
            celebs="-c $OPTARG"
            ;;
        p) # People
            people="-p $OPTARG"
            ;;
        P) # Places
            places="-P $OPTARG"
            ;;
        w) # Weapons
            weapons="-w $OPTARG"
            ;;
        m) # Monsters
            monsters="-m $OPTARG"
            ;;
        a) # Animals
            animals="-a $OPTARG"
            ;;
        f) # Foods
            foods="-f $OPTARG"
            ;;
        d) # Debug Mode
            debug="-d $OPTARG"
            debugl="$OPTARG"
            ;;
        r) # Reinforcement Engine
            rein="-r"
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
    esac
done

if [ -n "$stage1" ]; then
    stage1="$stage1 $celebs $people $places $weapons $monsters $animals $foods $debug $rein"
fi

if [ -n "$stage2" ]; then
    stage2="$stage2 $debugl"
fi

if [ -n "$stage4" ]; then
    stage4="$stage4 $clisp"
fi

if [ -n "$stage1" ]; then
    $stage1 >"./temp/${prefix}1.txt"
fi
if [ -n "$stage2" ]; then
    $stage2 <"./temp/${prefix}1.txt" >"./temp/${prefix}2.txt"
fi
if [ -n "$stage3" ]; then
    $stage3 "$debug" <"./temp/${prefix}2.txt" >"./temp/system.txt"
fi
if [ -n "$stage4" ]; then
    $stage4
fi
