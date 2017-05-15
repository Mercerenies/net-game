#!/bin/bash

function onpath {
    which $1 >/dev/null 2>/dev/null
}

check_flock=0
check_lua=0
lisp=''

if [ "$1" == "--help" ]; then
    >&2 echo "Usage: ./check.sh [-rein] [-client] [lisp]"
    >&2 echo " -rein Include the reinforcement learning engine in the checks"
    >&2 echo " -client Include the self-updating client in the checks"
    >&2 echo " (NOTE: Providing a custom Lisp implementation may result in some minor modifications being necessary)"
    exit
fi

case "$1" in
    -*)
        ;;
    *)
        lisp="$1"
        shift
        ;;
esac

for var in "$@"; do
    case "$var" in
        -rein)
            check_flock=1
            ;;
        -client)
            check_lua=1
            ;;
        *)
            echo "error: Invalid option '$var'."
            exit 1
            ;;
    esac
done

echo
echo 'Checking whether your system is set up correctly to handle the game...'

# Python Stage
echo
echo 'Checking Python...'
echo -n 'Does Python exist?'
if [ ! -x '/usr/bin/python3' ]; then
    echo ' No'
    echo 'error: Cannot find Python.'
    if onpath python3; then
        echo '  * Python is on your system but not in /usr/bin/python3.'
        echo '  * Please place a link or copy of Python at /usr/bin/python3.'
    fi
    exit 1
fi
echo ' Yes'
echo -n 'Is the version correct?'
pyversion=`/usr/bin/python3 --version 2>&1 | sed 's/^[^0-9]*\([0-9]\+\).*/\1/'`
if [ "$pyversion" -lt 3 ]; then
    echo ' No'
    echo "error: Python version is $pyversion, need at least Python 3"
    exit 1
fi
echo ' Yes'
echo -n 'Does the Wikipedia module exist?'
if ! python3 -c 'import wikipedia' >/dev/null 2>/dev/null; then
    echo
    echo 'error: Please install the Wikipedia module using `pip install wikipedia`.'
    exit 1
fi
echo ' Yes'
echo 'Python is ready.'

# Perl Stage
echo
echo 'Checking Perl...'
echo -n 'Does Perl exist?'
if [ ! -x '/usr/bin/perl' ]; then
    echo ' No'
    echo 'error: Cannot find Perl.'
    if onpath perl; then
        echo '  * Perl is on your system but not in /usr/bin/perl.'
        echo '  * Please place a link or copy of Perl in /usr/bin/.'
    fi
    exit 1
fi
echo ' Yes'
echo -n 'Do the necessary modules exist? '
if ! perl -e 'use XML::Simple; use JSON::PP;' >/dev/null 2>/dev/null; then
    echo ' No'
    echo 'error: Please verify that the following modules are accessible to Perl.'
    echo '  * XML::Simple'
    echo '  * JSON::PP'
    exit 1
fi
echo ' Yes'
echo 'Perl is ready.'

# Ruby Stage
echo
echo 'Checking Ruby...'
echo -n 'Does Ruby exist?'
if [ ! -x '/usr/bin/ruby' ]; then
    echo ' No'
    echo 'error: Cannot find Ruby.'
    if onpath ruby; then
        echo '  * Ruby is on your system but not in /usr/bin/ruby.'
        echo '  * Please place a link or copy of Ruby in /usr/bin/.'
    fi
    exit 1
fi
echo ' Yes'
echo -n 'Are the necessary gems installed?'
if ! ruby -e 'require "sxp"' >/dev/null 2>/dev/null; then
    echo ' No'
    echo 'error: Please install the SXP gem using `gem install sxp`.'
    if uname -s | grep 'CYGWIN' >/dev/null 2>/dev/null; then
        echo '  * Some Windows users find it useful to run the gem commands'
        echo '  * from the Windows command line rather than Cygwin.'
    fi
    exit 1
fi
echo ' Yes'
echo 'Ruby is ready.'

# Common Lisp Stage
echo
echo 'Checking Common Lisp...'
clisp='clisp'
setclisp=0
if [ -n "$lisp" ]; then
    clisp=$lisp
    setclisp=1
fi
echo "Common Lisp implementation is \`$clisp\`."
echo -n 'Does Common Lisp exist?'
if ! onpath $clisp; then
    echo ' No'
    echo 'error: Cannot find Common Lisp.'
    if [ $setclisp -eq 0 ]; then
        echo '  * Perhaps you forgot to supply the name of your'
        echo '  * implementation to this script.'
    fi
    exit 1
else
    echo ' Yes'
    if [ $clisp != 'clisp' ]; then
        echo 'warning: Using non-default Common Lisp implementation.'
        echo '  * The game assumes GNU CLISP as the Common Lisp implementation.'
        echo '  * Using another version may require some modification to ./lisp/os.lisp.'
    fi
fi
echo 'Common Lisp is ready.'

# Lua Stage
if [ "$check_lua" != 0 ]; then
    echo
    echo 'Checking Lua...'
    echo -n 'Does Lua exist?'
    if ! onpath lua; then
        echo ' No'
        echo 'error: Cannot find Lua.'
        echo '  * Lua is required for the client self-modifying'
        echo '  * gameplay but unnecessary for the legacy system.'
        exit 1
    fi
    echo ' Yes'
    echo -n 'Does the luasocket module exist?'
    if ! lua -e 'require "socket"' >/dev/null 2>/dev/null; then
        echo ' No'
        echo 'error: Please install the luasocket module.'
        exit 1
    fi
    echo ' Yes'
fi

# Misc Stage
echo
echo 'Checking miscellaneous properties...'
echo -n 'Does the temp directory exist?'
if [ ! -d './temp' ]; then
    echo ' No'
    if [ -e './temp' ]; then
        echo 'error: ./temp is not a directory.'
        echo '  * Please delete or rename ./temp file and'
        echo '  * run the script again.'
        exit 1
    else
        echo -n 'Creating temp directory...'
        if mkdir './temp'; then
            echo ' Done'
        else
            echo ' Error'
            echo 'error: Could not create directory.'
            echo '  * Please make sure you have appropriate permissions'
            echo '  * in this directory.'
            exit 1
        fi
    fi
else
    echo ' Yes'
fi
if [ "$check_flock" != 0 ]; then
    echo -n 'Is flock supported?'
    type flock >/dev/null 2>/dev/null
    if [ "$?" != 0 ]; then
        echo ' No'
        echo 'error: flock is not supported.'
        echo '  * flock is required for the reinforcement learning'
        echo '  * engine, which is optional and can be disabled.'
        exit 1
    else
        echo ' Yes'
    fi
fi

echo
echo 'Passed all checks. Enjoy your gameplay.'
