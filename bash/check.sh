#!/bin/bash

function onpath {
    which $1 >/dev/null 2>/dev/null
}

echo
echo 'Checking whether your system is set up correctly to handle the game...'

# Python Stage
echo
echo 'Checking Python...'
echo -n 'Does Python exist?'
if [ ! -x '/usr/bin/python3' ]; then
    echo
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
    echo
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
    echo
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
    echo
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
    echo
    echo 'error: Cannot find Ruby.'
    if onpath ruby; then
        echo '  * Ruby is on your system but not in /usr/bin/ruby.'
        echo '  * Please place a link or copy of Ruby in /usr/bin/.'
    fi
    exit 1
fi
echo ' Yes'
echo -n 'Are the necessary gems installed? '
if ! ruby -e 'require "sxp"' >/dev/null 2>/dev/null; then
    echo
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
if [ -n "$1" ]; then
    clisp=$1
    setclisp=1
fi
echo "Common Lisp implementation is \`$clisp\`."
echo -n 'Does Common Lisp exist?'
if ! onpath $clisp; then
    echo
    echo 'error: Cannot find Common Lisp.'
    if [ $setclisp -eq 0 ]; then
        echo '  * Perhaps you forgot to supply the name of your'
        echo '  * implementation to this script.'
    fi
    exit 1
fi
echo ' Yes'
echo 'Common Lisp is ready.'

echo
echo 'Passed all checks. Enjoy your gameplay.'
