#!/bin/sh

if test $# -lt 1 ; then
    echo "USAGE: $(basename $0) [infile]"
    exit
fi
infile=$1

evince ${infile} 2> /dev/null
