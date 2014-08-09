#!/bin/sh

if test $# -lt 1 ; then
    echo "USAGE: $(basename $0) [infile(s)]"
    exit
fi

infile=$*

if test $# -ge 2 ; then
    outfile=view.eps
    gs -q -dNOPAUSE -dBATCH -sDEVICE=epswrite -sOutputFile=${outfile} -c save pop -f ${infile}
    evince-previewer ${outfile}
#    evince ${outfile}
else
    evince-previewer ${infile}
#    evince ${infile}
fi
