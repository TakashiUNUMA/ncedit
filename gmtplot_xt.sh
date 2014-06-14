#!/bin/sh
#
# GMTPLOT
# original script coded by Takashi Unuma, Kyoto Univ.
# Last modified: 2014/06/14
#

if test $# -lt 1 ; then
    echo "USAGE: $(basename $0) [infile]"
    exit
fi
#infile=$1

for infile in $* ; do

echo ${infile}

prefix=$(echo ${infile%.nc4})

auto=0
if test ${prefix} = "rain" ; then
    title="RAIN"
    unit="[mm]"
    VARMIN=0.0
    VARMAX=150.0
    VARINT=25.0
    VARTYPE="seis"
    BINT=a50f25
elif test ${prefix} = "cpc" ; then
    title="COLD POOL INTECSITY"
    unit="[m/s]"
    VARMIN=2.5
    VARMAX=20.0
    VARINT=2.5
    VARTYPE="seis"
    BINT=a5f2.5
elif test ${prefix} = "cref" ; then
    title="COMPOSIT REFLECTIVITY"
    unit="[dBZ]"
    VARMIN=10.0
    VARMAX=60.0
    VARINT=5.0
    VARTYPE="seis"
    BINT=a10f5
else
    echo "automatically selected"
    title="${prefix}"
    auto=1
fi

# x and y label
xlabel="Distance [km]"
ylabel="Time [hour]"

# other parameters for drawing GMT
XINT=a20f10
YINT=a1f0.5
RANGE=-100/100/0/6
PRJ=x0.05/0.5

#RANGE=-50/50/0/6
#PRJ=x0.1/0.5


#------------------------------------------
# GMT setting
gmtdefaults -D > .gmtdefaults4
gmtset HEADER_FONT_SIZE             12p
gmtset HEADER_OFFSET              -0.1c
gmtset LABEL_FONT_SIZE              10p
gmtset ANOT_FONT_SIZE                8p
gmtset ANNOT_OFFSET_PRIMARY       0.10c
gmtset PLOT_DEGREE_FORMAT    ddd:mm:ssF
gmtset BASEMAP_TYPE               plain
gmtset TICK_LENGTH               -0.15c
gmtset FRAME_PEN                  0.50p
gmtset GRID_PEN                   0.20p
gmtset TICK_PEN                   0.50p
gmtset MEASURE_UNIT                  cm
#gmtset PAPER_MEDIA                   a4
gmtset PAPER_MEDIA                  a4+
#gmtset PAPER_MEDIA              letter+
#gmtset PAPER_MEDIA              ledger+
gmtset VECTOR_SHAPE                   2


# Useful option
gmtsta="-P -K"
gmtcon="-P -K -O"
gmtend="-P -O"

# Specify output file
psfile=${infile%.nc4}.eps

# psxy
CPALET=cpalet.cpt
if test ${auto} -eq 1 ; then
    grd2cpt ${infile} > ${CPALET}
else
    unucpt ${infile%.nc4} ${VARMIN} ${VARMAX} ${VARINT} ${VARTYPE}
fi
grdimage ${infile} -J${PRJ} -R${RANGE} -C${CPALET} -X3.0 -Y3.0 ${gmtsta} > ${psfile}
#grdimage ${infile} -J${PRJ} -R${RANGE} -C${CPALET} -X1.5 -Y1.5 ${gmtsta} > ${psfile}

# psscale
gmtset ANOT_FONT_SIZE 8p
gmtset TICK_LENGTH -0.1c
psscale -D1.35/-0.6/2.75/0.2h -B${BINT}/:"${unit}": -C${CPALET} -N ${gmtcon} >> ${psfile}

# psbasemap
gmtset TICK_LENGTH -0.15c
#psbasemap -J -R -B${XINT}:"${xlabel}":/${YINT}:"${ylabel}":SWne:."${title}": ${gmtcon} >> ${psfile}
psbasemap -J -R -B${XINT}:"${xlabel}":/${YINT}:"${ylabel}":SWne ${gmtcon} >> ${psfile}

# labels (pstext)
# x     y   size angle font place comment
cat << EOF | pstext -R1/10/1/10 -Jx1.0 -N ${gmtend} >> ${psfile}
   1.00   4.25  10   0.0    0    ML ${title}
  -0.25  -0.25   1   0.0    0    ML .
  -0.25   4.75   1   0.0    0    ML .
  12.0   -0.25   1   0.0    0    ML .
  12.0    4.75   1   0.0    0    ML .
EOF

# convert from ps to png
ps2raster -Tg -A ${psfile}

# convert from ps to pdf
ps2raster -Tf -A ${psfile}

rm -f .gmt*
rm -f cpalet.cpt

done

#echo "ps2png..."
#time ls *.eps | parallel -j +0 ps2raster -Tg -A {}
#time ls *.eps | parallel -j 6 ps2raster -Tg -A {}
#echo "ps2pdf..."
#time ls *.eps | parallel -j +0 ps2raster -Tf -A {}
#time ls *.eps | parallel -j 6 ps2raster -Tf -A {}

echo "done."

#rm -f *.eps
