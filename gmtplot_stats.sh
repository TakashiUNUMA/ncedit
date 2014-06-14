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


for infile in $* ; do

echo ${infile}

prefix=$(echo ${infile%.txt})

# x label
xlabel="Time [hour]"
XINT=a1f0.5

# other parameters for drawing GMT
TMIN=0
TMAX=6

if test  ${prefix} = "qcbot" ; then
    title="CLOUD BOTTOM HEIGHT"
    ylabel="${prefix} [km]"
    VARMIN=0.0
    VARMAX=3.0
    RANGE=${TMIN}/${TMAX}/${VARMIN}/${VARMAX}
    YINT=a1f0.5
    PRJ=x1.0/1.0

elif test ${prefix} = "qctop" ; then
    title="CLOUD TOP HEIGHT"
    ylabel="${prefix} [km]"
    VARMIN=0.0
    VARMAX=6.0
    RANGE=${TMIN}/${TMAX}/${VARMIN}/${VARMAX}
    YINT=a1f0.5
    PRJ=x1.0/0.5

elif test  ${prefix} = "wmin" ; then
    title="MINIMUM VERTICAL VELOCITY"
    ylabel="${prefix} [m s@+-1@+]"
    VARMIN=-6.0
    VARMAX=0.0
    RANGE=${TMIN}/${TMAX}/${VARMIN}/${VARMAX}
    YINT=a1f0.5
    PRJ=x1.0/0.5

elif test  ${prefix} = "wmax" ; then
    title="MAXIMUM VERTICAL VELOCITY"
    ylabel="${prefix} [m s@+-1@+]"
    VARMIN=0.0
    VARMAX=6.0
    RANGE=${TMIN}/${TMAX}/${VARMIN}/${VARMAX}
    YINT=a1f0.5
    PRJ=x1.0/0.5

elif test  ${prefix} = "rhmin" ; then
    title="MINIMUM RELATIVE HUMIDITY"
    ylabel="${prefix} [%]"
    VARMIN=0.0
    VARMAX=30.0
    RANGE=${TMIN}/${TMAX}/${VARMIN}/${VARMAX}
    YINT=a10f5
    PRJ=x1.0/0.1

elif test  ${prefix} = "rhmax" ; then
    title="MAXMUM RELATIVE HUMIDITY"
    ylabel="${prefix} [%]"
    VARMIN=80.0
    VARMAX=110.0
    RANGE=${TMIN}/${TMAX}/${VARMIN}/${VARMAX}
    YINT=a10f5
    PRJ=x1.0/0.1

elif test  ${prefix} = "tmois" ; then
    title="TOTAL MOISTURE"
    ylabel="${prefix} * 10@+9@+ [kg]"
    VARMIN=42.0
    VARMAX=44.4
    RANGE=${TMIN}/${TMAX}/${VARMIN}/${VARMAX}
    YINT=a0.2f0.1
    PRJ=x1.0/2.15

elif test  ${prefix} = "tmass" ; then
    title="TOTAL DRY AIR MASS"
    ylabel="${prefix} * 10@+11@+ [kg]"
    VARMIN=73.58
    VARMAX=73.65
    RANGE=${TMIN}/${TMAX}/${VARMIN}/${VARMAX}
    YINT=a0.02f0.01
    PRJ=x1.0/42.5

elif test  ${prefix} = "et" ; then
    title="TOTAL ENERGY"
    ylabel="${prefix} * 10@+16@+ [kg]"
    VARMIN=189.16
    VARMAX=189.37
    RANGE=${TMIN}/${TMAX}/${VARMIN}/${VARMAX}
    YINT=a0.02f0.01
    PRJ=x1.0/30

elif test  ${prefix} = "ek" ; then
    title="TOTAL ENERGY"
    ylabel="${prefix} * 10@+12@+ [kg]"
    VARMIN=134.5
    VARMAX=137.5
    RANGE=${TMIN}/${TMAX}/${VARMIN}/${VARMAX}
    YINT=a1.0f0.5
    PRJ=x1.0/1.0

else
    echo "stop"
    exit
fi


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
psfile=${infile%.txt}.eps

# psxy
psxy ${infile} -J${PRJ} -R${RANGE} -W3 -X3.0 -Y3.0 ${gmtsta} > ${psfile}

# psbasemap
gmtset TICK_LENGTH -0.15c
psbasemap -J -R -B${XINT}:"${xlabel}":/${YINT}:"${ylabel}":SWne ${gmtcon} >> ${psfile}

# labels (pstext)
# x     y   size angle font place comment
cat << EOF | pstext -R1/10/1/10 -Jx1.0 -N ${gmtend} >> ${psfile}
   1.00   4.25  10   0.0    0    ML ${title}
  -0.25  -0.25   1   0.0    0    ML .
  -0.25   4.75   1   0.0    0    ML .
   8.0   -0.25   1   0.0    0    ML .
   8.0    4.75   1   0.0    0    ML .
EOF

# convert from ps to png
ps2raster -Tg -A ${psfile}

# convert from ps to pdf
ps2raster -Tf -A ${psfile}

rm -f .gmt*

done

#echo "ps2png..."
#time ls *.eps | parallel -j +0 ps2raster -Tg -A {}
#time ls *.eps | parallel -j 6 ps2raster -Tg -A {}
#echo "ps2pdf..."
#time ls *.eps | parallel -j +0 ps2raster -Tf -A {}
#time ls *.eps | parallel -j 6 ps2raster -Tf -A {}

echo "done."

#rm -f *.eps
