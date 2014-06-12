#!/bin/sh
#
# GMTPLOT
# original script coded by Takashi Unuma, Kyoto Univ.
# Last modified: 2014/06/12
#

if test $# -lt 1 ; then
    echo "USAGE: $(basename $0) [infile]"
    exit
fi
infile=$1

for infile in $* ; do

echo ${infile}

title="Qc+Qr+Qi+Qs+Qg"
unit="[g/kg]"
VARMIN=0.5
VARMAX=4.0
VARINT=0.5
BINT=a1f0.5

#title="VERTICAL VELOCITY"
#unit="[m/s]"
#VARMIN=-5.0
#VARMAX=5.0
#VARINT=1.0
#VARTYPE="polar"
#BINT=a2f1


xlabel="Distance [km]"
ylabel="Height [km]"

XINT=a10f5
YINT=a5f1
RANGE=-50/50/0/15
PRJ=x0.1/0.2

wind=1
VECTOROPT="-S50 -Q0.005/0.2/0.1n0.45 -G0 -I4/1"
VECTORPOS=25

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
gmtset PAPER_MEDIA                   a4
gmtset VECTOR_SHAPE                   2


# Useful option
gmtsta="-P -K"
gmtcon="-P -K -O"
gmtend="-P -O"

# Specify output file
psfile=${infile%.nc4}.ps

# psxy
CPALET=cpalet.cpt
#grd2cpt ${infile} > ${CPALET}
unucpt ${infile%.nc4} ${VARMIN} ${VARMAX} ${VARINT} ${VARTYPE}
grdimage ${infile} -J${PRJ} -R${RANGE} -C${CPALET} -X3.0 -Y3.0 ${gmtsta} > ${psfile}

# psscale
gmtset ANOT_FONT_SIZE 8p
gmtset TICK_LENGTH -0.1c
psscale -D1.35/-0.6/2.75/0.2h -B${BINT}/:"${unit}": -C${CPALET} -N ${gmtcon} >> ${psfile}

if test ${wind} -eq 1 ; then
    suffix=$(echo ${infile} | cut -d "_" -f 2)
    grdvector uinterp_${suffix} winterp_${suffix} -J -R ${VECTOROPT} ${gmtcon} >> ${psfile}
    psxy -R -J -Sv0.02/0.15/0.1 -L -G0 -W1 -N ${gmtcon} << EOF >> ${psfile}
  ${VECTORPOS} -3.5 0 0.4
EOF
    pstext -R -Jx -N -O -K << EOF >> $psfile
  ${VECTORPOS} -5 8 0.0 0 5 20 [m/s]
EOF
fi

# psbasemap
gmtset TICK_LENGTH -0.15c
psbasemap -J -R -B${XINT}:"${xlabel}":/${YINT}:"${ylabel}":SWne:."${title} FT = ${suffix%.nc4}": ${gmtcon} >> ${psfile}

# labels (pstext)
# x     y   size angle font place comment
cat << EOF | pstext -R1/10/1/10 -Jx1.0 -N ${gmtend} >> ${psfile}
#  0.5    8.15  12   0.0   0    ML    (b)
  -0.25  -0.25   1   0.0   0    ML    .
  -0.25   5.25   1   0.0   0    ML    .
  12.0   -0.25   1   0.0   0    ML    .
  12.0    5.25   1   0.0   0    ML    .
EOF

# convert from ps to png
ps2raster -Tg -A ${psfile}

# convert from ps to pdf
ps2raster -Tf -A ${psfile}

rm -f .gmt*

done
