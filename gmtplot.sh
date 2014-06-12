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
#infile=output.nc4
infile=$1


title="Qc+Qr+Qi+Qs+Qg"
xlabel="Distance [km]"
ylabel="Height [km]"
unit="[g/kg]"

XINT=a10f5
YINT=a5f1
RANGE=-20/30/0/15
PRJ=x0.2
BINT=a1f0.5

#------------------------------------------
# GMT setting
gmtdefaults -D > .gmtdefaults4
gmtset HEADER_FONT_SIZE        12p
gmtset HEADER_OFFSET         -0.1c
gmtset LABEL_FONT_SIZE         10p
gmtset ANOT_FONT_SIZE           8p
gmtset ANNOT_OFFSET_PRIMARY  0.10c
gmtset PLOT_DEGREE_FORMAT    ddd:mm:ssF
gmtset BASEMAP_TYPE          plain
gmtset TICK_LENGTH           -0.15c
gmtset FRAME_PEN             0.50p
gmtset GRID_PEN              0.20p
gmtset TICK_PEN              0.50p
gmtset MEASURE_UNIT             cm
gmtset PAPER_MEDIA              a4

# Useful option
gmtsta='-P -K'
gmtcon='-P -K -O'
gmtend='-P -O'

# Specify output file
psfile=${infile%.nc4}.ps

# psxy
grd2cpt ${infile} > cpalet.cpt
grdimage ${infile} -J${PRJ} -R${RANGE} -Ccpalet.cpt -X1.5 -Y1.5 ${gmtsta} > ${psfile}
#grdcontour ${infile} -J${PRJ} -R${RANGE} -Ccpalet.cpt -X1.2 -Y1.2 ${gmtsta} > ${psfile}

# psscale
gmtset ANOT_FONT_SIZE 8p
psscale -D1.5/-0.4/3.0/0.15h -B${BINT}/:"${unit}": -Ccpalet.cpt -N ${gmtcon} >> ${psfile}

# psbasemap
psbasemap -J -R -B${XINT}:"${xlabel}":/${YINT}:"${ylabel}":SWne:."${title}": ${gmtcon} >> ${psfile}


# labels (pstext)
# x     y   size angle font place comment
cat << EOF | pstext -R1/10/1/10 -Jx1.0 -N ${gmtend} >> ${psfile}
# 0.5   8.15  12   0.0   0    ML    (b)
-1.5  -0.25   1   0.0   0    ML    .
-1.5   5.25   1   0.0   0    ML    .
12.0  -0.25   1   0.0   0    ML    .
12.0   5.25   1   0.0   0    ML    .
EOF

# convert from ps to png
ps2raster -Tg -A ${psfile}

# convert from ps to pdf
ps2raster -Tf -A ${psfile}

rm -f .gmt*
