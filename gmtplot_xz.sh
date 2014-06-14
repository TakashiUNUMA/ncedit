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

prefix=$(echo ${infile} | cut -d "_" -f 1)
suffix=$(echo ${infile} | cut -d "_" -f 2)
ftime=${suffix%.nc4}

auto=0
if test ${prefix} = "water" ; then
    title="Qc+Qr+Qi+Qs+Qg"
    unit="[g/kg]"
    VARMIN=0.5
    VARMAX=4.0
    VARINT=0.5
    VARTYPE="seis"
    BINT=a1f0.5
elif test ${prefix} = "dbz" ; then
    title="PSEUDO-REFLECTIVITY"
    unit="[dBZ]"
    VARMIN=10.0
    VARMAX=60.0
    VARINT=5.0
    VARTYPE="seis"
    BINT=a10f5
elif test ${prefix} = "thetae" ; then
    title="EQUIVALENT POTENTIAL TEMPERATURE"
    unit="[K]"
    VARMIN=333.0
    VARMAX=354.0
    VARINT=3.0
    VARTYPE="seis"
    BINT=a6f3
elif test ${prefix} = "buoyancy" ; then
    title="BUOYANCY"
    unit="[m@+2@+/s@+2@+]"
    VARMIN=-0.05
    VARMAX=0.05
    VARINT=0.005
    VARTYPE="polar"
    BINT=a0.05f0.005
elif test ${prefix} = "winterp" ; then
    title="VERTICAL VELOCITY"
    unit="[m/s]"
    VARMIN=-5.0
    VARMAX=5.0
    VARINT=1.0
    VARTYPE="polar"
    BINT=a2f1
else
    echo "automatically selected"
    title="${prefix}"
    auto=1
fi

# x and y label
xlabel="Distance [km]"
ylabel="Height [km]"

# other parameters for drawing GMT
XINT=a50f10
YINT=a5f1
RANGE=-100/100/0/15
PRJ=x0.05/0.2

# wind vector options
wind=0
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
#psfile=${infile%.nc4}.ps
psfile=${infile%.nc4}.eps

# psxy
CPALET=cpalet.cpt
if test ${auto} -eq 1 ; then
    grd2cpt ${infile} > ${CPALET}
else
    unucpt ${infile%.nc4} ${VARMIN} ${VARMAX} ${VARINT} ${VARTYPE}
fi
grdimage ${infile} -J${PRJ} -R${RANGE} -C${CPALET} -X3.0 -Y3.0 ${gmtsta} > ${psfile}

# psscale
gmtset ANOT_FONT_SIZE 8p
gmtset TICK_LENGTH -0.1c
psscale -D1.35/-0.6/2.75/0.2h -B${BINT}/:"${unit}": -C${CPALET} -N ${gmtcon} >> ${psfile}

# grdvector
if test ${wind} -eq 1 ; then
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
#psbasemap -J -R -B${XINT}:"${xlabel}":/${YINT}:"${ylabel}":SWne:."${title}": ${gmtcon} >> ${psfile}
psbasemap -J -R -B${XINT}:"${xlabel}":/${YINT}:"${ylabel}":SWne ${gmtcon} >> ${psfile}

# labels (pstext)
#  x      y   size angle font place comment
cat << EOF | pstext -R1/10/1/10 -Jx1.0 -N ${gmtend} >> ${psfile}
   1.00   4.25  10   0.0    0    ML ${title}
   8.65   4.25  10   0.0    0    ML FT = ${ftime}
  11.025  4.25  10   0.0    0    MR [min]
  -0.25  -0.25   1   0.0    0    ML .
  -0.25   4.75   1   0.0    0    ML .
  12.0   -0.25   1   0.0    0    ML .
  12.0    4.75   1   0.0    0    ML .
EOF

# convert from ps to png
#ps2raster -Tg -A ${psfile}

# convert from ps to pdf
#ps2raster -Tf -A ${psfile}

#rm -f ${psfile}
rm -f .gmt*
rm -f cpalet.cpt

done

echo "ps2png..."
#ls *.ps | parallel -j +0 ps2raster -Tg -A {}
time ls *.eps | parallel -j 6 ps2raster -Tg -A {}
echo "ps2pdf..."
#ls *.ps | parallel -j +0 ps2raster -Tf -A {}
time ls *.eps | parallel -j 6 ps2raster -Tf -A {}
echo "done."
#rm -f *.ps
