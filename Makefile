#
# Makefile for ncedit.f90
# original makefile coded by Takashi Unuma, Kyoto Univ.
# Last modified: 2014/06/14
#

#COMPILER=INTEL
#COMPILER=GNU
COMPILER=PGI

#-------------------------------------------------
# Make for Intel Compiler on Linux x86_64 system
ifeq ($(COMPILER),INTEL)
FC	= ifort
#FFLAGS	= -FR -O3 -xHost -assume byterecl -i-dynamic -fno-alias -unroll0 -ipo
FFLAGS	= -FR -g -O0 -assume byterecl -i-dynamic -warn all -check all
# -- rx2000,gpgpu
#NETCDF	= /home/unuma/usr/local/netcdf-4.1.3
#ZLIB	= /home/unuma/usr/local/zlib-1.2.5
#HDF5	= /home/unuma/usr/local/hdf5-1.8.7
# -- kudpc
NETCDF	= /LARGE0/gr10053/b31894/lib/netcdf-4.1.3-intel
ZLIB	= /LARGE0/gr10053/b31894/lib/hdf5-1.8.7-intel
HDF5	= /LARGE0/gr10053/b31894/lib/zlib-1.2.5-intel
endif
# -----
# Make for GNU Compiler on Linux x86_64 system
ifeq ($(COMPILER),GNU)
FC      = gfortran
#FFLAGS	 = -frecord-marker=4 -ffree-form -O3 -ftree-vectorize -funroll-loops -fno-range-check
#FFLAGS	 = -frecord-marker=4 -ffree-form -O -Wall -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbounds-check -fno-range-check
FFLAGS	 = -frecord-marker=4 -ffree-form -O -Wall -ffpe-trap=invalid,zero,overflow -fbounds-check -fno-range-check
# -- rx2000,gpgpu
#NETCDF  = /home/unuma/usr/local/netcdf-4.1.3-gnu
#ZLIB    = /home/unuma/usr/local/zlib-1.2.5-gnu
#HDF5    = /home/unuma/usr/local/hdf5-1.8.7-gnu
# -- qsmcs,mcs
NETCDF  = /usr
ZLIB    = /usr
HDF5    = /usr
# -- kudpc
#NETCDF  = /LARGE0/gr10053/b31894/lib/netcdf-4.1.3-gnu
#ZLIB    = /LARGE0/gr10053/b31894/lib/hdf5-1.8.12-gnu
#HDF5    = /LARGE0/gr10053/b31894/lib/zlib-1.2.8-gnu
endif
# -----
# Make for PGI Compiler on Linux x86_64 system
ifeq ($(COMPILER),PGI)
FC	= pgfortran
FFLAGS	= -pc 64 -Kieee -O0 -Ktrap=fp -Minform=inform -Mbounds -Mlre=noassoc
# -- nimbus
NETCDF	= /usr/local/netcdf-4.1.3-pgi
ZLIB	= /usr/local/zlib-1.2.5-pgi
HDF5	= /usr/local/hdf5-1.8.7-pgi
# -- kudpc
#NETCDF  = /LARGE0/gr10053/b31894/lib/netcdf-4.1.3-pgi
#ZLIB    = /LARGE0/gr10053/b31894/lib/hdf5-1.8.12-pgi
#HDF5    = /LARGE0/gr10053/b31894/lib/zlib-1.2.8-pgi
endif
#-------------------------------------------------
#-- You shouldn't need to change anything below here
#-------------------------------------------------


OUTPUTINC = -I$(NETCDF)/include -I$(HDF5)/include -I$(ZLIB)/include
OUTPUTLIB = -L$(NETCDF)/lib -L$(HDF5)/lib -L$(ZLIB)/lib
LINKOPTS  = -lnetcdf -lnetcdff -lhdf5_hl -lhdf5 -lz -lm -lcurl

.SUFFIXES:
.SUFFIXES: .f90 .o

all: 	ncedit ncedit_stats

ncedit: ncedit.f90
	$(FC) ncedit.f90 $(FFLAGS) $(OUTPUTINC) $(OUTPUTLIB) $(LINKOPTS) -o $@

ncedit_stats: ncedit_stats.f90
	$(FC) ncedit_stats.f90 $(FFLAGS) $(OUTPUTINC) $(OUTPUTLIB) $(LINKOPTS) -o $@

clean:
	rm -rf *.o *genmod.* *.f90~ Makefile~ ncedit ncedit_stats

############################################################ end
.NOEXPORT:
