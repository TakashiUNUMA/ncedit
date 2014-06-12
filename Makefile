#
# Makefile for ncedit.f90
# original makefile coded by Takashi Unuma, Kyoto Univ.
# Last modified: 2014/06/12
#

#-------------------------------------------------
# Make for Intel Compiler on Linux x86_64 system
#FC      = ifort
#NETCDF  = /home/unuma/usr/local/netcdf-4.1.3
#ZLIB    = /home/unuma/usr/local/zlib-1.2.5
#HDF5    = /home/unuma/usr/local/hdf5-1.8.7
#FFLAGS  = -I$(NETCDF)/include -FR -O3 -xSSE4.2 -assume byterecl -i-dynamic -fno-alias -unroll0 -ipo
#FFLAGS  = -I$(NETCDF)/include -FR -g -O0 -assume byterecl -i-dynamic -warn all -check all
# -----
# Make for GNU Compiler on Linux x86_64 system
FC      = gfortran
#NETCDF  = /home/unuma/usr/local/netcdf-4.1.3-gnu
#ZLIB    = /home/unuma/usr/local/zlib-1.2.5-gnu
#HDF5    = /home/unuma/usr/local/hdf5-1.8.7-gnu
#NETCDF  = /usr
#ZLIB    = /usr
#HDF5    = /usr
#FFLAGS	= -I$(NETCDF)/include -frecord-marker=4 -ffree-form -O3 -ftree-vectorize -funroll-loops -fno-range-check
#FFLAGS	= -I$(NETCDF)/include -frecord-marker=4 -ffree-form -O -Wall -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbounds-check -fno-range-check
# -----
# Make for PGI Compiler on Linux x86_64 system
FC      = pgfortran
NETCDF  = /usr/local/netcdf-4.1.3-pgi
ZLIB    = /usr/local/zlib-1.2.5-pgi
HDF5    = /usr/local/hdf5-1.8.7-pgi
FFLAGS	= -I$(NETCDF)/include -pc 64 -Kieee -O0 -Ktrap=fp -Minform=inform -Mbounds -Mlre=noassoc
#-------------------------------------------------


#LDFLAGS = $(NETCDF)/lib/libnetcdff.la $(NETCDF)/lib/libnetcdf.la -L$(ZLIB)/lib -L$(HDF5)/lib
LDFLAGS = -L$(NETCDF)/lib -L$(ZLIB)/lib -L$(HDF5)/lib
LIBS    = -lhdf5_hl -lhdf5 -lm -lcurl -lnetcdf -lnetcdff -lz

OBJECT  = ncedit.o

.SUFFIXES:
.SUFFIXES: .f90 .o

all: ncedit

ncedit: $(OBJECT)
	libtool --tag=F77 --mode=link $(FC) $(FFLAGS) $(LDFLAGS) -o $@ $(OBJECT) $(LIBS)

.f90.o:
	$(FC) $(FFLAGS) -c -o $@ $*.f90

clean:
	rm -rf *.o *genmod.* *.f90~ Makefile~ ncedit .libs

############################################################ end
.NOEXPORT:
