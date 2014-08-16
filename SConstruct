# -*- coding: utf-8 -*-
#
# SConstruct for ncedit.f90 and ncedit_stats.f90
#
# coded by Takashi Unuma, Kyoto Univ.
# last modified: 2014/08/17
#

# load python modules
import os
import os.path
import sys

# check environmental value
FORTRAN  = os.environ.get('FC')

# check compiler
if FORTRAN == "":
    print u"Please set FORTRAN value as an environmental value"
    print u" ex 1) $ export FC=gfortran"
    print u" ex 2) $ export FC=ifort"
    print u" ex 3) $ export FC=pgfortran"
    sys.exit()

# fortran flags
if FORTRAN == "gfortran":
    FFLAG   = ['-frecord-marker=4','-ffree-form','-fno-range-check','-O3','-ftree-vectorize','-funroll-loops','-fopenmp']
    DFLAG   = ['-frecord-marker=4','-ffree-form','-fno-range-check','-O','-fopenmp','-fbounds-check','-Wall','-Wuninitialized','-ffpe-trap=invalid,zero,overflow']

elif FORTRAN == "ifort":
    FFLAG   = ['-FR','-i-dynamic','-O3','-xHost','-fno-alias','-unroll0','-ipo','-openmp']
    DFLAG   = ['-FR','-i-dynamic','-O0','-openmp','-warn all','-check all','-gen_interfaces','-fpe0','-ftrapuv']

elif FORTRAN == "pgfortran":
    FFLAG   = ['-m64','-Mfree','-Kieee','-O3','-fast','-Ktrap=none','-mp','-Minfo']
    DFLAG   = ['-m64','-Mfree','-Kieee','-O0','-mp','-Minfo','-Ktrap=fp','-Minform=inform','-Mbounds','-Mlre=noassoc']

else:
    print FORTRAN + u"is not supported on this SConstruct (for now)"
    sys.exit()

# debug
if ARGUMENTS.get('debug', 0):
    FFLAGS  = DFLAG
else:
    FFLAGS  = FFLAG

# define library path
NETCDF = os.environ.get('NETCDF')
HDF5 = os.environ.get('HDF')
ZLIB = os.environ.get('ZLIB')

# check library path
if NETCDF == "":
    print u"Please set NETCDF value as an environmental value"
    sys.exit()
if HDF5 == "":
    print u"Please set HDF5 value as an environmental value"
    sys.exit()
if ZLIB == "":
    print u"Please set ZLIB value as an environmental value"
    sys.exit()

# includes and libs
INCFLAG = [NETCDF + '/include', HDF5 + '/include', ZLIB + '/include']
LIBFLAG = [NETCDF + '/lib', HDF5 + '/lib', ZLIB + '/lib']

# required library
REQLIB = ['netcdf', 'netcdff', 'hdf5_hl', 'hdf5', 'z', 'm']


# common env
env = Environment(ENV=os.environ,F90=FORTRAN,LINK=FORTRAN,LINKFLAGS=FFLAGS,F90FLAGS=FFLAGS,F90PATH=INCFLAG)

# build ncedit
env.Program('ncedit.f90', LIBS=REQLIB, LIBPATH=LIBFLAG)

# build ncedit_stats
env.Program('ncedit_stats.f90', LIBS=REQLIB, LIBPATH=LIBFLAG)
