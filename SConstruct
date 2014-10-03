# -*- coding: utf-8 -*-
#
# SConstruct for ncedit.f90 and ncedit_stats.f90
#
# coded by Takashi Unuma, Kyoto Univ.
# last modified: 2014/10/03
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
    print u" ex 4) $ export FC=f95"
    print u" ex 5) $ export FC=g95"
    sys.exit()

# fortran flags
if FORTRAN == "gfortran":
    OPT  = ['-frecord-marker=4','-ffree-form','-fno-range-check','-O3','-ftree-vectorize','-funroll-loops','-fopenmp']
    OPT1 = ['-frecord-marker=4','-ffree-form','-fno-range-check','-O','-fbounds-check','-Wall','-Wuninitialized','-Wmaybe-uninitialized','-ffpe-trap=invalid,zero,overflow']
    OPT2 = ['-frecord-marker=4','-ffree-form','-fno-range-check','-O','-fopenmp','-fbounds-check','-Wall','-Wuninitialized','-ffpe-trap=invalid,zero,overflow']

elif FORTRAN == "ifort":
    OPT  = ['-FR','-i-dynamic','-O3','-xHost','-fno-alias','-unroll0','-ipo','-openmp']
    OPT1 = ['-FR','-i-dynamic','-O0','-openmp','-W1','-C','-gen_interfaces','-fpe0','-ftrapuv']
    OPT2 = ['-FR','-i-dynamic','-O0','-openmp','-W1','-C','-gen_interfaces','-fpe0','-ftrapuv']

elif FORTRAN == "pgfortran":
    OPT  = ['-m64','-Mfree','-Kieee','-O3','-fast','-Ktrap=none','-mp']
    OPT1 = ['-m64','-Mfree','-Kieee','-O0','-Ktrap=fp','-Minform=inform','-Mbounds','-Mlre=noassoc']
    OPT2 = ['-m64','-Mfree','-Kieee','-O0','-mp','-Minfo','-Ktrap=fp','-Minform=inform','-Mbounds','-Mlre=noassoc']

elif FORTRAN == "f95":
    OPT  = ['-ffree-form','-fno-range-check','-O3','-ftree-vectorize','-funroll-loops','-fopenmp']
    OPT1 = ['-ffree-form','-fno-range-check','-O0','-Wall','-Wuninitialized','-ffpe-trap=invalid,zero,overflow']
    OPT2 = ['-ffree-form','-fno-range-check','-O0','-fopenmp','-Wall','-Wuninitialized','-ffpe-trap=invalid,zero,overflow']

elif FORTRAN == "g95":
    OPT  = ['-ffree-form','-O3'] # not supported OpenMP directive
    OPT1 = ['-ffree-form','-O0','-fbounds-check','-Wall','-Wuninitialized','-ftrace=full','-pedantic','-std=f95']
    OPT2 = OPT1 # As in OPT1 because there is no OpenMP flag in g95 compiler

else:
    print u"Your specified compiler is not supported on this SConstruct (for now)"
    sys.exit()

# flags for debugging
if ARGUMENTS.get('debug') == "1":
    FFLAGS = OPT1 # without optimized flag and OpenMP flag
elif ARGUMENTS.get('debug') == "2":
    FFLAGS = OPT2 # with OpenMP flag but without optimized flag
else:
    FFLAGS = OPT  # with optimized flags and OpenMP flag

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

# build ncedit_pdata
env.Program('ncedit_pdata.f90', LIBS=REQLIB, LIBPATH=LIBFLAG)
