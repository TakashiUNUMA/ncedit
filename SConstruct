#
# SConstruct for ncedit.f90 and ncedit_stats.f90
#
# coded by Takashi Unuma, Kyoto Univ.
# last modified: 2014/08/16
#

# load python modules
import os
import os.path

# select compiler
#COMPILER = "GNU"
COMPILER = "INTEL"
#COMPILER = "PGI"

if COMPILER == "GNU":
    FORTRAN = 'gfortran'
    FFLAG   = ['-frecord-marker=4','-ffree-form','-fno-range-check','-O','-fopenmp']
    DFLAG   = ['-fbounds-check','-Wall','-Wuninitialized','-ffpe-trap=invalid,zero,overflow']
    INCFLAG = ['/usr/include']
    LIBFLAG = ['/usr/lib']

elif COMPILER == "INTEL":
    FORTRAN = 'ifort'
    FFLAG   = ['-FR','-i-dynamic','-O0','-openmp']
    DFLAG   = ['-warn all','-check all','-gen_interfaces','-fpe0','-ftrapuv']
    INCFLAG = ['/home/unuma/usr/local/netcdf-4.1.3-intel/include','/home/unuma/usr/local/hdf-1.8.7-intel/include','/home/unuma/usr/local/zlib-1.2.5-intel/include']
    LIBFLAG = ['/home/unuma/usr/local/netcdf-4.1.3-intel/lib','/home/unuma/usr/local/hdf-1.8.7-intel/lib','/home/unuma/usr/local/zlib-1.2.5-intel/lib']

elif COMPILER == "PGI":
    FORTRAN = 'pgfortran'
    FFLAG   = ['-m64','-Mfree','-Kieee','-O0','-mp']
    DFLAG   = ['-Minfo','-Ktrap=fp','-Minform=inform','-Mbounds','-Mlre=noassoc']
    INCFLAG = ['/home/unuma/usr/local/netcdf-4.1.3-pgi/include','/home/unuma/usr/local/hdf-1.8.7-pgi/include','/home/unuma/usr/local/zlib-1.2.5-pgi/include']
    LIBFLAG = ['/home/unuma/usr/local/netcdf-4.1.3-pgi/lib','/home/unuma/usr/local/hdf-1.8.7-pgi/lib','/home/unuma/usr/local/zlib-1.2.5-pgi/lib']

else:
    print COMPILER + u"is not supported on this SConstruct (for now)"

#cccccccccccccccccccccccccccccccccccccccccccccccccccccc
#-- You shouldn't need to change anything below here --
#cccccccccccccccccccccccccccccccccccccccccccccccccccccc

# debug
if ARGUMENTS.get('debug', 0):
    FFLAGS  = FFLAG + DFLAG
else:
    FFLAGS  = FFLAG

# common env
env = Environment(ENV=os.environ,F90=FORTRAN,LINK=FORTRAN,LINKFLAGS=FFLAGS,F90FLAGS=FFLAGS,F90PATH=INCFLAG)

# required library
REQLIB = ['netcdf', 'netcdff', 'hdf5_hl', 'hdf5', 'z', 'm', 'curl']

# build ncedit
env.Program('ncedit.f90', LIBS=REQLIB, LIBPATH=LIBFLAG)

# build ncedit_stats
env.Program('ncedit_stats.f90', LIBS=REQLIB, LIBPATH=LIBFLAG)
