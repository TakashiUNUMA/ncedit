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
#COMPILER = "INTEL"
COMPILER = "PGI"

if COMPILER == "GNU":
    FORTRAN = 'gfortran'
    FFLAG   = ['-frecord-marker=4','-ffree-form','-fno-range-check','-O3','-ftree-vectorize','-funroll-loops','-fopenmp']
    DFLAG   = ['-frecord-marker=4','-ffree-form','-fno-range-check','-O','-fopenmp','-fbounds-check','-Wall','-Wuninitialized','-ffpe-trap=invalid,zero,overflow']
    INCFLAG = ['/usr/include'] # for qsmcs
    LIBFLAG = ['/usr/lib'] # for qsmcs

elif COMPILER == "INTEL":
    FORTRAN = 'ifort'
    FFLAG   = ['-FR','-i-dynamic','-O3','-xHost','-fno-alias','-unroll0','-ipo','-openmp']
    DFLAG   = ['-FR','-i-dynamic','-O0','-openmp','-warn all','-check all','-gen_interfaces','-fpe0','-ftrapuv']
#    INCFLAG = ['/home/unuma/usr/local/netcdf-4.1.3-intel/include','/home/unuma/usr/local/hdf-1.8.7-intel/include','/home/unuma/usr/local/zlib-1.2.5-intel/include']
#    LIBFLAG = ['/home/unuma/usr/local/netcdf-4.1.3-intel/lib','/home/unuma/usr/local/hdf-1.8.7-intel/lib','/home/unuma/usr/local/zlib-1.2.5-intel/lib']
    INCFLAG = ['/LARGE0/gr10053/b31894/lib/netcdf-4.1.3-intel/include','/LARGE0/gr10053/b31894/lib/hdf5-1.8.7-intel/include','/LARGE0/gr10053/b31894/lib/zlib-1.2.5-intel/include']
    LIBFLAG = ['/LARGE0/gr10053/b31894/lib/netcdf-4.1.3-intel/lib','/LARGE0/gr10053/b31894/lib/hdf5-1.8.7-intel/lib','/LARGE0/gr10053/b31894/lib/zlib-1.2.5-intel/lib']

elif COMPILER == "PGI":
    FORTRAN = 'pgfortran'
    FFLAG   = ['-m64','-Mfree','-Kieee','-O3','-fast','-Ktrap=none','-mp','-Minfo']
    DFLAG   = ['-m64','-Mfree','-Kieee','-O0','-mp','-Minfo','-Ktrap=fp','-Minform=inform','-Mbounds','-Mlre=noassoc']
#    INCFLAG = ['/home/unuma/usr/local/netcdf-4.1.3-pgi/include','/home/unuma/usr/local/hdf5-1.8.7-pgi/include','/home/unuma/usr/local/zlib-1.2.5-pgi/include']
#    LIBFLAG = ['/home/unuma/usr/local/netcdf-4.1.3-pgi/lib','/home/unuma/usr/local/hdf5-1.8.7-pgi/lib','/home/unuma/usr/local/zlib-1.2.5-pgi/lib']
#    INCFLAG = ['/LARGE0/gr10053/b31894/lib/netcdf-4.1.3-pgi/include','/LARGE0/gr10053/b31894/lib/hdf5-1.8.12-pgi/include','/LARGE0/gr10053/b31894/lib/zlib-1.2.8-pgi/include'] # for kudpc
#    LIBFLAG = ['/LARGE0/gr10053/b31894/lib/netcdf-4.1.3-pgi/lib','/LARGE0/gr10053/b31894/lib/hdf5-1.8.12-pgi/lib','/LARGE0/gr10053/b31894/lib/zlib-1.2.8-pgi/lib'] # for kudpc
    INCFLAG = ['/usr/local/netcdf-4.1.3-pgi/include','/usr/local/hdf5-1.8.7-pgi/include','/usr/local/zlib-1.2.5-pgi/include'] # for nimbus
    LIBFLAG = ['/usr/local/netcdf-4.1.3-pgi/lib','/usr/local/hdf5-1.8.7-pgi/lib','/usr/local/zlib-1.2.5-pgi/lib'] # for nimbus

else:
    print COMPILER + u"is not supported on this SConstruct (for now)"

#cccccccccccccccccccccccccccccccccccccccccccccccccccccc
#-- You shouldn't need to change anything below here --
#cccccccccccccccccccccccccccccccccccccccccccccccccccccc

# debug
if ARGUMENTS.get('debug', 0):
    FFLAGS  = DFLAG
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
