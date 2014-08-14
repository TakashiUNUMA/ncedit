#
# SConstruct for ncedit.f90 and ncedit_stats.f90
#
# coded by Takashi Unuma, Kyoto Univ.
# last modified: 2014/08/14
#

# select compiler
COMPILER = "GNU"
#COMPILER = "INTEL"
#COMPILER = "PGI"

if COMPILER == "GNU":
    # GNU compiler and its flags
    FORTRAN = 'gfortran'
    FFLAG   = ['-frecord-marker=4','-ffree-form','-fno-range-check','-O','-fopenmp']
    DFLAG   = ['-fbounds-check','-Wall','-Wuninitialized','-ffpe-trap=invalid,zero,overflow']
    if ARGUMENTS.get('debug', 0):
        FFLAGS  = FFLAG + DFLAG
    else:
        FFLAGS  = FFLAG
    INCFLAG = ['/usr/include']
    LIBFLAG = ['/usr/lib']

elif COMPILER == "INTEL":
    # Intel compiler and its flags
    FORTRAN = '/opt/intel/bin/ifort'
    #FFLAGS  = ['-assume byterecl','-FR','-i-dynamic','-O0','-openmp']
    FFLAG   = ['-FR','-i-dynamic','-O0','-openmp']
    DFLAG   = ['-warn all','-check all','-gen_interfaces','-fpe0','-ftrapuv']
    if ARGUMENTS.get('debug', 0):
        FFLAGS  = FFLAG + DFLAG
    else:
        FFLAGS  = FFLAG
    INCFLAG = ['/home/unuma/usr/local/netcdf-4.1.3-intel/include','/home/unuma/usr/local/hdf-1.8.7-intel/include','/home/unuma/usr/local/zlib-1.2.5-intel/include']
    LIBFLAG = ['/home/unuma/usr/local/netcdf-4.1.3-intel/lib','/home/unuma/usr/local/hdf-1.8.7-intel/lib','/home/unuma/usr/local/zlib-1.2.5-intel/lib']

elif COMPILER == "PGI":
    # PGI compiler and its flags
    FORTRAN = 'pgfortran'
    FFLAG   = ['-pc 64','-Mfree','-Kieee','-O0','-mp']
    DFLAG   = ['-Minfo','-Ktrap=fp','-Minform=inform','-Mbounds','-Mlre=noassoc']
    if ARGUMENTS.get('debug', 0):
        FFLAGS  = FFLAG + DFLAG
    else:
        FFLAGS  = FFLAG
    INCFLAG = ['/home/unuma/usr/local/netcdf-4.1.3-pgi/include','/home/unuma/usr/local/hdf-1.8.7-pgi/include','/home/unuma/usr/local/zlib-1.2.5-pgi/include']
    LIBFLAG = ['/home/unuma/usr/local/netcdf-4.1.3-pgi/lib','/home/unuma/usr/local/hdf-1.8.7-pgi/lib','/home/unuma/usr/local/zlib-1.2.5-pgi/lib']

else:
    print COMPILER + u"is not supported on this SConstruct (for now)"


#cccccccccccccccccccccccccccccccccccccccccccccccccccccc
#-- You shouldn't need to change anything below here --
#cccccccccccccccccccccccccccccccccccccccccccccccccccccc

# common env
env = Environment(F90=FORTRAN,LINK=FORTRAN,LINKFLAGS=FFLAGS,F90FLAGS=FFLAGS,F90PATH=INCFLAG)
# required library
REQLIB = ['netcdf', 'netcdff', 'hdf5_hl', 'hdf5', 'z', 'm', 'curl']

# build ncedit
env.Program('ncedit.f90', LIBS=REQLIB, LIBPATH=LIBFLAG)
# build ncedit_stats
env.Program('ncedit_stats.f90', LIBS=REQLIB, LIBPATH=LIBFLAG)
