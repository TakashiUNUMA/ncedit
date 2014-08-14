#
# SConstruct for ncedit.f90 and ncedit_stats.f90
#
# coded by Takashi Unuma, Kyoto Univ.
# last modified: 2014/08/14
#

# GNU compiler and its flags
GFORTRAN = 'gfortran'
GFFLAGS  = ['-frecord-marker=4','-ffree-form','-fno-range-check','-O','-fopenmp']
GDFLAGS  = ['-fbounds-check','-Wall','-Wuninitialized','-ffpe-trap=invalid,zero,overflow']

# Intel compiler and its flags
IFORTRAN = '/opt/intel/bin/ifort'
#IFFLAGS  = ['-assume byterecl','-FR','-i-dynamic','-O0','-openmp']
IFFLAGS  = ['-FR','-i-dynamic','-O0','-openmp']
IDFLAGS  = ['-warn all','-check all','-gen_interfaces','-fpe0','-ftrapuv']

# PGI compiler and its flags
PFORTRAN = 'pgfortran'
PFFLAGS  = ['-pc 64','-Mfree','-Kieee','-O0','-mp']
PDFLAGS  = ['-Minfo','-Ktrap=fp','-Minform=inform','-Mbounds','-Mlre=noassoc']


# common env
env = Environment(F90=GFORTRAN,LINK=GFORTRAN,LINKFLAGS=GFFLAGS,F90FLAGS=GFFLAGS,F90PATH=['/usr/include'])
#env = Environment(F90=IFORTRAN,LINK=IFORTRAN,LINKFLAGS=IFFLAGS,F90FLAGS=IFFLAGS,F90PATH=['/home/unuma/usr/local/netcdf-4.1.3-intel/include','/home/unuma/usr/local/hdf-1.8.7-intel/include','/home/unuma/usr/local/zlib-1.2.5-intel/include'])
#env = Environment(F90=PFORTRAN,LINK=PFORTRAN,LINKFLAGS=PFFLAGS,F90FLAGS=PFFLAGS,F90PATH=['/usr/include'])

# build ncedit
env.Program('ncedit.f90',
            LIBS=['netcdf', 'netcdff', 'hdf5_hl', 'hdf5', 'z', 'm', 'curl'],
#            LIBPATH=['/home/unuma/usr/local/netcdf-4.1.3-intel/lib','/home/unuma/usr/local/hdf-1.8.7-intel/lib','/home/unuma/usr/local/zlib-1.2.5-intel/lib'])
            LIBPATH=['/usr/lib'])

# build ncedit_stats
env.Program('ncedit_stats.f90',
            LIBS=['netcdf', 'netcdff', 'hdf5_hl', 'hdf5', 'z', 'm', 'curl'],
#            LIBPATH=['/home/unuma/usr/local/netcdf-4.1.3-intel/lib','/home/unuma/usr/local/hdf-1.8.7-intel/lib','/home/unuma/usr/local/zlib-1.2.5-intel/lib'])
            LIBPATH=['/usr/lib'])
