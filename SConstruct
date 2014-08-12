#
# SConstruct for ncedit.f90 and ncedit_stats.f90
#
# coded by Takashi Unuma, Kyoto Univ.
# last modified: 2014/08/12
#

# common env
env = Environment(tools=['default','gfortran'], 
                  F90='gfortran',
                  LINK='gfortran',
                  LINKFLAGS='-I/usr/include -frecord-marker=4 -ffree-form -O -fopenmp -fbounds-check -fno-range-check -Wall -Wuninitialized -ffpe-trap=invalid,zero,overflow',
                  F90FLAGS='-I/usr/include -frecord-marker=4 -ffree-form -O -fopenmp -fbounds-check -fno-range-check -Wall -Wuninitialized -ffpe-trap=invalid,zero,overflow')

# ncedit
env.Program('ncedit.f90',
            LIBS=['netcdf', 'netcdff', 'hdf5_hl', 'hdf5', 'z', 'm', 'curl'],
            LIBPATH=['/usr/lib'])

# ncedit_stats
env.Program('ncedit_stats.f90',
            LIBS=['netcdf', 'netcdff', 'hdf5_hl', 'hdf5', 'z', 'm', 'curl'],
            LIBPATH=['/usr/lib'])
