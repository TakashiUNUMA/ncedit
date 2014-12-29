!
! N C E D I T (pdata)
!
! original program coded by Takashi Unuma, Kyoto Univ.
! Last modified: 2014/12/20
!

program ncedit_pdata

  use netcdf
  
  implicit none

  integer :: tmax, nparcel
  real, dimension(:),   allocatable :: time
  real, dimension(:,:), allocatable :: x, z
  real, dimension(:,:), allocatable :: w, pwdt, vpga, buoy, load
  character(len=20) :: varname
  character(len=42) :: input, output
  integer :: debug_level

  ! local variables
  integer :: i, t, ncid, varid, tdimid
  integer, dimension(2) :: istart, icount

  ! undefined value
!  real :: nan = -999.
  real :: nan = -2147483648.
  

  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Input parameters from namelist
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  namelist /param/ tmax,nparcel,input,debug_level
  open(unit=10,file='namelist.ncedit_pdata',form='formatted',status='old',access='sequential')
  read(10,nml=param)
  close(unit=10)

  if(debug_level.ge.100) print '(a30)',     "----- values on namelist -----"
  if(debug_level.ge.100) print '(a18,i6)',  " tmax          = ", tmax
  if(debug_level.ge.100) print '(a18,i6)',  " nparcel       = ", nparcel
  if(debug_level.ge.100) print '(a18,a)',   " input         = ", trim(input)
  if(debug_level.ge.100) print '(a18,i6)',  " debug_level   = ", debug_level
  if(debug_level.ge.100) print '(a30)',     "----- values on namelist -----"
  if(debug_level.ge.100) print *, ""

  ! allocate arrays
  allocate( time(tmax) )
  allocate( x(nparcel,tmax), z(nparcel,tmax), w(nparcel,tmax) )
  allocate( pwdt(nparcel,tmax), vpga(nparcel,tmax), buoy(nparcel,tmax), load(nparcel,tmax) )
  istart = (/ 1, 1 /)
  icount = (/ nparcel, tmax /)
  x(1:nparcel,1:tmax) = nan
  z(1:nparcel,1:tmax) = nan
  w(1:nparcel,1:tmax) = nan
  pwdt(1:nparcel,1:tmax) = nan
  vpga(1:nparcel,1:tmax) = nan
  buoy(1:nparcel,1:tmax) = nan
  load(1:nparcel,1:tmax) = nan


  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Input 4D file
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! open the original netcdf file
  call check( nf90_open(input, nf90_nowrite, ncid) )
  if(debug_level.ge.100) print *, "Success: open the file"
  if(debug_level.ge.200) print *, " ncid          = ", ncid

  ! inquire and get time coordinate
  call check( nf90_inq_varid(ncid, 'time', tdimid) )
  if(debug_level.ge.100) print *, "Success: inquire the tdimid"
  if(debug_level.ge.200) print *, " tdimid        = ", tdimid
  call check( nf90_get_var(ncid, tdimid, time) )
  if(debug_level.ge.100) print *, "Success: get the time coordinate"
  if(debug_level.ge.200) print *, " time(:)       = ", time
  if(debug_level.ge.100) print *, ""

  call check( nf90_inq_varid(ncid, "x", varid) )
  if(debug_level.ge.100) print *, " Success: inquire the varid"
  if(debug_level.ge.200) print *, "  varid         = ", varid
  if(debug_level.ge.300) print *, "   istart       = ", istart
  if(debug_level.ge.300) print *, "   icount       = ", icount
  call check( nf90_get_var(ncid, varid, x, start = istart, count = icount ) )
  if(debug_level.ge.100) print *, " Success: get the var array (x)"
  if(debug_level.ge.200) print *, "  x(:,:) = ", x(:,:)

  call check( nf90_inq_varid(ncid, "z", varid) )
  if(debug_level.ge.100) print *, " Success: inquire the varid"
  if(debug_level.ge.200) print *, "  varid         = ", varid
  if(debug_level.ge.300) print *, "   istart       = ", istart
  if(debug_level.ge.300) print *, "   icount       = ", icount
  call check( nf90_get_var(ncid, varid, z, start = istart, count = icount ) )
  if(debug_level.ge.100) print *, " Success: get the var array (z)"
  if(debug_level.ge.200) print *, "  z(:,:) = ", z(:,:)
  if(debug_level.ge.100) print *, ""

  call check( nf90_inq_varid(ncid, "w", varid) )
  if(debug_level.ge.100) print *, " Success: inquire the varid"
  if(debug_level.ge.200) print *, "  varid         = ", varid
  if(debug_level.ge.300) print *, "   istart       = ", istart
  if(debug_level.ge.300) print *, "   icount       = ", icount
  call check( nf90_get_var(ncid, varid, w, start = istart, count = icount ) )
  if(debug_level.ge.100) print *, " Success: get the var array (w)"
  if(debug_level.ge.200) print *, "  w(:,:) = ", w(:,:)
  if(debug_level.ge.100) print *, ""

  call check( nf90_inq_varid(ncid, "pwdt", varid) )
  if(debug_level.ge.100) print *, " Success: inquire the varid"
  if(debug_level.ge.200) print *, "  varid         = ", varid
  if(debug_level.ge.300) print *, "   istart       = ", istart
  if(debug_level.ge.300) print *, "   icount       = ", icount
  call check( nf90_get_var(ncid, varid, pwdt, start = istart, count = icount ) )
  if(debug_level.ge.100) print *, " Success: get the var array (pwdt)"
  if(debug_level.ge.200) print *, "  pwdt(:,:) = ", pwdt(:,:)
  if(debug_level.ge.100) print *, ""

  call check( nf90_inq_varid(ncid, "vpga", varid) )
  if(debug_level.ge.100) print *, " Success: inquire the varid"
  if(debug_level.ge.200) print *, "  varid         = ", varid
  if(debug_level.ge.300) print *, "   istart       = ", istart
  if(debug_level.ge.300) print *, "   icount       = ", icount
  call check( nf90_get_var(ncid, varid, vpga, start = istart, count = icount ) )
  if(debug_level.ge.100) print *, " Success: get the var array (vpga)"
  if(debug_level.ge.200) print *, "  vpga(:,:) = ", vpga(:,:)
  if(debug_level.ge.100) print *, ""

  call check( nf90_inq_varid(ncid, "buoy", varid) )
  if(debug_level.ge.100) print *, " Success: inquire the varid"
  if(debug_level.ge.200) print *, "  varid         = ", varid
  if(debug_level.ge.300) print *, "   istart       = ", istart
  if(debug_level.ge.300) print *, "   icount       = ", icount
  call check( nf90_get_var(ncid, varid, buoy, start = istart, count = icount ) )
  if(debug_level.ge.100) print *, " Success: get the var array (buoy)"
  if(debug_level.ge.200) print *, "  buoy(:,:) = ", buoy(:,:)
  if(debug_level.ge.100) print *, ""

  call check( nf90_inq_varid(ncid, "load", varid) )
  if(debug_level.ge.100) print *, " Success: inquire the varid"
  if(debug_level.ge.200) print *, "  varid         = ", varid
  if(debug_level.ge.300) print *, "   istart       = ", istart
  if(debug_level.ge.300) print *, "   icount       = ", icount
  call check( nf90_get_var(ncid, varid, load, start = istart, count = icount ) )
  if(debug_level.ge.100) print *, " Success: get the var array (load)"
  if(debug_level.ge.200) print *, "  load(:,:) = ", load(:,:)
  if(debug_level.ge.100) print *, ""


  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Output 1D file
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! writeout data to output file
  do i = 1, nparcel, 1
     write (output, '("pdata_", i3.3, ".txt")') i
     open(20, file=output, status='replace')
     if(debug_level.ge.100) print *, " Success: open the output file as ",trim(output)
     do t = 1, tmax, 1
        write(20,111) time(t)/real(3600.), x(i,t)*0.001, z(i,t)*0.001, &
                      w(i,t), pwdt(i,t), vpga(i,t), buoy(i,t), load(i,t)
        if(debug_level.ge.200) print 222, "t,time_out,var_outs = ",t,time(t)/real(3600.),x(i,t)*0.001,z(i,t)*0.001, w(i,t), pwdt(i,t), vpga(i,t), buoy(i,t), load(i,t)
     end do
     close(20)
  end do
  if(debug_level.ge.100) print *, "Success: write out data to the output file"
  if(debug_level.ge.100) print *, ""

  ! formats
111 format(f8.3,7f18.8)
222 format(a23,i5,f8.3,7f18.8)

  ! deallocate the allocated arrays in this program
  deallocate( time,x,z )
  if(debug_level.ge.100) print *, "Success: deallocate the allocated arrays"
  if(debug_level.ge.100) print *, ""

  if(debug_level.ge.100) print *, "All done."

contains
  
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! subroutine of check for netcdf I/O
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  subroutine check(status)
    integer, intent ( in) :: status
    if(status /= nf90_noerr) then 
       print *, trim(nf90_strerror(status))
       stop 2
    end if
  end subroutine check
  
end program ncedit_pdata
