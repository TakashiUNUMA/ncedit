!
! Program of ncedit_status.f90
! original program coded by Takashi Unuma, Kyoto Univ.
! Last modified: 2014/06/14
!

program ncedit_stats

  use netcdf
  
  implicit none

  integer :: ncid, varid, tdimid, tmax
  real, dimension(:), allocatable :: time, var_in
  character(len=20) :: varname
  character(len=42) :: input, output
  integer :: debug_level

  ! local variables
  integer :: t

  ! undefined value
  real :: nan = -999.
!  real :: nan = -2147483648.

  
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Input parameters from namelist
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  namelist /param/ tmax,varname,input,output,debug_level
  open(unit=10,file='namelist.ncedit_stats',form='formatted',status='old',access='sequential')
  read(10,nml=param)
  close(unit=10)

  if(debug_level.ge.100) print '(a30)',     "----- values on namelist -----"
  if(debug_level.ge.100) print '(a18,i6)',  " tmax          = ", tmax
  if(debug_level.ge.100) print '(a18,a)',   " varname       = ", trim(varname)
  if(debug_level.ge.100) print '(a18,a)',   " input         = ", trim(input)
  if(debug_level.ge.100) print '(a18,a)',   " output        = ", trim(output)
  if(debug_level.ge.100) print '(a18,i6)',  " debug_level   = ", debug_level
  if(debug_level.ge.100) print '(a30)',     "----- values on namelist -----"
  if(debug_level.ge.100) print *, ""

  ! allocate arrays
  allocate( time(tmax), var_in(tmax) )
  time(:)   = nan
  var_in(:) = nan

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
  
  ! inquire and get var
  if(debug_level.ge.100) print *, "varname of ",trim(varname)," is selected"
  call check( nf90_inq_varid(ncid, varname, varid) )
  if(debug_level.ge.100) print *, "Success: inquire the varid"
  if(debug_level.ge.200) print *, " varid         = ", varid
  call check( nf90_get_var(ncid, varid, var_in) )
  if(debug_level.ge.100) print *, "Success: get the var array"
  if(debug_level.ge.100) print *, " var_in(:)     = ", var_in

  ! close netcdf file
  call check( nf90_close(ncid) )
  if(debug_level.ge.100) print *, "Success: close the netcdf data"
  if(debug_level.ge.100) print *, ""


  ! x-t array
  if(debug_level.ge.100) print *, "x-t array"
  do t = 1, tmax, 1
!     time(t) = time(t)/real(60.) ! uint: [second] -> [minute]
     time(t) = real( time(t)/dble(3600.) ) ! uint: [second] -> [hour]
  end do

  select case (varname)
  case ('rain')
     if(debug_level.ge.100) print *, " unit: [cm] -> [mm]"
     do t = 1, tmax, 1
        var_in(t) = var_in(t)*real(10.) ! unit: [cm] -> [mm]
        if(debug_level.ge.200) print *, "t,time,var_in = ",t,time(t),var_in(t)
     end do

  case ('qcbot','qctop')
     if(debug_level.ge.100) print *, " unit: [m] -> [km]"
     do t = 1, tmax, 1
        var_in(t) = var_in(t)/dble(1000.) ! unit: [m] -> [km]
        if(debug_level.ge.200) print 222, "t,time,var_in = ",t,time(t),var_in(t)
     end do

  case ('rhmin','rhmax')
     if(debug_level.ge.100) print *, " unit: [%]"
     do t = 1, tmax, 1
        var_in(t) = var_in(t)*real(100.) ! unit: [%]
        if(debug_level.ge.200) print 222, "t,time,var_in = ",t,time(t),var_in(t)
     end do

  ! case default
  !    do t = 1, tmax, 1
  !       var_out(i,t) = var_in(i,1,t,1)
  !    end do
  !    if(debug_level.ge.200) print *, "t,iy,var_out = ",t,iy(t),var_out(xselect,t)
  end select
  if(debug_level.ge.100) print *, ""


  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Output 1D file
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! create the file
  open(unit=20,file=output)
  if(debug_level.ge.100) print *, "Success: open the output file as ",trim(output)

  ! writeout data to output file
  do t = 1, tmax, 1
     write(20,111) time(t), var_in(t)
     if(debug_level.ge.200) print 222, "t,time,var_in = ",t,time(t),var_in(t)
  end do
  if(debug_level.ge.100) print *, "Success: write out data to the output file"

  ! close the file
  close(unit=20)
  if(debug_level.ge.100) print *, "Success: close the output file"
  if(debug_level.ge.100) print *, ""
  if(debug_level.ge.100) print *, "All done."

  ! formats
111 format(f8.3,f25.8)
222 format(a17,i5,f8.3,f25.8)

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
  
end program ncedit_stats
