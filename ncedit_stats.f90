!
! N C E D I T (stats)
!
! original program coded by Takashi Unuma, Kyoto Univ.
! Last modified: 2014/10/02
!

program ncedit_stats

  use netcdf
  
  implicit none

  integer :: tmax
  real, dimension(:), allocatable :: time_in, var_in, var_in_tmp
  real, dimension(:), allocatable :: time_out, var_out
  character(len=20) :: varname
  character(len=42) :: input, output
  integer :: debug_level

  ! local variables
  integer :: t, ncid, varid, tdimid

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
  allocate( time_in(tmax), var_in(tmax) )
  allocate( time_out(tmax), var_out(tmax) )
  allocate( var_in_tmp(tmax) )
  time_in(:)  = nan
  var_in(:)   = nan
  var_in_tmp(:) = nan
  time_out(:) = nan
  var_out(:)  = nan

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
  call check( nf90_get_var(ncid, tdimid, time_in) )
  if(debug_level.ge.100) print *, "Success: get the time coordinate"
  if(debug_level.ge.200) print *, " time(:)       = ", time_in
  if(debug_level.ge.100) print *, ""
  
  ! inquire and get var
  if(debug_level.ge.100) print *, "varname (",trim(varname),") is selected"
  select case (varname)
  case ('preeff')
     call check( nf90_inq_varid(ncid, 'train', varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.200) print *, " var_in(:)     = ", var_in

     call check( nf90_inq_varid(ncid, 'tcond', varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in_tmp) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.200) print *, " var_in_tmp(:) = ", var_in_tmp

  case default
     call check( nf90_inq_varid(ncid, varname, varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.200) print *, " var_in(:)     = ", var_in
  end select

  ! close netcdf file
  call check( nf90_close(ncid) )
  if(debug_level.ge.100) print *, "Success: close the netcdf data"
  if(debug_level.ge.100) print *, ""


  ! t array
  if(debug_level.ge.100) print *, "output time as time_out array"
  if(debug_level.ge.100) print *, " unit: [second] -> [hour]"
  do t = 1, tmax, 1
!     time_out(t) = time_in(t)/real(60.) ! uint: [second] -> [minute]
     time_out(t) = real( time_in(t)/dble(3600.) ) ! uint: [second] -> [hour]
  end do

  select case (varname)
  case ('qcbot','qctop')
     if(debug_level.ge.100) print *, " unit: [m] -> [km]"
     do t = 1, tmax, 1
        var_out(t) = var_in(t)/dble(1000.) ! unit: [m] -> [km]
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do

  case ('maxqv','maxqr')
     if(debug_level.ge.100) print *, " unit: [kg kg^-1] -> [g kg^-1]"
     do t = 1, tmax, 1
        var_out(t) = var_in(t)*real(1000.) ! unit: [kg kg^-1] -> [g kg^-1]
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do

  case ('rhmin','rhmax')
     if(debug_level.ge.100) print *, " unit: [%]"
     do t = 1, tmax, 1
        var_out(t) = var_in(t)*real(100.) ! unit: [%]
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do

  case ('tmois')
     if(debug_level.ge.100) print *, "output as relative error"
     if(debug_level.ge.100) print *, " unit: [%] -> [* 10^-4 %]"
     do t = 1, tmax, 1
!        var_out(t) = ((var_in(t)-var_in(1))/dble(var_in(1)))*real(10000.) ! unit: [kg] -> 10^9 [kg]
        var_out(t) = var_in(t)-var_in(1)
        if(var_out(t).lt.1.0d3) then
           var_out(t) = 0.
        else
           var_out(t) = ((var_in(t)-var_in(1))/dble(var_in(1)))*real(10000.) ! unit: [kg] -> [* 10^-4 kg]
        end if
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do

  case ('tmass')
     if(debug_level.ge.100) print *, "output as relative error"
     if(debug_level.ge.100) print *, " unit: [%] -> [* 10^-4 %]"
     do t = 1, tmax, 1
!        var_out(t) = ((var_in(t)-var_in(1))/dble(var_in(1)))*real(10000.) ! unit: [kg] -> [* 10^-4 kg]
        var_out(t) = var_in(t)-var_in(1)
        if(var_out(t).lt.1.0d3) then
           var_out(t) = 0.
        else
           var_out(t) = ((var_in(t)-var_in(1))/dble(var_in(1)))*real(10000.) ! unit: [kg] -> [* 10^-4 kg]
        end if
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do

  case ('et')
     if(debug_level.ge.100) print *, "output as relative error"
     if(debug_level.ge.100) print *, " unit: [%] -> [* 10^-4 %]"
!     if(debug_level.ge.100) print *, " unit: [kg] -> 10^16 [kg]"
     do t = 1, tmax, 1
!        var_out(t) = ((var_in(t)-var_in(1))/dble(var_in(1)))*real(10000.) ! unit: [kg] -> [* 10^-4 kg]
        var_out(t) = var_in(t)-var_in(1)
        if(var_out(t).lt.1.0d3) then
           var_out(t) = 0.
        else
           var_out(t) = ((var_in(t)-var_in(1))/dble(var_in(1)))*real(10000.) ! unit: [kg] -> [* 10^-4 kg]
        end if
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do

  case ('ek')
     if(debug_level.ge.100) print *, "output as relative error"
     if(debug_level.ge.100) print *, " unit: [%] -> [* 10^-4 %]"
!     if(debug_level.ge.100) print *, " unit: [kg] -> 10^12 [kg]"
     do t = 1, tmax, 1
!        var_out(t) = var_in(t)/real(1.0d12) ! unit: [kg] -> 10^12 [kg]
!        var_out(t) = ((var_in(t)-var_in(1))/dble(var_in(1)))*real(10000.) ! unit: [kg] -> [* 10^-4 kg]
        var_out(t) = var_in(t)-var_in(1)
        if(var_out(t).lt.1.0d3) then
           var_out(t) = 0.
        else
           var_out(t) = ((var_in(t)-var_in(1))/dble(var_in(1)))*real(10000.) ! unit: [kg] -> [* 10^-4 kg]
        end if
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do

  case ('ei')
     if(debug_level.ge.100) print *, "output as relative error"
     if(debug_level.ge.100) print *, " unit: [%] -> [* 10^-4 %]"
!     if(debug_level.ge.100) print *, " unit: [kg] -> 10^16 [kg]"
     do t = 1, tmax, 1
!        var_out(t) = var_in(t)/real(1.0d16) ! unit: [kg] -> 10^16 [kg]
!        var_out(t) = ((var_in(t)-var_in(1))/dble(var_in(1)))*real(10000.) ! unit: [kg] -> [* 10^-4 kg]
        var_out(t) = var_in(t)-var_in(1)
        if(var_out(t).lt.1.0d3) then
           var_out(t) = 0.
        else
           var_out(t) = ((var_in(t)-var_in(1))/dble(var_in(1)))*real(10000.) ! unit: [kg] -> [* 10^-4 kg]
        end if
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do

  case ('ep')
     if(debug_level.ge.100) print *, "output as relative error"
     if(debug_level.ge.100) print *, " unit: [%] -> [* 10^-4 %]"
!     if(debug_level.ge.100) print *, " unit: [kg] -> 10^16 [kg]"
     do t = 1, tmax, 1
!        var_out(t) = var_in(t)/real(1.0d16) ! unit: [kg] -> 10^16 [kg]
!        var_out(t) = ((var_in(t)-var_in(1))/dble(var_in(1)))*real(10000.) ! unit: [kg] -> [* 10^-4 kg]
        var_out(t) = var_in(t)-var_in(1)
        if(var_out(t).lt.1.0d3) then
           var_out(t) = 0.
        else
           var_out(t) = ((var_in(t)-var_in(1))/dble(var_in(1)))*real(10000.) ! unit: [kg] -> [* 10^-4 kg]
        end if
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do

  case ('le')
     if(debug_level.ge.100) print *, "output as relative error"
     if(debug_level.ge.100) print *, " unit: [%] -> [* 10^-4 %]"
!     if(debug_level.ge.100) print *, " unit: [kg] -> 10^13 [kg]"
     do t = 1, tmax, 1
!        var_out(t) = var_in(t)/real(1.0d13) ! unit: [kg] -> 10^13 [kg]
        var_out(t) = var_in(t)-var_in(1)
        if(var_out(t).lt.1.0d3) then
           var_out(t) = 0.
        else
           var_out(t) = ((var_in(t)-var_in(1))/dble(var_in(1)))*real(10000.) ! unit: [kg] -> [* 10^-4 kg]
        end if
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do

  case ('train')
     if(debug_level.ge.100) print *, " unit: [kg] -> [* 10^10 kg]"
     do t = 1, tmax, 1
        var_out(t) = var_in(t)/real(1.0d10)
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do

  case ('tcond')
     if(debug_level.ge.100) print *, " unit: [kg] -> [* 10^10 kg]"
     do t = 1, tmax, 1
        var_out(t) = var_in(t)/real(1.0d10)
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do

  case ('tevar')
     if(debug_level.ge.100) print *, " unit: [kg] -> [* 10^10 kg]"
     do t = 1, tmax, 1
        var_out(t) = var_in(t)/real(1.0d10)
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do

  case ('preeff')
     if(debug_level.ge.100) print *, "output as Precip. efficiency [%]"
     do t = 1, tmax, 1
        ! definition: Precip. dfficiency = total amount of rainfall / total amount of condensation
        if ( (var_in(t).gt.0.).and.(var_in_tmp(t).gt.0.) ) then
           var_out(t) = (var_in(t)/var_in_tmp(t))*real(1.0d2)
        else
           var_out(t) = 0.
        end if
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do

  case default
     do t = 1, tmax, 1
        var_out(t) = real(var_in(t))
        if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
     end do
     
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
     write(20,111) time_out(t), var_out(t)
     if(debug_level.ge.200) print 222, "t,time_out,var_out = ",t,time_out(t),var_out(t)
  end do
  if(debug_level.ge.100) print *, "Success: write out data to the output file"

  ! close the file
  close(unit=20)
  if(debug_level.ge.100) print *, "Success: close the output file"
  if(debug_level.ge.100) print *, ""

  ! formats
111 format(f8.3,f18.8)
222 format(a22,i5,f8.3,f18.8)

  ! deallocate the allocated arrays in this program
  deallocate( time_in,time_out,var_in,var_out )
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
  
end program ncedit_stats
