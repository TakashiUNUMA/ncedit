!
! N C E D I T
!
! original program coded by Takashi Unuma, Kyoto Univ.
! last modified: 2014/08/10
!

program ncedit

  use netcdf
  
  implicit none

  ! I/O values and arrays
  integer :: flag, xselect, yselect, zselect, tselect
  integer :: interp_x, interp_y
  real :: dx, dy, angle
  real, dimension(:),       allocatable :: x, y, z, time_in
  real, dimension(:,:,:,:), allocatable :: var_in
  real, dimension(:),       allocatable :: ix, iy
  real, dimension(:,:),     allocatable :: var_out
  character(len=20) :: varname, interp_method, output_type
  character(len=42) :: input, output
  integer :: deflate_level, debug_level

  ! local variables
  integer :: i, j, k, t, ipoint, nx, ny
  integer :: imax, jmax, kmax, tmax
  integer :: ncid, varid, xdimid, ydimid, zdimid, tdimid, xvarid, yvarid
  integer, dimension(2) :: ostart, ocount, dimids, chunks
  integer, dimension(4) :: istart, icount
  integer, dimension(2) :: ipoints
  real :: tmp0
  real :: gbcape,gbcin,gblclp,gblfcp,gblnbp,gblclz,gblfcz,gblnbz
  real, dimension(:),       allocatable :: yy
  real, dimension(:,:),     allocatable :: tmp, tmp1, tmp2, tmp3, tmp4, tmp5
  real, dimension(:,:,:),   allocatable :: tmpi, tmpi1, tmpi2, tmpi3
  real, dimension(:,:,:,:), allocatable :: var_inc
  real, dimension(:,:,:),   allocatable :: tmpc1, tmpc2, tmpc3
  real, parameter :: pi = 3.14159265
  real, parameter :: t0 = 273.15

  ! undefined value
!  real :: nan = (/ Z'7fffffff' /) ! not work with gfortran
!  real :: nan = -999.
  real :: nan = -2147483648.

  
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Input parameters from namelist
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  namelist /param/ imax,jmax,kmax,tmax,varname,input, &
                   xselect,yselect,zselect,tselect,   & 
                   output,output_type,flag,           &
                   nx,ny,dx,dy,angle,                 &
                   interp_x,interp_y,interp_method,   &
                   deflate_level,debug_level
  open(unit=10,file='namelist.ncedit',form='formatted',status='old',access='sequential')
  read(10,nml=param)
  close(unit=10)

  if(debug_level.ge.100) print '(a30)',     "----- values on namelist -----"
  if(debug_level.ge.100) print '(a18,i6)',  " imax          = ", imax
  if(debug_level.ge.100) print '(a18,i6)',  " jmax          = ", jmax
  if(debug_level.ge.100) print '(a18,i6)',  " kmax          = ", kmax
  if(debug_level.ge.100) print '(a18,i6)',  " tmax          = ", tmax
  if(debug_level.ge.100) print '(a18,a)',   " varname       = ", trim(varname)
  if(debug_level.ge.100) print '(a18,a)',   " input         = ", trim(input)
  if(debug_level.ge.100) print '(a18,i6)',  " xselect       = ", xselect
  if(debug_level.ge.100) print '(a18,i6)',  " yselect       = ", yselect
  if(debug_level.ge.100) print '(a18,i6)',  " zselect       = ", zselect
  if(debug_level.ge.100) print '(a18,i6)',  " tselect       = ", tselect
  if(debug_level.ge.100) print '(a18,a)',   " output        = ", trim(output)
  if(debug_level.ge.100) print '(a18,a)',   " output_type   = ", trim(output_type)
  if(debug_level.ge.100) print '(a18,i6)',  " flag          = ", flag
  if(debug_level.ge.100) print '(a18,i6)',  "  nx           = ", nx
  if(debug_level.ge.100) print '(a18,i6)',  "  ny           = ", ny
  if(debug_level.ge.100) print '(a18,f6.3)',"  dx           = ", dx
  if(debug_level.ge.100) print '(a18,f6.3)',"  dy           = ", dy
  if(debug_level.ge.100) print '(a18,f6.2)',"  angle        = ", angle
  if(debug_level.ge.100) print '(a18,i6)',  " interp_x      = ", interp_x
  if(debug_level.ge.100) print '(a18,i6)',  " interp_y      = ", interp_y
  if(debug_level.ge.100) print '(a18,a)',   " interp_method = ", trim(interp_method)
  if(debug_level.ge.100) print '(a18,i6)',  " deflate_level = ", deflate_level
  if(debug_level.ge.100) print '(a18,i6)',  " debug_level   = ", debug_level
  if(debug_level.ge.100) print '(a30)',     "----- values on namelist -----"
  if(debug_level.ge.100) print *, ""


  ! check error
  if(xselect.gt.imax) then
     print *, " Error: xselect exceeds imax "
     print *, "  xselect = ",xselect
     print *, "  imax    = ",imax
     print *, "Please reduce the value of xselect"
     stop
  end if
  if(yselect.gt.jmax) then
     print *, " Error: yselect exceeds jmax "
     print *, "  yselect = ",yselect
     print *, "  jmax    = ",jmax
     print *, "Please reduce the value of yselect"
     stop
  end if
  if(zselect.gt.kmax) then
     print *, " Error: zselect exceeds kmax "
     print *, "  zselect = ",zselect
     print *, "  kmax    = ",kmax
     print *, "Please reduce the value of zselect"
     stop
  end if
  if(tselect.gt.tmax) then
     print *, " Error: tselect exceeds tmax "
     print *, "  tselect = ",tselect
     print *, "  tmax    = ",tmax
     print *, "Please reduce the value of tselect"
     stop
  end if

  if( (flag.eq.1).or.(flag.eq.2).or.(flag.eq.3) ) then
     if( (interp_x.ne.1).and.(nx.ne.imax) ) then
        print *, " Error: nx .ne. imax "
        print *, "  nx   = ",nx
        print *, "  imax = ",imax
        stop
     end if
  end if

  if( flag.eq.1 ) then
     if( (interp_y.ne.1).and.(ny.ne.jmax) ) then
        print *, " Error: ny .ne. jmax "
        print *, "  ny   = ",ny
        print *, "  jmax = ",jmax
        stop
     end if
  else if( flag.eq.2 ) then
     if( (interp_y.ne.1).and.(ny.ne.kmax) ) then
        print *, " Error: ny .ne. kmax "
        print *, "  ny   = ",ny
        print *, "  kmax = ",kmax
        stop
     end if
  else if( flag.eq.3 ) then
     if( (interp_y.ne.1).and.(ny.ne.tmax) ) then
        print *, " Error: ny .ne. tmax "
        print *, "  ny   = ",ny
        print *, "  tmax = ",tmax
        stop
     end if
  end if
  

  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Input 4D file
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! open the original netcdf file
  call check( nf90_open(input, nf90_nowrite, ncid) )
  if(debug_level.ge.100) print *, "Success: open the file"
  if(debug_level.ge.200) print *, " ncid          = ", ncid

  ! inquire and get x coordinate
  call check( nf90_inq_varid(ncid, 'ni', xdimid) )
  if(debug_level.ge.100) print *, "Success: inquire the xdimid"
  if(debug_level.ge.200) print *, " xdimid        = ", xdimid
!  call check( nf90_inquire_dimension(ncid, xdimid, len = imax) )
!  if(debug_level.ge.100) print *, "Success: inquire the xdimid"
!  if(debug_level.ge.200) print *, "  imax         = ", imax
  allocate( x(imax) )
  call check( nf90_get_var(ncid, xdimid, x) )
  if(debug_level.ge.100) print *, "Success: get the x coordinate"
  if(debug_level.ge.200) print *, " x(:)          = ", x

  ! inquire and get y coordinate
  call check( nf90_inq_varid(ncid, 'nj', ydimid) )
  if(debug_level.ge.100) print *, "Success: inquire the ydimid"
  if(debug_level.ge.200) print *, " ydimid        = ", ydimid
!  call check( nf90_inquire_dimension(ncid, ydimid, len = jmax) )
!  if(debug_level.ge.100) print *, "Success: inquire the ydimid"
!  if(debug_level.ge.200) print *, "  jmax         = ", jmax
  allocate( y(jmax) )
  call check( nf90_get_var(ncid, ydimid, y) )
  if(debug_level.ge.100) print *, "Success: get the y coordinate"
  if(debug_level.ge.200) print *, " y(:)          = ", y

  ! inquire and get z coordinate
  call check( nf90_inq_varid(ncid, 'nk', zdimid) )
  if(debug_level.ge.100) print *, "Success: inquire the zdimid"
  if(debug_level.ge.200) print *, " zdimid        = ", zdimid
!  call check( nf90_inquire_dimension(ncid, zdimid, len = kmax) )
!  if(debug_level.ge.100) print *, "Success: inquire the zdimid"
!  if(debug_level.ge.200) print *, "  kmax         = ", kmax
  allocate( z(kmax) )
  call check( nf90_get_var(ncid, zdimid, z) )
  if(debug_level.ge.100) print *, "Success: get the z coordinate"
  if(debug_level.ge.200) print *, " z(:)          = ", z

  ! inquire and get time coordinate
  call check( nf90_inq_varid(ncid, 'time', tdimid) )
  if(debug_level.ge.100) print *, "Success: inquire the tdimid"
  if(debug_level.ge.200) print *, " tdimid        = ", tdimid
!  call check( nf90_inquire_dimension(ncid, tdimid, len = tmax) )
!  if(debug_level.ge.100) print *, "Success: inquire the tdimid"
!  if(debug_level.ge.200) print *, "  tmax         = ", tmax
  allocate( time_in(tmax) )
  call check( nf90_get_var(ncid, tdimid, time_in) )
  if(debug_level.ge.100) print *, "Success: get the time coordinate"
  if(debug_level.ge.200) print *, " time_in(:)    = ", time_in
  if(debug_level.ge.100) print *, ""


  ! allocate arrays
  select case (flag)
  case (1)
     ! x-y
     select case (varname)
     case ('rain','cpc','cref')
        allocate( var_in(imax,jmax,1,1) )
        istart = (/ 1, 1, tselect, 1 /)
        icount = (/ imax, jmax, 1, 1 /)
     case default
        allocate( var_in(imax,jmax,1,1) )
        istart = (/ 1, 1, zselect, tselect /)
        icount = (/ imax, jmax, 1, 1 /)
     end select
     allocate( tmp(imax,jmax) )
     allocate( tmp1(imax,jmax),tmp2(imax,jmax),tmp3(imax,jmax) )
     tmp(:,:) = nan
     tmp1(:,:) = nan
     tmp2(:,:) = nan
     tmp3(:,:) = nan
  case (2)
     ! x-z
     allocate( var_in(imax,1,kmax,1) )
     istart = (/ 1, yselect, 1, tselect /)
     icount = (/ imax, 1, kmax, 1 /)
     allocate( tmp(imax,kmax) )
     allocate( tmp1(imax,kmax),tmp2(imax,kmax),tmp3(imax,kmax) )
     allocate( tmp4(imax,kmax),tmp5(imax,kmax) )
     tmp(:,:) = nan
     tmp1(:,:) = nan
     tmp2(:,:) = nan
     tmp3(:,:) = nan
     tmp4(:,:) = nan
     tmp5(:,:) = nan
  case (3)
     ! x-t
     allocate( var_in(imax,1,tmax,1) )
     istart = (/ 1, yselect, 1, 1 /)
     icount = (/ imax, 1, tmax, 1 /)
     allocate( tmp(imax,tmax) )
     tmp(1:imax,1:tmax) = nan
     ! for calculating CAPE using getcape
     allocate( var_inc(imax,1,kmax,tmax) )
     istart = (/ 1, yselect, 1, 1 /)
     icount = (/ imax, 1, kmax, tmax /)
     allocate( tmpc1(imax,kmax,tmax),tmpc2(imax,kmax,tmax),tmpc3(imax,kmax,tmax) )
     tmpc1(1:imax,1:kmax,1:tmax) = nan
     tmpc2(1:imax,1:kmax,1:tmax) = nan
     tmpc3(1:imax,1:kmax,1:tmax) = nan
  case (4)
     ! time series of a value
     allocate( var_in(imax,jmax,tmax,1) )
     istart = (/ 1, 1, 1, 1 /)
     icount = (/ imax, jmax, tmax, 1 /)
     allocate( tmp(tmax,1) )
     tmp(1:tmax,1) = nan
  case (5)
     ! arbitrary cross-section specifying (xselect,yselect) and angle
     allocate( var_in(imax,jmax,kmax,1) )
     istart = (/ 1, 1, 1, tselect /)
     icount = (/ imax, jmax, kmax, 1 /)
     allocate( tmpi(imax,jmax,kmax) )
     allocate( tmpi1(imax,jmax,kmax),tmpi2(imax,jmax,kmax),tmpi3(imax,jmax,kmax) )
     tmpi(:,:,:) = nan
     tmpi1(:,:,:) = nan
     tmpi2(:,:,:) = nan
     tmpi3(:,:,:) = nan
     allocate( tmp(imax,kmax) )
     tmp(:,:) = nan
  end select
  var_in(:,:,:,:) = nan

  
  ! inquire and get var
  if(debug_level.ge.100) print *, "varname of ",trim(varname)," is selected"
  select case (varname)
  case ('water')
     ! for all water (qc+qr+qi+qc+qg) on the microphysics processes
     ! *** this section work with flag = 2 ***
     if(flag.ne.2) then
        print *, " flag = ", flag, "is under construction for now..."
        stop
     end if
     ! --- read qc
     call check( nf90_inq_varid(ncid, "qc", varid) )
     if(debug_level.ge.100) print *, " Success: inquire the varid"
     if(debug_level.ge.200) print *, "  varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, " Success: get the var array"
     if(debug_level.ge.200) print *, "  var_in(1,1,1,1) = ", var_in(1,1,1,1)
     select case (flag)
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp1(i,k) = var_in(i,1,k,1)
        end do
        end do
     end select
     ! --- read qr
     call check( nf90_inq_varid(ncid, "qr", varid) )
     if(debug_level.ge.100) print *, " Success: inquire the varid"
     if(debug_level.ge.200) print *, "  varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, " Success: get the var array"
     if(debug_level.ge.200) print *, "  var_in(1,1,1,1) = ", var_in(1,1,1,1)
     select case (flag)
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp2(i,k) = var_in(i,1,k,1)
        end do
        end do
     end select
     ! --- read qi
     call check( nf90_inq_varid(ncid, "qi", varid) )
     if(debug_level.ge.100) print *, " Success: inquire the varid"
     if(debug_level.ge.200) print *, "  varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, " Success: get the var array"
     if(debug_level.ge.200) print *, "  var_in(1,1,1,1) = ", var_in(1,1,1,1)
     select case (flag)
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp3(i,k) = var_in(i,1,k,1)
        end do
        end do
     end select
     ! --- read qs
     call check( nf90_inq_varid(ncid, "qs", varid) )
     if(debug_level.ge.100) print *, " Success: inquire the varid"
     if(debug_level.ge.200) print *, "  varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, " Success: get the var array"
     if(debug_level.ge.200) print *, "  var_in(1,1,1,1) = ", var_in(1,1,1,1)
     select case (flag)
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp4(i,k) = var_in(i,1,k,1)
        end do
        end do
     end select
     ! --- read qg
     call check( nf90_inq_varid(ncid, "qg", varid) )
     if(debug_level.ge.100) print *, " Success: inquire the varid"
     if(debug_level.ge.200) print *, "  varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, " Success: get the var array"
     if(debug_level.ge.200) print *, "  var_in(1,1,1,1) = ", var_in(1,1,1,1)
     select case (flag)
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp5(i,k) = var_in(i,1,k,1)
        end do
        end do
     end select
     ! --- calculate water = qc + qr + qi + qs + qg [kg/kg]
     select case (flag)
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp(i,k) = tmp1(i,k) + tmp2(i,k) + tmp3(i,k) + tmp4(i,k) + tmp5(i,k)
        end do
        end do
     end select

  case ('thetae')
     ! equivalent potential temperature [K]
     ! *** this section work with flag = 1, 2 or 5 ***
     if( (flag.ne.1).or.(flag.ne.2).or.(flag.ne.5) ) then
        print *, " flag = ", flag, "is under construction for now..."
        stop
     end if
     ! --- read prs [Pa]
     call check( nf90_inq_varid(ncid, "prs", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     select case (flag)
     case (1)
!$omp parallel do default(shared) &
!$omp private(i,j)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp1(i,j) = var_in(i,j,1,1)
        end do
        end do
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp1(i,k) = var_in(i,1,k,1)
        end do
        end do
     case (5)
!$omp parallel do default(shared) &
!$omp private(i,j,k)
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi1(i,j,k) = var_in(i,j,k,1)
        end do
        end do
        end do
     end select
     ! --- read theta [K]
     call check( nf90_inq_varid(ncid, "th", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     select case (flag)
     case (1)
!$omp parallel do default(shared) &
!$omp private(i,j)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp2(i,j) = var_in(i,j,1,1)
        end do
        end do
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp2(i,k) = var_in(i,1,k,1)
        end do
        end do
     case (5)
!$omp parallel do default(shared) &
!$omp private(i,j,k)
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi2(i,j,k) = var_in(i,j,k,1)
        end do
        end do
        end do
     end select
     ! --- read qv [kg/kg]
     call check( nf90_inq_varid(ncid, "qv", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     select case (flag)
     case (1)
!$omp parallel do default(shared) &
!$omp private(i,j,tmp0)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp3(i,j) = var_in(i,j,1,1)
           tmp0 = thetaP_2_T( tmp2(i,j), tmp1(i,j) )
           tmp(i,j) = thetae_Bolton( tmp0, tmp3(i,j), tmp1(i,j) )
        end do
        end do
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k,tmp0)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp3(i,k) = var_in(i,1,k,1)
           tmp0 = thetaP_2_T( tmp2(i,k), tmp1(i,k) )
           tmp(i,k) = thetae_Bolton( tmp0, tmp3(i,k), tmp1(i,k) )
        end do
        end do
     case (5)
!$omp parallel do default(shared) &
!$omp private(i,j,k,tmp0)
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi3(i,j,k) = var_in(i,j,k,1)
           tmp0 = thetaP_2_T( tmpi2(i,j,k), tmpi1(i,j,k) )
           tmpi(i,j,k) = thetae_Bolton( tmp0, tmpi3(i,j,k), tmpi1(i,j,k) )
        end do
        end do
        end do
     end select

  case ('cape','cin','lfc')
     ! convective available potential energy [J kg-1]
     ! convective inhibition [J kg-1]
     ! *** this section work with flag = 3 ***
     if(flag.ne.3) then
        print *, " flag = ", flag, "is under construction for now..."
        stop
     end if
     ! --- read prs [Pa]
     call check( nf90_inq_varid(ncid, "prs", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_inc, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (prs)"
     if(debug_level.ge.200) print *, " var_inc(1,1,1,1) = ", var_inc(1,1,1,1)
     select case (flag)
     case (3)
!$omp parallel do default(shared) &
!$omp private(i,k,t)
        do t = 1, tmax, 1
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmpc1(i,k,t) = var_inc(i,1,k,t)*real(0.01) ! unit: [hPa]
        end do
        end do
        end do
     end select
     ! --- read theta [K]
     call check( nf90_inq_varid(ncid, "th", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_inc, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (th)"
     if(debug_level.ge.200) print *, " var_inc(1,1,1,1) = ", var_inc(1,1,1,1)
     select case (flag)
     case (3)
!$omp parallel do default(shared) &
!$omp private(i,k,t,tmp0)
        do t = 1, tmax, 1
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp0 = var_inc(i,1,k,t)
           ! calculate temperature [degree C] using theta [K] and pressure [Pa]
           tmpc2(i,k,t) = thetaP_2_T( tmp0, tmpc1(i,k,t)*100. ) - t0 ! unit: [degree C]
        end do
        end do
        end do
     end select
     ! --- read qv [kg/kg]
     call check( nf90_inq_varid(ncid, "qv", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_inc, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (qv)"
     if(debug_level.ge.200) print *, " var_inc(1,1,1,1) = ", var_inc(1,1,1,1)
     select case (flag)
     case (3)
!$omp parallel do default(shared) &
!$omp private(i,k,t)
        do t = 1, tmax, 1
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmpc3(i,k,t) = var_inc(i,1,k,t)
        end do
        end do
        end do
        ! calculate CAPE, CIN or LFC
        select case (varname)
        case ('cape')
           if(debug_level.ge.100) print *, " Now calculating CAPE [J kg-1]"
!$omp parallel do default(shared) &
!$omp private(i,t)
           do t = 1, tmax, 1
           do i = 1, imax, 1
              CALL getcape( 2,kmax,tmpc1(i,:,t),tmpc2(i,:,t),tmpc3(i,:,t),tmp(i,t), &
                   gbcin,gblclp,gblfcp,gblnbp,gblclz,gblfcz,gblnbz,debug_level,1 )
           end do
           if(debug_level.ge.200) print *, "  t,cape = ",t,tmp(xselect,t)
           end do
           !
        case ('cin')
           if(debug_level.ge.100) print *, " Now calculating CIN [J kg-1]"
!$omp parallel do default(shared) &
!$omp private(i,t)
           do t = 1, tmax, 1
           do i = 1, imax, 1
              CALL getcape( 2,kmax,tmpc1(i,:,t),tmpc2(i,:,t),tmpc3(i,:,t),gbcape,   &
                   tmp(i,t),gblclp,gblfcp,gblnbp,gblclz,gblfcz,gblnbz,debug_level,1 )
           end do
           if(debug_level.ge.200) print *, "  t,cin = ",t,tmp(xselect,t)
           end do
           !
        case ('lfc')
           if(debug_level.ge.100) print *, " Now calculating LFC [m]"
!           if(debug_level.ge.100) print *, " Now calculating LFC [hPa]"
!$omp parallel do default(shared) &
!$omp private(i,t)
           do t = 1, tmax, 1
           do i = 1, imax, 1
              CALL getcape( 2,kmax,tmpc1(i,:,t),tmpc2(i,:,t),tmpc3(i,:,t),gbcape,   &
                   gbcin,gblclp,gblfcp,gblnbp,gblclz,tmp(i,t),gblnbz,debug_level,1  ) ! unit: [m]
!                   gbcin,gblclp,tmp(i,t),gblnbp,gblclz,gblfcz,gblnbz,debug_level,1  ) ! unit: [Pa]
           end do
           if(debug_level.ge.200) print *, "  t,lfc = ",t,tmp(xselect,t)
           end do
        end select
        if(debug_level.ge.100) print *, " Done"
     end select

  case ('rws')
     ! wind speed that projected on the specified vertical cross section [m/s]
     ! *** this section work with flag = 5 ***
     if(flag.ne.5) then
        print *, " flag = ", flag, "is under construction for now..."
        stop
     end if
     ! --- read uinterp [m/s]
     call check( nf90_inq_varid(ncid, "uinterp", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     select case (flag)
     case (5)
!$omp parallel do default(shared) &
!$omp private(i,j,k)
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi1(i,j,k) = var_in(i,j,k,1)
        end do
        end do
        end do
     end select
     ! --- read vinterp [m/s]
     call check( nf90_inq_varid(ncid, "vinterp", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     select case (flag)
     case (5)
!$omp parallel do default(shared) &
!$omp private(i,j,k,tmp0)
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi2(i,j,k) = var_in(i,j,k,1)
           tmp0 = atan2(tmpi1(i,j,k),tmpi2(i,j,k))
           tmpi(i,j,k) = sqrt(tmpi1(i,j,k)**2 + tmpi2(i,j,k)**2)*cos(tmp0-(angle*(pi/180.)))
        end do
        end do
        end do
     end select

  case ('maxrain')
     ! maximum rain rate [mm h^-1]
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, " flag = ", flag, "is under construction for now..."
        stop
     end if
     ! --- read rain [cm]
     call check( nf90_inq_varid(ncid, "rain", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     select case (flag)
     case (4)
        ! for t = 1
        tmp(1,1) = 0.0
!$omp parallel do default(shared) &
!$omp private(i,j,t,tmp0)
        ! for t >= 2
        do t = 2, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp0 = (var_in(i,j,t,1) - var_in(i,j,t-1,1))*real(600.) ! unit [cm] -> [mm/h]
           tmp(t,1) = max(tmp(t,1),tmp0)
        end do
        end do
        end do
     end select

  case default
     ! the others
     ! *** this section work with flag = 1, 2, 3, 4, and 5 ***
     call check( nf90_inq_varid(ncid, varname, varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)

  end select

  ! close netcdf file
  call check( nf90_close(ncid) )
  if(debug_level.ge.100) print *, "Success: close the netcdf data"
  if(debug_level.ge.100) print *, ""


  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! make arrays
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  if(flag.eq.4) then
     !ccccccccccccccccccccccccccccccccccccccccccccccccc
     ! Output 1D file
     !ccccccccccccccccccccccccccccccccccccccccccccccccc
     ! create the file
     open(unit=20,file=output)
     if(debug_level.ge.100) print *, "Success: open the output file as ",trim(output)
     
     ! writeout data to output file
     do t = 1, tmax, 1
        write(20,111) real(time_in(t)/dble(60.)), tmp(t,1)
        if(debug_level.ge.200) print 222, "t,time,var = ",t,real(time_in(t)/dble(60.)), tmp(t,1)
     end do
     if(debug_level.ge.100) print *, "Success: write out data to the output file"
     
     ! close the file
     close(unit=20)
     if(debug_level.ge.100) print *, "Success: close the output file"
     if(debug_level.ge.100) print *, ""

     ! formats
111  format(f8.3,f18.8)
222  format(a22,i5,f8.3,f18.8)

     ! deallocate the allocated arrays in this program
     deallocate( x,y,z,time_in,var_in,tmp )
     if(debug_level.ge.100) print *, "Success: deallocate the allocated arrays"
     if(debug_level.ge.100) print *, ""
     
  else
     !ccccccccccccccccccccccccccccccccccccccccccccccccc
     ! Output 2D netcdf file
     !ccccccccccccccccccccccccccccccccccccccccccccccccc
     allocate( ix(nx), iy(ny) )
     allocate( var_out(nx,ny) )
     var_out(:,:) = nan

     if(flag.eq.1) then
        ! x-y
        if(debug_level.ge.100) print *, "x-y array"
        ! select one of the 2D array
        select case (varname)
        case ('thetae')
           print *, "The tmp array has already allocated"
        case ('rain')
           if(debug_level.ge.100) print *, " unit: [cm] -> [mm]"
           tmp(:,:) = var_in(:,:,1,1)*real(10.) ! unit: [cm] -> [mm]
        case default
           tmp(:,:) = var_in(:,:,1,1)
        end select
        if(debug_level.ge.200) print *, " tmp(",xselect,",",yselect,")    = ", tmp(xselect,yselect)
        var_out(:,:) = tmp(:,:)

        ! ix array
        if(interp_x.eq.1) then
           ! interpolate the stretched x-coordinate to constant dx coordinate
           do k = 1, nx, 1
              ix(k) = -real(((nx/2)*dx)-(dx/2)) + (k-1)*dx
           end do
        else
           ix(:) = x(:)
        end if
        if(debug_level.ge.200) print *, " ix(:)         = ", ix

        ! iy array
        if(interp_y.eq.1) then
           ! interpolate the stretched y-coordinate to constant dy coordinate
           do k = 1, ny, 1
              iy(k) = -real(((ny/2)*dy)-(dy/2)) + (k-1)*dy
           end do
        else
           iy(:) = y(:)
        end if
        if(debug_level.ge.200) print *, " iy(:)         = ", iy
        
     else if(flag.eq.2) then
        ! x-z
        if(debug_level.ge.100) print *, "x-z array"
        ! select one of the 2D array
        select case (varname)
        case ('water')
           print *, "The tmp array has already allocated"
           if(debug_level.ge.100) print *, " unit: [kg/kg] -> [g/kg]"
           tmp(:,:) = tmp(:,:)*real(1000.) ! unit: [kg/kg] -> [g/kg]
        case ('qc','qr','qi','qs','qg')
           tmp(:,:) = var_in(:,1,:,1)
           if(debug_level.ge.100) print *, " unit: [kg/kg] -> [g/kg]"
           tmp(:,:) = tmp(:,:)*real(1000.) ! unit: [kg/kg] -> [g/kg]
        case ('thetae')
           print *, "The tmp array has already allocated"
        case default
           tmp(:,:) = var_in(:,1,:,1)
        end select
        if(debug_level.ge.200) print *, " tmp(",xselect,",:)    = ", tmp(xselect,:)
        
        ! ix array
        if(interp_x.eq.1) then
           ! interpolate the stretched x-coordinate to constant dx coordinate
           do k = 1, nx, 1
              ix(k) = -real(((nx/2)*dx)-(dx/2)) + (k-1)*dx
           end do
        else
           ix(:) = x(:)
        end if
        if(debug_level.ge.200) print *, " ix(:)         = ", ix

        ! iy array
        if(interp_y.eq.1) then
           ! interpolate the stretched y-coordinate to constant dy coordinate
           do k = 1, ny, 1
              iy(k) = (k-1)*dy
           end do
           if(debug_level.ge.200) print *, " iy(:)         = ", iy
           
           select case (interp_method)
           case ('linear')
              ! z coordinate points are linearly interpolated by interp_linear
              do i = 1, imax
                 CALL interp_linear( kmax, ny, z, iy, tmp(i,:), var_out(i,:), debug_level )
              end do
           case ('near')
              ! z coordinate points are interpolated by nearest_interp_1d
              do i = 1, imax
                 CALL nearest_interp_1d( kmax, z(:), tmp(i,:), ny, iy(:), var_out(i,:) )
              end do
           case ('stpk')
              ! z coordinate points are linearly interpolated by STPK library
              do j = 2, ny-1, 1
                 !CALL nearest_search_1d( z, iy(j), ipoint)
                 CALL interpo_search_1d( z, iy(j), ipoint)
                 if(debug_level.ge.300) print *, " j,ipoint,iy,y = ", j,ipoint,iy(j),y(ipoint)
                 do i = 1, imax, 1
                    var_out(i,j) = tmp(i,ipoint)
                 end do
              end do
           end select
        else
           ! use the values of the original z-coordinate
           iy(:) = z(:)
           ! use original data 
           var_out(:,:) = tmp(:,:)
        end if
        if(debug_level.ge.100) print *, " var_out(",xselect,",:) = ", var_out(xselect,:)

     else if (flag.eq.3) then
        ! x-t array
        if(debug_level.ge.100) print *, "x-t array"
        ! ix
        if(interp_x.eq.1) then
           ! interpolate the stretched x-coordinate to constant dx coordinate
           do k = 1, nx, 1
              ix(k) = -real(((nx/2)*dx)-(dx/2)) + (k-1)*dx
           end do
        else
           ix(:) = x(:)
        end if

        ! iy
        iy(:) = real(time_in(:)/dble(60.)) ! uint: [minute] -> [hour]
        !iy(:) = time(:) ! uint: [second]

        select case (varname)
        case ('rain')
           if(debug_level.ge.100) print *, " unit: [cm] -> [mm]"
!$omp parallel do default(shared) &
!$omp private(i,t)
           do t = 1, tmax, 1
           do i = 1, imax, 1
              var_out(i,t) = var_in(i,1,t,1)*real(10.) ! unit: [cm] -> [mm]
           end do
           if(debug_level.ge.200) print *, "t,iy,var_out = ",t,iy(t),var_out(xselect,t)
           end do
        case ('cape','cin','lfc')
!$omp parallel do default(shared) &
!$omp private(i,t)
           do t = 1, tmax, 1
           do i = 1, imax, 1
              var_out(i,t) = tmp(i,t)
           end do
           if(debug_level.ge.200) print *, "t,iy,var_out = ",t,iy(t),var_out(xselect,t)
           end do
        case default
!$omp parallel do default(shared) &
!$omp private(i,t)
           do t = 1, tmax, 1
           do i = 1, imax, 1
              var_out(i,t) = var_in(i,1,t,1)
           end do
           if(debug_level.ge.200) print *, "t,iy,var_out = ",t,iy(t),var_out(xselect,t)
           end do
        end select

     else if(flag.eq.5) then
        ! arvitrary cross-section
        if(debug_level.ge.100) print *, "arvitrary cross-section"
        ! ix and yy array
        ! create the coordinate that the user specified point and angle
        if( (xselect.lt.x(1)).or.(xselect.gt.x(imax)) ) then
           print *, "ERROR: xselect must be specified between x(1) and x(imax)"
           stop
        end if
        if( (yselect.lt.y(1)).or.(yselect.gt.y(jmax)) ) then
           print *, "ERROR: yselect must be specified between y(1) and y(imax)"
           stop
        end if

        tmp0 = abs((x(imax)*cos(angle*(pi/180.)))*2)/real(imax)
        if(debug_level.ge.200) print *, " dix           = ", tmp0
        do i = 1, imax, 1
           ix(i) = xselect + x(1)*cos(angle*(pi/180.)) + (i-1)*tmp0
        end do
        if(debug_level.ge.200) print *, " ix(:)         = ", ix

        allocate( yy(jmax) )
        tmp0 = abs((y(jmax)*sin(angle*(pi/180.)))*2)/real(jmax)
        if(debug_level.ge.200) print *, " dyy           = ", tmp0
        do i = 1, jmax, 1
           yy(i) = yselect + y(1)*sin(angle*(pi/180.)) + (i-1)*tmp0
        end do
        if(debug_level.ge.200) print *, " yy(:)         = ", yy

        select case (varname)
        case ('thetae','rws')
!$omp parallel do default(shared) &
!$omp private(i,k,ipoints)
           do k = 1, kmax, 1
           do i = 1, nx, 1
              CALL find4point( imax, x, ix(i), ipoints(1), debug_level )
              CALL find4point( jmax, y, yy(i), ipoints(2), debug_level )
              if(debug_level.ge.300) print *, " ipoints               = ", ipoints(1), ipoints(2)
              if(debug_level.ge.300) print *, " tmpi                  = ", tmpi(ipoints(1),ipoints(2),k)
              tmp(i,k) = ( tmpi(ipoints(1)+1,ipoints(2)+1,k) + tmpi(ipoints(1)+1,ipoints(2),k) + &
                           tmpi(ipoints(1)  ,ipoints(2)+1,k) + tmpi(ipoints(1)  ,ipoints(2),k) )/real(4.)
              if(debug_level.ge.300) print *, " tmp(",i,",",k,")      = ", tmp(i,k)
           end do
           if(debug_level.ge.200) print *, " tmp(",nx/2,",",k,")      = ", tmp(nx/2,k)
           end do

        case default
!$omp parallel do default(shared) &
!$omp private(i,k,ipoints)
           do k = 1, kmax, 1
           do i = 1, nx, 1
              CALL find4point( imax, x, ix(i), ipoints(1), debug_level )
              CALL find4point( jmax, y, yy(i), ipoints(2), debug_level )
              if(debug_level.ge.300) print *, " ipoints               = ", ipoints(1), ipoints(2)
              if(debug_level.ge.300) print *, " var_in                = ", var_in(ipoints(1),ipoints(2),k,1)
              tmp(i,k) = ( var_in(ipoints(1)+1,ipoints(2)+1,k,1) + var_in(ipoints(1)+1,ipoints(2),k,1) + &
                           var_in(ipoints(1)  ,ipoints(2)+1,k,1) + var_in(ipoints(1)  ,ipoints(2),k,1) )/real(4.)
              if(debug_level.ge.300) print *, " tmp(",i,",",k,")      = ", tmp(i,k)
           end do
           if(debug_level.ge.200) print *, " tmp(",nx/2,",",k,")      = ", tmp(nx/2,k)
           end do

        end select

        ! iy array
        if(interp_y.eq.1) then
           ! interpolate the stretched y-coordinate to constant dy coordinate
           do k = 1, ny, 1
              iy(k) = (k-1)*dy
           end do
           if(debug_level.ge.200) print *, " iy(:)         = ", iy
           
           select case (interp_method)
           case ('linear')
              ! z coordinate points are linearly interpolated by interp_linear
              do i = 1, imax
                 CALL interp_linear( kmax, ny, z, iy, tmp(i,:), var_out(i,:), debug_level )
              end do
           case ('near')
              ! z coordinate points are interpolated by nearest_interp_1d
              do i = 1, imax
                 CALL nearest_interp_1d( kmax, z(:), tmp(i,:), ny, iy(:), var_out(i,:) )
              end do
           case ('stpk')
              ! z coordinate points are linearly interpolated by STPK library
              do j = 2, ny-1, 1
                 !CALL nearest_search_1d( z, iy(j), ipoint)
                 CALL interpo_search_1d( z, iy(j), ipoint)
                 if(debug_level.ge.300) print *, " j,ipoint,iy,y = ", j,ipoint,iy(j),y(ipoint)
                 do i = 1, imax, 1
                    var_out(i,j) = tmp(i,ipoint)
                 end do
              end do
           end select
        else
           ! use the values of the original z-coordinate
           iy(:) = z(:)
           ! use original data 
           var_out(:,:) = tmp(:,:)
        end if
        if(debug_level.ge.100) print *, " var_out(",nx/2,",:) = ", var_out(nx/2,:)

     end if
     if(debug_level.ge.100) print *, ""
     
     
     !ccccccccccccccccccccccccccccccccccccccccccccccccc
     ! Output 2D file
     !ccccccccccccccccccccccccccccccccccccccccccccccccc
     ! create the file
     select case (output_type)
     case ('nc3')
        call check( nf90_create(output, nf90_classic_model, ncid) )
     case ('nc3_64bit')
        call check( nf90_create(output, nf90_64bit_offset, ncid) )
     case ('nc4')
        call check( nf90_create(output, nf90_netcdf4, ncid) )
     end select
     if(debug_level.ge.100) print *, "Success: create the 2D output file"
     
     ! define the dimensions
     if(debug_level.ge.100) print *, " Start: define mode"
     call check( nf90_def_dim(ncid, "x", nx, xdimid) )
     call check( nf90_def_dim(ncid, "y", ny, ydimid) )
     if(debug_level.ge.100) print *, "  Success: define the dimensions"
     
     ! define the coordinate variables. 
     call check( nf90_def_var(ncid, "x", NF90_REAL, xdimid, xvarid) )
     call check( nf90_def_var(ncid, "y", NF90_REAL, ydimid, yvarid) )
     if(debug_level.ge.100) print *, "  Success: define the coordinate variables"
     
     ! define the netCDF variables for the 2D data with compressed format(netcdf4).
     dimids = (/ xdimid, ydimid /)
     chunks = (/ nx, ny /)
     select case (output_type)
     case ('nc3','nc3_64bit')
        call check( nf90_def_var(ncid, "z", NF90_REAL, dimids, varid) )
     case ('nc4')
        call check( nf90_def_var(ncid, "z", NF90_REAL, dimids, varid, &
             chunksizes = chunks, shuffle = .TRUE., deflate_level = deflate_level) )
     end select
     if(debug_level.ge.100) print *, "  Success: define the netcdf variables"
     
     ! end define mode
     call check( nf90_enddef(ncid) )
     if(debug_level.ge.100) print *, " End: define mode"
     
     ! write the coordinate variable data
     call check( nf90_put_var(ncid, xvarid, ix) )
     call check( nf90_put_var(ncid, yvarid, iy) )
     if(debug_level.ge.100) print *, "Success: write the coordinate variable data"
     
     ! write the data array
     ostart = (/ 1, 1 /)
     ocount = (/ nx, ny /)
     call check( nf90_put_var(ncid, varid, var_out, start = ostart, count = ocount) )
     if(debug_level.ge.100) print *, "Success: write the data array"
     
     ! close the file
     call check( nf90_close(ncid) )
     if(debug_level.ge.100) print *, "Success: close the netcdf file"
     if(debug_level.ge.100) print *, ""

     ! deallocate the allocated arrays in this program
     deallocate( ix,iy,var_out )
     select case (flag)
     case (1)
        deallocate( x,y,z,time_in,var_in,tmp,tmp1,tmp2,tmp3 )
     case (2)
        deallocate( x,y,z,time_in,var_in,tmp,tmp1,tmp2,tmp3,tmp4,tmp5 )
     case (3)
        deallocate( x,y,z,time_in,var_in,tmp )
     case (5)
        deallocate( x,y,z,time_in,var_in,tmp,yy,tmpi,tmpi1,tmpi2,tmpi3 )
     end select
     if(debug_level.ge.100) print *, "Success: deallocate the allocated arrays"
     if(debug_level.ge.100) print *, ""

  end if
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


  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! subroutine of find4point
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  subroutine find4point( znum, z_in, z_tmp, ipoint, debug_level )
    implicit none
    integer :: i, znum, ipoint
    real, dimension(znum) :: z_in
    real :: z_tmp
    integer :: debug_level
    do i = 1, znum
       if (z_tmp.ge.z_in(i)) then
          ipoint = i
          if(debug_level.ge.300) print *, " DEBUG: i,z_tmp,z_in,ipoint = ", i,z_tmp,z_in(i),ipoint
       end if
    end do
    if(debug_level.ge.300) print *, " DEBUG: ipoint = ", ipoint
  end subroutine find4point


  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! subroutine of interp_linear
  !  original program coded by Takashi Unuma, Kyoto Univ.
  !  last modified: 2014/06/29
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  subroutine interp_linear( znum, iznum, z_in, z_out, data_in, data_out, debug_level )
    implicit none
    integer :: i, znum, iznum
    real, dimension(znum)  :: z_in, data_in
    real, dimension(iznum) :: z_out, data_out
    real :: tmp
    integer :: debug_level
    do i = 1, iznum
       if(i.eq.1) then
          data_out(i) = data_in(1)
       else if(i.eq.iznum) then
          data_out(i) = data_in(znum)
       else
          ! 1. 内挿点の１つ前と後の座標及び値を取得: find4point
          CALL find4point( znum, z_in, z_out(i), ipoint, debug_level )
          ! 2. 用意した４点を基に tmp を計算
          tmp = (data_in(ipoint+1)-data_in(ipoint))/(z_in(ipoint+1)-z_in(ipoint))
          ! 3. 内挿点 data_out(i) を計算
          data_out(i) = (tmp*z_out(i)) + data_in(ipoint) - (tmp*z_in(ipoint))
       end if
       if(debug_level.ge.300) print *, " i,z_out,data_out = ", i, z_out(i), data_out(i)
    end do
  end subroutine interp_linear


  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! subroutine of nearest_interp_1d
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  subroutine nearest_interp_1d( nd, xd, yd, ni, xi, yi )
    !*****************************************************************************80
    !
    !! NEAREST_INTERP_1D evaluates the nearest neighbor interpolant.
    !
    !  Discussion:
    !
    !    The nearest neighbor interpolant L(ND,XD,YD)(X) is the piecewise
    !    constant function which interpolates the data (XD(I),YD(I)) for I = 1
    !    to ND.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 September 2012
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) ND, the number of data points.
    !    ND must be at least 1.
    !
    !    Input, real ( kind = 8 ) XD(ND), the data points.
    !
    !    Input, real ( kind = 8 ) YD(ND), the data values.
    !
    !    Input, integer ( kind = 4 ) NI, the number of interpolation points.
    !
    !    Input, real ( kind = 8 ) XI(NI), the interpolation points.
    !
    !    Output, real ( kind = 8 ) YI(NI), the interpolated values.
    !
    implicit none
    integer :: nd, ni
    integer :: i, j, k
    real :: d, d2
    real :: xd(nd), xi(ni), yd(nd), yi(ni)
    do i = 1, ni
       k = 1
       d = abs ( xi(i) - xd(k) )
       do j = 2, nd
          d2 = abs ( xi(i) - xd(j) )
          if ( d2 < d ) then
             k = j
             d = d2
          end if
       end do
       yi(i) = yd(k)
    end do
    return
  end subroutine nearest_interp_1d


  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! subroutine of nearest_search_1d on STPK
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  subroutine nearest_search_1d( x, point, i )
    ! 1 次元最近傍探索ルーチン
    ! interpo_search_1d から値を求め, その値と +1 した値の距離を比較して
    ! 距離の短い方を選択する.
    implicit none
    real, intent(in) :: x(:)  ! 漸増配列
    real, intent(in) :: point  ! この点
    integer, intent(inout) :: i  ! point の最近傍地点の要素番号
    real :: tmp1, tmp2
    integer :: j, nx
    
    nx=size(x)
    
    call interpo_search_1d( x, point, j )
    
    if(j==0)then  ! i=1 にしたいので, tmp1 にx(1), tmp2 に x(2) を入れれば, 後の if 文
       ! でうまく処理される.
       tmp1=x(j+1)
       tmp2=x(j+2)
    else
       if(j==nx)then  ! i=nx にしたいので, tmp2 に x(nx), tmp1 に x(nx-1) を入れれば,
          ! 後の if 文でうまく処理される.
          tmp1=x(j)
          tmp2=x(j-1)
       else
          tmp1=x(j)
          tmp2=x(j+1)
       end if
    end if
    
    if(abs(point-tmp1)>abs(tmp2-point))then
       i=j+1
    else
       i=j
    end if
    
  end subroutine nearest_search_1d


  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! subroutine of interpo_search_1d on STPK
  !ccccccccccccccccccccccccccccccccccccccccccccccccc  
  subroutine interpo_search_1d( x, point, i, undeff, stdopt )
    ! 漸増配列（要素数が増えるごとに値が大きくなる配列）のなかで,
    ! point の前に来る要素番号を出力する.
    implicit none
    real, intent(in) :: x(:)  ! 漸増配列
    real, intent(in) :: point  ! この点
    integer, intent(inout) :: i  ! point の値を越えない最大の値をもつ要素番号
    integer, intent(in), optional :: undeff  ! 探索範囲の配列要素より小さい値を探索しようとした際, undef を返すが, その undef 値を設定する. default では 0.
    logical, intent(in), optional :: stdopt  ! 探索範囲が見つからない旨の標準出力を表示させないようにする.
                                             ! default では .false. (表示させる)
    integer :: nx, j
    integer :: just
    logical :: stderr
    
    nx=size(x)
    if(present(undeff))then
       just=undeff
    else
       just=-2147483648
    end if
    
    if(present(stdopt))then
       stderr=stdopt
    else
       stderr=.true.
    end if
    
    do j=1,nx
       if(x(1)>point)then
          if(stderr.eqv..false.)then
             write(*,*) "****** WARNING ******"
             write(*,*) "searching point was not found :", x(1), point
             write(*,*) "Abort. Exit.!!!"
          end if
          i=just
          exit
       end if
       
       if(present(undeff))then
          if(x(j)/=real(undeff))then
             if(x(j)<=point)then
                i=j
             else
                exit
             end if
          end if
       else
          if(x(j)<=point)then
             i=j
          else
             exit
          end if
       end if
    end do
    
  end subroutine interpo_search_1d


  !ccccccccccccccccccccccccccccccccccccc
  !----- STPK: thermo_function.f90 -----
  !ccccccccccccccccccccccccccccccccccccc
  real function thetae_Bolton(T,qv,P)
    ! Bolton(1980) による手法を用いて相当温位を計算する.
    ! この相当温位は偽断熱過程での相当温位である.
    ! T_LCL を用いるので, そのための関数を使用する.
    implicit none
    real, intent(in) :: T  ! 温度 [K]
    real, intent(in) :: qv  ! 混合比 [kg / kg]
    real, intent(in) :: P  ! 全圧 [Pa]
    real :: T_LCL, qvs
    real, parameter :: a=0.2854, b=0.28, c=3376.0, d=0.81
    real, parameter :: p0=1.0e5
    
    T_LCL=TqvP_2_TLCL(T,qv,P)
    qvs=TP_2_qvs(T_LCL,P)
    thetae_Bolton=T*((p0/P)**(a*(1.0-b*qvs))) *exp((c/T_LCL-2.54)*qvs*(1.0+d*qvs))
    
    return
  end function thetae_Bolton
  
  real function TqvP_2_TLCL(T,qv,P)  !! 温度と混合比と全圧から T_LCL を計算する
    ! 混合比から水蒸気圧を求め, そこから T_LCL を計算する
    implicit none
    real, intent(in) :: T  ! 温度 [K]
    real, intent(in) :: qv  ! 混合比 [kg / kg]
    real, intent(in) :: P  ! 全圧 [Pa]
    real, parameter :: coe=2840.0, a=3.5, b=4.805, c=55.0
    real :: e
    
    e=qvP_2_e(qv,P)
    e=e*1.0e-2
    if(e>0.0)then
       TqvP_2_TLCL=coe/(a*log(T)-log(e)-b)+55.0
    else
       TqvP_2_TLCL=coe/(a*log(T)-b)+55.0  ! true ??
    end if
    
    return
  end function TqvP_2_TLCL
  
  real function qvP_2_e(qv,P)  ! 混合比と全圧から水蒸気圧を計算する
    implicit none
    real, intent(in) :: qv  ! 混合比 [kg / kg]
    real, intent(in) :: P  ! 全圧 [Pa]
    real :: eps
    real, parameter :: Rd=287.0
    real, parameter :: Rv=461.0    

    eps=Rd/Rv
    qvP_2_e=P*qv/(eps+qv)
    
    return
  end function qvP_2_e
  
  real function TP_2_qvs(T,P)  ! 温度と全圧から飽和混合比を計算する
    ! ここでは, es_Bolton を用いて飽和水蒸気圧を計算した後,
    ! eP_2_qv を用いて混合比に変換することで飽和混合比を計算する.
    implicit none
    real, intent(in) :: T  ! 温度 [K]
    real, intent(in) :: P  ! 大気の全圧 [Pa]
    real :: eps
    real :: es
    real, parameter :: Rd=287.0
    real, parameter :: Rv=461.0    
    
    eps=Rd/Rv
    es=es_Bolton(T)
    TP_2_qvs=eps*es/(P-es)
    
    return
  end function TP_2_qvs
  
  real function es_Bolton(T)  ! Bolton(1980) の手法を用いて飽和水蒸気圧を計算する.
    implicit none
    real, intent(in) :: T  ! 大気の温度 [K]
    real, parameter :: a=17.67, c=29.65
    real, parameter :: e0=611.0
    real, parameter :: t0=273.15
    
    es_Bolton=e0*exp(a*((T-t0)/(T-c)))
    
    return
  end function es_Bolton
  
  real function eP_2_qv(e,P)  ! 水蒸気圧と全圧から混合比を計算する
    implicit none
    real, intent(in) :: e  ! 水蒸気圧 [Pa]
    real, intent(in) :: P  ! 大気の全圧 [Pa]
    real :: eps
    real, parameter :: Rd=287.0
    real, parameter :: Rv=461.0    
        
    eps=Rd/Rv
    eP_2_qv=eps*e/(P-e)
    
    return
  end function eP_2_qv

  real function thetaP_2_T(theta,P)  ! 温位, 圧力から温度を計算する(乾燥大気として計算)
    implicit none
    real, intent(in) :: theta  ! 温位 [K]
    real, intent(in) :: P  ! 湿潤大気の全圧 [Pa]
    real, parameter :: Rd=287.0
    real, parameter :: Cpd=1004.0
    real, parameter :: p0=1.0e5
    real :: kappa
    
    kappa=Rd/Cpd
    
    thetaP_2_T=theta*(P/p0)**kappa
    
    return
  end function thetaP_2_T

!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------

    subroutine getcape( source, nk, p_in, t_in, q_in, cape, cin,       &
                        plcl, plfc, pel, zlcl, zlfc, zel,              &
                        debug_level, zgrid                             )
    implicit none

    integer, intent(in) :: source,nk,debug_level,zgrid
    real, dimension(nk), intent(in) :: p_in,t_in,q_in
    real, intent(out) :: cape,cin
    real, intent(out) :: zlcl,zlfc,zel,plcl,plfc,pel

!-----------------------------------------------------------------------
!
!  getcape - a fortran90 subroutine to calculate Convective Available
!            Potential Energy (CAPE) from a sounding.
!
!   *** Modified version to calculate Lifted Condensation Level (LCL)  ***
!   *** Level of Free Convection (LFC), and Equilibrium Level (EL)     ***
!
!  Version 1.04                           Last modified:  8 October 2010
!
!  Author:  George H. Bryan
!           Mesoscale and Microscale Meteorology Division
!           National Center for Atmospheric Research
!           Boulder, Colorado, USA
!           gbryan@ucar.edu
!
!  Disclaimer:  This code is made available WITHOUT WARRANTY.
!
!  References:  Bolton (1980, MWR, p. 1046) (constants and definitions)
!               Bryan and Fritsch (2004, MWR, p. 2421) (ice processes)
!
!-----------------------------------------------------------------------
!
!  Input:     nk - number of levels in the sounding (integer)
!
!           p_in - one-dimensional array of pressure (mb) (real)
!
!           t_in - one-dimensional array of temperature (C) (real)
!
!           q_in - one-dimensional array of water vapor mixing ratio (kg/kg) (real)
!
!  Output:  cape - Convective Available Potential Energy (J/kg) (real)
!
!            cin - Convective Inhibition (J/kg) (real)
!
!-----------------------------------------------------------------------
!  User options:

    real, parameter :: pinc = 10.0    ! Pressure increment (Pa)
                                      ! (smaller number yields more accurate
                                      !  results,larger number makes code 
                                      !  go faster)

!!!    integer, parameter :: source = 2    ! Source parcel:
!!!                                        ! 1 = surface
!!!                                        ! 2 = most unstable (max theta-e)
!!!                                        ! 3 = mixed-layer (specify ml_depth)

    real, parameter :: ml_depth =  500.0  ! depth (m) of mixed layer 
                                          ! for source=3

    integer, parameter :: adiabat = 1   ! Formulation of moist adiabat:
                                        ! 1 = pseudoadiabatic, liquid only
                                        ! 2 = reversible, liquid only
                                        ! 3 = pseudoadiabatic, with ice
                                        ! 4 = reversible, with ice

!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------
!            No need to modify anything below here:
!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------

    logical :: doit,ice,cloud,not_converged
    integer :: k,kmax,n,nloop,i
    real, dimension(nk) :: p,t,td,pi,q,th,thv,z,pt,pb,pc,ptv,ptd,pqv,pql

    real :: the,maxthe,parea,narea,lfc
    real :: th1,p1,t1,qv1,ql1,qi1,b1,pi1,thv1,qt,dp,dz,ps,frac
    real :: th2,p2,t2,qv2,ql2,qi2,b2,pi2,thv2
    real :: thlast,fliq,fice,tbar,qvbar,qlbar,qibar,lhv,lhs,lhf,rm,cpm
    real*8 :: avgth,avgqv
!    real :: getqvl,getqvi,getthx,gettd ! comment out for compiling with gfortran
    real :: ee,psource,tsource,qvsource

!-----------------------------------------------------------------------

    real, parameter :: g     = 9.81
    real, parameter :: p00   = 100000.0
    real, parameter :: cp    = 1005.7
    real, parameter :: rd    = 287.04
    real, parameter :: rv    = 461.5
    real, parameter :: xlv   = 2501000.0
    real, parameter :: xls   = 2836017.0
    real, parameter :: t0    = 273.15
    real, parameter :: cpv   = 1875.0
    real, parameter :: cpl   = 4190.0
    real, parameter :: cpi   = 2118.636
    real, parameter :: lv1   = xlv+(cpl-cpv)*t0
    real, parameter :: lv2   = cpl-cpv
    real, parameter :: ls1   = xls+(cpi-cpv)*t0
    real, parameter :: ls2   = cpi-cpv

    real, parameter :: rp00  = 1.0/p00
    real, parameter :: eps   = rd/rv
    real, parameter :: reps  = rv/rd
    real, parameter :: rddcp = rd/cp
    real, parameter :: cpdrd = cp/rd
    real, parameter :: cpdg  = cp/g

    real, parameter :: converge = 0.0002

!-----------------------------------------------------------------------

!---- convert p,t,td to mks units; get pi,q,th,thv ----!

    do k=1,nk
        p(k) = 100.0*p_in(k)
        t(k) = 273.15+t_in(k)
        q(k) = q_in(k)
!!!       td(k) = 273.15+td_in(k)
       ee = alog((q(k)/eps)*p(k)/100.0/(1.0+(q(k)/eps)))
       td(k) = 273.15+(243.5*ee-440.8)/(19.48-ee)
       pi(k) = (p(k)*rp00)**rddcp
!!!        q(k) = getqvl(p(k),td(k))
       th(k) = t(k)/pi(k)
      thv(k) = th(k)*(1.0+reps*q(k))/(1.0+q(k))
!!!      print *,k,t(k)-273.15,td(k)-273.15
    enddo

!---- get height using the hydrostatic equation ----!

    z(1) = 0.0
    do k=2,nk
      dz = -cpdg*0.5*(thv(k)+thv(k-1))*(pi(k)-pi(k-1))
      z(k) = z(k-1) + dz
    enddo

!----------------------------------------------------------------

!---- find source parcel ----!

  IF(source.eq.1)THEN
    ! use surface parcel
    kmax = 1

  ELSEIF(source.eq.2)THEN
    ! use most unstable parcel (max theta-e)

    IF(p(1).lt.50000.0)THEN
      ! first report is above 500 mb ... just use the first level reported
      kmax = 1
      maxthe = getthx(p(1),t(1),td(1),q(1))
    ELSE
      ! find max thetae below 500 mb
      maxthe = 0.0
      do k=1,nk
        if(p(k).ge.50000.0)then
          the = getthx(p(k),t(k),td(k),q(k))
          if( the.gt.maxthe )then
            maxthe = the
            kmax = k
          endif
        endif
      enddo
    ENDIF
    if(debug_level.ge.300) print *,'  kmax,maxthe = ',kmax,maxthe

  ELSEIF(source.eq.3)THEN
    ! use mixed layer

    IF( (z(2)-z(1)).gt.ml_depth )THEN
      ! the second level is above the mixed-layer depth:  just use the
      ! lowest level

      avgth = th(1)
      avgqv = q(1)
      kmax = 1

    ELSEIF( z(nk).lt.ml_depth )THEN
      ! the top-most level is within the mixed layer:  just use the
      ! upper-most level

      avgth = th(nk)
      avgqv = q(nk)
      kmax = nk

    ELSE
      ! calculate the mixed-layer properties:

      avgth = 0.0
      avgqv = 0.0
      k = 2
      if(debug_level.ge.300) print *,'  ml_depth = ',ml_depth
      if(debug_level.ge.300) print *,'  k,z,th,q:'
      if(debug_level.ge.300) print *,1,z(1),th(1),q(1)

      do while( (z(k).le.ml_depth) .and. (k.le.nk) )

        if(debug_level.ge.300) print *,k,z(k),th(k),q(k)

        avgth = avgth + 0.5*(z(k)-z(k-1))*(th(k)+th(k-1))
        avgqv = avgqv + 0.5*(z(k)-z(k-1))*(q(k)+q(k-1))

        k = k + 1

      enddo

      th2 = th(k-1)+(th(k)-th(k-1))*(ml_depth-z(k-1))/(z(k)-z(k-1))
      qv2 =  q(k-1)+( q(k)- q(k-1))*(ml_depth-z(k-1))/(z(k)-z(k-1))

      if(debug_level.ge.300) print *,999,ml_depth,th2,qv2

      avgth = avgth + 0.5*(ml_depth-z(k-1))*(th2+th(k-1))
      avgqv = avgqv + 0.5*(ml_depth-z(k-1))*(qv2+q(k-1))

      if(debug_level.ge.300) print *,k,z(k),th(k),q(k)

      avgth = avgth/ml_depth
      avgqv = avgqv/ml_depth

      kmax = 1

    ENDIF

    if(debug_level.ge.300) print *,avgth,avgqv

  ELSEIF(source.eq.4)THEN
    ! use specified level's parcel
    kmax = zgrid
    if(debug_level.ge.300) print *,'  source = ',source
    if(debug_level.ge.300) print *,'  kmax   = ',kmax

  ELSE

    print *
    print *,'  Unknown value for source'
    print *
    print *,'  source = ',source
    print *
    stop

  ENDIF

!---- define parcel properties at initial location ----!
    narea = 0.0

  if( (source.eq.1).or.(source.eq.2).or.(source.eq.4) )then
    k    = kmax
    th2  = th(kmax)
    pi2  = pi(kmax)
    p2   = p(kmax)
    t2   = t(kmax)
    thv2 = thv(kmax)
    qv2  = q(kmax)
    b2   = 0.0
  elseif( source.eq.3 )then
    k    = kmax
    th2  = avgth
    qv2  = avgqv
    thv2 = th2*(1.0+reps*qv2)/(1.0+qv2)
    pi2  = pi(kmax)
    p2   = p(kmax)
    t2   = th2*pi2
    b2   = g*( thv2-thv(kmax) )/thv(kmax)
  endif

    psource = p2
    tsource = t2
   qvsource = qv2

    ql2 = 0.0
    qi2 = 0.0
    qt  = qv2

    cape = 0.0
    cin  = 0.0
    lfc  = 0.0

    doit = .true.
    cloud = .false.
    if(adiabat.eq.1.or.adiabat.eq.2)then
      ice = .false.
    else
      ice = .true.
    endif

      the = getthx(p2,t2,t2,qv2)
      if(debug_level.ge.300) print *,'  the = ',the

      pt(k) = t2
      if( cloud )then
        ptd(k) = t2
      else
!        ptd(k) = gettd(p2,t2,qv2)
        ptd(k) = gettd(p2,qv2)
      endif
      ptv(k) = t2*(1.0+reps*qv2)/(1.0+qv2)
      pb(k) = 0.0
      pqv(k) = qv2
      pql(k) = 0.0

      zlcl = -1.0
      zlfc = -1.0
      zel  = -1.0

!---- begin ascent of parcel ----!

      if(debug_level.ge.300)then
        print *,'  Start loop:'
        print *,'  p2,th2,qv2 = ',p2,th2,qv2
      endif

    do while( doit .and. (k.lt.nk) )

        k = k+1
       b1 =  b2

       dp = p(k-1)-p(k)

      if( dp.lt.pinc )then
        nloop = 1
      else
        nloop = 1 + int( dp/pinc )
        dp = dp/float(nloop)
      endif

      do n=1,nloop

         p1 =  p2
         t1 =  t2
        pi1 = pi2
        th1 = th2
        qv1 = qv2
        ql1 = ql2
        qi1 = qi2
        thv1 = thv2

        p2 = p2 - dp
        pi2 = (p2*rp00)**rddcp

        thlast = th1
        i = 0
        not_converged = .true.

        do while( not_converged )
          i = i + 1
          t2 = thlast*pi2
          if(ice)then
            fliq = max(min((t2-233.15)/(273.15-233.15),1.0),0.0)
            fice = 1.0-fliq
          else
            fliq = 1.0
            fice = 0.0
          endif
          qv2 = min( qt , fliq*getqvl(p2,t2) + fice*getqvi(p2,t2) )
          qi2 = max( fice*(qt-qv2) , 0.0 )
          ql2 = max( qt-qv2-qi2 , 0.0 )

          tbar  = 0.5*(t1+t2)
          qvbar = 0.5*(qv1+qv2)
          qlbar = 0.5*(ql1+ql2)
          qibar = 0.5*(qi1+qi2)

          lhv = lv1-lv2*tbar
          lhs = ls1-ls2*tbar
          lhf = lhs-lhv

          rm=rd+rv*qvbar
          cpm=cp+cpv*qvbar+cpl*qlbar+cpi*qibar
          th2=th1*exp(  lhv*(ql2-ql1)/(cpm*tbar)     &
                       +lhs*(qi2-qi1)/(cpm*tbar)     &
                       +(rm/cpm-rd/cp)*alog(p2/p1) )

          if(i.gt.90) print *,i,th2,thlast,th2-thlast
          if(i.gt.100)then
            print *
            print *,'  Error:  lack of convergence'
            print *
            print *,'  ... stopping iteration '
            print *
            stop 1001
          endif
          if( abs(th2-thlast).gt.converge )then
            thlast=thlast+0.3*(th2-thlast)
          else
            not_converged = .false.
          endif
        enddo

        ! Latest pressure increment is complete.  Calculate some
        ! important stuff:

        if( ql2.ge.1.0e-10 ) cloud = .true.
        if( cloud .and. zlcl.lt.0.0 )then
           zlcl = z(k-1)+(z(k)-z(k-1))*float(n)/float(nloop)
           plcl = p(k-1)+(p(k)-p(k-1))*float(n)/float(nloop)
        endif

        IF(adiabat.eq.1.or.adiabat.eq.3)THEN
          ! pseudoadiabat
          qt  = qv2
          ql2 = 0.0
          qi2 = 0.0
        ELSEIF(adiabat.le.0.or.adiabat.ge.5)THEN
          print *
          print *,'  Undefined adiabat'
          print *
          stop 10000
        ENDIF

      enddo

      thv2 = th2*(1.0+reps*qv2)/(1.0+qv2+ql2+qi2)
        b2 = g*( thv2-thv(k) )/thv(k)
        dz = -cpdg*0.5*(thv(k)+thv(k-1))*(pi(k)-pi(k-1))

      if( zlcl.gt.0.0 .and. zlfc.lt.0.0 .and. b2.gt.0.0 )then
        if( b1.gt.0.0 )then
          zlfc = zlcl
          plfc = plcl
        else
          zlfc = z(k-1)+(z(k)-z(k-1))*(0.0-b1)/(b2-b1)
          plfc = p(k-1)+(p(k)-p(k-1))*(0.0-b1)/(b2-b1)
        endif
      endif

      if( zlfc.gt.0.0 .and. zel.lt.0.0 .and. b2.lt.0.0 )then
        zel = z(k-1)+(z(k)-z(k-1))*(0.0-b1)/(b2-b1)
        pel = p(k-1)+(p(k)-p(k-1))*(0.0-b1)/(b2-b1)
      endif

      the = getthx(p2,t2,t2,qv2)

      pt(k) = t2
      if( cloud )then
        ptd(k) = t2
      else
!        ptd(k) = gettd(p2,t2,qv2)
        ptd(k) = gettd(p2,qv2)
      endif
      ptv(k) = t2*(1.0+reps*qv2)/(1.0+qv2)
      pb(k) = b2
      pqv(k) = qv2
      pql(k) = ql2

      ! Get contributions to CAPE and CIN:

      if( (b2.ge.0.0) .and. (b1.lt.0.0) )then
        ! first trip into positive area
        ps = p(k-1)+(p(k)-p(k-1))*(0.0-b1)/(b2-b1)
        frac = b2/(b2-b1)
        parea =  0.5*b2*dz*frac
        narea = narea-0.5*b1*dz*(1.0-frac)
        if(debug_level.ge.300)then
          print *,'      b1,b2 = ',b1,b2
          print *,'      p1,ps,p2 = ',p(k-1),ps,p(k)
          print *,'      frac = ',frac
          print *,'      parea = ',parea
          print *,'      narea = ',narea
        endif
        cin  = cin  + narea
        narea = 0.0
      elseif( (b2.lt.0.0) .and. (b1.gt.0.0) )then
        ! first trip into neg area
        ps = p(k-1)+(p(k)-p(k-1))*(0.0-b1)/(b2-b1)
        frac = b1/(b1-b2)
        parea =  0.5*b1*dz*frac
        narea = -0.5*b2*dz*(1.0-frac)
        if(debug_level.ge.300)then
          print *,'      b1,b2 = ',b1,b2
          print *,'      p1,ps,p2 = ',p(k-1),ps,p(k)
          print *,'      frac = ',frac
          print *,'      parea = ',parea
          print *,'      narea = ',narea
        endif
      elseif( b2.lt.0.0 )then
        ! still collecting negative buoyancy
        parea =  0.0
        narea = narea-0.5*dz*(b1+b2)
      else
        ! still collecting positive buoyancy
        parea =  0.5*dz*(b1+b2)
        narea =  0.0
      endif

      cape = cape + max(0.0,parea)
      pc(k) = cape

      if(debug_level.ge.300)then
        write(6,102) p2,b1,b2,cape,cin,cloud
102     format(5(f13.4),2x,l1)
      endif

      if( (p(k).le.10000.0).and.(b2.lt.0.0) )then
        ! stop if b < 0 and p < 100 mb
        doit = .false.
      endif

    enddo

!    zlcl = zlcl ! unit: [m]
!    zlfc = zlfc ! unit: [m]
!    zel  = zel  ! unit: [m]

!    plcl = plcl*100. ! unit: [hPa]
!    plfc = plfc*100. ! unit: [hPa]
!    pel  = pel*100.  ! unit: [hPa]

    if(debug_level.ge.200) print *,'  zlcl,zlfc,zel = ',zlcl,zlfc,zel
    if(debug_level.ge.200) print *,'  plcl,plfc,pel = ',plcl,plfc,pel

!---- All done ----!

    return
    end subroutine getcape

!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------

    real function getqvl(p,t)
    implicit none

    real :: p,t,es

    real, parameter :: eps = 287.04/461.5

    es = 611.2*exp(17.67*(t-273.15)/(t-29.65))
    getqvl = eps*es/(p-es)

    return
    end function getqvl

!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------

    real function getqvi(p,t)
    implicit none

    real :: p,t,es

    real, parameter :: eps = 287.04/461.5

    es = 611.2*exp(21.8745584*(t-273.15)/(t-7.66))
    getqvi = eps*es/(p-es)

    return
    end function getqvi

!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------

    real function getthx(p,t,td,q)
    implicit none

    real :: p,t,td,q
    real :: tlcl

    if( (td-t).ge.-0.1 )then
      tlcl = t
    else
      tlcl = 56.0 + ( (td-56.0)**(-1) + 0.00125*alog(t/td) )**(-1)
    endif

    getthx=t*( (100000.0/p)**(0.2854*(1.0-0.28*q)) )   &
            *exp( ((3376.0/tlcl)-2.54)*q*(1.0+0.81*q) )

    return
    end function getthx

!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------

    real function gettd(p,q)
    implicit none

    real :: p,q

    real :: el
    real, parameter :: eps = 287.04/461.5

    el = alog((q/eps)*p/100.0/(1.0+(q/eps)))
    gettd = 273.15+(243.5*el-440.8)/(19.48-el)

    return
    end function gettd

!-----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!-----------------------------------------------------------------------
  
end program ncedit
