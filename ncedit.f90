!
! Program of ncedit.f90
! original program coded by Takashi Unuma, Kyoto Univ.
! Last modified: 2014/06/12
!

program ncedit

  use netcdf
  
  implicit none

  integer :: ncid, varid, xdimid, ydimid, zdimid, tdimid, xvarid, yvarid, deflate_level
  integer :: i, j, k, t, l, imax, jmax, kmax, tmax, ny, ipoint
  integer :: flag, xselect, yselect, zselect, tselect
  integer, dimension(2) :: start, count, dimids, chunks
  integer, dimension(4) :: istart, icount
  real :: dy, tmp0
  real, dimension(:),     allocatable :: x, y, iy, z, time
  real, dimension(:,:),   allocatable :: var_out, tmp, tmp1, tmp2, tmp3
  real, dimension(:,:,:,:), allocatable :: var_in
  character(len=20) :: varname
  character(len=42) :: input, output
  integer :: debug_level

!  real :: nan = (/ Z'7fffffff' /)
!  real :: nan = -999.
  real :: nan = -2147483648.

  
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Input parameters from namelist
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  namelist /param/ imax,jmax,kmax,tmax,varname,input,      &
                   xselect,yselect,zselect,tselect,output, &
                   flag,ny,dy,                             &
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
  if(debug_level.ge.100) print '(a18,i6)',  " flag          = ", flag
  if(debug_level.ge.100) print '(a18,i6)',  "  ny           = ", ny
  if(debug_level.ge.100) print '(a18,f6.3)',"  dy           = ", dy
  if(debug_level.ge.100) print '(a18,i6)',  " deflate_level = ", deflate_level
  if(debug_level.ge.100) print '(a18,i6)',  " debug_level   = ", debug_level
  if(debug_level.ge.100) print '(a30)',     "----- values on namelist -----"
  if(debug_level.ge.100) print *, ""

  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Initialization
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  allocate( x(imax), y(jmax), z(kmax), time(tmax), iy(ny) )
  allocate( var_out(imax,ny) )
  var_out(:,:) = nan

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
  call check( nf90_get_var(ncid, xdimid, x) )
  if(debug_level.ge.100) print *, "Success: get the x coordinate"
  if(debug_level.ge.200) print *, " x(:)          = ", x

  ! inquire and get y coordinate
  call check( nf90_inq_varid(ncid, 'nj', ydimid) )
  if(debug_level.ge.100) print *, "Success: inquire the ydimid"
  if(debug_level.ge.200) print *, " ydimid        = ", ydimid
  call check( nf90_get_var(ncid, ydimid, y) )
  if(debug_level.ge.100) print *, "Success: get the y coordinate"
  if(debug_level.ge.200) print *, " y(:)          = ", y

  ! inquire and get z coordinate
  call check( nf90_inq_varid(ncid, 'nk', zdimid) )
  if(debug_level.ge.100) print *, "Success: inquire the zdimid"
  if(debug_level.ge.200) print *, " zdimid        = ", zdimid
  call check( nf90_get_var(ncid, zdimid, z) )
  if(debug_level.ge.100) print *, "Success: get the z coordinate"
  if(debug_level.ge.200) print *, " z(:)          = ", z

  ! inquire and get time coordinate
  call check( nf90_inq_varid(ncid, 'time', tdimid) )
  if(debug_level.ge.100) print *, "Success: inquire the tdimid"
  if(debug_level.ge.200) print *, " tdimid        = ", tdimid
  call check( nf90_get_var(ncid, tdimid, time) )
  if(debug_level.ge.100) print *, "Success: get the time coordinate"
  if(debug_level.ge.200) print *, " time(:)       = ", time
  if(debug_level.ge.100) print *, ""


  select case (flag)
  case (1)
     allocate( var_in(imax,jmax,1,1) )
     istart = (/ 1, 1, zselect, tselect /)
     icount = (/ imax, jmax, 1, 1 /)
     allocate( tmp(imax,jmax) )
     tmp(:,:) = nan
  case (2)
     allocate( var_in(imax,1,kmax,1) )
     istart = (/ 1, yselect, 1, tselect /)
     icount = (/ imax, 1, kmax, 1 /)
     allocate( tmp(imax,kmax),tmp1(imax,kmax),tmp2(imax,kmax),tmp3(imax,kmax) )
     tmp(:,:) = nan
     tmp1(:,:) = nan
     tmp2(:,:) = nan
     tmp3(:,:) = nan
  case (3)
     allocate( var_in(imax,1,tmax,1) )
     istart = (/ 1, yselect, 1, 1 /)
     icount = (/ imax, 1, tmax, 1 /)
     allocate( tmp(imax,tmax) )
     tmp(:,:) = nan
!  case default
!     allocate( var_in(imax,jmax,kmax,tmax) )
  end select
  var_in(:,:,:,:) = nan

  
  ! inquire and get var
  if(debug_level.ge.100) print *, "varname of ",trim(varname)," is selected"
  select case (varname)
  case ('water')
     ! for all water (qc+qr+qi+qc+qg) on the microphysics processes
     tmp(:,:) = 0.
     do l = 1, 5, 1
        if(l.eq.1) varname = "qc"
        if(l.eq.2) varname = "qr"
        if(l.eq.3) varname = "qi"
        if(l.eq.4) varname = "qs"
        if(l.eq.5) varname = "qg"
        if(debug_level.ge.100) print *, "varname = ",trim(varname)
        call check( nf90_inq_varid(ncid, varname, varid) )
        if(debug_level.ge.100) print *, " Success: inquire the varid"
        if(debug_level.ge.200) print *, "  varid         = ", varid
!        call check( nf90_get_var(ncid, varid, var_in) )
!        call check( nf90_get_var(ncid, varid, var_in(:,yselect,:,tselect), &
        call check( nf90_get_var(ncid, varid, var_in, &
                                 start = istart, count = icount ) )
        if(debug_level.ge.100) print *, " Success: get the var array"
        if(debug_level.ge.100) print *, "  var_in(1,1,1,1) = ", var_in(1,1,1,1)
!        tmp(:,:) = tmp(:,:) + var_in(:,yselect,:,tselect)
        if (i.eq.1) then
           do k = 1, kmax, 1
           do i = 1, imax, 1
              tmp(i,k) = var_in(i,1,k,1)
           end do
           end do
        else
           do k = 1, kmax, 1
           do i = 1, imax, 1
              tmp(i,k) = tmp(i,k) + var_in(i,1,k,1)
           end do
           end do
        end if
     end do

  case ('thetae')
     ! equivalent potential temperature [K]
     ! read prs [Pa]
     call check( nf90_inq_varid(ncid, "prs", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.100) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp1(i,k) = var_in(i,1,k,1)
     end do
     end do
     ! read theta [K]
     call check( nf90_inq_varid(ncid, "th", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.100) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp2(i,k) = var_in(i,1,k,1)
     end do
     end do
     ! read qv [kg/kg]
     call check( nf90_inq_varid(ncid, "qv", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.100) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp3(i,k) = var_in(i,1,k,1)
     end do
     end do
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp0 = thetaP_2_T( tmp2(i,k), tmp1(i,k) )
        tmp(i,k) = thetae_Bolton( tmp0, tmp3(i,k), tmp1(i,k) )
     end do
     end do
     
  case default
     ! the others
     call check( nf90_inq_varid(ncid, varname, varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     select case (flag)
     case (1) ! x-y
!        call check( nf90_get_var(ncid, varid, var_in(:,:,zselect,tselect), &
        call check( nf90_get_var(ncid, varid, var_in, &
                                 start = istart, count = icount ) )
     case (2) ! x-z
!        call check( nf90_get_var(ncid, varid, var_in(:,yselect,:,tselect), &
        call check( nf90_get_var(ncid, varid, var_in, &
                                 start = istart, count = icount ) )
     case (3) ! x-t
!        call check( nf90_get_var(ncid, varid, var_in(:,yselect,:,1), &
        call check( nf90_get_var(ncid, varid, var_in, &
                                 start = istart, count = icount ) )
     case default
     call check( nf90_get_var(ncid, varid, var_in) )
     end select
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.100) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)

  end select

  ! close netcdf file
  call check( nf90_close(ncid) )
  if(debug_level.ge.100) print *, "Success: close the netcdf data"
  if(debug_level.ge.100) print *, ""


  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! make 2D array
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  if(flag.eq.1) then
     ! x-y array
     ! select one of the 2D array
     tmp(:,:) = var_in(:,:,1,1)
     print *, "under construction"
     stop

  else if(flag.eq.2) then
     ! x-z array
     ! select one of the 2D array
     select case (varname)
     case ('water')
        print *, "The tmp array has already allocated"
        if(debug_level.ge.100) print *, " unit: [kg/kg] -> [g/kg]"
        tmp(:,:) = tmp(:,:)*real(1000.) ! unit: [kg/kg] -> [g/kg]
     case ('qc','qr','qi','qs','qg')
!        tmp(:,:) = var_in(:,yselect,:,tselect)
        tmp(:,:) = var_in(:,1,:,1)
        if(debug_level.ge.100) print *, " unit: [kg/kg] -> [g/kg]"
        tmp(:,:) = tmp(:,:)*real(1000.) ! unit: [kg/kg] -> [g/kg]
     case ('thetae')
        print *, "The tmp array has already allocated"
     case default
!        tmp(:,:) = var_in(:,yselect,:,tselect)
        tmp(:,:) = var_in(:,1,:,1)
     end select
     if(debug_level.ge.200) print *, " tmp(",xselect,",:)    = ", tmp(xselect,:)
     
     ! interpolate the stretched coordinate to constant dy coordinate
     do k = 1, ny, 1
        iy(k) = (k-1)*dy
     end do
     if(debug_level.ge.200) print *, " iy(:)         = ", iy
     ! z coordinate points are manually selected as follows;
     do j = 1, ny, 1
        if (j.eq.1) then
           ipoint = 1
        else if (j.eq.2) then
           ipoint = 5
        else if (j.eq.3) then
           ipoint = 10
        else if (j.eq.4) then
           ipoint = 15
        else if (j.eq.5) then
           ipoint = 19
        else if (j.eq.6) then
           ipoint = 22
        else if (j.eq.7) then
           ipoint = 24
        else if (j.eq.8) then
           ipoint = 26
        else if (j.eq.9) then
           ipoint = 28
        else if (j.eq.10) then
           ipoint = 29
        else if (j.eq.11) then
           ipoint = 31
        else if (j.eq.12) then
           ipoint = 32
        else if (j.eq.13) then
           ipoint = 34
        else
           ipoint = ipoint + 1
        end if
        var_out(:,j) = tmp(:,ipoint)
        if(debug_level.ge.200) print *, " var_out(",xselect,",",j,") = ", var_out(xselect,j)
        !ccccc interpolation is not work well... ccccc
        ! call nearest_search_1d( iy, y(j), ipoint)
        ! call interpo_search_1d( iy, y(j), ipoint)
        ! if(debug_level.ge.100) print *, " j,ipoint,iy,y = ", j,ipoint,iy(j),y(ipoint)
        ! var_out(:,j) = tmp(:,ipoint)
        ! do i = 1, imax, 1
        !    var_out(i,j) = tmp(i,ipoint)
        ! end do
        !ccccccccccccccccccccccccccccccccccccccccccccc
     end do
     !ccccc interpolation is not work well... ccccc
     ! if(debug_level.ge.100) print *, " ny            = ", ny
     ! if(debug_level.ge.100) print *, " iy(:)         = ", iy
     ! do i = 1, imax
     !    CALL nearest_interp_1d( kmax, y(:), tmp(i,:), ny, iy(:), var_out(i,:) )
     !    if(debug_level.ge.100) print *, " var_out(",i,",:) = ", var_out(i,:)
     ! end do
     !ccccccccccccccccccccccccccccccccccccccccccccc
     
  else if (flag.eq.3) then
     ! x-t array
     if(debug_level.ge.100) print *, "x-t array"
     iy(:) = time(:)/real(60.) ! uint: [second] -> [hour]
!     iy(:) = time(:) ! uint: [second]
     select case (varname)
     case ('rain')
        if(debug_level.ge.100) print *, " unit: [cm] -> [mm]"
        do t = 1, tmax, 1
        do i = 1, imax, 1
!              var_out(i,t) = var_in(i,yselect,t,zselect)
           var_out(i,t) = var_in(i,1,t,1)
           var_out(i,t) = var_out(i,t)*real(10.) ! unit: [cm] -> [mm]
        end do
        if(debug_level.ge.200) print *, "t,iy,var_out = ",t,iy(t),var_out(xselect,t)
        end do
     case default
        do t = 1, tmax, 1
        do i = 1, imax, 1
!              var_out(i,t) = var_in(i,yselect,t,zselect)
           var_out(i,t) = var_in(i,1,t,1)
        end do
        if(debug_level.ge.200) print *, "t,iy,var_out = ",t,iy(t),var_out(xselect,t)
        end do
     end select
  end if
  if(debug_level.ge.100) print *, ""


  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Output 2D file
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! create the file
  call check( nf90_create(output, nf90_netcdf4, ncid) )
  if(debug_level.ge.100) print *, "Success: create the 2D output file"

  ! define the dimensions
  if(debug_level.ge.100) print *, " Start: define mode"
  call check( nf90_def_dim(ncid, "x", imax, xdimid) )
  call check( nf90_def_dim(ncid, "y", ny, ydimid) )
  if(debug_level.ge.100) print *, "  Success: define the dimensions"

  ! define the coordinate variables. 
  call check( nf90_def_var(ncid, "x", NF90_REAL, xdimid, xvarid) )
  call check( nf90_def_var(ncid, "y", NF90_REAL, ydimid, yvarid) )
  if(debug_level.ge.100) print *, "  Success: define the coordinate variables"

  ! define the netCDF variables for the 2D data with compressed format(netcdf4).
  dimids = (/ xdimid, ydimid /)
  chunks = (/ imax, ny /)
  call check( nf90_def_var(ncid, "z", NF90_REAL, dimids, varid, &
       chunksizes = chunks, shuffle = .TRUE., deflate_level = deflate_level) )
  if(debug_level.ge.100) print *, "  Success: define the netcdf variables"

  ! end define mode
  call check( nf90_enddef(ncid) )
  if(debug_level.ge.100) print *, " End: define mode"

  ! write the coordinate variable data
  call check( nf90_put_var(ncid, xvarid, x) )
  call check( nf90_put_var(ncid, yvarid, iy) )
  if(debug_level.ge.100) print *, "Success: write the coordinate variable data"


  ! write the pretend data
  count = (/ imax, ny /)
  start = (/ 1, 1 /)
  call check( nf90_put_var(ncid, varid, var_out, start = start, count = count) )
  if(debug_level.ge.100) print *, "Success: write the pretend data"

  ! close the file
  call check( nf90_close(ncid) )
  if(debug_level.ge.100) print *, "Success: close the netcdf file"
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
  subroutine nearest_search_1d( x, point, i, hx, hp )
    ! 1 次元最近傍探索ルーチン
    ! interpo_search_1d から値を求め, その値と +1 した値の距離を比較して
    ! 距離の短い方を選択する.
    implicit none
    real, intent(in) :: x(:)  ! 漸増配列
    real, intent(in) :: point  ! この点
    integer, intent(inout) :: i  ! point の最近傍地点の要素番号
    real, intent(in), optional :: hx(size(x))  ! x 座標のスケール因子
    real, intent(in), optional :: hp  ! point でのスケール因子 !! まだ用意しただけ
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
  
end program ncedit
