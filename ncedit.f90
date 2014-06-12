!
! Program of ncedit.f90
! original program coded by Takashi Unuma, Kyoto Univ.
! Last modified: 2014/06/12
!

program ncedit

  use netcdf
  
  implicit none

  integer :: ncid, varid, xdimid, ydimid, zdimid, tdimid, xvarid, yvarid, deflate_level
  integer :: i, j, k, t, imax, jmax, kmax, tmax, ny, ipoint
  integer :: flag, xselect, yselect, zselect, tselect
  integer, dimension(2) :: start, count, dimids, chunks
  real :: dy
  real, dimension(:),     allocatable :: x, y, iy, z, time
  real, dimension(:,:),   allocatable :: var_out, tmp
  real, dimension(:,:,:,:), allocatable :: var_in
  character(len=20) :: varname
  character(len=42) :: input, output
  integer :: debug_level

!  real :: nan = (/ Z'7fffffff' /)
  real :: nan = -999.

  
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

  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Initialization
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  allocate( x(imax), y(jmax), z(kmax), time(tmax), iy(ny) )
  allocate( tmp(imax,kmax), var_out(imax,ny) )
  select case (flag)
  case (3)
     allocate( var_in(imax,jmax,tmax,kmax) )
  case default
     allocate( var_in(imax,jmax,kmax,tmax) )
  end select
  var_in(:,:,:,:) = nan
  tmp(:,:)        = nan
  var_out(:,:)    = nan

  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Input 4D file
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! open the original netcdf file
  call check( nf90_open(input, nf90_nowrite, ncid) )
  if(debug_level.ge.100) print *, "Success: open the file"
  if(debug_level.ge.100) print *, " ncid          = ", ncid

  ! inquire and get x coordinate
  call check( nf90_inq_varid(ncid, 'ni', xdimid) )
  if(debug_level.ge.100) print *, "Success: inquire the xdimid"
  if(debug_level.ge.100) print *, " xdimid        = ", xdimid
  call check( nf90_get_var(ncid, xdimid, x) )
  if(debug_level.ge.100) print *, "Success: get the x coordinate"
  if(debug_level.ge.100) print *, " x(:)          = ", x

  ! inquire and get y coordinate
  call check( nf90_inq_varid(ncid, 'nj', ydimid) )
  if(debug_level.ge.100) print *, "Success: inquire the ydimid"
  if(debug_level.ge.100) print *, " ydimid        = ", ydimid
  call check( nf90_get_var(ncid, ydimid, y) )
  if(debug_level.ge.100) print *, "Success: get the y coordinate"
  if(debug_level.ge.100) print *, " y(:)          = ", y

  ! inquire and get z coordinate
  call check( nf90_inq_varid(ncid, 'nk', zdimid) )
  if(debug_level.ge.100) print *, "Success: inquire the zdimid"
  if(debug_level.ge.100) print *, " zdimid        = ", zdimid
  call check( nf90_get_var(ncid, zdimid, z) )
  if(debug_level.ge.100) print *, "Success: get the z coordinate"
  if(debug_level.ge.100) print *, " z(:)          = ", z
  !  if(debug_level.ge.100) then
  !     do k = 1, kmax, 1
  !        print *, " z(",k,") = ", z(k)
  !     end do
  !  end if

  ! inquire and get time coordinate
  call check( nf90_inq_varid(ncid, 'time', tdimid) )
  if(debug_level.ge.100) print *, "Success: inquire the tdimid"
  if(debug_level.ge.100) print *, " tdimid        = ", tdimid
  call check( nf90_get_var(ncid, tdimid, time) )
  if(debug_level.ge.100) print *, "Success: get the time coordinate"
  if(debug_level.ge.100) print *, " time(:)       = ", time

  
  ! inquire and get var
  select case (varname)
  case ('water')
     ! for all water (qc+qr+qi+qc+qg) on the microphysics processes
     call check( nf90_inq_varid(ncid, "qc", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.100) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.100) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     tmp(:,:) = var_in(:,yselect,:,tselect)
     
     call check( nf90_inq_varid(ncid, "qr", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.100) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.100) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     tmp(:,:) = tmp(:,:) + var_in(:,yselect,:,tselect)
     
     call check( nf90_inq_varid(ncid, "qi", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.100) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.100) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     tmp(:,:) = tmp(:,:) + var_in(:,yselect,:,tselect)
     
     call check( nf90_inq_varid(ncid, "qs", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.100) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.100) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     tmp(:,:) = tmp(:,:) + var_in(:,yselect,:,tselect)
     
     call check( nf90_inq_varid(ncid, "qg", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.100) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.100) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     tmp(:,:) = tmp(:,:) + var_in(:,yselect,:,tselect)
     
  case default
     ! the others
     call check( nf90_inq_varid(ncid, varname, varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.100) print *, " varid         = ", varid
     call check( nf90_get_var(ncid, varid, var_in) )
     if(debug_level.ge.100) print *, "Success: get the var array"
     if(debug_level.ge.100) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)

  end select

  ! close netcdf file
  call check( nf90_close(ncid) )
  if(debug_level.ge.100) print *, "Success: close the netcdf data"


  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! make 2D array
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  if(flag.eq.1) then
     ! x-y array
     ! select one of the 2D array
     tmp(:,:) = var_in(:,:,zselect,tselect)
     print *, "under construction"

  else if(flag.eq.2) then
     ! x-z array
     ! select one of the 2D array
     select case (varname)
     case ('water')
        print *, "It has already allocated"
     case default
        tmp(:,:) = var_in(:,yselect,:,tselect)
     end select
     if(debug_level.ge.100) print *, " tmp(",xselect,",:)    = ", tmp(xselect,:)
     
     ! interpolate the stretched coordinate to constant dy coordinate
     do k = 1, ny, 1
        iy(k) = (k-1)*dy
     end do
     if(debug_level.ge.100) print *, " iy(:)         = ", iy
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
        if(debug_level.ge.100) print *, " var_out(",xselect,",",j,") = ", var_out(xselect,j)
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

     ! t-x array
     print *, "t-x array"
     iy(:) = time(:)
     do t = 1, tmax, 1
        do i = 1, imax, 1
           var_out(i,t) = var_in(i,yselect,t,zselect)
        end do
        print *, "t,iy,var_out = ",t,iy(t),var_out(xselect,t)
     end do

  end if


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
  
end program ncedit
