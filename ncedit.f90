!
! N C E D I T
!
! original program coded by Takashi Unuma, Kyoto Univ.
! last modified: 2015/01/05
!

program ncedit

  use netcdf
  
  implicit none

  ! I/O variables and arrays
  integer :: flag, xselect, yselect, zselect, tselect
  integer :: interp_x, interp_y
  integer :: imax, jmax, kmax, tmax
  real :: dx, dy, angle
  real, dimension(:),       allocatable :: x, y, z, time_in
  real, dimension(:,:,:,:), allocatable :: var_in
  real, dimension(:),       allocatable :: ix, iy
  real, dimension(:,:),     allocatable :: var_out
  character(len=20) :: varname, interp_method, output_type
  character(len=42) :: input, output
  integer :: deflate_level, debug_level

  ! local variables
  character(len=20) :: ivarname
  integer :: i, j, k, t, ipoint, nx, ny
  integer :: inx, iny, inz, int, ista, iend
  integer :: time_min, time_max
  integer :: ncid, varid, xdimid, ydimid, zdimid, tdimid, xvarid, yvarid
  integer, dimension(2) :: ostart, ocount, dimids, chunks
  integer, dimension(4) :: istart, icount, iistart, iicount
  integer, dimension(2) :: ipoints
  real :: tmp0, tmpmax, tmpmin
  real :: gbcape, gbcin, gblclp, gblfcp, gblnbp, gblclz, gblfcz, gblnbz
  real, dimension(:),       allocatable :: yy, itmp1, itmp2, itmp3
  real, dimension(:,:),     allocatable :: tmp, tmp1, tmp2, tmp3, tmp4, tmp5
  real, dimension(:,:,:),   allocatable :: tmpi, tmpi1, tmpi2, tmpi3, tmpi4, tmpi5
  real, dimension(:,:,:),   allocatable :: tmpc, tmpc1, tmpc2, tmpc3
  real, dimension(:,:,:,:), allocatable :: ivar_in, tmp4d, tmp4d1, tmp4d2, tmp4d3
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
  if(debug_level.ge.0)   print '(a18,a)',   " output        = ", trim(output)
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


  ! check run-time-errors
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

  ! check the output format
  if( (flag.eq.1).or.(flag.eq.2).or.(flag.eq.3) ) then
     select case (output_type)
     case ('b6h','a6h','all')
        print *, " Error: the variable of output_type is incorrect"
        print *, "  output_type should be 'nc3', 'nc3_64bit', or 'nc4'"
        stop
     end select
  end if
  if( flag.eq.4 ) then
     select case (varname)
     case ('vtotwcave','vtotwcstd','vwmax','vwave',         &
           'vthetaeave','vthetaeavec','vthetaave','vqvave', &
           'vrhave','vrhavec','vqvavec'                     )
        select case (output_type)
        case ('nc3','nc3_64bit','nc4')
           print *, " Error: the variable of output_type is incorrect"
           print *, "  output_type should be 'b6h', 'a6h', or 'all'"
           stop
        case ('b6h')
           output = trim(varname)//"_"//trim(output_type)//".txt"
           time_min = 121 ! referred to as 2h
           time_max = 360 ! referred to as 6h
        case ('a6h')
           output = trim(varname)//"_"//trim(output_type)//".txt"
           time_min = 361 ! referred to as 6h
           time_max = 600 ! referred to as 10h
        case ('all')
           output = trim(varname)//"_"//trim(output_type)//".txt"
           time_min = 2   ! referred to as 0h
           time_max = 721 ! referred to as 12h
        case default
           time_min = nan
           time_max = nan
        end select
     end select
  end if

  ! define area averaging
  ista = 1
  iend = imax
!  ista = imax/2 + 1
!  iend = imax/2 + 100
!  ista = imax/2 - 49
!  iend = imax/2 + 150


  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Input 4D file
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! open the original netcdf file
  call check( nf90_open(input, nf90_nowrite, ncid) )
  if(debug_level.ge.100) print *, "Success: open the file"
  if(debug_level.ge.200) print *, " ncid          = ", ncid

  ! inquire and get x coordinate
  call check( nf90_inq_varid(ncid, 'ni', xdimid) )
  if(debug_level.ge.200) print *, "Success: inquire the xdimid"
  if(debug_level.ge.200) print *, " xdimid        = ", xdimid
!  call check( nf90_inquire_dimension(ncid, xdimid, len = imax) )
!  if(debug_level.ge.100) print *, "Success: inquire the xdimid"
!  if(debug_level.ge.200) print *, "  imax         = ", imax
  allocate( x(imax) )
  call check( nf90_get_var(ncid, xdimid, x) )
  if(debug_level.ge.100) print *, "Success: get the x coordinate"
  if(debug_level.ge.200) print *, " x(1:imax)          = ", x

  ! inquire and get y coordinate
  call check( nf90_inq_varid(ncid, 'nj', ydimid) )
  if(debug_level.ge.200) print *, "Success: inquire the ydimid"
  if(debug_level.ge.200) print *, " ydimid        = ", ydimid
!  call check( nf90_inquire_dimension(ncid, ydimid, len = jmax) )
!  if(debug_level.ge.100) print *, "Success: inquire the ydimid"
!  if(debug_level.ge.200) print *, "  jmax         = ", jmax
  allocate( y(jmax) )
  call check( nf90_get_var(ncid, ydimid, y) )
  if(debug_level.ge.100) print *, "Success: get the y coordinate"
  if(debug_level.ge.200) print *, " y(1:jmax)          = ", y

  ! inquire and get z coordinate
  call check( nf90_inq_varid(ncid, 'nk', zdimid) )
  if(debug_level.ge.200) print *, "Success: inquire the zdimid"
  if(debug_level.ge.200) print *, " zdimid        = ", zdimid
!  call check( nf90_inquire_dimension(ncid, zdimid, len = kmax) )
!  if(debug_level.ge.100) print *, "Success: inquire the zdimid"
!  if(debug_level.ge.200) print *, "  kmax         = ", kmax
  allocate( z(kmax) )
  call check( nf90_get_var(ncid, zdimid, z) )
  if(debug_level.ge.100) print *, "Success: get the z coordinate"
  if(debug_level.ge.200) print *, " z(1:kmax)          = ", z

  ! inquire and get time coordinate
  call check( nf90_inq_varid(ncid, 'time', tdimid) )
  if(debug_level.ge.200) print *, "Success: inquire the tdimid"
  if(debug_level.ge.200) print *, " tdimid        = ", tdimid
!  call check( nf90_inquire_dimension(ncid, tdimid, len = tmax) )
!  if(debug_level.ge.100) print *, "Success: inquire the tdimid"
!  if(debug_level.ge.200) print *, "  tmax         = ", tmax
  allocate( time_in(tmax) )
  call check( nf90_get_var(ncid, tdimid, time_in) )
  if(debug_level.ge.100) print *, "Success: get the time coordinate"
  if(debug_level.ge.200) print *, " time_in(1:tmax)    = ", time_in
  if(debug_level.ge.100) print *, ""


  ! allocate arrays
  select case (flag)
  case (1)
     ! x-y
     select case (varname)
     case ('rain','cpc','cref')
        inx = imax
        iny = jmax
        inz = 1
        int = 1
        allocate( var_in(imax,jmax,1,1) ) ! xy
        istart = (/ 1, 1, tselect, 1 /)
        icount = (/ imax, jmax, 1, 1 /)
        var_in(1:imax,1:jmax,1,1) = nan
     case default
        inx = imax
        iny = jmax
        inz = 1
        int = 1
        allocate( var_in(imax,jmax,1,1) ) ! xy
        istart = (/ 1, 1, zselect, tselect /)
        icount = (/ imax, jmax, 1, 1 /)
        var_in(1:imax,1:jmax,1,1) = nan
     end select
     allocate( tmp(imax,jmax) )
     allocate( tmp1(imax,jmax),tmp2(imax,jmax),tmp3(imax,jmax) )
     allocate( tmp4(imax,jmax),tmp5(imax,jmax) )
     tmp(1:imax,1:jmax) = nan
     tmp1(1:imax,1:jmax) = nan
     tmp2(1:imax,1:jmax) = nan
     tmp3(1:imax,1:jmax) = nan
     tmp4(1:imax,1:jmax) = nan
     tmp5(1:imax,1:jmax) = nan
     !
  case (2)
     ! x-z
     select case (varname)
     case ('lwdt')
        ! for calculation of LWDT in FT98
        inx = imax
        iny = 1
        inz = kmax
        int = 2
        allocate( var_in(imax,1,kmax,2) ) ! xz
        istart = (/ 1, yselect, 1, tselect /)
        icount = (/ imax, 1, kmax, 2 /)
        var_in(1:imax,1,1:kmax,1:2) = 0.
        allocate( tmp(imax,kmax) )
        tmp(1:imax,1:kmax) = 0.
     case ('pwdt')
        ! for calculation of PWDT (=VPGA+BUOY) in FT98 and TS00
        inx = imax
        iny = 1
        inz = kmax
        int = 1
        allocate( var_in(imax,1,kmax,1) ) ! xz
        istart = (/ 1, yselect, 1, tselect /)
        icount = (/ imax, 1, kmax, 1 /)
        var_in(1:imax,1,1:kmax,1) = 0.
        allocate( ivar_in(imax,1,kmax,1) ) ! for base state (t = 1)
        iistart = (/ 1, yselect, 1, 1 /)
        iicount = (/ imax, 1, kmax, 1 /)
        ivar_in(1:imax,1,1:kmax,1) = 0.
        allocate( tmp(imax,kmax) )
        tmp(1:imax,1:kmax) = 0.
        allocate( tmp1(imax,kmax),tmp2(imax,kmax),tmp3(imax,kmax) )
        tmp1(1:imax,1:kmax) = nan
        tmp2(1:imax,1:kmax) = nan
        tmp3(1:imax,1:kmax) = nan
     case ('wadv','load')
        ! for calculations of WADV and LOAD in FT98 and TS00
        inx = imax
        iny = 1
        inz = kmax
        int = 1
        allocate( var_in(imax,1,kmax,1) ) ! xz
        istart = (/ 1, yselect, 1, tselect /)
        icount = (/ imax, 1, kmax, 1 /)
        var_in(1:imax,1,1:kmax,1) = 0.
        allocate( tmp(imax,kmax) )
        tmp(1:imax,1:kmax) = 0.
        allocate( tmp1(imax,kmax) )
        tmp1(1:imax,1:kmax) = 0.
     case ('vpga','buoy','dbdx','dbdz')
        ! for calculations of VPGA and BUOY in FT98
        inx = imax
        iny = 1
        inz = kmax
        int = 1
        allocate( var_in(imax,1,kmax,1) ) ! xz
        istart = (/ 1, yselect, 1, tselect /)
        icount = (/ imax, 1, kmax, 1 /)
        var_in(1:imax,1,1:kmax,1) = 0.
        allocate( ivar_in(imax,1,kmax,1) ) ! for base state (t = 1)
        iistart = (/ 1, yselect, 1, 1 /)
        iicount = (/ imax, 1, kmax, 1 /)
        ivar_in(1:imax,1,1:kmax,1) = 0.
        allocate( tmp(imax,kmax) )
        tmp(1:imax,1:kmax) = 0.
        allocate( tmp1(imax,kmax),tmp2(imax,kmax) )
        tmp1(1:imax,1:kmax) = nan
        tmp2(1:imax,1:kmax) = nan
     case ('sruinterp','srvinterp')
        ! for calculations of storm relative wind speed (wind speed - base state)
        inx = imax
        iny = 1
        inz = kmax
        int = 1
        allocate( var_in(imax,1,kmax,1) ) ! xz
        istart = (/ 1, yselect, 1, tselect /)
        icount = (/ imax, 1, kmax, 1 /)
        var_in(1:imax,1,1:kmax,1) = 0.
        allocate( ivar_in(imax,1,kmax,1) ) ! for base state (t = 1)
        iistart = (/ 1, yselect, 1, 1 /)
        iicount = (/ imax, 1, kmax, 1 /)
        ivar_in(1:imax,1,1:kmax,1) = 0.
        allocate( tmp(imax,kmax) )
        tmp(1:imax,1:kmax) = 0.
     case default
        inx = imax
        iny = 1
        inz = kmax
        int = 1
        allocate( var_in(imax,1,kmax,1) ) ! xz
        istart = (/ 1, yselect, 1, tselect /)
        icount = (/ imax, 1, kmax, 1 /)
        var_in(1:imax,1,1:kmax,1) = nan
        allocate( tmp(imax,kmax) )
        allocate( tmp1(imax,kmax),tmp2(imax,kmax),tmp3(imax,kmax) )
        allocate( tmp4(imax,kmax),tmp5(imax,kmax) )
        tmp(1:imax,1:kmax) = nan
        tmp1(1:imax,1:kmax) = nan
        tmp2(1:imax,1:kmax) = nan
        tmp3(1:imax,1:kmax) = nan
        tmp4(1:imax,1:kmax) = nan
        tmp5(1:imax,1:kmax) = nan
     end select
     !
  case (3)
     ! x-t
     select case (varname)
     case ('cape','cin','lfc','lnb','lins')
        ! Calculate mlCAPE, mlCIN, LFC, LNB, and mlCAPE/mlCIN by using getcape
        inx = imax
        iny = 1
        inz = kmax
        int = tmax
        allocate( var_in(imax,1,kmax,tmax) ) ! xzt
        istart = (/ 1, yselect, 1, 1 /)
        icount = (/ imax, 1, kmax, tmax /)
        var_in(1:imax,1,1:kmax,1:tmax) = nan
        allocate( tmpc1(imax,kmax,tmax),tmpc2(imax,kmax,tmax),tmpc3(imax,kmax,tmax) )
        tmpc1(1:imax,1:kmax,1:tmax) = nan
        tmpc2(1:imax,1:kmax,1:tmax) = nan
        tmpc3(1:imax,1:kmax,1:tmax) = nan
        allocate( tmp(imax,tmax) )
        tmp(1:imax,1:tmax) = nan
     case ('thetae')
        ! Calculate thetae
        inx = imax
        iny = 1
        inz = 1
        int = tmax
        allocate( var_in(imax,1,1,tmax) ) ! xt
        istart = (/ 1, yselect, zselect, 1 /)
        icount = (/ imax, 1, 1, tmax /)
        var_in(1:imax,1,1,1:tmax) = nan
        allocate( tmp1(imax,tmax),tmp2(imax,tmax),tmp3(imax,tmax) )
        tmp1(1:imax,1:tmax) = nan
        tmp2(1:imax,1:tmax) = nan
        tmp3(1:imax,1:tmax) = nan
        allocate( tmp(imax,tmax) )
        tmp(1:imax,1:tmax) = nan
     case ('mlthetae')
        ! Calculate mixed-layer thetae
        inx = imax
        iny = 1
        inz = 5
        int = tmax
        allocate( var_in(imax,1,5,tmax) ) ! xt
        istart = (/ 1, yselect, 1, 1 /)
        icount = (/ imax, 1, 5, tmax /)
        var_in(1:imax,1,1:5,1:tmax) = nan
        allocate( tmp(imax,tmax) )
        tmp(1:imax,1:tmax) = nan
        allocate( tmpi1(imax,5,tmax),tmpi2(imax,5,tmax) )
        allocate( tmpi3(imax,5,tmax),tmpi(imax,5,tmax) )
        tmpi1(1:imax,1:5,1:tmax) = nan
        tmpi2(1:imax,1:5,1:tmax) = nan
        tmpi3(1:imax,1:5,1:tmax) = nan
        tmpi(1:imax,1:5,1:tmax) = nan
     case ('xvort','yvort','zvort')
        inx = imax
        iny = 1
        inz = 1
        int = tmax
        allocate( var_in(imax,1,1,tmax) ) ! xt
        istart = (/ 1, yselect, zselect, 1 /)
        icount = (/ imax, 1, 1, tmax /)
        var_in(1:imax,1,1,1:tmax) = nan
        allocate( tmp(imax,tmax) )
        tmp(1:imax,1:tmax) = nan
     case ('xtwater')
        inx = imax
        iny = jmax
        inz = 1
        int = tmax
        allocate( var_in(imax,jmax,1,tmax) ) ! xyt
        istart = (/ 1, 1, zselect, 1 /)
        icount = (/ imax, jmax, 1, tmax /)
        var_in(1:imax,1:jmax,1,1:tmax) = nan
        allocate( tmp(imax,tmax) )
        allocate( tmp1(imax,tmax),tmp2(imax,tmax),tmp3(imax,tmax) )
        allocate( tmp4(imax,tmax),tmp5(imax,tmax) )
        tmp(1:imax,1:tmax) = nan
        tmp1(1:imax,1:tmax) = nan
        tmp2(1:imax,1:tmax) = nan
        tmp3(1:imax,1:tmax) = nan
        tmp4(1:imax,1:tmax) = nan
        tmp5(1:imax,1:tmax) = nan
     case ('cxtwater')
        inx = imax
        iny = jmax
        inz = 1
        int = tmax
        allocate( var_in(imax,jmax,1,tmax) ) ! xyt
        istart = (/ 1, 1, zselect, 1 /)
        icount = (/ imax, jmax, 1, tmax /)
        var_in(1:imax,1:jmax,1,1:tmax) = nan
        allocate( tmp(imax,tmax) )
        allocate( tmp1(imax,tmax),tmp2(imax,tmax),tmp3(imax,tmax) )
        allocate( tmp4(imax,tmax),tmp5(imax,tmax) )
        tmp(1:imax,1:tmax) = 0.
        tmp1(1:imax,1:tmax) = nan
        tmp2(1:imax,1:tmax) = nan
        tmp3(1:imax,1:tmax) = nan
        tmp4(1:imax,1:tmax) = nan
        tmp5(1:imax,1:tmax) = nan
     case ('pw')
        ! precipitable water
        inx = imax
        iny = 1
        inz = kmax
        int = tmax
        allocate( var_in(imax,1,kmax,tmax) ) ! xzt
        istart = (/ 1, yselect, 1, 1 /)
        icount = (/ imax, 1, kmax, tmax /)
        var_in(1:imax,1,1:kmax,1:tmax) = nan
        allocate( tmpc1(imax,kmax,tmax),tmpc2(imax,kmax,tmax) )
        tmpc1(1:imax,1:kmax,1:tmax) = nan
        tmpc2(1:imax,1:kmax,1:tmax) = nan
        allocate( tmp(imax,tmax) )
        tmp(1:imax,1:tmax) = nan
        !
     ! case ('cpc','cph')
     !    inx = imax
     !    iny = 1
     !    inz = kmax
     !    int = tmax
     !    allocate( var_in(imax,1,kmax,tmax) ) ! xt
     !    istart = (/ 1, yselect, 1, 1 /)
     !    icount = (/ imax, 1, kmax, tmax /)
     !    var_in(1:imax,1,1:kmax,1:tmax) = nan
     !    allocate( tmp(imax,tmax) )
     !    allocate( tmpi1(imax,kmax,tmax),tmpi2(imax,kmax,tmax),tmpi3(imax,kmax,tmax) )
     !    allocate( tmpi4(imax,kmax,tmax),tmpi5(imax,kmax,tmax) )
     !    tmp(1:imax,1:tmax) = nan
     !    tmpi1(1:imax,1:kmax,1:tmax) = nan
     !    tmpi2(1:imax,1:kmax,1:tmax) = nan
     !    tmpi3(1:imax,1:kmax,1:tmax) = nan
     !    tmpi4(1:imax,1:kmax,1:tmax) = nan
     !    tmpi5(1:imax,1:kmax,1:tmax) = nan
        !
     case default
        inx = imax
        iny = 1
        inz = tmax
        int = 1
        allocate( var_in(imax,1,tmax,1) )
        istart = (/ 1, yselect, 1, 1 /)
        icount = (/ imax, 1, tmax, 1 /)
        var_in(1:imax,1,1:tmax,1) = nan
        allocate( tmp(imax,tmax) )
        tmp(1:imax,1:tmax) = nan
     end select
     !
  case (4)
     select case (varname)
     case ('maxrain','averain','apw','apm','aps','ape')
        ! a variable for time series
        inx = imax
        iny = jmax
        inz = tmax
        int = 1
        ista = imax/2      ! which is reffered to X = 0 km in the X cordinate
        iend = imax/2 + 20 ! which is reffered to X = +20 km in the X cordinate
        allocate( var_in(imax,jmax,tmax,1) ) ! xyt
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ imax, jmax, tmax, 1 /)
        var_in(1:imax,1:jmax,1:tmax,1) = nan
        allocate( tmp(tmax,1) )
        tmp(1:tmax,1) = nan
     case ('vtotwcave','vtotwcstd','vwmax','vwave','vthetaave','vqvave','vqvavec')
        ! mean/std values of the vertical profile of total water- and ice-phase mixing ratio
        ! maximum/mean values of the vertical profile of updraft velocity
        ! mean values of the vertical profile of theta and water vapor mixing ratio
        inx = imax
        iny = jmax
        inz = 1
        int = tmax
        allocate( var_in(imax,jmax,1,tmax) ) ! xyt + z-loop
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ imax, jmax, 1, tmax /)
        var_in(1:imax,1:jmax,1,1:tmax) = nan
        allocate( tmp(kmax,1) )
        tmp(1:kmax,1) = 0.
        allocate( tmpi(imax,jmax,tmax) )
        tmpi(1:imax,1:jmax,1:tmax) = 0.
     case ('vthetaeave','vthetaeavec','vrhave','vrhavec')
        ! A mean value of the vertical profile of theta-e and that inside of clouds
        ! mean values of the vertical profile of relative humidity and that inside of clouds
        inx = imax
        iny = jmax
        inz = 1
        int = tmax
        allocate( var_in(imax,jmax,1,tmax) ) ! xyt + z-loop
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ imax, jmax, 1, tmax /)
        var_in(1:imax,1:jmax,1,1:tmax) = nan
        allocate( tmp(tmax,1) )
        tmp(1:kmax,1) = 0.
        allocate( tmpi(imax,jmax,tmax) )
        tmpi(1:imax,1:jmax,1:tmax) = 0.
        allocate( tmpi1(imax,jmax,tmax),tmpi2(imax,jmax,tmax),tmpi3(imax,jmax,tmax) )
        tmpi1(1:imax,1:jmax,1:tmax) = 0.
        tmpi2(1:imax,1:jmax,1:tmax) = 0.
        tmpi3(1:imax,1:jmax,1:tmax) = 0.
        allocate( tmpc(imax,jmax,tmax) )
        tmpc(1:imax,1:jmax,1:tmax) = 0.
     case ('vtheta','vqv')
        ! An area-averaged value of the vertical profile of theta and qv
        inx = imax
        iny = jmax
        inz = 1
        int = 1
        allocate( var_in(imax,jmax,1,1) ) ! xy + z-loop
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ imax, jmax, 1, 1 /)
        var_in(1:imax,1:jmax,1,1) = nan
        allocate( tmp(tmax,1) )
        tmp(1:kmax,1) = 0.
     case ('vthetae','vthetaes','vthetav')
        ! An area-averaged value of the vertical profile of theta-e,theta-es, and virtual theta
        inx = imax
        iny = jmax
        inz = 1
        int = 1
        ista = imax/2 - 90 ! which is reffered to X = -90 km in the X cordinate
        iend = imax/2 - 39 ! which is reffered to X = -40 km in the X cordinate
        allocate( var_in(imax,jmax,1,1) ) ! xy + z-loop
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ imax, jmax, 1, 1 /)
        var_in(1:imax,1:jmax,1,1) = nan
        allocate( tmp(tmax,1) )
        tmp(1:kmax,1) = 0.
        allocate( tmp1(imax,jmax),tmp2(imax,jmax),tmp3(imax,jmax),tmp4(imax,jmax) )
        tmp1(1:imax,1:jmax) = 0.
        tmp2(1:imax,1:jmax) = 0.
        tmp3(1:imax,1:jmax) = 0.
        tmp4(1:imax,1:jmax) = 0.
     case ('vthetaeca')
        ! An area-averaged value of the vertical profile of theta-e in the convective area
        inx = imax
        iny = jmax
        inz = 1
        int = 1
        ista = imax/2 + 11 ! which is reffered to X = +10 km in the X cordinate
        iend = imax/2 + 50 ! which is reffered to X = +50 km in the X cordinate
        allocate( var_in(imax,jmax,1,1) ) ! xy + z-loop
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ imax, jmax, 1, 1 /)
        var_in(1:imax,1:jmax,1,1) = nan
        allocate( tmp(tmax,1) )
        tmp(1:kmax,1) = 0.
        allocate( tmp1(imax,jmax),tmp2(imax,jmax),tmp3(imax,jmax),tmp4(imax,jmax) )
        tmp1(1:imax,1:jmax) = 0.
        tmp2(1:imax,1:jmax) = 0.
        tmp3(1:imax,1:jmax) = 0.
        tmp4(1:imax,1:jmax) = 0.
     case ('vcape')
        ! An area-averaged value of the vertical profile of CAPE
        inx = imax
        iny = jmax
        inz = kmax
        int = 1
        ista = imax/2 - 90 ! which is reffered to X = -90 km in the X cordinate
        iend = imax/2 - 39 ! which is reffered to X = -40 km in the X cordinate
        allocate( var_in(imax,jmax,kmax,1) ) ! xy + z-loop
        istart = (/ 1, 1, 1, tselect /)
        icount = (/ imax, jmax, kmax, 1 /)
        var_in(1:imax,1:jmax,1:kmax,1) = nan
        allocate( tmp(kmax,1) )
        tmp(1:kmax,1) = 0.
        allocate( tmpc1(imax,jmax,kmax),tmpc2(imax,jmax,kmax),tmpc3(imax,jmax,kmax) )
        tmpc1(1:imax,1:jmax,1:kmax) = nan
        tmpc2(1:imax,1:jmax,1:kmax) = nan
        tmpc3(1:imax,1:jmax,1:kmax) = nan
     case ('vcapeca')
        ! An area-averaged value of the vertical profile of CAPE in the convective area
        inx = imax
        iny = jmax
        inz = kmax
        int = 1
        ista = imax/2 + 11 ! which is reffered to X = +10 km in the X cordinate
        iend = imax/2 + 50 ! which is reffered to X = +50 km in the X cordinate
        allocate( var_in(imax,jmax,kmax,1) ) ! xy + z-loop
        istart = (/ 1, 1, 1, tselect /)
        icount = (/ imax, jmax, kmax, 1 /)
        var_in(1:imax,1:jmax,1:kmax,1) = nan
        allocate( tmp(kmax,1) )
        tmp(1:kmax,1) = 0.
        allocate( tmpc1(imax,jmax,kmax),tmpc2(imax,jmax,kmax),tmpc3(imax,jmax,kmax) )
        tmpc1(1:imax,1:jmax,1:kmax) = nan
        tmpc2(1:imax,1:jmax,1:kmax) = nan
        tmpc3(1:imax,1:jmax,1:kmax) = nan
     case ('tthetaeave')
        ! Time series of the area- and mixed layer-averaged value of theta-e
        inx = imax
        iny = jmax
        inz = 5
        int = tmax
        allocate( var_in(imax,jmax,5,tmax) ) ! xyt + z-loop
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ imax, jmax, 5, tmax /)
        var_in(1:imax,1:jmax,1:5,1:tmax) = nan
        allocate( tmp(tmax,1) )
        tmp(1:tmax,1) = 0.
        allocate( tmp4d(imax,jmax,5,tmax) )
        tmp4d(1:imax,1:jmax,1:5,1:tmax) = 0.
        allocate( tmp4d1(imax,jmax,5,tmax),tmp4d2(imax,jmax,5,tmax),tmp4d3(imax,jmax,5,tmax) )
        tmp4d1(1:imax,1:jmax,1:5,1:tmax) = 0.
        tmp4d2(1:imax,1:jmax,1:5,1:tmax) = 0.
        tmp4d3(1:imax,1:jmax,1:5,1:tmax) = 0.
     case ('tqvave')
        ! time series of the area-averaged value of qv
        inx = imax
        iny = jmax
        inz = 1
        int = tmax
        allocate( var_in(imax,jmax,1,tmax) ) ! xyt + z-loop
        istart = (/ 1, 1, zselect, 1 /)
        icount = (/ imax, jmax, 1, tmax /)
        var_in(1:imax,1:jmax,1,1:tmax) = nan
        allocate( tmp(tmax,1) )
        tmp(1:tmax,1) = 0.
        allocate( tmpi(imax,jmax,tmax) )
        tmpi(1:imax,1:jmax,1:tmax) = 0.
     case ('tcapeave','tcinave','tlfcave','tlnbave','tcapeavenc')
        ! for calculation of CAPE using getcape
        inx = imax
        iny = 1
        inz = kmax
        int = tmax
        allocate( var_in(imax,1,kmax,tmax) ) ! xzt
        istart = (/ 1, yselect, 1, 1 /)
        icount = (/ imax, 1, kmax, tmax /)
        var_in(1:imax,1,1:kmax,1:tmax) = nan
        allocate( tmpc1(imax,kmax,tmax),tmpc2(imax,kmax,tmax),tmpc3(imax,kmax,tmax) )
        tmpc1(1:imax,1:kmax,1:tmax) = 0.
        tmpc2(1:imax,1:kmax,1:tmax) = 0.
        tmpc3(1:imax,1:kmax,1:tmax) = 0.
        allocate( tmp(tmax,1) )
        tmp(1:tmax,1) = 0.
        allocate( tmp1(imax,tmax),tmp2(imax,tmax),tmp3(imax,tmax),tmp4(imax,tmax) )
        tmp1(1:imax,1:tmax) = 0.
        tmp2(1:imax,1:tmax) = 0.
        tmp3(1:imax,1:tmax) = 0.
        tmp4(1:imax,1:tmax) = 0.
        allocate( itmp1(kmax),itmp2(kmax),itmp3(kmax) )
     case ('tcapeaveus')
        ! for calculation of upstream CAPE using getcape
        inx = imax
        iny = 1
        inz = kmax
        int = tmax
        ista = imax/2 - 70 ! which is reffered to X = -70 km in the X cordinate
        iend = imax/2 + 19 ! which is reffered to X = -20 km in the X cordinate
        allocate( var_in(imax,1,kmax,tmax) ) ! xzt
        istart = (/ 1, yselect, 1, 1 /)
        icount = (/ imax, 1, kmax, tmax /)
        var_in(1:imax,1,1:kmax,1:tmax) = nan
        allocate( tmpc1(imax,kmax,tmax),tmpc2(imax,kmax,tmax),tmpc3(imax,kmax,tmax) )
        tmpc1(1:imax,1:kmax,1:tmax) = 0.
        tmpc2(1:imax,1:kmax,1:tmax) = 0.
        tmpc3(1:imax,1:kmax,1:tmax) = 0.
        allocate( tmp(tmax,1) )
        tmp(1:tmax,1) = 0.
        allocate( tmp1(imax,tmax),tmp2(imax,tmax),tmp3(imax,tmax),tmp4(imax,tmax) )
        tmp1(1:imax,1:tmax) = 0.
        tmp2(1:imax,1:tmax) = 0.
        tmp3(1:imax,1:tmax) = 0.
        tmp4(1:imax,1:tmax) = 0.
        allocate( itmp1(kmax),itmp2(kmax),itmp3(kmax) )
     case ('tcpc')
        inx = imax
        iny = 1
        inz = kmax
        int = tmax
        allocate( var_in(imax,1,kmax,tmax) ) ! xt
        istart = (/ 1, yselect, 1, 1 /)
        icount = (/ imax, 1, kmax, tmax /)
        var_in(1:imax,1,1:kmax,1:tmax) = nan
        allocate( tmp(tmax,1) )
        tmp(1:tmax,1) = 0.
        allocate( tmpi1(imax,kmax,tmax),tmpi2(imax,kmax,tmax),tmpi3(imax,kmax,tmax) )
        allocate( tmpi4(imax,kmax,tmax),tmpi5(imax,kmax,tmax) )
        tmpi1(1:imax,1:kmax,1:tmax) = nan
        tmpi2(1:imax,1:kmax,1:tmax) = nan
        tmpi3(1:imax,1:kmax,1:tmax) = nan
        tmpi4(1:imax,1:kmax,1:tmax) = nan
        tmpi5(1:imax,1:kmax,1:tmax) = nan
        allocate( tmp1(imax,tmax) )
        tmp1(1:imax,1:tmax) = 0.
     case ('tpwave')
        ! time series of the area-averaged value of pw
        inx = imax
        iny = jmax
        inz = kmax
        int = 1
        allocate( var_in(imax,jmax,kmax,1) ) ! xyz + t-loop
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ imax, jmax, kmax, 1 /)
        var_in(1:imax,1:jmax,1:kmax,1) = nan
        allocate( tmp(tmax,1) )
        tmp(1:tmax,1) = 0.
        allocate( tmpi(imax,jmax,tmax) )
        tmpi(1:imax,1:jmax,1:tmax) = 0.
        allocate( tmpi1(imax,jmax,kmax) )
        tmpi1(1:imax,1:jmax,1:kmax) = 0.
        allocate( tmpi2(imax,jmax,kmax) )
        tmpi2(1:imax,1:jmax,1:kmax) = 0.
     case ('tpwaveus')
        ! time series of the area-averaged value of pw in the upstream side
        inx = imax
        iny = jmax
        inz = kmax
        int = 1
        ista = imax/2 - 70 ! which is reffered to X = -70 km in the X cordinate
        iend = imax/2 + 19 ! which is reffered to X = -20 km in the X cordinate
        allocate( var_in(imax,jmax,kmax,1) ) ! xyz + t-loop
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ imax, jmax, kmax, 1 /)
        var_in(1:imax,1:jmax,1:kmax,1) = nan
        allocate( tmp(tmax,1) )
        tmp(1:tmax,1) = 0.
        allocate( tmpi(imax,jmax,tmax) )
        tmpi(1:imax,1:jmax,1:tmax) = 0.
        allocate( tmpi1(imax,jmax,kmax) )
        tmpi1(1:imax,1:jmax,1:kmax) = 0.
        allocate( tmpi2(imax,jmax,kmax) )
        tmpi2(1:imax,1:jmax,1:kmax) = 0.
     case default
        print *, "ERROR: Please define by yourself for ",trim(varname)," on this section"
        stop 1
     end select
     !
  case (5)
     ! arbitrary cross-section specifying (xselect,yselect) and angle
     inx = imax
     iny = jmax
     inz = kmax
     int = 1
     allocate( var_in(imax,jmax,kmax,1) ) ! xyz
     istart = (/ 1, 1, 1, tselect /)
     icount = (/ imax, jmax, kmax, 1 /)
     var_in(1:imax,1:jmax,1:kmax,1) = nan
     allocate( tmpi(imax,jmax,kmax) )
     allocate( tmpi1(imax,jmax,kmax),tmpi2(imax,jmax,kmax),tmpi3(imax,jmax,kmax) )
     tmpi(1:imax,1:jmax,1:kmax) = nan
     tmpi1(1:imax,1:jmax,1:kmax) = nan
     tmpi2(1:imax,1:jmax,1:kmax) = nan
     tmpi3(1:imax,1:jmax,1:kmax) = nan
     allocate( tmp(imax,kmax) )
     tmp(1:imax,1:kmax) = nan
     !
  case (6)
     select case (varname)
     case ('tzwater','tzqvpert','tzthpert')
        ! Averaged values of the vertical profile of 
        !  water condensation contents (qc+qr+qi+qg+qs), 
        !  perturbation theta, 
        !  perturbation water vapor mixing ratio
        inx = imax
        iny = jmax
        inz = kmax
        int = 1
        ista = imax/2 +  1 ! which is reffered to X = 1 km in the X cordinate
        iend = imax/2 + 50 ! which is reffered to X = 50 km in the X cordinate
        allocate( var_in(imax,jmax,kmax,1) ) ! xyz + t-loop
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ imax, jmax, kmax, 1 /)
        var_in(1:imax,1:jmax,1:kmax,1) = nan
        allocate( tmp(tmax,kmax) )
        tmp(1:tmax,1:kmax) = 0.
        allocate( tmpi(imax,jmax,kmax) )
        tmpi(1:imax,1:jmax,1:kmax) = 0.
     case ('tzthetae')
        ! Averaged values of the vertical profile of 
        !  equivalent potential temperature
        inx = imax
        iny = jmax
        inz = kmax
        int = 1
        allocate( var_in(imax,jmax,kmax,1) ) ! xyz + t-loop
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ imax, jmax, kmax, 1 /)
        var_in(1:imax,1:jmax,1:kmax,1) = nan
        allocate( tmp(tmax,kmax) )
        tmp(1:tmax,1:kmax) = 0.
        allocate( tmpi(imax,jmax,kmax) )
        tmpi(1:imax,1:jmax,1:kmax) = 0.
        allocate( tmpi1(imax,jmax,kmax) )
        tmpi1(1:imax,1:jmax,1:kmax) = nan
        allocate( tmpi2(imax,jmax,kmax) )
        tmpi2(1:imax,1:jmax,1:kmax) = nan
        allocate( tmpi3(imax,jmax,kmax) )
        tmpi3(1:imax,1:jmax,1:kmax) = nan
     end select
     !
  case (7)
     select case (varname)
     case ('watertave')
        ! Temporal averaged values of 
        !  water condensation contents (qc+qr+qi+qg+qs) (x-z)
        inx = imax
        iny = 1
        inz = kmax
        int = tmax
        allocate( var_in(imax,1,kmax,tmax) ) ! xzt
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ imax, 1, kmax, tmax /)
        var_in(1:imax,1,1:kmax,1:tmax) = nan
        allocate( tmp(imax,kmax) )
        tmp(1:imax,1:kmax) = 0.
        allocate( tmpi(imax,kmax,tmax) )
        tmpi(1:imax,1:kmax,1:tmax) = 0.
     end select
     !
  case (8)
     select case (varname)
     case ('webdymf')
        ! west- and east-boundary mass flux [kg s-1]
        inx = 1
        iny = jmax
        inz = kmax
        int = 1
        allocate( var_in(1,jmax,kmax,1) ) ! yz + t-loop
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ 1, jmax, kmax, 1 /)
        var_in(1,1:jmax,1:kmax,1) = nan
        allocate( tmp(tmax,4) )
        tmp(1:tmax,4) = 0.
        allocate( tmp1(jmax,kmax),tmp2(jmax,kmax),tmp3(jmax,kmax) )
        tmp1(1:jmax,1:kmax) = 0.
        tmp2(1:jmax,1:kmax) = 0.
        tmp3(1:jmax,1:kmax) = 0.
        allocate( itmp1(kmax),itmp2(kmax) )
        itmp1(1:kmax) = 0.
        itmp2(1:kmax) = 0.
     case ('webdyqvf')
        ! west- and east-boundary qv flux [kg s-1]
        inx = 1
        iny = jmax
        inz = kmax
        int = 1
        allocate( var_in(1,jmax,kmax,1) ) ! yz + t-loop
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ 1, jmax, kmax, 1 /)
        var_in(1,1:jmax,1:kmax,1) = nan
        allocate( tmp(tmax,4) )
        tmp(1:tmax,4) = 0.
        allocate( tmp1(jmax,kmax),tmp2(jmax,kmax),tmp3(jmax,kmax),tmp4(jmax,kmax) )
        tmp1(1:jmax,1:kmax) = 0.
        tmp2(1:jmax,1:kmax) = 0.
        tmp3(1:jmax,1:kmax) = 0.
        tmp4(1:jmax,1:kmax) = 0.
        allocate( itmp1(kmax),itmp2(kmax) )
        itmp1(1:kmax) = 0.
        itmp2(1:kmax) = 0.
     case ('snbdymf')
        ! south- and north-boundary mass flux [kg s-1]
        inx = imax
        iny = jmax
        inz = 1
        int = 1
        allocate( var_in(imax,jmax,1,1) ) ! xy + t-loop
        istart = (/ 1, 1, 1, 1 /)
        icount = (/ imax, jmax, 1, 1 /)
        var_in(1:imax,1:jmax,1,1) = nan
        allocate( tmp(tmax,4) )
        tmp(1:tmax,4) = 0.
        allocate( tmp1(imax,jmax),tmp2(imax,jmax),tmp3(imax,jmax) )
        tmp1(1:imax,1:jmax) = 0.
        tmp2(1:imax,1:jmax) = 0.
        tmp3(1:imax,1:jmax) = 0.
        allocate( itmp1(imax),itmp2(imax) )
        itmp1(1:imax) = 0.
        itmp2(1:imax) = 0.
     end select
  end select


  !ccccccccccccccccccccccccccccccccccccccccccccccc
  ! ----- Executable sections are from here -----
  !ccccccccccccccccccccccccccccccccccccccccccccccc
  ! inquire and get var
  if(debug_level.ge.100) print *, "varname (",trim(varname),") is selected"
  !
  select case (varname)
  case ('water','xtwater')
     ! The section for all water (qc+qr+qi+qc+qg) on the microphysics processes
     ! *** this section work with flag = 1, 2 and 3 only (for now) ***
     if( (flag.ne.1).and.(flag.ne.2).and.(flag.ne.3) ) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read qc
     ivarname = 'qc'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     select case (flag)
     case (1)
!$omp parallel do default(shared) &
!$omp private(i,j)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp1(i,j) = var_in(i,j,1,1)
        end do
        end do
!$omp end parallel do
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp1(i,k) = var_in(i,1,k,1)
        end do
        end do
!$omp end parallel do
     case (3)
!$omp parallel do default(shared) &
!$omp private(i,t)
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp1(i,t) = var_in(i,1,1,t)
        end do
        end do
!$omp end parallel do
     end select
     ! --- read qr
     ivarname = 'qr'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     select case (flag)
     case (1)
!$omp parallel do default(shared) &
!$omp private(i,j)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp2(i,j) = var_in(i,j,1,1)
        end do
        end do
!$omp end parallel do
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp2(i,k) = var_in(i,1,k,1)
        end do
        end do
!$omp end parallel do
     case (3)
!$omp parallel do default(shared) &
!$omp private(i,t)
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp2(i,t) = var_in(i,1,1,t)
        end do
        end do
!$omp end parallel do
     end select
     ! --- read qi
     ivarname = 'qi'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     select case (flag)
     case (1)
!$omp parallel do default(shared) &
!$omp private(i,j)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp3(i,j) = var_in(i,j,1,1)
        end do
        end do
!$omp end parallel do
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp3(i,k) = var_in(i,1,k,1)
        end do
        end do
!$omp end parallel do
     case (3)
!$omp parallel do default(shared) &
!$omp private(i,t)
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp3(i,t) = var_in(i,1,1,t)
        end do
        end do
!$omp end parallel do
     end select
     ! --- read qs
     ivarname = 'qs'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     select case (flag)
     case (1)
!$omp parallel do default(shared) &
!$omp private(i,j)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp4(i,j) = var_in(i,j,1,1)
        end do
        end do
!$omp end parallel do
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp4(i,k) = var_in(i,1,k,1)
        end do
        end do
!$omp end parallel do
     case (3)
!$omp parallel do default(shared) &
!$omp private(i,t)
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp4(i,t) = var_in(i,1,1,t)
        end do
        end do
!$omp end parallel do
     end select
     ! --- read qg
     ivarname = 'qg'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     select case (flag)
     case (1)
!$omp parallel do default(shared) &
!$omp private(i,j)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp5(i,j) = var_in(i,j,1,1)
        end do
        end do
!$omp end parallel do
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp5(i,k) = var_in(i,1,k,1)
        end do
        end do
!$omp end parallel do
     case (3)
!$omp parallel do default(shared) &
!$omp private(i,t)
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp5(i,t) = var_in(i,1,1,t)
        end do
        end do
!$omp end parallel do
     end select
     ! --- calculate water = qc + qr + qi + qs + qg [kg/kg]
     if(debug_level.ge.200) print *, " Calculate water = qc + qr + qi + qs + qg"
     select case (flag)
     case (1)
!$omp parallel do default(shared) &
!$omp private(i,j)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp(i,j) = tmp1(i,j) + tmp2(i,j) + tmp3(i,j) + tmp4(i,j) + tmp5(i,j)
        end do
        end do
!$omp end parallel do
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp(i,k) = tmp1(i,k) + tmp2(i,k) + tmp3(i,k) + tmp4(i,k) + tmp5(i,k)
        end do
        end do
!$omp end parallel do
     case (3)
!$omp parallel do default(shared) &
!$omp private(i,t)
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp(i,t) = tmp1(i,t) + tmp2(i,t) + tmp3(i,t) + tmp4(i,t) + tmp5(i,t)
        end do
        end do
!$omp end parallel do
     end select

  case ('cxtwater')
     ! The section for all water (qc+qr+qi+qc+qg) on the microphysics processes 5-10 km
     ! *** this section work with flag = 3 only (for now) ***
     if( flag.ne.3 ) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do k = 32, 43, 1 ! from 5 to 10 km
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, yselect, k, 1 /)
        ! --- read qc
        ivarname = 'qc'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp1(i,t) = var_in(i,1,1,t)
        end do
        end do
        ! --- read qr
        ivarname = 'qr'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp2(i,t) = var_in(i,1,1,t)
        end do
        end do
        ! --- read qi
        ivarname = 'qi'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp3(i,t) = var_in(i,1,1,t)
        end do
        end do
        ! --- read qs
        ivarname = 'qs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp4(i,t) = var_in(i,1,1,t)
        end do
        end do
        ! --- read qg
        ivarname = 'qg'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp5(i,t) = var_in(i,1,1,t)
        end do
        end do
        ! --- calculate water = qc + qr + qi + qs + qg [kg/kg]
        if(debug_level.ge.200) print *, " Calculate water = qc + qr + qi + qs + qg"
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp(i,t) = tmp(i,t) + tmp1(i,t) + tmp2(i,t) + tmp3(i,t) + tmp4(i,t) + tmp5(i,t)
        end do
        end do
     end do ! end of k-loop

  case ('thetae')
     ! equivalent potential temperature [K]
     ! *** this section work with flag = 1, 2, 3 or 5 ***
     if( flag.eq.4 ) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read prs [Pa]
     ivarname = 'prs'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     select case (flag)
     case (1)
!$omp parallel do default(shared) &
!$omp private(i,j)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp1(i,j) = var_in(i,j,1,1)
        end do
        end do
!$omp end parallel do
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp1(i,k) = var_in(i,1,k,1)
        end do
        end do
!$omp end parallel do
     case (3)
!$omp parallel do default(shared) &
!$omp private(i,t)
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp1(i,t) = var_in(i,1,1,t)
        end do
        end do
!$omp end parallel do
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
!$omp end parallel do
     end select
     ! --- read theta [K]
     ivarname = 'th'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     select case (flag)
     case (1)
!$omp parallel do default(shared) &
!$omp private(i,j)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp2(i,j) = var_in(i,j,1,1)
        end do
        end do
!$omp end parallel do
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp2(i,k) = var_in(i,1,k,1)
        end do
        end do
!$omp end parallel do
     case (3)
!$omp parallel do default(shared) &
!$omp private(i,t)
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp2(i,t) = var_in(i,1,1,t)
        end do
        end do
!$omp end parallel do
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
!$omp end parallel do
     end select
     ! --- read qv [kg/kg]
     ivarname = 'qv'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
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
!$omp end parallel do
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
!$omp end parallel do
     case (3)
!$omp parallel do default(shared) &
!$omp private(i,t,tmp0)
        do t = 1, tmax, 1
        do i = 1, imax, 1
           tmp3(i,t) = var_in(i,1,1,t)
           tmp0 = thetaP_2_T( tmp2(i,t), tmp1(i,t) )
           tmp(i,t) = thetae_Bolton( tmp0, tmp3(i,t), tmp1(i,t) )
        end do
        end do
!$omp end parallel do
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
!$omp end parallel do
     end select

  case ('mlthetae')
     ! mixed-layer equivalent potential temperature [K]
     ! *** this section work with flag = 3 ***
     if( flag.ne.3 ) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read prs [Pa]
     ivarname = 'prs'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,k,t)
     do t = 1, tmax, 1
     do k = 1, 5, 1    ! from 50 m to 450 m
     do i = 1, imax, 1
           tmpi1(i,k,t) = var_in(i,1,k,t)
     end do
     end do
     end do
!$omp end parallel do
     ! --- read theta [K]
     ivarname = 'th'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,k,t)
     do t = 1, tmax, 1
     do k = 1, 5, 1    ! from 50 m to 450 m
     do i = 1, imax, 1
           tmpi2(i,k,t) = var_in(i,1,k,t)
     end do
     end do
     end do
!$omp end parallel do
     ! --- read qv [kg/kg]
     ivarname = 'qv'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,k,t,tmp0)
     do t = 1, tmax, 1
     do k = 1, 5, 1    ! from 50 m to 450 m
     do i = 1, imax, 1
           tmpi3(i,k,t) = var_in(i,1,k,t)
           tmp0 = thetaP_2_T( tmpi2(i,k,t), tmpi1(i,k,t) )
           tmpi(i,k,t) = thetae_Bolton( tmp0, tmpi3(i,k,t), tmpi1(i,k,t) )
     end do
     end do
     end do
!$omp end parallel do
     ! average the values within the specified mixed-layer (z=50-450m)
     do t = 1, tmax, 1
     do i = 1, imax, 1
        tmp(i,t) = 0.d0
        do k = 1, 5, 1
           tmp(i,t) = tmp(i,t) + tmpi(i,k,t)
        end do
        tmp(i,t) = tmp(i,t)/real(5.0d0)
     end do
     end do

  case ('cape','cin','lfc','lnb','lins')
     ! Calculate as following variables:
     !  - 500 m mixed-layer convective available potential energy (mlCAPE) [J kg-1]
     !  - convective inhibition [J kg-1]
     !  - lebel of free convection [m]
     !  - lebel of neutral buoyancy [m]
     !  - the ratio of 500 m mixed-layer convective available potential energy (mlCAPE) [J kg-1]
     !    to convective inhibition (mlCIN) [J kg-1]
     ! *** this section work with flag = 3 only (for now) ***
     if(flag.ne.3) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read prs [Pa]
     ivarname = 'prs'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpc1(i,k,t) = var_in(i,1,k,t)*real(0.01) ! unit: [hPa]
     end do
     end do
     end do
     ! --- read theta [K]
     ivarname = 'th'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp0 = var_in(i,1,k,t)
        ! calculate temperature [degree C] using theta [K] and pressure [Pa]
        tmpc2(i,k,t) = thetaP_2_T( tmp0, tmpc1(i,k,t)*100. ) - t0 ! unit: [degree C]
     end do
     end do
     end do
     ! --- read qv [kg/kg]
     ivarname = 'qv'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpc3(i,k,t) = var_in(i,1,k,t)
     end do
     end do
     end do
     ! calculate CAPE, CIN, LFC, LNB, lins (latent instability)
!$omp parallel do default(shared)             &
!$omp private(i,t,gbcape,gbcin,gblfcz,gblnbz)
        do t = 1, tmax, 1
        do i = 1, imax, 1
           CALL getcape( 3,kmax,tmpc1(i,:,t),tmpc2(i,:,t),tmpc3(i,:,t),gbcape,gbcin, &
                         gblclp,gblfcp,gblnbp, gblclz,gblfcz,gblnbz, debug_level, 1  )
           select case (varname)
           case ('cape')
              tmp(i,t) = gbcape
           case ('cin')
              tmp(i,t) = gbcin
           case ('lfc')
              tmp(i,t) = gblfcz
           case ('lnb')
              tmp(i,t) = gblnbz
           case ('lins')
              if( (gbcape.gt.0.).and.(gbcin.gt.0.) ) then
                 tmp(i,t) = real(gbcape/gbcin)
              else
                 tmp(i,t) = 0.
              end if
           end select
        end do
        if(debug_level.ge.200) print *, "  t,",trim(varname)," = ",t,tmp(xselect,t)
        end do
!$omp end parallel do

  case ('pw')
     ! Calculate precipitable water [mm]
     ! *** this section work with flag = 3 only (for now) ***
     if(flag.ne.3) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read prs [Pa]
     ivarname = 'prs'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpc1(i,k,t) = var_in(i,1,k,t)
     end do
     end do
     end do
     ! --- read qv [kg/kg]
     ivarname = 'qv'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpc2(i,k,t) = var_in(i,1,k,t)
     end do
     end do
     end do
     ! calc. pw [mm]
     do t = 1, tmax, 1
     do i = 1, imax, 1
        tmp(i,t) = precip_water( tmpc1(i,:,t), tmpc2(i,:,t) )
     end do
     end do

  case ('lwdt') ! under construction ... 2014/09/18
     ! Calculate LWDT [m s-2], which is proposed by Fovell and Tan (1998)
     ! *** this section work with flag = 2 only (for now) ***
     if(flag.ne.2) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read winterp [m s-1]
     call check( nf90_inq_varid(ncid, "winterp", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  istart       = ", istart
     if(debug_level.ge.300) print *, "  icount       = ", icount
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (winterp)"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1:2) = ", var_in(1,1,1,1:2)
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp(i,k) = (var_in(i,1,k,2) - var_in(i,1,k,1))/real(60.)
     end do
     end do

  case ('wadv') ! under construction ... 2014/09/18, Fixed 2014/12/14
     ! Calculate WADV [m s-2], which is proposed by Fovell and Tan (1998)
     ! *** this section work with flag = 2 only (for now) ***
     if(flag.ne.2) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read uinterp [m s-1]
     call check( nf90_inq_varid(ncid, "uinterp", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  istart       = ", istart
     if(debug_level.ge.300) print *, "  icount       = ", icount
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (uinterp)"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp1(i,k) = var_in(i,1,k,1)
     end do
     end do
     ! --- read winterp [m s-1]
     call check( nf90_inq_varid(ncid, "winterp", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  istart       = ", istart
     if(debug_level.ge.300) print *, "  icount       = ", icount
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (winterp)"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     do k = 1, kmax-1, 1
     do i = 1, imax-1, 1
        tmp(i,k) = - tmp1(i,k)                               &
                   * ( (var_in(i+1,1,k,1) - var_in(i,1,k,1)) &
                     / ( (x(i+1) - x(i))*1.0d3 ) )           &
                   - var_in(i,1,k,1)                         &
                   * ( (var_in(i,1,k+1,1) - var_in(i,1,k,1)) &
                     / ( (z(k+1) - z(k))*1.0d3 ) )           ! fixed 2014/12/14
     end do
     end do

  case ('pwdt')
     ! Calculate PWDT [m s-2], which is proposed by Fovell and Tan (1998)
     ! *** this section work with flag = 2 only (for now) ***
     if(flag.ne.2) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read rho [kg m-3] (t = 1) as a base state
     call check( nf90_inq_varid(ncid, "rho", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  iistart       = ", iistart
     if(debug_level.ge.300) print *, "  iicount       = ", iicount
     call check( nf90_get_var(ncid, varid, ivar_in, start = iistart, count = iicount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (rho, t=1)"
     if(debug_level.ge.200) print *, " ivar_in(1,1,1,1) = ", ivar_in(1,1,1,1)
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp1(i,k) = ivar_in(i,1,k,1)
     end do
     end do
     ! --- read prspert [Pa]
     call check( nf90_inq_varid(ncid, "prspert", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  istart       = ", istart
     if(debug_level.ge.300) print *, "  icount       = ", icount
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (prspert)"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     ! calculate VPGA as tmp2
     do k = 1, kmax-1, 1
     do i = 1, imax, 1
        tmp2(i,k) = - ( 1/tmp1(i,k) )                         &
                    * ( (var_in(i,1,k+1,1) - var_in(i,1,k,1)) &
                        / ( (z(k+1) - z(k))*real(1.0d3) ) )     ! fixed 2014/12/19
     end do
     if(debug_level.ge.200) print *, " vpga,rho,dp',dz = ", tmp2(xselect,k), tmp1(xselect,k), &
              var_in(xselect,1,k+1,1)-var_in(xselect,1,k,1), (z(k+1)-z(k))*real(1.0d3)
     end do
     ! --- read buoyancy [m s-2] (t=1)
     ivar_in(:,:,:,:) = 0.
     call check( nf90_inq_varid(ncid, "buoyancy", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  iistart       = ", iistart
     if(debug_level.ge.300) print *, "  iicount       = ", iicount
     call check( nf90_get_var(ncid, varid, ivar_in, start = iistart, count = iicount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (buoyancy, t=1)"
     if(debug_level.ge.200) print *, " ivar_in(1,1,1,1) = ", ivar_in(1,1,1,1)
     ! --- read buoyancy [m s-2] (t=tselect)
     var_in(:,:,:,:) = 0.
     call check( nf90_inq_varid(ncid, "buoyancy", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  istart       = ", istart
     if(debug_level.ge.300) print *, "  icount       = ", icount
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (buoyancy, t=tselect)"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     ! calculate BUOY and PWDT as tmp3 and tmp, respectively
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp3(i,k) = var_in(i,1,k,1) - ivar_in(i,1,k,1)
        tmp(i,k) = tmp2(i,k) + tmp3(i,k)
     end do
     if(debug_level.ge.200) print *, " k,pwdt,vpga,buoy = ", k,tmp(xselect,k),tmp2(xselect,k),tmp3(xselect,k)
     end do

  case ('vpga') ! under construction ... 2014/09/18, Fixed 2014/12/19
     ! Calculate VPGA [m s-2], which is proposed by Fovell and Tan (1998)
     ! *** this section work with flag = 2 only (for now) ***
     if(flag.ne.2) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read rho [K] (t = 1) as a base state
     call check( nf90_inq_varid(ncid, "rho", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  iistart       = ", iistart
     if(debug_level.ge.300) print *, "  iicount       = ", iicount
     call check( nf90_get_var(ncid, varid, ivar_in, start = iistart, count = iicount ) )
     if(debug_level.ge.100) print *, "Success: get the var array  (rho, t=1)"
     if(debug_level.ge.200) print *, " ivar_in(1,1,1,1) = ", ivar_in(1,1,1,1)
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp1(i,k) = ivar_in(i,1,k,1)
     end do
     end do
     ! --- read prspert [Pa]
     call check( nf90_inq_varid(ncid, "prspert", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  istart       = ", istart
     if(debug_level.ge.300) print *, "  icount       = ", icount
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (prspert)"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     do k = 1, kmax-1, 1
     do i = 1, imax, 1
        tmp(i,k) = - ( 1/tmp1(i,k) )                         &
                   * ( (var_in(i,1,k+1,1) - var_in(i,1,k,1)) &
                       / ( (z(k+1) - z(k))*1.0d3 ) )         ! fixed 2014/12/19
     end do
     if(debug_level.ge.200) print *, " rho,dp',dz = ", tmp1(xselect,k), var_in(xselect,1,k+1,1)-var_in(xselect,1,k,1), z(k+1)-z(k)
     end do

  case ('buoy') ! under construction ... 2014/09/18, Fixed 2014/12/14
     ! Calculate BUOY [m s-2], which is proposed by Fovell and Tan (1988)
     ! *** this section work with flag = 2 only (for now) ***
     if(flag.ne.2) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read buoyancy [m s-2] (t=1)
     call check( nf90_inq_varid(ncid, "buoyancy", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  iistart       = ", iistart
     if(debug_level.ge.300) print *, "  iicount       = ", iicount
     call check( nf90_get_var(ncid, varid, ivar_in, start = iistart, count = iicount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (buoyancy, t=1)"
     if(debug_level.ge.200) print *, " ivar_in(1,1,1,1) = ", ivar_in(1,1,1,1)
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp1(i,k) = ivar_in(i,1,k,1)
     end do
     end do
     ! --- read buoyancy [m s-2] (t=tselect)
     call check( nf90_inq_varid(ncid, "buoyancy", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  istart       = ", istart
     if(debug_level.ge.300) print *, "  icount       = ", icount
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (buoyancy, t=tselect)"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp(i,k) = var_in(i,1,k,1) - tmp1(i,k)
     end do
     end do

  case ('load')
     ! Calculate LOAD [m s-2], which is proposed by Takemi and Satomura (2000)
     ! *** this section work with flag = 2 only (for now) ***
     if(flag.ne.2) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read qc [kg kg-1]
     ivarname = 'qc'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp1(i,k) = tmp1(i,k) + var_in(i,1,k,1)
     end do
     end do
     ! --- read qr [kg kg-1]
     ivarname = 'qr'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp1(i,k) = tmp1(i,k) + var_in(i,1,k,1)
     end do
     end do
     ! --- read qi [kg kg-1]
     ivarname = 'qi'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp1(i,k) = tmp1(i,k) + var_in(i,1,k,1)
     end do
     end do
     ! --- read qs [kg kg-1]
     ivarname = 'qs'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp1(i,k) = tmp1(i,k) + var_in(i,1,k,1)
     end do
     end do
     ! --- read qg [kg kg-1]
     ivarname = 'qg'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp1(i,k) = tmp1(i,k) + var_in(i,1,k,1)
     end do
     end do
     ! calculate the term of water loading in the vertical momentum equation as a diagnostic value
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp(i,k) = - 9.81*tmp1(i,k)
     end do
     end do

  case ('dbdx')
     ! Calculate dB/dx [-], which is proposed by Fovell and Tan (1988)
     ! *** this section work with flag = 2 only (for now) ***
     if(flag.ne.2) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read buoyancy [m s-2] (t=1)
     call check( nf90_inq_varid(ncid, "buoyancy", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  iistart       = ", iistart
     if(debug_level.ge.300) print *, "  iicount       = ", iicount
     call check( nf90_get_var(ncid, varid, ivar_in, start = iistart, count = iicount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (buoyancy, t=1)"
     if(debug_level.ge.200) print *, " ivar_in(1,1,1,1) = ", ivar_in(1,1,1,1)
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp1(i,k) = ivar_in(i,1,k,1)
     end do
     end do
     ! --- read buoyancy [m s-2] (t=tselect)
     call check( nf90_inq_varid(ncid, "buoyancy", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  istart       = ", istart
     if(debug_level.ge.300) print *, "  icount       = ", icount
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (buoyancy, t=tselect)"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     do k = 1, kmax, 1
     do i = 1, imax-1, 1
        tmp(i,k) = &
             ( (var_in(i+1,1,k,1)-tmp1(i+1,k)) - (var_in(i,1,k,1)-tmp1(i,k)) ) &
             / ( (x(i+1) - x(i))*0.1 )
     end do
     end do

  case ('dbdz')
     ! Calculate dB/dz [-], which is proposed by Fovell and Tan (1988)
     ! *** this section work with flag = 2 only (for now) ***
     if(flag.ne.2) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read buoyancy [m s-2] (t=1)
     call check( nf90_inq_varid(ncid, "buoyancy", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  iistart       = ", iistart
     if(debug_level.ge.300) print *, "  iicount       = ", iicount
     call check( nf90_get_var(ncid, varid, ivar_in, start = iistart, count = iicount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (buoyancy, t=1)"
     if(debug_level.ge.200) print *, " ivar_in(1,1,1,1) = ", ivar_in(1,1,1,1)
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp1(i,k) = ivar_in(i,1,k,1)
     end do
     end do
     ! --- read buoyancy [m s-2] (t=tselect)
     call check( nf90_inq_varid(ncid, "buoyancy", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  istart       = ", istart
     if(debug_level.ge.300) print *, "  icount       = ", icount
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (buoyancy, t=tselect)"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     do k = 1, kmax-1, 1
     do i = 1, imax, 1
        tmp(i,k) = &
             ( (var_in(i,1,k+1,1)-tmp1(i,k+1)) - (var_in(i,1,k,1)-tmp1(i,k)) ) &
             / (z(k+1) - z(k))
     end do
     end do

  case ('sruinterp')
     ! Calculate storm relative wind speed for the u-component wind
     ! *** this section work with flag = 2 only (for now) ***
     if(flag.ne.2) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read uinterp [m s-1] (t=1)
     call check( nf90_inq_varid(ncid, "uinterp", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  iistart       = ", iistart
     if(debug_level.ge.300) print *, "  iicount       = ", iicount
     call check( nf90_get_var(ncid, varid, ivar_in, start = iistart, count = iicount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (uinterp, t=1)"
     if(debug_level.ge.200) print *, " ivar_in(1,1,1,1) = ", ivar_in(1,1,1,1)
     ! --- read uinterp [m s-1] (t=tselect)
     call check( nf90_inq_varid(ncid, "uinterp", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  istart       = ", istart
     if(debug_level.ge.300) print *, "  icount       = ", icount
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (uinterp, t=tselect)"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     select case (flag)
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp(i,k) = var_in(i,1,k,1) - ivar_in(i,1,k,1)
        end do
        end do
!$omp end parallel do
     end select

  case ('srvinterp')
     ! Calculate storm relative wind speed for the v-component wind
     ! *** this section work with flag = 2 only (for now) ***
     if(flag.ne.2) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read uinterp [m s-1] (t=1)
     call check( nf90_inq_varid(ncid, "vinterp", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  iistart       = ", iistart
     if(debug_level.ge.300) print *, "  iicount       = ", iicount
     call check( nf90_get_var(ncid, varid, ivar_in, start = iistart, count = iicount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (vinterp, t=1)"
     if(debug_level.ge.200) print *, " ivar_in(1,1,1,1) = ", ivar_in(1,1,1,1)
     ! --- read uinterp [m s-1] (t=tselect)
     call check( nf90_inq_varid(ncid, "vinterp", varid) )
     if(debug_level.ge.100) print *, "Success: inquire the varid"
     if(debug_level.ge.200) print *, " varid         = ", varid
     if(debug_level.ge.300) print *, "  istart       = ", istart
     if(debug_level.ge.300) print *, "  icount       = ", icount
     call check( nf90_get_var(ncid, varid, var_in, start = istart, count = icount ) )
     if(debug_level.ge.100) print *, "Success: get the var array (vinterp, t=tselect)"
     if(debug_level.ge.200) print *, " var_in(1,1,1,1) = ", var_in(1,1,1,1)
     select case (flag)
     case (2)
!$omp parallel do default(shared) &
!$omp private(i,k)
        do k = 1, kmax, 1
        do i = 1, imax, 1
           tmp(i,k) = var_in(i,1,k,1) - ivar_in(i,1,k,1)
        end do
        end do
!$omp end parallel do
     end select

  case ('rws')
     ! wind speed that projected on the specified vertical cross section [m/s]
     ! *** this section work with flag = 5 ***
     if(flag.ne.5) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read uinterp [m/s]
     ivarname = 'uinterp'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
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
!$omp end parallel do
     end select
     ! --- read vinterp [m/s]
     ivarname = 'vinterp'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
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
!$omp end parallel do
     end select

  case ('rainrate')
     ! rain rate [mm h^-1]
     ! *** this section work with flag = 3 ***
     if(flag.ne.3) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read rain [cm]
     ivarname = 'rain'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     ! for t = 1
     tmp(1,1) = 0.0
!$omp parallel do default(shared) &
!$omp private(i,t)
     ! for t >= 2
     do t = 2, tmax, 1
     do i = 1, imax, 1
        ! the value for dt = 1 [min] (= 60 sec)
        tmp(i,t) = (var_in(i,1,t,1) - var_in(i,1,t-1,1))*real(60.*10.) ! unit [cm] -> [mm/h]
     end do
     end do
!$omp end parallel do

  case ('maxrain')
     ! maximum rain rate [mm h^-1]
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read rain [cm]
     ivarname = 'rain'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     select case (flag)
     case (4)
        ! for t = 1
        tmp(1,1) = 0.0
!$omp parallel do default(shared) &
!$omp private(i,j,t,tmp0)
        ! for t >= 2
        do t = 2, tmax, 1
        do j = 1, jmax, 1
        do i = ista, iend, 1
           tmp0 = (var_in(i,j,t,1) - var_in(i,j,t-1,1))*real(600.) ! unit [cm] -> [mm/h]
           tmp(t,1) = max(tmp(t,1),tmp0)
        end do
        end do
        end do
!$omp end parallel do
     end select

  case ('averain')
     ! area-averaged rain rate [mm h^-1]
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read rain [cm]
     ivarname = 'rain'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     select case (flag)
     case (4)
        ! for t = 1
        tmp(1,1) = 0.0
        ! for t >= 2
        do t = 2, tmax, 1
           tmp0 = 0.
           ipoint = 0
           do j = 1, jmax, 1
           do i = ista, iend, 1
              ! unit [cm] -> [mm/h]
              if ( ((var_in(i,j,t,1) - var_in(i,j,t-1,1))*real(600.)).ge.0. ) then
                 ! calculate the summation if the tmp0 is equal to or larger than 0. [mm/h]
                 tmp0 = tmp0 + ((var_in(i,j,t,1) - var_in(i,j,t-1,1))*real(600.))
                 ! calculate the number of grid points (x-y) which satisfies this if section
                 ipoint = ipoint + 1
              end if
           end do ! end of i-loop
           end do ! end of j-loop
           if(debug_level.ge.200) print *, " t,tmp0,ipoint = ", t,tmp0,ipoint
           if ( tmp0.gt.0. ) then
              tmp(t,1) = tmp0/real(ipoint)
           else
              tmp(t,1) = 0.
           end if
        end do ! end of t-loop
     end select

  case ('apw','apm','aps','ape')
     ! area of precipitation [km^2]
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     tmp(1:tmax,1) = 0. ! The tmp array is cleared with zero instend of nan
     ! --- read rain [cm]
     ivarname = 'rain'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     select case (flag)
     case (4)
        select case (varname)
        case ('apw') ! area of weak precipitation 0.1 - 1.0 [mm/h]
           tmpmin = 0.1
           tmpmax = 1.0
        case ('apm') ! area of midium precipitation 1.0 - 10.0 [mm/h]
           tmpmin = 1.0
           tmpmax = 10.0
        case ('aps') ! area of strong precipitation 10.0 - 50.0 [mm/h]
           tmpmin = 10.0
           tmpmax = 50.0
        case ('ape') ! area of extreme precipitation 50.0 - 200.0 [mm/h]
           tmpmin = 50.0
           tmpmax = 200.0
        end select
        ! for t = 1
        tmp(1,1) = 0.0
        ! for t >= 2
        do t = 2, tmax, 1
        do j = 1, jmax, 1
        do i = ista, iend, 1
           tmp0 = (var_in(i,j,t,1) - var_in(i,j,t-1,1))*real(600.) ! unit [cm] -> [mm/h] for dt = 1 min
           if( (tmp0.ge.tmpmin).and.(tmp0.lt.tmpmax) ) tmp(t,1) = tmp(t,1) + 1. ! for dx = 1 km
        end do
        end do
        end do
     end select

  case ('vtotwcave','vtotwcstd')
     ! mean- and std-variables of the vertical profile of total water- and ice-phase mixing ratio
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, 1 /)
        tmpi(:,:,:) = 0. 
        ! --- read qc
        ivarname = 'qc'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,t) = tmpi(i,j,t) + var_in(i,j,1,t)
        end do
        end do
        end do
        ! --- read qr
        ivarname = 'qr'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,t) = tmpi(i,j,t) + var_in(i,j,1,t)
        end do
        end do
        end do
        ! --- read qi
        ivarname = 'qi'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,t) = tmpi(i,j,t) + var_in(i,j,1,t)
        end do
        end do
        end do
        ! --- read qs
        ivarname = 'qs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,t) = tmpi(i,j,t) + var_in(i,j,1,t)
        end do
        end do
        end do
        ! --- read qg
        ivarname = 'qg'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,t) = tmpi(i,j,t) + var_in(i,j,1,t)
        end do
        end do
        end do
        select case (varname)
        case ('vtotwcave')
           ! calc. averaged value
           tmp0 = 0.
           do t = time_min, time_max, 1
           do j = 1, jmax, 1
           do i = ista, iend, 1
              tmp0 = tmp0 + tmpi(i,j,t)
           end do
           end do
           end do
           tmp(k,1) = tmp0/real((iend-ista+1)*jmax*(time_max-time_min+1)) ! unit: [kg/kg]
        case ('vtotwcstd')
           print *, " under construction"
           stop
!!! !$omp parallel do default(shared) &
!!! !$omp private(i,j,t)              &
!!! !$omp reduction(+:tmp0)
!        do t = 1, tmax, 1
!        do j = 1, jmax, 1
!        do i = 1, imax, 1
!           tmp0 = tmp0 + (tmpi(i,j,t)-mean)**2
!           tmp(k,1) = sqrt(tmp0/real(imax*jmax*tmax))
!        end do
!        end do
!        end do
!!! !$omp end parallel do
        end select
     end do ! end of k-loop

  case ('vwmax','vwave')
     ! maximum- and mean-values of the vertical profile of updraft velocity
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, 1 /)
        tmpi(:,:,:) = 0. 
        ! --- read winterp
        ivarname = 'winterp'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
        select case (varname)
        case ('vwmax')
           ! calc. max value
           tmp0   = 0.
           do t = time_min, time_max, 1
              tmpmax = -1.e30
              do j = 1, jmax, 1
!              do i = 1, imax, 1
              do i = ista, iend, 1
                 if (tmpi(i,j,t).gt.tmpmax) then
                    tmpmax = tmpi(i,j,t)
                 end if
              end do
              end do
              tmp0 = tmp0 + tmpmax
           end do
           tmp(k,1) = tmp0/real(time_max-time_min+1) ! unit: [m/s]
        case ('vwave')
           ! calc. mean value
           ipoint = 0
           tmp0 = 0.
           do t = time_min, time_max, 1
           do j = 1, jmax, 1
           do i = ista, iend, 1
              if (tmpi(i,j,t).gt.0.) then
                 tmp0 = tmp0 + tmpi(i,j,t)
                 ipoint = ipoint + 1
              end if
           end do
           end do
           end do
           if(debug_level.ge.200) print *, " t,tmp0,ipoint = ", t,tmp0,ipoint
           if ( ipoint.gt.0. ) then
              tmp(k,1) = tmp0/real(ipoint)
           else
              tmp(k,1) = 0.
           end if
        end select
     end do ! end of k-loop

  case ('vthetaave')
     ! A mean-value of the vertical profile of theta
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     if(time_min.eq.nan) then
        print *, "ERROR: please define the values both time_min and time_max"
        stop 21
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, 1 /)
        tmpi(:,:,:) = 0. 
        ! --- read theta [K]
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! calc. mean value
        tmp0 = 0.
        do t = time_min, time_max, 1
        do j = 1, jmax, 1
        do i = ista, iend, 1
           tmp0 = tmp0 + tmpi(i,j,t)
        end do
        end do
        end do
        tmp(k,1) = tmp0/real((iend-ista+1)*jmax*(time_max-time_min+1)) ! unit: [m/s]
     end do ! end of k-loop

  case ('vtheta')
     ! A area-averaged value of the vertical profile of theta
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, tselect /)
        ! --- read theta [K]
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        ! calc. mean value
        tmp0 = 0.
        do j = 1, jmax, 1
        do i = ista, iend, 1
           tmp0 = tmp0 + var_in(i,j,1,1)
        end do
        end do
        tmp(k,1) = tmp0/real((iend-ista+1)*jmax) ! unit: [K]
     end do ! end of k-loop

  case ('vqvave')
     ! A mean-value of the vertical profile of water vapor mixing ratio
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     if(time_min.eq.nan) then
        print *, "ERROR: please define the values both time_min and time_max"
        stop 21
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, 1 /)
        tmpi(:,:,:) = 0. 
        ! --- read qv [kg/kg]
        ivarname = 'qv'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! calc. mean value
        tmp0 = 0.
        do t = time_min, time_max, 1
        do j = 1, jmax, 1
        do i = ista, iend, 1
           tmp0 = tmp0 + tmpi(i,j,t)
        end do
        end do
        end do
        tmp(k,1) = tmp0/real((iend-ista+1)*jmax*(time_max-time_min+1)) ! unit: [m/s]
     end do ! end of k-loop

  case ('vqv')
     ! A area-averaged value of the vertical profile of qv
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, tselect /)
        ! --- read qv [kg/kg]
        ivarname = 'qv'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        ! calc. mean value
        tmp0 = 0.
        do j = 1, jmax, 1
        do i = ista, iend, 1
           tmp0 = tmp0 + var_in(i,j,1,1)
        end do
        end do
        tmp(k,1) = tmp0/real((iend-ista+1)*jmax) ! unit: [kg/kg]
     end do ! end of k-loop

  case ('vqvavec')
     ! A mean-value of the vertical profile of water vapor mixing ratio
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     if(time_min.eq.nan) then
        print *, "ERROR: please define the values both time_min and time_max"
        stop 21
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, 1 /)
        tmpi(:,:,:) = 0. 
        ! --- read qv [kg/kg]
        ivarname = 'qv'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! --- read qc [kg/kg]
        ivarname = 'qc'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpc(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! --- read qr [kg/kg]
        ivarname = 'qr'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t) &
!$omp reduction(+:tmpc)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpc(i,j,t) = tmpc(i,j,t) + var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! calc. mean value
        ipoint = 0
        tmp0 = 0.
        do t = time_min, time_max, 1
        do j = 1, jmax, 1
        do i = ista, iend, 1
           ! cloud area is defined as tmpc >= 0.01 [g/kg]
           if ( tmpc(i,j,t).gt.real(0.01*0.001) ) then
              tmp0 = tmp0 + tmpi(i,j,t)
              ipoint = ipoint + 1
           end if
        end do
        end do
        end do
        if ( ipoint.gt.0. ) then
           tmp(k,1) = tmp0/real(ipoint)
        else
           tmp(k,1) = 0.
        end if
     end do ! end of k-loop

  case ('vthetaeave')
     ! A mean-value of the vertical profile of theta-e
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     if(time_min.eq.nan) then
        print *, "ERROR: please define the values both time_min and time_max"
        stop 21
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, 1 /)
        tmpi(:,:,:) = 0. 
        ! --- read prs [Pa]
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi1(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! --- read theta [K]
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi2(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! --- read qv [kg/kg]
        ivarname = 'qv'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t,tmp0)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi3(i,j,t) = var_in(i,j,1,t)
           tmp0 = thetaP_2_T( tmpi2(i,j,t), tmpi1(i,j,t) )
           tmpi(i,j,t) = thetae_Bolton( tmp0, tmpi3(i,j,t), tmpi1(i,j,t) )
        end do
        end do
        end do
!$omp end parallel do
        ! calc. mean value
        ipoint = 0
        tmp0 = 0.
        do t = time_min, time_max, 1
        do j = 1, jmax, 1
        do i = ista, iend, 1
           if (tmpi(i,j,t).gt.0.) then
              tmp0 = tmp0 + tmpi(i,j,t)
              ipoint = ipoint + 1
           end if
        end do
        end do
        end do
        if ( ipoint.gt.0. ) then
           tmp(k,1) = tmp0/real(ipoint)
        else
           tmp(k,1) = 0.
        end if
     end do ! end of k-loop

  case ('vthetae')
     ! A area-averaged value of the vertical profile of theta-e
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, tselect /)
        ! --- read prs [Pa]
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp1(i,j) = var_in(i,j,1,1)
        end do
        end do
!$omp end parallel do
        ! --- read theta [K]
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp2(i,j) = var_in(i,j,1,1)
        end do
        end do
!$omp end parallel do
        ! --- read qv [kg/kg]
        ivarname = 'qv'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,tmp0)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp3(i,j) = var_in(i,j,1,1)
           tmp0 = thetaP_2_T( tmp2(i,j), tmp1(i,j) )
           tmp4(i,j) = thetae_Bolton( tmp0, tmp3(i,j), tmp1(i,j) )
        end do
        end do
!$omp end parallel do
        ! calc. mean value
        tmp0 = 0.
        do j = 1, jmax, 1
        do i = ista, iend, 1
           tmp0 = tmp0 + tmp4(i,j)
        end do
        end do
        tmp(k,1) = tmp0/real((iend-ista+1)*jmax)
     end do ! end of k-loop

  case ('vthetaeca')
     ! A area-averaged vertical profile of theta-e in the convective area
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, tselect /)
        ! --- read prs [Pa]
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp1(i,j) = var_in(i,j,1,1)
        end do
        end do
        ! --- read theta [K]
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp2(i,j) = var_in(i,j,1,1)
        end do
        end do
        ! --- read qv [kg/kg]
        ivarname = 'qv'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp3(i,j) = var_in(i,j,1,1)
           tmp0 = thetaP_2_T( tmp2(i,j), tmp1(i,j) )
           tmp4(i,j) = thetae_Bolton( tmp0, tmp3(i,j), tmp1(i,j) )
        end do
        end do
        tmp1 = 0.
        ! --- read winterp [m/s]
        ivarname = 'winterp'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp1(i,j) = var_in(i,j,1,1)
        end do
        end do
        ! calc. qt (=qc+qr+qs+qi+qg)
        tmp2 = 0.
        ! --- read qc [kg/kg]
        ivarname = 'qc'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp2(i,j) = tmp2(i,j) + var_in(i,j,1,1)
        end do
        end do
        ! --- read qr [kg/kg]
        ivarname = 'qr'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp2(i,j) = tmp2(i,j) + var_in(i,j,1,1)
        end do
        end do
        ! --- read qi [kg/kg]
        ivarname = 'qi'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp2(i,j) = tmp2(i,j) + var_in(i,j,1,1)
        end do
        end do
        ! --- read qs [kg/kg]
        ivarname = 'qs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp2(i,j) = tmp2(i,j) + var_in(i,j,1,1)
        end do
        end do
        ! --- read qg [kg/kg]
        ivarname = 'qg'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp2(i,j) = tmp2(i,j) + var_in(i,j,1,1)
           tmp2(i,j) = tmp2(i,j)*real(1000.) ! unit [g/kg]
        end do
        end do
        ! calc. mean value
        tmp0 = 0.
        ipoint = 0
        do j = 1, jmax, 1
        do i = ista, iend, 1
           ! calc. mean value if the conditions of w_vave > 1.0 [m/s] and qt > 0.01 [g/kg] are satisfied
!           if( (tmp1(i,j).gt.0.1).and.(tmp2(i,j).gt.0.01) ) then
           if( (tmp2(i,j).gt.0.01) ) then
              tmp0 = tmp0 + tmp4(i,j)
              ipoint = ipoint + 1
           end if
        end do
        end do
        print *, "  ipoint = ", ipoint
        if (ipoint.gt.0) then
           tmp(k,1) = tmp0/real(ipoint)
        else
           tmp(k,1) = 0.
        end if
     end do ! end of k-loop

  case ('vthetaes')
     ! A area-averaged value of the vertical profile of theta-es
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, tselect /)
        ! --- read prs [Pa]
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp1(i,j) = var_in(i,j,1,1)
        end do
        end do
        ! --- read theta [K]
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp2(i,j) = var_in(i,j,1,1)
        end do
        end do
!$omp parallel do default(shared) &
!$omp private(i,j,tmp0)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp0 = thetaP_2_T( tmp2(i,j), tmp1(i,j) )
           tmp3(i,j) = thetaes_Bolton( tmp0, tmp1(i,j) )
        end do
        end do
!$omp end parallel do
        ! calc. mean value
        tmp0 = 0.
        do j = 1, jmax, 1
        do i = ista, iend, 1
           tmp0 = tmp0 + tmp3(i,j)
        end do
        end do
        tmp(k,1) = tmp0/real((iend-ista+1)*jmax)
     end do ! end of k-loop

  case ('vthetaeavec')
     ! A mean-value of the vertical profile of theta-e inside of clouds
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     if(time_min.eq.nan) then
        print *, "ERROR: please define the values both time_min and time_max"
        stop 21
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, 1 /)
        tmpi(:,:,:) = 0. 
        ! --- read prs [Pa]
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi1(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! --- read theta [K]
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi2(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! --- read qv [kg/kg]
        ivarname = 'qv'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t,tmp0)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi3(i,j,t) = var_in(i,j,1,t)
           tmp0 = thetaP_2_T( tmpi2(i,j,t), tmpi1(i,j,t) )
           tmpi(i,j,t) = thetae_Bolton( tmp0, tmpi3(i,j,t), tmpi1(i,j,t) )
        end do
        end do
        end do
!$omp end parallel do
        ! --- read qc [kg/kg]
        ivarname = 'qc'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpc(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! --- read qr [kg/kg]
        ivarname = 'qr'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t) &
!$omp reduction(+:tmpc)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpc(i,j,t) = tmpc(i,j,t) + var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! calc. mean value
        ipoint = 0
        tmp0 = 0.
        do t = time_min, time_max, 1
        do j = 1, jmax, 1
        do i = ista, iend, 1
           ! cloud area is defined as tmpc >= 0.01 [g/kg]
           if ( tmpc(i,j,t).gt.real(0.01*0.001) ) then
              tmp0 = tmp0 + tmpi(i,j,t)
              ipoint = ipoint + 1
           end if
        end do
        end do
        end do
        if ( ipoint.gt.0. ) then
           tmp(k,1) = tmp0/real(ipoint)
        else
           tmp(k,1) = 0.
        end if
     end do ! end of k-loop

  case ('vthetav')
     ! A area-averaged value of the vertical profile of virtual theta
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, tselect /)
        ! --- read prs [Pa]
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp1(i,j) = var_in(i,j,1,1)
        end do
        end do
!$omp end parallel do
        ! --- read theta [K]
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp2(i,j) = var_in(i,j,1,1)
        end do
        end do
!$omp end parallel do
        ! --- read qv [kg/kg]
        ivarname = 'qv'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,tmp0)
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp3(i,j) = var_in(i,j,1,1)
           tmp0 = thetaP_2_T( tmp2(i,j), tmp1(i,j) )
           tmp4(i,j) = TqvP_2_thetav( tmp0, tmp3(i,j), tmp1(i,j) )
        end do
        end do
!$omp end parallel do
        ! calc. mean value
        tmp0 = 0.
        do j = 1, jmax, 1
        do i = ista, iend, 1
           tmp0 = tmp0 + tmp4(i,j)
        end do
        end do
        tmp(k,1) = tmp0/real((iend-ista+1)*jmax)
     end do ! end of k-loop

  case ('vcape')
     ! A area-averaged value of the vertical profile of CAPE
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read prs [Pa]
     ivarname = 'prs'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do k = 1, kmax, 1
     do j = 1, jmax, 1
     do i = 1, imax, 1
        tmpc1(i,j,k) = var_in(i,j,k,1)*real(0.01) ! unit: [hPa]
     end do
     end do
     end do
     ! --- read theta [K]
     ivarname = 'th'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do k = 1, kmax, 1
     do j = 1, jmax, 1
     do i = 1, imax, 1
        tmp0 = var_in(i,j,k,1)
        ! calculate temperature [degree C] using theta [K] and pressure [Pa]
        tmpc2(i,j,k) = thetaP_2_T( tmp0, tmpc1(i,j,k)*100. ) - t0 ! unit: [degree C]
     end do
     end do
     end do
     ! --- read qv [kg/kg]
     ivarname = 'qv'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do k = 1, kmax, 1
     do j = 1, jmax, 1
     do i = 1, imax, 1
        tmpc3(i,j,k) = var_in(i,j,k,1)
     end do
     end do
     end do
     do k = 1, kmax
        ! calculate CAPE, CIN, LFC, and LNB
        tmp0 = 0.
        do j = 1, jmax, 1
        do i = ista, iend, 1
           CALL getcape( 4,kmax,tmpc1(i,j,:),tmpc2(i,j,:),tmpc3(i,j,:), &
                         gbcape,gbcin,gblclp,gblfcp,gblnbp,gblclz,      &
                         gblfcz,gblnbz,debug_level,k                    )
           tmp0 = tmp0 + gbcape
        end do
        end do
        tmp(k,1) = tmp0/real((iend-ista+1)*jmax)
        print '(a12,x,i3,f8.3,f10.2)', " k,z,cape = ", k,z(k),tmp(k,1)
     end do ! end of k loop

  case ('vcapeca')
     ! A area-averaged value of the vertical profile of CAPE in the convective area
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read prs [Pa]
     ivarname = 'prs'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do k = 1, kmax, 1
     do j = 1, jmax, 1
     do i = 1, imax, 1
        tmpc1(i,j,k) = var_in(i,j,k,1)*real(0.01) ! unit: [hPa]
     end do
     end do
     end do
     ! --- read theta [K]
     ivarname = 'th'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do k = 1, kmax, 1
     do j = 1, jmax, 1
     do i = 1, imax, 1
        tmp0 = var_in(i,j,k,1)
        ! calculate temperature [degree C] using theta [K] and pressure [Pa]
        tmpc2(i,j,k) = thetaP_2_T( tmp0, tmpc1(i,j,k)*100. ) - t0 ! unit: [degree C]
     end do
     end do
     end do
     ! --- read qv [kg/kg]
     ivarname = 'qv'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do k = 1, kmax, 1
     do j = 1, jmax, 1
     do i = 1, imax, 1
        tmpc3(i,j,k) = var_in(i,j,k,1)
     end do
     end do
     end do
     do k = 1, kmax
        ! calculate CAPE, CIN, LFC, and LNB
        tmp0 = 0.
        do j = 1, jmax, 1
        do i = ista, iend, 1
           CALL getcape( 4,kmax,tmpc1(i,j,:),tmpc2(i,j,:),tmpc3(i,j,:), &
                         gbcape,gbcin,gblclp,gblfcp,gblnbp,gblclz,      &
                         gblfcz,gblnbz,debug_level,k                    )
           tmp0 = tmp0 + gbcape
        end do
        end do
        tmp(k,1) = tmp0/real((iend-ista+1)*jmax)
        print '(a12,x,i3,f8.3,f10.2)', " k,z,cape = ", k,z(k),tmp(k,1)
     end do ! end of k loop

  case ('vrhave')
     ! A mean-value of the vertical profile of relative humidity
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     if(time_min.eq.nan) then
        print *, "ERROR: please define the values both time_min and time_max"
        stop 21
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, 1 /)
        tmpi(:,:,:) = 0. 
        ! --- read prs [Pa]
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi1(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! --- read theta [K]
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi2(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! --- read qv [kg/kg]
        ivarname = 'qv'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t,tmp0)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi3(i,j,t) = var_in(i,j,1,t)
           tmp0 = thetaP_2_T( tmpi2(i,j,t), tmpi1(i,j,t) )
           tmpi(i,j,t) = qvTP_2_RH( tmpi3(i,j,t), tmp0, tmpi1(i,j,t) )
        end do
        end do
        end do
!$omp end parallel do
        ! calc. mean value
        ipoint = 0
        tmp0 = 0.
        do t = time_min, time_max, 1
        do j = 1, jmax, 1
        do i = ista, iend, 1
           if (tmpi(i,j,t).gt.0.) then
              tmp0 = tmp0 + tmpi(i,j,t)
              ipoint = ipoint + 1
           end if
        end do
        end do
        end do
        if ( ipoint.gt.0. ) then
           tmp(k,1) = tmp0/real(ipoint)
        else
           tmp(k,1) = 0.
        end if
     end do ! end of k-loop

  case ('vrhavec')
     ! A mean-value of the vertical profile of relative humidity inside of convective clouds
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     if(time_min.eq.nan) then
        print *, "ERROR: please define the values both time_min and time_max"
        stop 21
     end if
     do k = 1, kmax, 1
        if(debug_level.ge.100) print *, " z = ", k
        istart = (/ 1, 1, k, 1 /)
        tmpi(:,:,:) = 0. 
        ! --- read prs [Pa]
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi1(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! --- read theta [K]
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi2(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! --- read qv [kg/kg]
        ivarname = 'qv'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t,tmp0)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi3(i,j,t) = var_in(i,j,1,t)
           tmp0 = thetaP_2_T( tmpi2(i,j,t), tmpi1(i,j,t) )
           tmpi(i,j,t) = qvTP_2_RH( tmpi3(i,j,t), tmp0, tmpi1(i,j,t) )
        end do
        end do
        end do
!$omp end parallel do
        ! --- read qc [kg/kg]
        ivarname = 'qc'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpc(i,j,t) = var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! --- read qr [kg/kg]
        ivarname = 'qr'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t) &
!$omp reduction(+:tmpc)
        do t = 1, tmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpc(i,j,t) = tmpc(i,j,t) + var_in(i,j,1,t)
        end do
        end do
        end do
!$omp end parallel do
        ! calc. mean value
        ipoint = 0
        tmp0 = 0.
        do t = time_min, time_max, 1
        do j = 1, jmax, 1
        do i = ista, iend, 1
           ! cloud area is defined as tmpc >= 0.01 [g/kg]
           if ( tmpc(i,j,t).gt.real(0.01*0.001) ) then
              tmp0 = tmp0 + tmpi(i,j,t)
              ipoint = ipoint + 1
           end if
        end do
        end do
        end do
        if ( ipoint.gt.0. ) then
           tmp(k,1) = tmp0/real(ipoint)
        else
           tmp(k,1) = 0.
        end if
     end do ! end of k-loop

  case ('tthetaeave')
     ! Area and mixed layer averaged value of theta-e
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read prs [Pa]
     ivarname = 'prs'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,k,t)
     do t = 1, tmax, 1
     do k = 1, 5, 1
     do j = 1, jmax, 1
     do i = 1, imax, 1
        tmp4d1(i,j,k,t) = var_in(i,j,k,t)
     end do
     end do
     end do
     end do
!$omp end parallel do
     ! --- read theta [K]
     ivarname = 'th'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,k,t)
     do t = 1, tmax, 1
     do k = 1, 5, 1
     do j = 1, jmax, 1
     do i = 1, imax, 1
        tmp4d2(i,j,k,t) = var_in(i,j,k,t)
     end do
     end do
     end do
     end do
!$omp end parallel do
     ! --- read qv [kg/kg]
     ivarname = 'qv'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,k,t,tmp0)
     do t = 1, tmax, 1
     do k = 1, 5, 1
     do j = 1, jmax, 1
     do i = 1, imax, 1
        tmp4d3(i,j,k,t) = var_in(i,j,k,t)
        tmp0 = thetaP_2_T( tmp4d2(i,j,k,t), tmp4d1(i,j,k,t) )
        tmp4d(i,j,k,t) = thetae_Bolton( tmp0, tmp4d3(i,j,k,t), tmp4d1(i,j,k,t) )
     end do
     end do
     end do
     end do
!$omp end parallel do
     ! calc. mean value
     do t = 1, tmax, 1
        tmp0 = 0.
        do k = 1, 5, 1
        do j = 1, jmax, 1
        do i = ista, iend, 1
           tmp0 = tmp0 + tmp4d(i,j,k,t)
        end do
        end do
        end do
        tmp(t,1) = tmp0/real((iend-ista+1)*jmax*5)
     end do ! end of t-loop

  case ('tqvave')
     ! Area-averaged value of qv
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     tmpi(:,:,:) = 0. 
     ! --- read qv [kg/kg]
     ivarname = 'qv'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!$omp parallel do default(shared) &
!$omp private(i,j,t)
     do t = 1, tmax, 1
     do j = 1, jmax, 1
     do i = 1, imax, 1
        tmpi(i,j,t) = var_in(i,j,1,t)
     end do
     end do
     end do
!$omp end parallel do
     ! calc. mean value
     do t = 1, tmax, 1
        ipoint = 0
        tmp0 = 0.
        do j = 1, jmax, 1
        do i = ista, iend, 1
           if (tmpi(i,j,t).gt.0.) then
              tmp0 = tmp0 + tmpi(i,j,t)
              ipoint = ipoint + 1
           end if
        end do
        end do
        if(debug_level.ge.200) print *, " t,tmp0,ipoint = ", t,tmp0,ipoint
        if ( ipoint.gt.0. ) then
           tmp(t,1) = tmp0/real(ipoint)
        else
           tmp(t,1) = 0.
        end if
     end do ! end of t-loop

  case ('tcapeave','tcinave','tlfcave','tlnbave','tcapeavenc','tcapeaveus')
     ! Calculate as following variables:
     !  - mixed-layer convective available potential energy [J kg-1]
     !  - mixed-layer convective inhibition [J kg-1]
     !  - lebel of free convection [m]
     !  - lebel of neutral buoyancy [m]
     !  - mixed-layer convective available potential energy w/o cold pool [J kg-1]
     !  - upstream side of mixed-layer convective available potential energy [J kg-1]
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 only (for now) ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read prs [Pa]
     ivarname = 'prs'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpc1(i,k,t) = var_in(i,1,k,t)*real(0.01) ! unit: [hPa]
     end do
     end do
     end do
     ! --- read theta [K]
     ivarname = 'th'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp0 = var_in(i,1,k,t)
        ! calculate temperature [degree C] using theta [K] and pressure [Pa]
        tmpc2(i,k,t) = thetaP_2_T( tmp0, tmpc1(i,k,t)*100. ) - t0 ! unit: [degree C]
     end do
     end do
     end do
     ! --- read qv [kg/kg]
     ivarname = 'qv'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpc3(i,k,t) = var_in(i,1,k,t)
     end do
     end do
     end do
     ! --- thpert [K]
     ivarname = 'thpert'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     ! calculate CAPE, CIN, LFC, and LNB
     do t = 1, tmax, 1
     do i = ista, iend, 1
        ! calc. mixed-layer (500 m) CAPE
        CALL getcape( 3,kmax,tmpc1(i,:,t),tmpc2(i,:,t),tmpc3(i,:,t),   &
                      tmp1(i,t),tmp2(i,t),gblclp,gblfcp,gblnbp,gblclz, &
                      tmp3(i,t),tmp4(i,t),debug_level,1                )
        if(i.eq.256) print '(i5,8f10.2)', t, tmp1(i,t),tmp2(i,t),gblclz,tmp3(i,t),tmp4(i,t), &
                                             gblclp,gblfcp,gblnbp
     end do
     end do
     ! calc. area-averaged value
     do t = 1, tmax, 1
        ipoint = 0
        tmp0 = 0.
        do i = ista, iend, 1
           select case (varname)
           case ('tcapeave')
              if(tmp1(i,t).gt.0.) then
                 tmp0 = tmp0 + tmp1(i,t)
                 ipoint = ipoint + 1
              end if
           case ('tcinave')
              if(tmp2(i,t).gt.0.) then
                 tmp0 = tmp0 + tmp2(i,t)
                 ipoint = ipoint + 1
              end if
           case ('tlfcave')
              if(tmp3(i,t).gt.0.) then
                 tmp0 = tmp0 + tmp3(i,t)
                 ipoint = ipoint + 1
              end if
           case ('tlnbave')
              if(tmp4(i,t).gt.7000.) then
                 tmp0 = tmp0 + tmp4(i,t)
                 ipoint = ipoint + 1
              end if
           case ('tcapeavenc')
              if( (tmp1(i,t).gt.0.).and.(var_in(i,1,1,t).gt.-1.) )then
                 tmp0 = tmp0 + tmp1(i,t)
                 ipoint = ipoint + 1
              end if
           case ('tcapeaveus')
              if(tmp1(i,t).gt.0.) then
                 tmp0 = tmp0 + tmp1(i,t)
                 ipoint = ipoint + 1
              end if
           end select
        end do
        if(ipoint.gt.0) then
           tmp(t,1) = tmp0/real(ipoint)
        else
           tmp(t,1) = 0.
        end if
        if(debug_level.ge.100) print *, "  t,",trim(varname)," = ",t,tmp(t,1)
     end do

  case ('tpwave','tpwaveus')
     ! Calculate as following variables:
     ! - area-averaged precipitable water [mm]
     ! - upstream side of area-averaged precipitable water [mm]
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 4 only (for now) ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do t = 1, tmax, 1
        if(debug_level.ge.100) print *, " t = ", t
        istart = (/ 1, 1, 1, t /)
        ! --- read prs [Pa]
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi1(i,j,k) = var_in(i,j,k,1)
        end do
        end do
        end do
        ! --- read qv [kg/kg]
        ivarname = 'qv'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi2(i,j,k) = var_in(i,j,k,1)
        end do
        end do
        end do
        ! calc. pw [mm]
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,t) = precip_water( tmpi1(i,j,:), tmpi2(i,j,:) )
        end do
        end do
     end do ! end of t-loop
     ! calc. area-averaged value
     do t = 1, tmax, 1
        ipoint = 0
        tmp0 = 0.
        do j = 1, jmax
        do i = ista, iend, 1
           tmp0 = tmp0 + tmpi(i,j,t)
           ipoint = ipoint + 1
        end do
        end do
        if(ipoint.gt.0) then
           tmp(t,1) = tmp0/real(ipoint)
        else
           tmp(t,1) = 0.
        end if
        if(debug_level.ge.100) print *, "  t,",trim(varname)," = ",t,tmp(t,1)
     end do ! end of t-loop

  case ('tzwater')
     ! t-z axis of horizontally averaged total water- and ice-phase mixing ratio
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 6 ***
     if(flag.ne.6) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do t = 1, tmax, 1
        if(debug_level.ge.100) print *, " t = ", t
        istart = (/ 1, 1, 1, t /)
        tmpi(:,:,:) = 0. 
        ! --- read qc
        ivarname = 'qc'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,k) = tmpi(i,j,k) + var_in(i,j,k,1)
        end do
        end do
        end do
        ! --- read qr
        ivarname = 'qr'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,k) = tmpi(i,j,k) + var_in(i,j,k,1)
        end do
        end do
        end do
        ! --- read qi
        ivarname = 'qi'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,k) = tmpi(i,j,k) + var_in(i,j,k,1)
        end do
        end do
        end do
        ! --- read qs
        ivarname = 'qs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,k) = tmpi(i,j,k) + var_in(i,j,k,1)
        end do
        end do
        end do
        ! --- read qg
        ivarname = 'qg'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,k) = tmpi(i,j,k) + var_in(i,j,k,1)
        end do
        end do
        end do
        ! --- calc. horizontally averaged value
        do k = 1, kmax, 1
           tmp0 = 0.
           do j = 1, jmax, 1
           do i = ista, iend, 1
              tmp0 = tmp0 + tmpi(i,j,k)
           end do
           end do
           tmp(t,k) = tmp0/real((iend-ista+1)*jmax) ! unit: [kg/kg]
        end do
     end do ! end of t-loop

  case ('tzqvpert')
     ! t-z axis of horizontally averaged perturbation water vapor mixing ratio
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 6 ***
     if(flag.ne.6) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do t = 1, tmax, 1
        if(debug_level.ge.100) print *, " t = ", t
        istart = (/ 1, 1, 1, t /)
        tmpi(:,:,:) = 0. 
        ! --- read qvpert
        ivarname = 'qvpert'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,k) = var_in(i,j,k,1)
        end do
        end do
        end do
        ! --- average horizontally
        do k = 1, kmax, 1
           tmp0 = 0.
           do j = 1, jmax, 1
           do i = ista, iend, 1
              tmp0 = tmp0 + tmpi(i,j,k)
           end do
           end do
           tmp(t,k) = tmp0/real((iend-ista+1)*jmax) ! unit: [kg/kg]
        end do
     end do ! end of t-loop

  case ('tzthpert')
     ! t-z axis of horizontally averaged perturbation potential temperature
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 6 ***
     if(flag.ne.6) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do t = 1, tmax, 1
        if(debug_level.ge.100) print *, " t = ", t
        istart = (/ 1, 1, 1, t /)
        tmpi(:,:,:) = 0. 
        ! --- read thpert
        ivarname = 'thpert'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi(i,j,k) = var_in(i,j,k,1)
        end do
        end do
        end do
        ! --- average horizontally
        do k = 1, kmax, 1
           tmp0 = 0.
           do j = 1, jmax, 1
           do i = ista, iend, 1
              tmp0 = tmp0 + tmpi(i,j,k)
           end do
           end do
           tmp(t,k) = tmp0/real((iend-ista+1)*jmax) ! unit: [kg/kg]
        end do
     end do ! end of t-loop

  case ('tzthetae')
     ! t-z axis of horizontally averaged equivalent potential temperature
     ! The horizontal averaging depends on ista and iend
     ! *** this section work with flag = 6 ***
     if(flag.ne.6) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do t = 1, tmax, 1
        if(debug_level.ge.100) print *, " t = ", t
        istart = (/ 1, 1, 1, t /)
        tmpi(:,:,:) = 0. 
        ! --- read prs [Pa]
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi1(i,j,k) = var_in(i,j,k,1)
        end do
        end do
        end do
        ! --- read theta [K]
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi2(i,j,k) = var_in(i,j,k,1)
        end do
        end do
        end do
        ! --- read qv [kg/kg]
        ivarname = 'qv'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmpi3(i,j,k) = var_in(i,j,k,1)
           tmp0 = thetaP_2_T( tmpi2(i,j,k), tmpi1(i,j,k) )
           tmpi(i,j,k) = thetae_Bolton( tmp0, tmpi3(i,j,k), tmpi1(i,j,k) )
        end do
        end do
        end do
        ! --- calc. horizontally averaged value
        do k = 1, kmax, 1
           tmp0 = 0.
           do j = 1, jmax, 1
           do i = ista, iend, 1
              tmp0 = tmp0 + tmpi(i,j,k)
           end do
           end do
           tmp(t,k) = tmp0/real((iend-ista+1)*jmax) ! unit: [kg/kg]
        end do
     end do ! end of t-loop

!   case ('cpc','cph')
!      ! Cold pool intensity and height
!      ! *** this section work with flag = 3 ***
!      if(flag.ne.3) then
!         print *, "WARNING: flag = ", flag, "is under construction for now..."
!         stop 2
!      end if
!      ! --- thpert [K]
!      ivarname = 'thpert'
!      call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!      do t = 1, tmax, 1
!      do k = 1, kmax, 1
!      do i = 1, imax, 1
!         tmpi1(i,k,t) = var_in(i,1,k,t)
!      end do
!      end do
!      end do
!      ! --- th (t=1) [K]
!      ivarname = 'th'
!      call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!      do t = 1, tmax, 1
!      do k = 1, kmax, 1
!      do i = 1, imax, 1
!         tmpi2(i,k,t) = var_in(i,1,k,1) ! all the value is set to one for t = 1
!      end do
!      end do
!      end do
!      ! --- qvpert [kg/kg]
!      ivarname = 'qvpert'
!      call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!      do t = 1, tmax, 1
!      do k = 1, kmax, 1
!      do i = 1, imax, 1
!         tmpi3(i,k,t) = var_in(i,1,k,t)
!      end do
!      end do
!      end do
!      ! --- qc [kg/kg]
!      ivarname = 'qc'
!      call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!      do t = 1, tmax, 1
!      do k = 1, kmax, 1
!      do i = 1, imax, 1
!         tmpi4(i,k,t) = var_in(i,1,k,t)
!      end do
!      end do
!      end do
!      ! --- qr [kg/kg]
!      ivarname = 'qr'
!      call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
!      do t = 1, tmax, 1
!      do k = 1, kmax, 1
!      do i = 1, imax, 1
!         tmpi5(i,k,t) = var_in(i,1,k,t)
!      end do
!      end do
!      end do
!      ! calc. cpc and cph
!      do t = 1, tmax, 1
!      do i = 1, imax, 1
!         tmp(i,t) = 0.0
!         select case (varname)
!         case ('cpc')
!            if(tmpi1(i,1,t).le.-1.)then
! !           if(tmpi1(i,1,t).le.-0.1)then
!            do k = 1, kmax, 1
!               if( (tmpi1(i,k,t).le.-1.).and.(z(k).le.5.) )then
! !              if( (tmpi1(i,k,t).le.-0.1).and.(z(k).le.5.) )then
!                  tmp(i,t) = tmp(i,t)                                &
!                           - 2*9.81*( (tmpi1(i,k,t)/tmpi2(i,k,t))    &
!                                      + 0.608*tmpi3(i,k,t)           &
!                                      - tmpi4(i,k,t)                 &
!                                      - tmpi5(i,k,t) )*(z(k+1)-z(k))
!               end if
!            end do
!            if(tmp(i,t).ne.0.0)then
!               tmp(i,t) = sqrt(tmp(i,t))
!            end if
!            end if
!         case ('cph')
!            if(tmpi1(i,1,t).le.-1.)then
!            do k = 1, kmax, 1
!               if( (tmpi1(i,k,t).le.-1.).and.(z(k).le.5.) )then
!                  tmp(i,t) = z(k)*real(1000.) ! unit: [m]
!               end if
!            end do
!            end if
!         end select
!      end do
!      end do

  case ('tcpc')
     ! Area-averaged Cold pool intensity
     ! *** this section work with flag = 4 ***
     if(flag.ne.4) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- thpert [K]
     ivarname = 'thpert'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpi1(i,k,t) = var_in(i,1,k,t)
     end do
     end do
     end do
     ! --- th (t=1) [K]
     ivarname = 'th'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpi2(i,k,t) = var_in(i,1,k,1) ! all the value is set to one for t = 1
     end do
     end do
     end do
     ! --- qvpert [kg/kg]
     ivarname = 'qvpert'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpi3(i,k,t) = var_in(i,1,k,t)
     end do
     end do
     end do
     ! --- qc [kg/kg]
     ivarname = 'qc'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpi4(i,k,t) = var_in(i,1,k,t)
     end do
     end do
     end do
     ! --- qr [kg/kg]
     ivarname = 'qr'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpi5(i,k,t) = var_in(i,1,k,t)
     end do
     end do
     end do
     ! calc. cpc
     do t = 1, tmax, 1
        tmp0 = 0.0
        ipoint = 0
        do i = 1, imax, 1
        tmp1(i,t) = 0.0
        if(tmpi1(i,1,t).le.-1.)then
        do k = 1, kmax, 1
           if( (tmpi1(i,k,t).le.-1.).and.(z(k).le.5.) )then
              tmp1(i,t) = tmp1(i,t)                       &
                   - 2*9.81*( (tmpi1(i,k,t)/tmpi2(i,k,t)) &
                   + 0.608*tmpi3(i,k,t)                   &
                   - tmpi4(i,k,t)                         &
                   - tmpi5(i,k,t) )*(z(k+1)-z(k))
           end if
        end do ! end of k-loop
        if(tmp1(i,t).ne.0.0)then
           tmp1(i,t) = sqrt(tmp1(i,t))
           tmp0 = tmp0 + tmp1(i,t)
           ipoint = ipoint + 1
        end if
        end if
        end do ! end of i-loop
        if(ipoint.eq.0)then
           tmp(t,1) = 0.
        else
           tmp(t,1) = tmp0/real(ipoint)
        end if
     end do ! end of t-loop

  case ('webdymf')
     ! inward and outward mass fluxes of west and east boundaries
     ! *** this section work with flag = 8 ***
     if(flag.ne.8) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do t = 1, tmax, 1
        if(debug_level.ge.100) print *, " t = ", t
        istart = (/ 1, 1, 1, t /)
        ! west boundary
        ! --- read prs
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
           tmp1(j,k) = var_in(1,j,k,1)
        end do
        end do
        ! --- read th
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
           tmp0 = var_in(1,j,k,1)
           ! calculate temperature [degree C] using theta [K] and pressure [Pa]
           tmp2(j,k) = thetaP_2_T( tmp0, tmp1(j,k) ) ! unit: [K]
        end do
        end do
        ! --- read uinterp
        ivarname = 'uinterp'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
           tmp3(j,k) = var_in(1,j,k,1)
        end do
        end do
        ! --- calc. mass fluxes
        itmp1(:) = 0.0 ! fluxout at west bdy
        itmp2(:) = 0.0 ! fluxin  at west bdy
        do k = 1, kmax, 1
           do j = 1, jmax, 1
              tmp0 = TP_2_rho( tmp2(j,k), tmp1(j,k) )
              itmp1(k) = itmp1(k) - min(0.0,tmp0*tmp3(j,k))
              itmp2(k) = itmp2(k) + max(0.0,tmp0*tmp3(j,k))
           end do
           tmp(t,1) = tmp(t,1) + itmp1(k)
           tmp(t,2) = tmp(t,2) + itmp2(k)
        end do
        ! ---
        ! east boundary
        istart = (/ imax, 1, 1, t /)
        ! --- read prs
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
           tmp1(j,k) = var_in(1,j,k,1)
        end do
        end do
        ! --- read th
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
           tmp0 = var_in(1,j,k,1)
           ! calculate temperature [degree C] using theta [K] and pressure [Pa]
           tmp2(j,k) = thetaP_2_T( tmp0, tmp1(j,k) ) ! unit: [K]
        end do
        end do
        ! --- read uinterp
        ivarname = 'uinterp'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
           tmp3(j,k) = var_in(1,j,k,1)
        end do
        end do
        ! --- calc. mass fluxes
        itmp1(:) = 0.0 ! fluxout at east bdy
        itmp2(:) = 0.0 ! fluxin  at east bdy
        do k = 1, kmax, 1
           do j = 1, jmax, 1
              tmp0 = TP_2_rho( tmp2(j,k), tmp1(j,k) )
              itmp1(k) = itmp1(k) + max(0.0,tmp0*tmp3(j,k))
              itmp2(k) = itmp2(k) - min(0.0,tmp0*tmp3(j,k))
           end do
           tmp(t,3) = tmp(t,3) + itmp1(k)
           tmp(t,4) = tmp(t,4) + itmp2(k)
        end do
        if(debug_level.ge.100) print *, "  win,eout = ", tmp(t,2),tmp(t,3)
        if(debug_level.ge.100) print *, "  wout,ein = ", tmp(t,1),tmp(t,4)
     end do ! end of t-loop

  case ('snbdymf')
     ! inward and outward mass fluxes of south and north boundaries
     ! *** this section work with flag = 8 ***
     if(flag.ne.8) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do t = 1, tmax, 1
        if(debug_level.ge.100) print *, " t = ", t
        istart = (/ 1, 1, 1, t /)
        ! south boundary
        ! --- read prs
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp1(i,j) = var_in(i,j,1,1)
        end do
        end do
        ! --- read th
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp0 = var_in(i,j,1,1)
           ! calculate temperature [degree C] using theta [K] and pressure [Pa]
           tmp2(i,j) = thetaP_2_T( tmp0, tmp1(i,j) ) ! unit: [K]
        end do
        end do
        ! --- read winterp
        ivarname = 'winterp'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp3(i,j) = var_in(i,j,1,1)
        end do
        end do
        ! --- calc. mass fluxes
        itmp1(:) = 0.0 ! fluxout at south bdy
        itmp2(:) = 0.0 ! fluxin  at south bdy
        do i = 1, imax, 1
           do j = 1, jmax, 1
              tmp0 = TP_2_rho( tmp2(i,j), tmp1(i,j) )
              itmp1(i) = itmp1(i) - min(0.0,tmp0*tmp3(i,j))
              itmp2(i) = itmp2(i) + max(0.0,tmp0*tmp3(i,j))
           end do
           tmp(t,1) = tmp(t,1) + itmp1(i)
           tmp(t,2) = tmp(t,2) + itmp2(i)
        end do
        ! ---
        ! north boundary
        istart = (/ 1, 1, kmax, t /)
        ! --- read prs
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp1(i,j) = var_in(i,j,1,1)
        end do
        end do
        ! --- read th
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp0 = var_in(i,j,1,1)
           ! calculate temperature [degree C] using theta [K] and pressure [Pa]
           tmp2(i,j) = thetaP_2_T( tmp0, tmp1(i,j) ) ! unit: [K]
        end do
        end do
        ! --- read winterp
        ivarname = 'winterp'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do j = 1, jmax, 1
        do i = 1, imax, 1
           tmp3(i,j) = var_in(i,j,1,1)
        end do
        end do
        ! --- calc. mass fluxes
        itmp1(:) = 0.0 ! fluxout at east bdy
        itmp2(:) = 0.0 ! fluxin  at east bdy
        do i = 1, imax, 1
           do j = 1, jmax, 1
              tmp0 = TP_2_rho( tmp2(i,j), tmp1(i,j) )
              itmp1(i) = itmp1(i) + max(0.0,tmp0*tmp3(i,j))
              itmp2(i) = itmp2(i) - min(0.0,tmp0*tmp3(i,j))
           end do
           tmp(t,3) = tmp(t,3) + itmp1(i)
           tmp(t,4) = tmp(t,4) + itmp2(i)
        end do
        if(debug_level.ge.100) print *, "  sin,nout = ", tmp(t,2),tmp(t,3)
        if(debug_level.ge.100) print *, "  sout,nin = ", tmp(t,1),tmp(t,4)
     end do ! end of t-loop

  case ('webdyqvf')
     ! inward and outward qv fluxes of west and east boundaries
     ! *** this section work with flag = 8 ***
     if(flag.ne.8) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     do t = 1, tmax, 1
        if(debug_level.ge.100) print *, " t = ", t
        istart = (/ 1, 1, 1, t /)
        ! west boundary
        ! --- read prs
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
           tmp1(j,k) = var_in(1,j,k,1)
        end do
        end do
        ! --- read th
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
           tmp0 = var_in(1,j,k,1)
           ! calculate temperature [degree C] using theta [K] and pressure [Pa]
           tmp2(j,k) = thetaP_2_T( tmp0, tmp1(j,k) ) ! unit: [K]
        end do
        end do
        ! --- read uinterp
        ivarname = 'uinterp'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
           tmp3(j,k) = var_in(1,j,k,1)
        end do
        end do
        ! --- read qv
        ivarname = 'qv'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
           tmp4(j,k) = var_in(1,j,k,1)
        end do
        end do
        ! --- calc. qv fluxes
        itmp1(:) = 0.0 ! fluxout at west bdy
        itmp2(:) = 0.0 ! fluxin  at west bdy
        do k = 1, kmax, 1
           do j = 1, jmax, 1
              tmp0 = TP_2_rho( tmp2(j,k), tmp1(j,k) )
              itmp1(k) = itmp1(k) - min(0.0,tmp0*tmp3(j,k)*tmp4(j,k))
              itmp2(k) = itmp2(k) + max(0.0,tmp0*tmp3(j,k)*tmp4(j,k))
           end do
           tmp(t,1) = tmp(t,1) + itmp1(k)
           tmp(t,2) = tmp(t,2) + itmp2(k)
        end do
        ! ---
        ! east boundary
        istart = (/ imax, 1, 1, t /)
        ! --- read prs
        ivarname = 'prs'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
           tmp1(j,k) = var_in(1,j,k,1)
        end do
        end do
        ! --- read th
        ivarname = 'th'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
           tmp0 = var_in(1,j,k,1)
           ! calculate temperature [degree C] using theta [K] and pressure [Pa]
           tmp2(j,k) = thetaP_2_T( tmp0, tmp1(j,k) ) ! unit: [K]
        end do
        end do
        ! --- read uinterp
        ivarname = 'uinterp'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
           tmp3(j,k) = var_in(1,j,k,1)
        end do
        end do
        ! --- read qv
        ivarname = 'qv'
        call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
        do k = 1, kmax, 1
        do j = 1, jmax, 1
           tmp4(j,k) = var_in(1,j,k,1)
        end do
        end do
        ! --- calc. qv fluxes
        itmp1(:) = 0.0 ! fluxout at east bdy
        itmp2(:) = 0.0 ! fluxin  at east bdy
        do k = 1, kmax, 1
           do j = 1, jmax, 1
              tmp0 = TP_2_rho( tmp2(j,k), tmp1(j,k) )
              itmp1(k) = itmp1(k) + max(0.0,tmp0*tmp3(j,k)*tmp4(j,k))
              itmp2(k) = itmp2(k) - min(0.0,tmp0*tmp3(j,k)*tmp4(j,k))
           end do
           tmp(t,3) = tmp(t,3) + itmp1(k)
           tmp(t,4) = tmp(t,4) + itmp2(k)
        end do
        if(debug_level.ge.100) print *, " win,eout = ", tmp(t,2),tmp(t,3)
        if(debug_level.ge.100) print *, " wout,ein = ", tmp(t,1),tmp(t,4)
     end do ! end of t-loop

  case ('watertave')
     ! x-z axis of temporally averaged water condansation mixing ratio
     ! *** this section work with flag = 7 ***
     if(flag.ne.7) then
        print *, "WARNING: flag = ", flag, "is under construction for now..."
        stop 2
     end if
     ! --- read qc
     ivarname = 'qc'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpi(i,k,t) = tmpi(i,k,t) + var_in(i,1,k,t)
     end do
     end do
     end do
     ! --- read qr
     ivarname = 'qr'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpi(i,k,t) = tmpi(i,k,t) + var_in(i,1,k,t)
     end do
     end do
     end do
     ! --- read qi
     ivarname = 'qi'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpi(i,k,t) = tmpi(i,k,t) + var_in(i,1,k,t)
     end do
     end do
     end do
     ! --- read qg
     ivarname = 'qg'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpi(i,k,t) = tmpi(i,k,t) + var_in(i,1,k,t)
     end do
     end do
     end do
     ! --- read qs
     ivarname = 'qs'
     call getncvar( ivarname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     do t = 1, tmax, 1
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmpi(i,k,t) = tmpi(i,k,t) + var_in(i,1,k,t)
     end do
     end do
     end do
     ! --- average temporally
     do k = 1, kmax, 1
     do i = 1, imax, 1
        tmp0 = 0.
        do t = 1, tmax, 1
           tmp0 = tmp0 + tmpi(i,k,t)
        end do
        tmp(i,k) = tmp0/real(tmax) ! unit: [kg/kg]
     end do ! end of i-loop
     end do ! end of k-loop

  case default
     ! the others
     ! *** this section work with flag = 1, 2, 3, 4, and 5 ***
     if(debug_level.ge.100) print *, "Use default case"
     call getncvar( varname, inx, iny, inz, int, ncid, varid, var_in, istart, icount, debug_level )
     !
  end select


  ! close netcdf file
  call check( nf90_close(ncid) )
  if(debug_level.ge.100) print *, "Success: close the netcdf data"
  if(debug_level.ge.100) print *, ""


  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! make arrays
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  if((flag.eq.4).or.(flag.eq.8)) then
     !ccccccccccccccccccccccccccccccccccccccccccccccccc
     ! Output 1D file
     ! The following variables are work with this option:
     ! (time series)
     !  "maxrain", "averain", "apw", "apm", "aps", "ape", 
     !  "tthetaeave", "tqvave", "tcapeave", "tcinave", "tlfcave", "tlnbave", "tcapeavenc", "tcapeaveus", "tcpc", "tpwave", "tpwaveus"
     ! (time and area (x-y) averaged vertical profile)
     !  "vtotwcave", "vtotwcstd", "vqvave", "vqvavec", "vthetaeave", "vthetaeavec", 
     !  "vrhave", "vrhavec", "vwmax", "vwave", 
     ! (area averaged vertical profile)
     !  "vtheta", "vqv", "vthetae", "vthetaeca", "vthetaes", "vthetav", "vcape", "vcapeca"
     ! (time series of mass fluxes on west and east boundry)
     !  "webdymf", "webdyqvf", "snbdymf"
     !ccccccccccccccccccccccccccccccccccccccccccccccccc
     ! create the file
     open(unit=20,file=output)
     if(debug_level.ge.100) print *, "Success: open the output file as ",trim(output)
     
     ! writeout data to output file
     select case (varname)
     case ('maxrain','averain','apw','apm','aps','ape',         &
           'tthetaeave','tqvave','tcapeave','tcinave',          &
           'tcapeavenc','tcpc','tpwave','tpwaveus','tcapeaveus' )
        do t = 1, tmax, 1
           write(20,111) real(time_in(t)/dble(60.)), tmp(t,1)
           if(debug_level.ge.200) print 222, "t,time,var = ", t, real(time_in(t)/dble(60.)), tmp(t,1)
        end do
     case ('tlfcave','tlnbave')
        do t = 1, tmax, 1
           write(20,111) real(time_in(t)/dble(60.)), tmp(t,1) !*real(0.001) ! unit: [m] -> [km]
           if(debug_level.ge.200) print 222, "t,time,var = ", t, real(time_in(t)/dble(60.)), tmp(t,1) !*real(0.001) ! unit: [m] -> [km]
        end do
     case ('vtotwcave','vtotwcstd','vqvave','vqvavec','vqv')
        if(debug_level.ge.200) print *, " unit: [kg/kg] -> [g/kg]"
        do k = 1, kmax, 1
           write(20,111) z(k), tmp(k,1)*real(1000.) ! unit: [kg/kg] -> [g/kg]
           if(debug_level.ge.200) print 222, "k,z,var = ", k, z(k), tmp(k,1)*real(1000.) ! unit: [kg/kg] -> [g/kg]
        end do
     case ('vwmax','vwave')
        do k = 1, kmax, 1
           write(20,111) z(k), tmp(k,1) ! unit: [m/s]
           if(debug_level.ge.200) print 222, "k,z,var = ", k, z(k), tmp(k,1) ! unit: [m/s]
        end do
     case ('vthetaeave','vthetaave','vthetaeavec','vrhave','vrhavec', &
           'vtheta','vthetav', 'vthetae','vthetaeca','vthetaes',      &
           'vcape','vcapeca')
        do k = 1, kmax, 1
           write(20,111) z(k), tmp(k,1)
           if(debug_level.ge.200) print 222, "k,z,var = ", k, z(k), tmp(k,1)
        end do
     case ('webdymf','snbdymf','webdyqvf')
        do t = 1, tmax, 1
           write(20,112) real(time_in(t)/dble(60.)), tmp(t,1),tmp(t,2),tmp(t,3),tmp(t,4)
           if(debug_level.ge.200) print 223, "t,time,var = ", t, real(time_in(t)/dble(60.)), tmp(t,1),tmp(t,2),tmp(t,3),tmp(t,4)
        end do
     end select
     if(debug_level.ge.100) print *, "Success: write out data to the output file"
     
     ! close the file
     close(unit=20)
     if(debug_level.ge.100) print *, "Success: close the output file"
     if(debug_level.ge.100) print *, ""

     ! formats
111  format(f8.3,f18.8)
112  format(f8.3,4f18.8)
222  format(a22,i5,f8.3,f18.8)
223  format(a22,i5,f8.3,f18.8)

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
           if(debug_level.ge.100) print *, "The tmp array has already allocated"
        case ('water')
           if(debug_level.ge.100) print *, " unit: [kg/kg] -> [g/kg]"
           tmp(:,:) = tmp(:,:)*real(1000.) ! unit: [kg/kg] -> [g/kg]
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
           if(debug_level.ge.100) print *, "The tmp array has already allocated for ", trim(varname)
           if(debug_level.ge.200) print *, " unit: [kg/kg] -> [g/kg]"
           tmp(:,:) = tmp(:,:)*real(1000.) ! unit: [kg/kg] -> [g/kg]
        case ('qc','qr','qi','qs','qg','pt01','pt02','pt03')
           tmp(:,:) = var_in(:,1,:,1)
           if(debug_level.ge.200) print *, " unit: [kg/kg] -> [g/kg]"
           tmp(:,:) = tmp(:,:)*real(1000.) ! unit: [kg/kg] -> [g/kg]
        case ('thetae','sruinterp','srvinterp')
           if(debug_level.ge.100) print *, "The tmp array has already allocated for ", trim(varname)
        case ('lwdt','wadv','vpga','buoy','load','pwdt','dbdx','dbdz')
           if(debug_level.ge.100) print *, "The tmp array has already allocated for ", trim(varname)
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

           if(iy(ny).gt.z(kmax)) then
              print *, ""
              print *, "ERROR: iy(ny) exceeds z(kmax)"
              print *, "        iy(ny)  = ", iy(ny)
              print *, "        z(kmax) = ", z(kmax)
              print *, "       iy(ny) should be smaller than or equal to z(kmax)"
              print *, "*** Please reduce the values of ny or dy ***"
              print *, ""
              stop 3
           end if

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
        if(debug_level.ge.200) print *, " var_out(",xselect,",:) = ", var_out(xselect,:)

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
        iy(:) = real(time_in(:)/60.) ! uint: [minute] -> [hour]
        !iy(:) = time(:) ! uint: [second]

        select case (varname)
        case ('rain')
           if(debug_level.ge.200) print *, " unit: [cm] -> [mm]"
           do t = 1, tmax, 1
           do i = 1, imax, 1
              var_out(i,t) = var_in(i,1,t,1)*real(10.) ! unit: [cm] -> [mm]
           end do
           if(debug_level.ge.200) print *, "t,iy,var_out = ",t,iy(t),var_out(xselect,t)
           end do
        case ('cape','cin','lins','thetae','mlthetae','rainrate','cpc','pw')
           do t = 1, tmax, 1
           do i = 1, imax, 1
              var_out(i,t) = tmp(i,t)
           end do
           if(debug_level.ge.200) print *, "t,iy,var_out = ",t,iy(t),var_out(xselect,t)
           end do
        case ('lfc','lnb','cph')
           if(debug_level.ge.200) print *, " unit: [m] -> [km]"
           do t = 1, tmax, 1
           do i = 1, imax, 1
              var_out(i,t) = tmp(i,t)*real(0.001) ! unit: [m] -> [km]
           end do
           if(debug_level.ge.200) print *, "t,iy,var_out = ",t,iy(t),var_out(xselect,t)
           end do
        case ('yvort')
           if(debug_level.ge.200) print *, " unit: [s-1] -> [*10-2 s-2]"
           do t = 1, tmax, 1
           do i = 1, imax, 1
              var_out(i,t) = var_in(i,1,t,1)*real(100.) ! unit: [s-1] -> [*10-2 s-2]
           end do
           if(debug_level.ge.200) print *, "t,iy,var_out = ",t,iy(t),var_out(xselect,t)
           end do
        case ('xtwater','cxtwater')
           if(debug_level.ge.200) print *, " unit: [kg/kg] -> [g/kg]"
           do t = 1, tmax, 1
           do i = 1, imax, 1
              var_out(i,t) = tmp(i,t)*real(1000.)
           end do
           if(debug_level.ge.200) print *, "t,iy,var_out = ",t,iy(t),var_out(xselect,t)
           end do
        case default
           do t = 1, tmax, 1
           do i = 1, imax, 1
              var_out(i,t) = var_in(i,1,t,1)
           end do
           if(debug_level.ge.200) print *, "t,iy,var_out = ",t,iy(t),var_out(xselect,t)
           end do
        end select

     else if(flag.eq.5) then
        ! arvitrary cross-section
        if(debug_level.ge.0) print *, "arvitrary cross-section"
        ! ix and yy array
        ! create the coordinate that the user specified point and angle
        if( (xselect.lt.x(1)).or.(xselect.gt.x(imax)) ) then
           print *, "ERROR: xselect must be specified between x(1) and x(imax)"
           stop 3
        end if
        if( (yselect.lt.y(1)).or.(yselect.gt.y(jmax)) ) then
           print *, "ERROR: yselect must be specified between y(1) and y(imax)"
           stop 3
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
!$omp end parallel do
           !
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
!$omp end parallel do
           !
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
        if(debug_level.ge.200) print *, " var_out(",nx/2,",:) = ", var_out(nx/2,:)

     else if(flag.eq.6) then
        ! t-z
        if(debug_level.ge.100) print *, "t-z array"
        !
        ! select one of the 2D array
        select case (varname)
        case ('tzwater','tzqvpert')
           if(debug_level.ge.200) print *, "The tmp array has already allocated for ", trim(varname)
           if(debug_level.ge.200) print *, " unit: [kg/kg] -> [g/kg]"
           tmp(:,:) = tmp(:,:)*real(1000.) ! unit: [kg/kg] -> [g/kg]
        case ('tzthpert','tzthetae')
           if(debug_level.ge.200) print *, "The tmp array has already allocated for ", trim(varname)
        end select
        if(debug_level.ge.200) print *, " tmp(",xselect,",:)    = ", tmp(xselect,:)
        
        ! ix array
        ix(:) = real(time_in(:)/60.) ! uint: [minute] -> [hour]
        !ix(:) = time_in(:)
        if(debug_level.ge.200) print *, " ix(:)         = ", ix

        ! iy array
        if(interp_y.eq.1) then
           ! interpolate the stretched y-coordinate to constant dy coordinate
           do k = 1, ny, 1
              iy(k) = (k-1)*dy
           end do
           if(debug_level.ge.200) print *, " iy(:)         = ", iy

           if(iy(ny).gt.z(kmax)) then
              print *, ""
              print *, "ERROR: iy(ny) exceeds z(kmax)"
              print *, "        iy(ny)  = ", iy(ny)
              print *, "        z(kmax) = ", z(kmax)
              print *, "       iy(ny) should be smaller than or equal to z(kmax)"
              print *, "*** Please reduce the values of ny or dy ***"
              print *, ""
              stop 3
           end if

           select case (interp_method)
           case ('linear')
              ! z coordinate points are linearly interpolated by interp_linear
              do i = 1, tmax
                 CALL interp_linear( kmax, ny, z, iy, tmp(i,:), var_out(i,:), debug_level )
              end do
           case ('near')
              ! z coordinate points are interpolated by nearest_interp_1d
              do i = 1, tmax
                 CALL nearest_interp_1d( kmax, z(:), tmp(i,:), ny, iy(:), var_out(i,:) )
              end do
           case ('stpk')
              ! z coordinate points are linearly interpolated by STPK library
              do j = 2, ny-1, 1
                 !CALL nearest_search_1d( z, iy(j), ipoint)
                 CALL interpo_search_1d( z, iy(j), ipoint)
                 if(debug_level.ge.300) print *, " j,ipoint,iy,y = ", j,ipoint,iy(j),y(ipoint)
                 do i = 1, tmax, 1
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
        if(debug_level.ge.200) print *, " var_out(",xselect,",:) = ", var_out(xselect,:)

     else if(flag.eq.7) then
        ! x-z
        if(debug_level.ge.100) print *, "x-z array (time ave.)"

        ! select one of the 2D array
        select case (varname)
        case ('watertave')
           if(debug_level.ge.100) print *, "The tmp array has already allocated for ", trim(varname)
           if(debug_level.ge.200) print *, " unit: [kg/kg] -> [g/kg]"
           tmp(:,:) = tmp(:,:)*real(1000.) ! unit: [kg/kg] -> [g/kg]
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

           if(iy(ny).gt.z(kmax)) then
              print *, ""
              print *, "ERROR: iy(ny) exceeds z(kmax)"
              print *, "        iy(ny)  = ", iy(ny)
              print *, "        z(kmax) = ", z(kmax)
              print *, "       iy(ny) should be smaller than or equal to z(kmax)"
              print *, "*** Please reduce the values of ny or dy ***"
              print *, ""
              stop 3
           end if

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
        if(debug_level.ge.200) print *, " var_out(",xselect,",:) = ", var_out(xselect,:)

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
        deallocate( x,y,z,time_in,var_in,tmp) !,tmp1,tmp2,tmp3,tmp4,tmp5 )
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
  ! subroutine of getncvar
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  subroutine getncvar( ivar, nx, ny, nz, nt, ncid, varid, var_io, istart, icount, debug_level )
    implicit none
    integer, intent(in   ) :: nx, ny, nz, nt, debug_level
    integer, intent(inout) :: ncid, varid
    integer, dimension(4), intent(inout) :: istart, icount
    real, dimension(nx,ny,nz,nt), intent(inout) :: var_io
    character(len=20), intent(in) :: ivar
    !
    call check( nf90_inq_varid(ncid, trim(ivar), varid) )
    if(debug_level.ge.200) print *, " Success: inquire the varid"
    if(debug_level.ge.200) print *, "  varid         = ", varid
    if(debug_level.ge.300) print *, "   istart       = ", istart
    if(debug_level.ge.300) print *, "   icount       = ", icount
    call check( nf90_get_var(ncid, varid, var_io, start = istart, count = icount ) )
    if(debug_level.ge.200) print *, " Success: get the var array (",trim(ivar),")"
    if(debug_level.ge.200) print *, "  var_in(1,1,1,1) = ", var_io(1,1,1,1)
    !
  end subroutine getncvar

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
  real function thetae_Bolton( T, qv, P )
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

  real function thetaes_Bolton(T,P)  ! Bolton(1980) による手法を用いて飽和相当温位を計算する.
    implicit none
    real, intent(in) :: T  ! 温度 [K]
    real, intent(in) :: P  ! 全圧 [Pa]
    real :: qvs
    real, parameter :: a=0.2854, b=0.28, c=3376.0, d=0.81
    real, parameter :: p0=1.0e5
    
    qvs=TP_2_qvs(T,P)
    thetaes_Bolton=T*((p0/P)**(a*(1.0-b*qvs)))*exp((c/T-2.54)*qvs*(1.0+d*qvs))
    
    return
  end function thetaes_Bolton
  
  real function TqvP_2_TLCL( T, qv, P )  !! 温度と混合比と全圧から T_LCL を計算する
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
  
  real function qvP_2_e( qv, P )  ! 混合比と全圧から水蒸気圧を計算する
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
  
  real function TP_2_qvs( T, P )  ! 温度と全圧から飽和混合比を計算する
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
  
  real function es_Bolton( T )  ! Bolton(1980) の手法を用いて飽和水蒸気圧を計算する.
    implicit none
    real, intent(in) :: T  ! 大気の温度 [K]
    real, parameter :: a=17.67, c=29.65
    real, parameter :: e0=611.0
    real, parameter :: t0=273.15
    
    es_Bolton=e0*exp(a*((T-t0)/(T-c)))
    
    return
  end function es_Bolton
  
  real function eP_2_qv( e, P )  ! 水蒸気圧と全圧から混合比を計算する
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

  real function thetaP_2_T( theta, P )  ! 温位, 圧力から温度を計算する(乾燥大気として計算)
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

  real function TP_2_rho( T, P )  ! 乾燥大気の状態方程式から, 温度と気圧を与えて密度を得る.
    implicit none
    real, intent(in) :: T    ! 大気の温度 [K]
    real, intent(in) :: P    ! 大気の圧力 [Pa]
    real, parameter :: Rd=287.0

    TP_2_rho=P/(Rd*T)
    
    return
  end function TP_2_rho

  real function eT_2_RH( e, T )  ! 水蒸気圧と温度から相対湿度を計算する
    ! $RH=(e/es)\times 100$ という定義から計算.
    implicit none
    real, intent(in) :: e  ! 水蒸気圧 [Pa]
    real, intent(in) :: T  ! 温度 [K]
    real :: es
    
    es=es_Bolton(T)
    eT_2_RH=100.0*e/es
    
    return
  end function eT_2_RH

  real function qvTP_2_RH( qv, T, P )  ! 混合比と温度から相対湿度を計算する.
    ! qvP_2_e から水蒸気圧を計算し, 相対湿度の定義を用いる.
    implicit none
    real, intent(in) :: qv  ! 相対湿度 [kg / kg]
    real, intent(in) :: T   ! 温度 [K]
    real, intent(in) :: P   ! 全圧 [Pa]
    real :: e
    
    e=qvP_2_e(qv,P)
    qvTP_2_RH=eT_2_RH(e,T)
    
    return
  end function qvTP_2_RH

  real function TqvP_2_thetav( T, qv, P )  ! 温度, 水蒸気混合比, 圧力から仮温位を計算する.
    implicit none
    real, intent(in) :: qv  ! 水蒸気混合比 [kg / kg]
    real, intent(in) :: T   ! 温度 [K]
    real, intent(in) :: P   ! 圧力 [Pa]
    real :: kappa, Tv
    real, parameter :: Rd=287.0
    real, parameter :: Cpd=1004.0

    kappa=Rd/Cpd
    Tv=qvT_2_Tv(qv,T)
    TqvP_2_thetav=theta_dry(Tv,P)
    
    return
  end function TqvP_2_thetav

  real function qvT_2_Tv( qv, T )  ! 温度と水蒸気混合比から仮温度を計算する.
    implicit none
    real, intent(in) :: qv  ! 水蒸気混合比 [kg / kg]
    real, intent(in) :: T   ! 温度 [K]
    real :: eps
    real, parameter :: Rd=287.0
    real, parameter :: Rv=461.0
    
    eps=Rd/Rv
    qvT_2_Tv=T*(1.0+qv/eps)/(1.0+qv)
    
    return
  end function qvT_2_Tv

  real function theta_dry( T, P )  ! 乾燥大気における温位を計算する
    ! ただし, 湿潤大気においても, 観測される全圧を P として計算することができ
    ! その結果は別関数 theta_moist の結果とそれほど変わらない.
    implicit none
    real, intent(in) :: T  ! 温度 [K]
    real, intent(in) :: P  ! 乾燥大気の気圧(もしくは, 湿潤大気の全圧) [Pa]
    real :: kappa
    real, parameter :: Rd=287.0
    real, parameter :: Cpd=1004.0
    real, parameter :: p0=1.0e5

    kappa=Rd/Cpd
    theta_dry=T*(p0/P)**kappa
    
    return
  end function theta_dry
  
  real function precip_water( p, qv, undef )  ! 可降水量を計算する. 単位は [kg/m^2]
    ! 積分範囲は p の要素数の上下端で自動的に指定.
    ! 気象がわかる数と式より, 高度座標で積分するのではなく,
    ! 静力学平衡から気圧座標へ置き直して積分.
    implicit none
    real, intent(in) :: p(:)  ! 圧力 [Pa]
    real, intent(in) :: qv(size(p))  ! 水蒸気混合比 [kg/kg]
    real, intent(in), optional :: undef  ! undef
    integer :: nx, i
    real, dimension(size(p)) :: tmp_p, tmp_qv
    real :: precip

    real, parameter :: g = 9.81

    nx=size(p)
    
    !-- 積分のため, 順序の入れ替え
    do i=1,nx
       tmp_qv(i)=qv(nx-i+1)
       tmp_p(i)=p(nx-i+1)
    end do
    
    if(present(undef))then
       call rectangle_int( tmp_p, tmp_qv, tmp_p(1), tmp_p(nx), precip, undef )
    else
       call rectangle_int( tmp_p, tmp_qv, tmp_p(1), tmp_p(nx), precip )
    end if
    
    precip_water=precip/g
    
    return
  end function precip_water

  subroutine rectangle_int( x, y, bot, top, res, undeff )  ! 1 次元台形積分
    ! 不等間隔でも計算可能であるが, 精度は保証しない.
    implicit none
    real, intent(in) :: bot  ! 積分区間左端
    real, intent(in) :: top  ! 積分区間右端
    real, intent(in) :: x(:)  ! 積分変数
    real, intent(in) :: y(size(x))  ! 非積分関数
    real, intent(inout) :: res  ! 台形積分の積分値
    real, intent(in), optional :: undeff
    integer :: i, nx, i_bot, i_top
    real :: y_bot, y_top
    
    nx=size(x)
    
    res=0.0
    
    !-- bot < top でなければエラー
    if(bot>top)then
       write(*,*) "#### ERROR (algebra:rectangle_int) ####"
       write(*,*) "integrated interval must be bot < top. STOP"
       stop
    end if
    
    !-- 以下で積分域を決定
    !-- 実際には, 積分境界に最近する点
    
    if(x(1)>=bot)then
       if(x(1)>bot)then
          write(*,*) "#### WARNING ####"
          write(*,*) "there is NOT the bot in the x(i)."  ! このときは, i_bot=1としておいても, x(1)=bot なので, 余分計算はゼロとなり影響はない.
       end if
       i_bot=1
    end if
    if(x(nx)<=top)then
       if(x(nx)<top)then
          write(*,*) "#### WARNING ####"
          write(*,*) "there is NOT the top in the x(i)."  ! このとき, i_top=nx としても, x(nx)=top なので, 余分計算はゼロとなる.
       end if
       i_top=nx
    end if
    
    do i=2,nx-1
       if(x(i)>=bot)then  ! i_bot は bot - top 内の最も bot に近づく格子
          i_bot=i
          exit
       end if
    end do
    
    do i=nx-1,2,-1
       if(x(i)<=top)then  ! i_top は bot - top 内の最も top に近づく格子
          i_top=i
          exit
       end if
    end do
    
    !-- 以下で格子に当てはまらない部分を内挿で補完
    if(present(undeff))then
       if(i_bot/=1)then
          if(y(i_bot)/=undeff.and.y(i_bot-1)/=undeff.and.x(i_bot)/=x(i_bot-1))then
             y_bot=y(i_bot-1) +((y(i_bot)-y(i_bot-1))/(x(i_bot)-x(i_bot-1)))*(bot-x(i_bot-1))
          else
             y_bot=-y(i_bot)  ! 最後の積分でゼロにするための措置
          end if
       else
          y_bot=-y(i_bot)  ! 最後の積分でゼロにするための措置
       end if
       if(i_top/=nx)then
          if(y(i_top)/=undeff.and.y(i_top+1)/=undeff.and.x(i_top+1)/=x(i_top))then
             y_top=y(i_top) +((y(i_top+1)-y(i_top))/(x(i_top+1)-x(i_top)))*(x(i_top+1)-top)
          else
             y_top=-y(i_top)
          end if
       else
          y_top=-y(i_top)  ! 最後の積分でゼロにするための措置
       end if
    else
       if(i_bot/=1)then
          y_bot=y(i_bot-1) +((y(i_bot)-y(i_bot-1))/(x(i_bot)-x(i_bot-1)))*(bot-x(i_bot-1))
       else
          y_bot=-y(i_bot)  ! 最後の積分でゼロにするための措置
       end if
       if(i_top/=nx)then
          y_top=y(i_top) +((y(i_top+1)-y(i_top))/(x(i_top+1)-x(i_top)))*(x(i_top+1)-top)
       else
          y_top=-y(i_top)  ! 最後の積分でゼロにするための措置
       end if
    end if
    
    if(i_bot<i_top)then  ! 積分区間内に格子が 2 つ以上あるとき
       
       if(present(undeff))then
          do i=i_bot,i_top-1
             if(y(i+1)/=undeff.and.y(i)/=undeff)then
                if(i==i_bot)then
                   res=res+0.5*(x(i)-bot)*(y(i)+y_bot) +0.5*(x(i+1)-x(i))*(y(i+1)+y(i))
                   ! 下端の余りと通常の短冊分
                else
                   res=res+0.5*(x(i+1)-x(i))*(y(i+1)+y(i))  ! 通常の短冊
                end if
             end if
          end do
          res=res+0.5*(top-x(i_top))*(y_top+y(i_top))  ! 上端の余りのみ
       else
          do i=i_bot,i_top-1
             if(i==i_bot)then
                res=res+0.5*(x(i)-bot)*(y(i)+y_bot) +0.5*(x(i+1)-x(i))*(y(i+1)+y(i))
                ! 下端の余りと通常の短冊分
             else
                res=res+0.5*(x(i+1)-x(i))*(y(i+1)+y(i))  ! 通常の短冊
             end if
          end do
          res=res+0.5*(top-x(i_top))*(y_top+y(i_top))  ! 上端の余りのみ
       end if
    else
       if(present(undeff))then
          if(y(i_bot)/=undeff)then
             res=res+0.5*(x(i)-bot)*(y(i)+y_bot) +0.5*(top-x(i))*(y_top+y(i))
          else
             res=0.5*(top-bot)*(y_top+y_bot)
          end if
       else
          res=res+0.5*(x(i_bot)-bot)*(y(i_bot)+y_bot) +0.5*(top-x(i_bot))*(y_top+y(i_bot))
       end if
    end if
    
  end subroutine rectangle_int


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
    real(8) :: avgth,avgqv
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
!        nloop = 1 + int( dp/pinc )
        nloop = 1 + dp/pinc
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

    if(debug_level.ge.300) print *,'  zlcl,zlfc,zel = ',zlcl,zlfc,zel
    if(debug_level.ge.300) print *,'  plcl,plfc,pel = ',plcl,plfc,pel

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
