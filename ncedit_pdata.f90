!
! N C E D I T (pdata)
!
! original program coded by Takashi Unuma, Kyoto Univ.
! Last modified: 2015/01/04
!

program ncedit_pdata

  use netcdf
  
  implicit none

  integer :: tmax, nparcel
  real, dimension(:),   allocatable :: time
  real, dimension(:,:), allocatable :: x, z
  real, dimension(:,:), allocatable :: w, pwdt, vpga, buoy, load
  real, dimension(:,:), allocatable :: th, prs, qv, thetae
  character(len=20) :: varname
  character(len=42) :: input, output
  integer :: debug_level

  ! local variables
  integer :: i, t, ncid, varid, tdimid
  integer, dimension(2) :: istart, icount
  real :: tmp0

  ! undefined value
!  real :: nan = -9999.
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
  allocate( x(nparcel,tmax),z(nparcel,tmax),w(nparcel,tmax) )
  allocate( pwdt(nparcel,tmax),vpga(nparcel,tmax),buoy(nparcel,tmax),load(nparcel,tmax) )
  allocate( th(nparcel,tmax),prs(nparcel,tmax),qv(nparcel,tmax),thetae(nparcel,tmax) )
  istart = (/ 1, 1 /)
  icount = (/ nparcel, tmax /)
  x(1:nparcel,1:tmax) = nan
  z(1:nparcel,1:tmax) = nan
  w(1:nparcel,1:tmax) = nan
  pwdt(1:nparcel,1:tmax) = nan
  vpga(1:nparcel,1:tmax) = nan
  buoy(1:nparcel,1:tmax) = nan
  load(1:nparcel,1:tmax) = nan
  th(1:nparcel,1:tmax)  = nan
  prs(1:nparcel,1:tmax) = nan
  qv(1:nparcel,1:tmax)  = nan
  thetae(1:nparcel,1:tmax) = nan


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

  call check( nf90_inq_varid(ncid, "th", varid) )
  if(debug_level.ge.100) print *, " Success: inquire the varid"
  if(debug_level.ge.200) print *, "  varid         = ", varid
  if(debug_level.ge.300) print *, "   istart       = ", istart
  if(debug_level.ge.300) print *, "   icount       = ", icount
  call check( nf90_get_var(ncid, varid, th, start = istart, count = icount ) )
  if(debug_level.ge.100) print *, " Success: get the var array (th)"
  if(debug_level.ge.200) print *, "  th(:,:) = ", th(:,:)
  if(debug_level.ge.100) print *, ""

  call check( nf90_inq_varid(ncid, "prs", varid) )
  if(debug_level.ge.100) print *, " Success: inquire the varid"
  if(debug_level.ge.200) print *, "  varid         = ", varid
  if(debug_level.ge.300) print *, "   istart       = ", istart
  if(debug_level.ge.300) print *, "   icount       = ", icount
  call check( nf90_get_var(ncid, varid, prs, start = istart, count = icount ) )
  if(debug_level.ge.100) print *, " Success: get the var array (prs)"
  if(debug_level.ge.200) print *, "  prs(:,:) = ", prs(:,:)
  if(debug_level.ge.100) print *, ""

  call check( nf90_inq_varid(ncid, "qv", varid) )
  if(debug_level.ge.100) print *, " Success: inquire the varid"
  if(debug_level.ge.200) print *, "  varid         = ", varid
  if(debug_level.ge.300) print *, "   istart       = ", istart
  if(debug_level.ge.300) print *, "   icount       = ", icount
  call check( nf90_get_var(ncid, varid, qv, start = istart, count = icount ) )
  if(debug_level.ge.100) print *, " Success: get the var array (qv)"
  if(debug_level.ge.200) print *, "  qv(:,:) = ", qv(:,:)
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
        ! calculate equivalent potential temperature [K]
        tmp0 = thetaP_2_T( th(i,t), prs(i,t) ) ! temperature [degree C]
        thetae(i,t) = thetae_Bolton( tmp0, qv(i,t), prs(i,t) )
        write(20,111) time(t)/real(3600.), x(i,t)*0.001, z(i,t)*0.001,    &
                      w(i,t), pwdt(i,t), vpga(i,t), buoy(i,t), load(i,t), &
                      thetae(i,t)
        if(debug_level.ge.200) print 222, "t,time_out,var_outs = ",t,time(t)/real(3600.),x(i,t)*0.001,z(i,t)*0.001, w(i,t), pwdt(i,t), vpga(i,t), buoy(i,t), load(i,t), thetae(i,t)
     end do
     close(20)
  end do
  if(debug_level.ge.100) print *, "Success: write out data to the output file"
  if(debug_level.ge.100) print *, ""

  ! formats
111 format(f8.3,8f18.8)
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

  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! subroutine of getncvar
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! subroutine getncvar( ivar, nx, ny, nz, nt, ncid, varid, var_io, istart, icount, debug_level )
  !   implicit none
  !   integer, intent(in   ) :: nx, ny, nz, nt, debug_level
  !   integer, intent(inout) :: ncid, varid
  !   integer, dimension(2), intent(inout) :: istart, icount
  !   real, dimension(nx,ny,nz,nt), intent(inout) :: var_io
  !   character(len=20), intent(in) :: ivar
  !   !
  !   call check( nf90_inq_varid(ncid, trim(ivar), varid) )
  !   if(debug_level.ge.200) print *, " Success: inquire the varid"
  !   if(debug_level.ge.200) print *, "  varid         = ", varid
  !   if(debug_level.ge.300) print *, "   istart       = ", istart
  !   if(debug_level.ge.300) print *, "   icount       = ", icount
  !   call check( nf90_get_var(ncid, varid, var_io, start = istart, count = icount ) )
  !   if(debug_level.ge.200) print *, " Success: get the var array (",trim(ivar),")"
  !   if(debug_level.ge.200) print *, "  ",trim(ivar),"(:,:) = ", var_io(:,:)
    !
  ! end subroutine getncvar
  
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

end program ncedit_pdata
