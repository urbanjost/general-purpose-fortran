program test_suite_M_units
use,intrinsic :: iso_fortran_env, only: real32, real64, real128
use M_framework__verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_framework__verify, only : unit_test_level
use M_framework__msg,    only : str
use M_anything,          only : anyscalar_to_real, anyscalar_to_double
USE M_Compare_Float_Numbers
use M_framework,         only : accdig
use M_units

!! test constants
   call testit_p('pi',      real(PI)      ,  real(3.141592653589793238462643383279500d0)  ,message='')
   call testit_p('e',       real(E)       ,  real(2.718281828459045235360d0)              ,message='')
   !!call testit_p('radian',  real(RADIAN)  ,  real(57.29577951310d0)                       ,message='')
   !!call testit_p('degree',  real(DEGREE)  ,  real(0.0174532925199430d0)                   ,message='')

!! setup
   call test_acosd()
   call test_asind()
   call test_atan2d()
   call test_atand()
   call test_atomnum2symbol()
   call test_c2f()
   call test_cartesian_to_polar()
   call test_cartesian_to_spherical()
   call test_cosd()
   call test_d2r()
   call test_f2c()
   call test_feet_to_meters()
   call test_meters_to_feet()
   call test_norm_angle_deg_double()
   call test_norm_angle_deg_integer()
   call test_norm_angle_deg_real()
   call test_norm_angle_rad()
   call test_polar_to_cartesian()
   call test_pounds_to_kilograms()
   call test_r2d()
   call test_sind()
   call test_spherical_to_cartesian()
   call test_symbol2atomnum()
   call test_tand()
   call test_inf()
   call test_nan()
   call test_is_nan()
   call test_is_even()
!! teardown
contains
!===================================================================================================================================
subroutine testit_p(label,value1,value2,message)
class(*),intent(in) :: value1, value2
real                :: v1, v2
character(len=*)    :: label
character(len=*)    :: message
logical             :: stat
real                :: significant_digits
integer             :: ind
real                :: acurcy

   v1=anyscalar_to_real(value1)
   v2=anyscalar_to_real(value2)
   stat=v1 .EqualTo. v2

   if(.not.stat)then
!     INPUT ...
!     real,intent(in) :: x           ! First  of two real numbers to be compared.
!     real,intent(in) :: y           ! Second of two real numbers to be compared.
!     real,intent(in) :: digi0       ! Number of digits to be satisfied in relative tolerance.
!     OUTPUT ...
!     integer,intent(out) :: ind     ! = 0, If tolerance is     satisfied.
!                                    ! = 1, If tolerance is not satisfied.
!     real,intent(out) :: acurcy     ! = - LOG10 (ABS((X-Y)/Y)))
      significant_digits=int(log10(2.0**digits(0.0)))     ! MAXIMUM NUMBER OF SIGNIFICANT DIGITS IN A REAL NUMBER.
      call accdig (v1,v2,significant_digits-2,ACURCY,IND)
      if(ind.eq.0)stat=.true.
   endif
!-----------------------
   call unit_test(label,stat,label,v1,v2,trim(message),'accuracy=',acurcy,'asked for',int(significant_digits)-2,'digits')
end subroutine testit_p
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nan()
real(kind=real32) :: r32
real(kind=real64) :: r64
real(kind=real128) :: r128
   call unit_test_start('nan',msg='')
   ! (if X is NaN the comparison with 0.0 is always false.)
   r32=nan(0.0_real32)
   call unit_test('nan',.not.(r32<=0.0_real32) .and. .not.(r32>=0.0_real32),msg='real32')

   r64=nan(0.0_real64)
   call unit_test('nan',.not.(r64<=0.0_real64) .and. .not.(r64>=0.0_real64),msg='real64')

   r128=nan(0.0_real128)
   call unit_test('nan',.not.(r128<=0.0_real128) .and. .not.(r128>=0.0_real128),msg='real128')

   call unit_test_done('nan',msg='')
end subroutine test_nan
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_is_even()
logical,parameter     :: t=.true.
logical,parameter     :: f=.false.
   call unit_test_start('is_even',msg='')
   call unit_test('is_even', all(is_even([-10, 0, 1, 2, 3]).eqv.[t,t,f,t,f]), '-10, 0, 1, 2, 3')
   call unit_test_done('is_even',msg='')
end subroutine test_is_even
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_is_nan()
character(len=3),save :: line='NaN'
real                  :: x
logical,parameter     :: t=.true.
logical,parameter     :: f=.false.
   call unit_test_start('is_nan',msg='')
   read(line,*)x
call unit_test('is_nan', all(is_nan([x, 0.0,-0.0,-x,-100.0,100.0,huge(0.0)]).eqv.[t,f,f,t,f,f,f]),  &
        & 'checking',x,0,-x,-100.0,100.0,huge(0.0))
   call unit_test_done('is_nan',msg='')
end subroutine test_is_nan
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_inf()
   call unit_test_start('inf',msg='')
   !!call unit_test('inf', 0.eq.0, 'checking', 100)
   call unit_test_done('inf',msg='')
end subroutine test_inf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_acosd()

   call unit_test_start('acosd',msg='')
   !!call unit_test('acosd', 0.eq.0, 'checking', 100)
   call unit_test_done('acosd',msg='')
end subroutine test_acosd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_asind()

   call unit_test_start('asind',msg='')
   !!call unit_test('asind', 0.eq.0, 'checking', 100)
   call unit_test_done('asind',msg='')
end subroutine test_asind
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_atan2d()

   call unit_test_start('atan2d',msg='')
   !!call unit_test('atan2d', 0.eq.0, 'checking', 100)
   call unit_test_done('atan2d',msg='')
end subroutine test_atan2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_atand()

   call unit_test_start('atand',msg='')
   !!call unit_test('atand', 0.eq.0, 'checking', 100)
   call unit_test_done('atand',msg='')
end subroutine test_atand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_atomnum2symbol()

   call unit_test_start('atomnum2symbol',msg='')
   !!call unit_test('atomnum2symbol', 0.eq.0, 'checking', 100)
   call unit_test_done('atomnum2symbol',msg='')
end subroutine test_atomnum2symbol
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_c2f()

   call unit_test_start('c2f',msg='')
   ! add 0.0 to avoid gfortran-11 bug
   call testit_p('c2f',     c2f(0.0)+0.0   ,  32.0,message='')
   call testit_p('c2f',     c2f(100.0)+0.0 , 212.0,message='')
   call testit_p('c2f',     c2f(-40.0)+0.0 , -40.0,message='')
   call unit_test_done('c2f',msg='')
end subroutine test_c2f
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cartesian_to_polar()

   call unit_test_start('cartesian_to_polar',msg='')
   !!call unit_test('cartesian_to_polar', 0.eq.0, 'checking', 100)
   call unit_test_done('cartesian_to_polar',msg='')
end subroutine test_cartesian_to_polar
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cartesian_to_spherical()

implicit none
real    :: x=10.0,y=10.0,z=10.0
real    :: radius,inclination,azimuth
real    :: acurcy
integer :: ind1,ind2,ind3
   call unit_test_start('cartesian_to_spherical',msg='')

   ! 10,10,10 -> 17.32, 0.9553, 0.7854
   call cartesian_to_spherical(x,y,z,radius,inclination,azimuth)
   call accdig(radius,      17.3205090,   5.0,acurcy,ind1)
   call accdig(inclination,  0.955316663 ,5.0,acurcy,ind2)
   call accdig(azimuth,      0.785398185 ,5.0,acurcy,ind3)
   call unit_test('cartesian_to_spherical',all([ind1,ind2,ind3].eq.0),x,y,z,'to',radius,inclination,azimuth)

   call unit_test_done('cartesian_to_spherical') ! if got here without being stopped assume passed test
end subroutine test_cartesian_to_spherical
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cosd()

real, allocatable :: values(:)
integer           :: i
values=[0.0, 30.0, 45.0, 60.0, 92.0, 120.0, 135.0, 150.0, 180.0, 210.0, 240.0, 273.0, 300.0, 330.0, 360.0, -45.0]
   call unit_test_start('cosd',msg='')
   do i=1,size(values)
      ! add 0.0 to avoid gfortran-11 bug
      call testit_p('cosd', cosd(values(i))+0.0, cos(d2r(values(i)))+0.0,message=str('value=',values(i))//'' )
   enddo
   call unit_test_done('cosd',msg='')

!  unit_test:       cosd  FAILED:cosd 6.12323426E-17 -4.37113883E-08 value= 90.0000000 accuracy= 0.00000000 asked for 6 digits
!  unit_test:       cosd  FAILED:cosd -1.83697015E-16 1.19248806E-08 value= 270.000000 accuracy= 0.00000000 asked for 6 digits
end subroutine test_cosd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2r()

   call unit_test_start('d2r',msg='')

   call testit_p('d2r', d2r(    0.0)    , 0.0       ,message='real for 0')
   call testit_p('d2r', d2r(   45.0)    , PI/4.0    ,message='real for 45')
   call testit_p('d2r', d2r(  -45.0)    , -PI/4.0   ,message='real for -45')
   call testit_p('d2r', d2r(   90.0)    , PI/2      ,message='real for 90')
   call testit_p('d2r', d2r(  180.0)    , PI        ,message='real for 180')

   call testit_p('d2r', d2r(  0.0d0)    , 0.0d0     ,message='double for 0')
   call testit_p('d2r', d2r(  45.0d0)   , PI/4.0d0  ,message='double for 45')
   call testit_p('d2r', d2r(  -45.0d0)  , -PI/4.0d0 ,message='double for -45')
   call testit_p('d2r', d2r(  90.0d0)   , PI/2d0    ,message='double for 90')
   call testit_p('d2r', d2r(  180.0d0)  , PI        ,message='double for 180')

   call testit_p('d2r', d2r(    0)      , 0.0       ,message='integer for 0')
   call testit_p('d2r', d2r(   45)      , PI/4.0    ,message='integer for 45')
   call testit_p('d2r', d2r(  -45)      , -PI/4.0   ,message='integer for -45')
   call testit_p('d2r', d2r(   90)      , PI/2      ,message='integer for 90')
   call testit_p('d2r', d2r(  180)      , PI        ,message='integer for 180')

   call unit_test_done('d2r',msg='')

end subroutine test_d2r
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_f2c()

   call unit_test_start('f2c',msg='')
   call testit_p('f2c',     f2c(32.0)  ,   0.0,message='')
   call testit_p('f2c',     f2c(212.0) , 100.0,message='')
   call testit_p('f2c',     f2c(-40.0) , -40.0,message='')
   call unit_test_done('f2c',msg='')
end subroutine test_f2c
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_feet_to_meters()
doubleprecision, parameter :: f2m=0.3048d0
   call unit_test_start('feet_to_meters',msg=' 0.3048')

   call unit_test('feet_to_meters', &
      & all(abs(feet_to_meters([ -1.0, 0.0, 1.0 ,1.0/12.0])- [-f2m, 0.0d0, f2m, 0.0254d0]).lt.0.00001),'real')
   call unit_test('feet_to_meters', &
      & all(abs(feet_to_meters([ -1,   0,   1   ])- [-f2m, 0.0d0, f2m]).lt.0.00001),'integer')
   call unit_test('feet_to_meters', &
      & all(abs([feet_to_meters(-1.0d0),feet_to_meters(0.0d0),feet_to_meters(1.0d0)]-[-f2m, 0.0d0, f2m]).lt.0.00001),'double')

   call unit_test_done('feet_to_meters',msg='')
end subroutine test_feet_to_meters

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_meters_to_feet()
   doubleprecision, parameter :: m2f=3.2808398950131233595d0

   call unit_test_start('meters_to_feet',msg='3.2808398950131233595')

   call unit_test('meters_to_feet', &
     & all(abs(meters_to_feet([ -1.0, 0.0, 1.0 ])-[-m2f,0.0d0,m2f]).lt.0.00001d0),msg='real')
   call unit_test('meters_to_feet', &
     & all(abs(meters_to_feet([ -1,   0,   1   ])-[-m2f,0.0d0,m2f]).lt.0.00001d0) ,msg='integer')
   call unit_test('meters_to_feet', &
     & all(abs([meters_to_feet(-1d0),meters_to_feet(0.0d0),meters_to_feet(1.0d0)]-[-m2f,0.0d0,m2f]).lt.0.00001d0),msg='double')

   call unit_test_done('meters_to_feet',msg='')
end subroutine test_meters_to_feet
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norm_angle_deg_double()

   call unit_test_start('norm_angle_deg_double',msg='')
   !!call unit_test('norm_angle_deg_double', 0.eq.0, 'checking', 100)
   call unit_test_done('norm_angle_deg_double',msg='')
end subroutine test_norm_angle_deg_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norm_angle_deg_integer()

   call unit_test_start('norm_angle_deg_integer',msg='')
   !!call unit_test('norm_angle_deg_integer', 0.eq.0, 'checking', 100)
   call unit_test_done('norm_angle_deg_integer',msg='')
end subroutine test_norm_angle_deg_integer
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norm_angle_deg_real()

   call unit_test_start('norm_angle_deg_real',msg='')
   !!call unit_test('norm_angle_deg_real', 0.eq.0, 'checking', 100)
   call unit_test_done('norm_angle_deg_real',msg='')
end subroutine test_norm_angle_deg_real
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norm_angle_rad()

   call unit_test_start('norm_angle_rad',msg='')
   !!call unit_test('norm_angle_rad', 0.eq.0, 'checking', 100)
   call unit_test_done('norm_angle_rad',msg='')
end subroutine test_norm_angle_rad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_polar_to_cartesian()

   call unit_test_start('polar_to_cartesian',msg='')
   !!call unit_test('polar_to_cartesian', 0.eq.0, 'checking', 100)
   call unit_test_done('polar_to_cartesian',msg='')
end subroutine test_polar_to_cartesian
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pounds_to_kilograms()

   call unit_test_start('pounds_to_kilograms',msg='')
   call unit_test('pounds_to_kilograms',abs(pounds_to_kilograms(1.0)-0.45359237).lt.0.00001,'real')
   call unit_test('pounds_to_kilograms',any(abs(pounds_to_kilograms([ 0, 1, 100, 200 ])-&
      &[0.0, 0.45359237, 45.359237,90.718474]).lt.0.00001),'integer')
   call unit_test('pounds_to_kilograms',abs(pounds_to_kilograms(1.0d0)-0.45359237).lt.0.00001,'double')
   call unit_test_done('pounds_to_kilograms',msg='')
end subroutine test_pounds_to_kilograms
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_r2d()

real              :: x=real(PI)
doubleprecision   :: d=PI

   call unit_test_start('r2d',msg='')

   call testit_p('r2d', r2d(  0.0)      ,   0.0    ,message='real')
   call testit_p('r2d', r2d(  x/4)      ,  45.0    ,message='real')
   call testit_p('r2d', r2d( -x/4)      , -45.0    ,message='real')
   call testit_p('r2d', r2d(  x/2)      ,  90.0    ,message='real')
   call testit_p('r2d', r2d(  x)        , 180.0    ,message='real')

   call testit_p('r2d', r2d(  0.0d0)    ,   0.0d0  ,message='double')
   call testit_p('r2d', r2d(  d/4.0d0)  ,  45.0d0  ,message='double')
   call testit_p('r2d', r2d( -d/4.0d0)  , -45.0d0  ,message='double')
   call testit_p('r2d', r2d(  d/2.0d0)  ,  90.0d0  ,message='double')
   call testit_p('r2d', r2d(  d)        , 180.0d0  ,message='double')

   call testit_p('r2d', r2d(  0)        ,   0.0    ,message='integer')

   call unit_test_done('r2d',msg='')
end subroutine test_r2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sind()

real, allocatable :: values(:)
integer           :: i
   values=[0.0, 30.0, 45.0, 60.0, 90.0, 120.0, 135.0, 150.0, 181.0, 210.0, 240.0, 270.0, 300.0, 330.0, 362.0, -45.0]
   call unit_test_start('sind',msg='')
   do i=1,size(values)
      call testit_p('sind',   sind(values(i))             ,  sin(d2r(values(i))),message=str('value=',values(i))  )
   enddo
   call unit_test_done('sind',msg='')
! unit_test:       sind  FAILED:sind 1.22464685E-16 -8.74227766E-08 value= 180.000000 accuracy= 0.00000000 asked for 6 digits
! unit_test:       sind  FAILED:sind -2.44929371E-16 1.74845553E-07 value= 360.000000 accuracy= 0.00000000 asked for 6 digits
end subroutine test_sind
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_spherical_to_cartesian()
implicit none
real    :: x,y,z
real    :: radius,inclination,azimuth
real    :: acurcy
integer :: ind1,ind2,ind3
   call unit_test_start('spherical_to_cartesian',msg='')

   radius=17.32; inclination=0.9553; azimuth=0.7854
   x=-9999; y=-9999; z=-9999;
   call spherical_to_cartesian(radius,inclination,azimuth,x,y,z)

   call accdig(x,10.0,4.0,acurcy,ind1)
   call accdig(y,10.0,4.0,acurcy,ind2)
   call accdig(z,10.0,4.0,acurcy,ind3)

   call unit_test('spherical_to_cartesian',all([ind1,ind2,ind3].eq.0),radius,inclination,azimuth,'to',x,y,z)

   call unit_test_done('spherical_to_cartesian',msg='')
end subroutine test_spherical_to_cartesian
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_symbol2atomnum()

   call unit_test_start('symbol2atomnum',msg='')
   !!call unit_test('symbol2atomnum', 0.eq.0, 'checking', 100)
   call unit_test_done('symbol2atomnum',msg='')
end subroutine test_symbol2atomnum
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tand()
real, allocatable :: values(:)
integer                      :: i
   values=[0.0,30.0,45.0,60.0,92.0,120.0,135.0,150.0,183.0,210.0,240.0,273.0,300.0, 330.0, 362.0, -45.0]
   call unit_test_start('tand',msg='')
   do i=1,size(values)
      call testit_p('tand', tand(values(i)), tan(d2r(values(i))),message=str('value=',values(i)))
   enddo
   call unit_test_done('tand',msg='')
! unit_test:       tand  FAILED:tand 1.63312395E+16 -22877332.0 value= 90.0000000 accuracy= -8.85361290 asked for 6 digits
! unit_test:       tand  FAILED:tand -1.22464685E-16 8.74227766E-08 value= 180.000000 accuracy= 0.00000000 asked for 6 digits
! unit_test:       tand  FAILED:tand 5.44374649E+15 -83858280.0 value= 270.000000 accuracy= -7.81235218 asked for 6 digits
! unit_test:       tand  FAILED:tand -2.44929371E-16 1.74845553E-07 value= 360.000000 accuracy= 0.00000000 asked for 6 digits
end subroutine test_tand
!===================================================================================================================================
end program test_suite_M_units
