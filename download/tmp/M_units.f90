!>
!!##NAME
!!    M_units(3fm) - [M_units]convert between various physical units
!!
!!##SYNOPSIS
!!
!!
!!    procedure definitions:
!!
!!       elemental real function c2f(celsius)
!!       elemental real function f2c(fahrenheit)
!!
!!       elemental real function r2d(radians)
!!       elemental real function d2r(degrees)
!!
!!       elemental real function feet_to_meters(feet)
!!       elemental real function meters_to_feet(meters)
!!
!!       elemental real function sind(angle_in_degrees)
!!       elemental real function cosd(angle_in_degrees)
!!       elemental real function tand(angle_in_degrees)
!!       elemental real function asind(x)
!!       elemental real function acosd(x)
!!       elemental real function atand(x)
!!       elemental real function atan2d(x,y)
!!
!!       subroutine cartesian_to_spherical(x,y,z,radius,inclination,azimuth)
!!       subroutine spherical_to_cartesian(radius,inclination,azimuth,x,y,z)
!!       subroutine cartesian_to_polar(x,y,radius,inclination)
!!       subroutine polar_to_cartesian(radius,inclination,x,y)
!!
!!    Where the input values may be any standard scalar value supported by
!!    the anyscalar_to_real(3f) function (real,integer,doubleprecision) within
!!    the range allowed by the function.
!!
!!##DESCRIPTION
!!
!!    M_units(3fm) is a Fortran module that collects together basic
!!    procedures that are used to convert between various physical units
!!    and common named constants.
!!
!!       +---------------------------+
!!       |Angle|Area        |Base    |
!!       |-----+------------+--------|
!!       |Data |Energy      |Length  |
!!       |-----+------------+--------|
!!       |Mass |Power       |Pressure|
!!       |-----+------------+--------|
!!       |Speed|Temperature |Time    |
!!       +---------------------------+
!!
!!    Angular Units
!!
!!     o d2r:  Convert degrees to radians
!!     o r2d:  Convert radians to degrees
!!
!!    Temperature
!!
!!     o c2f:  Convert Celsius to Fahrenheit
!!     o f2c:  Convert Fahrenheit to Celsius
!!
!!    Distance
!!
!!     o feet_to_meters:  Convert feet to meters
!!     o meters_to_feet:  Convert meters to feet
!!
!!    REAL trig functions where input is in angular degrees instead of radians
!!
!!     o elemental real function sind(angle_in_degrees)
!!     o elemental real function cosd(angle_in_degrees)
!!     o elemental real function tand(angle_in_degrees)
!!     o elemental real function asind(x)
!!     o elemental real function acosd(x)
!!     o elemental real function atand(x)
!!     o elemental real function atan2d(x,y)
!!
!!    Coordinates
!!
!!     o cartesian_to_spherical:  Convert cartesian coordinates to spherical
!!     o spherical:  Convert spherical coordinates to cartesian
!!     o cartesian_to_polar:  Convert cartesian coordinates to polar
!!     o polar:  Convert polar coordinates to cartesian
!!
!!   And the real constants PI, RADIAN, DEGREE, E.
!!   And the double_precision constants PI_D, RADIAN_D, DEGREE_D, E_D.
!!
!!   Note that your compiler is less likely to inline small procedures in a
!!   module than it would statement functions or CONTAINED functions.
!!
!!##EXAMPLES
!!
!!
!!   Simple usage example:
!!
!!    program testit
!!    use M_units, only : r2d, d2r
!!    use M_units, only : f2c, c2f
!!    use M_units, only : sind, cosd, tand
!!    use M_units, only : asind, acosd, atand, atan2d
!!    use M_units, only : pi=>pi,radian,degree,e
!!    implicit none
!!    write(*,*)r2d([0.0,PI/4.0,PI/2.0,3.0*PI/2.0,PI])
!!    write(*,*)d2r([0.0,45.0,90.0,135.0,180.0])
!!    write(*,*)f2c([-40.0,32.0,212.0])
!!    write(*,*)c2f([-40.0,0.0,100.0])
!!    write(*,*)PI
!!    write(*,*)E
!!    end program testit
!!
!!   Results:
!!
!!    > 0.00000000   45.0000000   90.0000000   270.000000   180.000000
!!    > 0.00000000  0.785398185   1.57079637   2.35619450   3.14159274
!!    >-40.0000000   0.00000000   100.000000
!!    >-40.0000000   32.0000000   212.000000
!!    > 3.14159274
!!    > 2.71828175
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module m_units
use M_anyscalar,only : anyscalar_to_real
   implicit none                        ! require all variables to be declared
   private
!  common trigonometric functions using degrees instead of radians for units
      public sind
      public cosd
      public tand
      public asind
      public acosd
      public atand
      public atan2d
!  convert between degrees and radians
      public d2r
      public r2d
!  distance
      public feet_to_meters
      public meters_to_feet
!  convert between Celsius and Fahrenheit
      public f2c
      public c2f
!  convert between coordinate systems
      public cartesian_to_spherical
      public spherical_to_cartesian
      public cartesian_to_polar
      public polar_to_cartesian
!  constants
      doubleprecision,parameter,public :: pi_d       =  3.141592653589793238462643383279500d0
      doubleprecision,parameter,public :: e_d        =  2.718281828459045235360d0
      doubleprecision,parameter,public :: radian_d   = 57.29577951310d0                       !degrees
      doubleprecision,parameter,public :: degree_d   =  0.0174532925199430d0                  !radians

      real,parameter,public :: pi         =  3.141592653589793238462643383279500d0
      real,parameter,public :: e          =  2.718281828459045235360d0
      real,parameter,public :: radian     = 57.29577951310d0                       !degrees
      real,parameter,public :: degree     =  0.0174532925199430d0                  !radians

      doubleprecision,parameter        :: degrees_to_radians_D = PI_D / 180.0D+00
      real,parameter                   :: degrees_to_radians = real(PI / 180.0D+00)
contains
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!    c2f - [M_units]convert Celsius to Fahrenheit
!!##SYNOPSIS
!!
!!    elemental real function c2f(celsius)
!!
!!     class(*),intent(in) :: celsius
!!##DESCRIPTION
!!    Converts Celsius temperatures to Fahrenheit using the formula:
!!
!!     fahrenheit=(celsius+40.0)*9.0/5.0 - 40.0
!!##OPTIONS
!!    celsius    any standard scalar value supported by anyscalar_to_real(3f).
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program c2f_demo
!!    use M_units, only : c2f
!!    implicit none
!!       write(*,*)'With REAL array input    ', c2f([ -40.0, 0.0, 100.0 ])
!!       write(*,*)'With INTEGER array input ', c2f([ -40,   0,   100   ])
!!       write(*,*)'With DOUBLEPRECISION     ', c2f(-40.0d0),c2f(0.0d0),c2f(100.0d0)
!!    end program c2f_demo
!!
!!   Results
!!
!!    With REAL array input      -40.0000000       32.0000000       212.000000
!!    With INTEGER array input   -40.0000000       32.0000000       212.000000
!!    With DOUBLEPRECISION       -40.0000000       32.0000000       212.000000
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function c2f(celsius)
character(len=*),parameter::ident="@(#)M_units::c2f(3f): Convert celsius to fahrenheit"
class(*),intent(in)           :: celsius        ! celsius value to convert to fahrenheit
   real                       :: celsius_local
   celsius_local=anyscalar_to_real(celsius)
   c2f=(celsius_local+40.0)*9.0/5.0 - 40.0         ! do the conversion
end function c2f
!***********************************************************************************************************************************
!>
!!##NAME
!!    f2c - [M_units]convert Fahrenheit to Celsius
!!##SYNOPSIS
!!
!!    elemental real function f2c(fahrenheit)
!!
!!     class(*),intent(in) :: fahrenheit
!!##DESCRIPTION
!!    Converts Fahrenheit temperatures to Celsius using the formula:
!!
!!     celsius=(fahrenheit+40.0)*5.0/9.0 - 40.0
!!##OPTIONS
!!    fahrenheit    any standard scalar value supported by anyscalar_to_real(3f).
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program f2c_demo
!!    use M_units, only :  f2c
!!    implicit none
!!       write(*,*)'With REAL array input    ', f2c([ -40.0,32.0, 212.0 ])
!!       write(*,*)'With INTEGER array input ', f2c([ -40,  32,   212   ])
!!       write(*,*)'With DOUBLEPRECISION     ', f2c(-40.0d0),f2c(32.0d0),f2c(212.0d0)
!!    end program f2c_demo
!!
!!   Results
!!
!!    With REAL array input      -40.0000000       0.00000000       100.000000
!!    With INTEGER array input   -40.0000000       0.00000000       100.000000
!!    With DOUBLEPRECISION       -40.0000000       0.00000000       100.000000
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function f2c(fahrenheit)
character(len=*),parameter::ident="@(#)M_units::f2c(3f): Convert fahrenheit to celsius"
class(*),intent(in)           :: fahrenheit     ! input fahrenheit to convert to celsius
   real                       :: fahrenheit_local
   fahrenheit_local=anyscalar_to_real(fahrenheit)
   f2c=(fahrenheit_local+40.0)*5.0/9.0 - 40.0      ! do the conversion
end function f2c
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!    r2d - [M_units]convert radians to degrees
!!##SYNOPSIS
!!
!!    elemental real function r2d(radians)
!!
!!     class(*),intent(in) :: degrees
!!##DESCRIPTION
!!    Converts radians to degrees using the formula:
!!
!!     degrees=real(radians * 180.d0 / acos(-1.0d0))
!!##OPTIONS
!!    radians    any standard scalar value supported by anyscalar_to_real(3f).
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program r2d_demo
!!    use M_units, only :  r2d
!!    use M_units, only : pi=>pi
!!    implicit none
!!       write(*,*)'With REAL array input    ', r2d([ 0.0, PI/4.0, PI/2.0, 3.0*PI/2.0, PI ])
!!       write(*,*)'With INTEGER array input ', r2d([0,1,2,3,4])
!!       write(*,*)'With DOUBLEPRECISION     ', r2d(0.0d0),r2d(PI/4.0d0),r2d(PI/2.0d0),r2d(3.0d0*PI/2.0d0),r2d(PI)
!!    end program r2d_demo
!!
!!   Results
!!
!!     With REAL array input       0.00000000       45.0000000       90.0000000       270.000000       180.000000
!!     With INTEGER array input    0.00000000       57.2957802       114.591560       171.887344       229.183121
!!     With DOUBLEPRECISION        0.00000000       45.0000000       90.0000000       270.000000       180.000000
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function r2d(radians)
character(len=*),parameter::ident="@(#)M_units::r2d(3f): Convert radians to degrees"
class(*),intent(in)           :: radians        ! input radians to convert to degrees
   real                       :: radians_local
   radians_local=anyscalar_to_real(radians)
   r2d=real(radians_local * 180.d0 / acos(-1.0d0)) ! do the conversion
end function r2d
!***********************************************************************************************************************************
!>
!!##NAME
!!    d2r - [M_units]convert degrees to radians
!!##SYNOPSIS
!!
!!    elemental real function d2r(degrees)
!!
!!     class(*),intent(in) :: radians
!!##DESCRIPTION
!!    Converts degrees to radians using the formula:
!!
!!     radians=real(degrees*acos(-1.0d0)/180.d0)
!!##OPTIONS
!!    degrees    any standard scalar value supported by anyscalar_to_real(3f).
!!               This includes REAL, INTEGER, DOUBLEPRECISION, ... .
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program d2r_demo
!!    use M_units, only :  d2r
!!    implicit none
!!       write(*,*)'With REAL array input    ', d2r([0.0,45.0,90.0,135.0,180.0])
!!       write(*,*)'With INTEGER array input ', d2r([0,  45,  90,  135,  180  ])
!!       write(*,*)'With DOUBLEPRECISION     ', &
!!       & d2r(0.0d0),d2r(45.0d0),d2r(90.0d0),d2r(135.0d0),d2r(180.0d0)
!!    end program d2r_demo
!!
!!   Results
!!
!!    With REAL array input    0.00000 0.785398185 1.57079637 2.35619450 3.14159274
!!    With INTEGER array input 0.00000 0.785398185 1.57079637 2.35619450 3.14159274
!!    With DOUBLEPRECISION     0.00000 0.785398185 1.57079637 2.35619450 3.14159274
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function d2r(degrees)
character(len=*),parameter::ident="@(#)M_units::d2r(3f): Convert degrees to radians"
class(*),intent(in)           :: degrees        ! input degrees to convert to radians
   real                       :: degrees_local
   degrees_local=anyscalar_to_real(degrees)
   d2r=real(degrees_local*acos(-1.0d0)/180.d0)     ! do the unit conversion
end function d2r
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!    sind - [M_units]calculate sine of value in degrees
!!##SYNOPSIS
!!
!!    elemental real function sind(degrees)
!!
!!     class(*),intent(in) :: degrees
!!##DESCRIPTION
!!    Calculate sine of input value in degrees
!!
!!##OPTIONS
!!    degrees    any standard scalar value supported by anyscalar_to_real(3f).
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program sind_demo
!!    use M_units, only :  sind
!!    implicit none
!!       write(*,*)'With REAL array input    ', sind([ 0.0, 15.0, 30.0, 45.0, 60.0, 75.0, 90.0, 180.0, 270.0 ])
!!       write(*,*)'With INTEGER array input ', sind([0,15,30,45,60,75,90,180,270])
!!       write(*,*)'With DOUBLEPRECISION     ', sind(0.0d0),sind(15.0d0),sind(90.0/3.0d0),sind(90.0/2.0d0),sind(60.0d0),sind(75.0d0),&
!!       & sind(90.0d0),sind(180.0d0),sind(270.0d0)
!!    end program sind_demo
!!
!!   Results
!!
!!    With REAL array input      0.00000000      0.258819044      0.500000000
!!                               0.707106829     0.866025448      0.965925813
!!                               1.00000000     -8.74227766E-08  -1.00000000
!!    With INTEGER array input   0.00000000      0.258819044      0.500000000
!!                               0.707106829     0.866025448      0.965925813
!!                               1.00000000      -8.74227766E-08  -1.00000000
!!    With DOUBLEPRECISION       0.00000000      0.258819044      0.500000000
!!                               0.707106829     0.866025448      0.965925813
!!                               1.00000000      -8.74227766E-08  -1.00000000
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function sind(angle_in_degrees)
character(len=*),parameter::ident="@(#)M_units::sind(3f): sin(3f) with degrees as input instead of radians"
class(*),intent(in)           :: angle_in_degrees
   real                       :: angle_in_degrees_local
   angle_in_degrees_local=anyscalar_to_real(angle_in_degrees)
   sind=sin(angle_in_degrees_local*degrees_to_radians)
end function sind
!***********************************************************************************************************************************
!>
!!##NAME
!!    cosd - [M_units]calculate sine of value in degrees
!!##SYNOPSIS
!!
!!    elemental real function cosd(degrees)
!!
!!     class(*),intent(in) :: degrees
!!##DESCRIPTION
!!    Calculate cosine of input value in degrees
!!
!!##OPTIONS
!!    degrees    any standard scalar value supported by anyscalar_to_real(3f).
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program cosd_demo
!!    use M_units, only :  cosd
!!    implicit none
!!       write(*,*)'With REAL array input    ', &
!!       & cosd([ 0.0, 15.0, 30.0, 45.0, 60.0, 75.0, 90.0, 180.0, 270.0 ])
!!       write(*,*)'With INTEGER array input ',  &
!!       & cosd([0,15,30,45,60,75,90,180,270])
!!       write(*,*)'With DOUBLEPRECISION     ',        &
!!       & cosd(0.0d0),cosd(15.0d0),cosd(90.0/3.0d0),  &
!!       & cosd(90.0/2.0d0),cosd(60.0d0),cosd(75.0d0), &
!!       & cosd(90.0d0),cosd(180.0d0),cosd(270.0d0)
!!    end program cosd_demo
!!
!!   Results
!!
!!     With REAL array input      1.00000000       0.965925813   0.866025448
!!                                0.707106769      0.499999970   0.258819073
!!                               -4.37113883E-08  -1.00000000    1.19248806E-08
!!     With INTEGER array input   1.00000000       0.965925813   0.866025448
!!                                0.707106769      0.499999970   0.258819073
!!                               -4.37113883E-08  -1.00000000    1.19248806E-08
!!     With DOUBLEPRECISION       1.00000000       0.965925813   0.866025448
!!                                0.707106769      0.499999970   0.258819073
!!                               -4.37113883E-08  -1.00000000    1.19248806E-08
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function cosd(angle_in_degrees)
character(len=*),parameter::ident="@(#)M_units::cosd(3f): cos(3f) with degrees as input instead of radians"
class(*),intent(in)           :: angle_in_degrees
   real                       :: angle_in_degrees_local
   angle_in_degrees_local=anyscalar_to_real(angle_in_degrees)
   cosd=cos(angle_in_degrees_local*degrees_to_radians)
end function cosd
!***********************************************************************************************************************************
!>
!!##NAME
!!    tand - [M_units]calculate tangent of value in degrees
!!##SYNOPSIS
!!
!!    elemental real function tand(degrees)
!!
!!     class(*),intent(in) :: degrees
!!##DESCRIPTION
!!    Calculate tangent of input value in degrees
!!
!!##OPTIONS
!!    degrees    any standard scalar value supported by anyscalar_to_real(3f).
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program tand_demo
!!    use M_units, only :  tand
!!    implicit none
!!       write(*,*)'With REAL array input    ', tand([ 0.0, 15.0, 30.0, 45.0, 60.0, 75.0, 90.0, 180.0, 270.0 ])
!!       write(*,*)'With INTEGER array input ', tand([0,15,30,45,60,75,90,180,270])
!!       write(*,*)'With DOUBLEPRECISION     ', tand(0.0d0),tand(15.0d0),tand(90.0/3.0d0),tand(90.0/2.0d0),tand(60.0d0),tand(75.0d0),&
!!       & tand(90.0d0),tand(180.0d0),tand(270.0d0)
!!    end program tand_demo
!!
!!   Results
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function tand(angle_in_degrees)
character(len=*),parameter::ident="@(#)M_units::tand(3f): tan(3f) with degrees as input instead of radians"
class(*),intent(in)           :: angle_in_degrees
   real                       :: angle_in_degrees_local
   angle_in_degrees_local=anyscalar_to_real(angle_in_degrees)
   tand=tan(angle_in_degrees_local*degrees_to_radians)
end function tand
!***********************************************************************************************************************************
elemental real function asind(x)
character(len=*),parameter::ident="@(#)M_units::asind(3f): asin(3f) with degrees as output instead of radians"
class(*),intent(in)           :: x
   real                       :: x_local
   x_local=anyscalar_to_real(x)
   asind=asin(x_local)/degrees_to_radians
end function asind
!***********************************************************************************************************************************
elemental real function acosd(x)
character(len=*),parameter::ident="@(#)M_units::acosd(3f): calculate arc-cos of angle in degrees"
class(*),intent(in)           :: x
   real                       :: x_local
   x_local=anyscalar_to_real(x)
   acosd=acos(x_local)/degrees_to_radians
end function acosd
!***********************************************************************************************************************************
elemental real function atand(x)
character(len=*),parameter::ident="@(#)M_units::atand(3f): result is arc-tangent of angle in degrees"
class(*),intent(in)           :: x
   real                       :: x_local
   x_local=anyscalar_to_real(x)
   atand=atan(x_local)/degrees_to_radians
end function atand
!***********************************************************************************************************************************
elemental real function atan2d(x,y)
character(len=*),parameter::ident="@(#)M_units::atan2d(3f): calculate arc-tangent of angle in degrees"
class(*),intent(in)           :: x
class(*),intent(in)           :: y
   real                       :: x_local
   real                       :: y_local
   x_local=anyscalar_to_real(x)
   y_local=anyscalar_to_real(y)
   atan2d=atan2(x_local,y_local)/degrees_to_radians
end function atan2d
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!    feet_to_meters - [M_units]converts a measurement in feet to meters
!!##SYNOPSIS
!!
!!    elemental real function feet_to_meters(feet)
!!
!!     class(*),intent(in) :: feet
!!##DESCRIPTION
!!    Converts feet to meters using the formula:
!!
!!     meters = 0.0254 * 12.0 * feet
!!##OPTIONS
!!    feet   any standard scalar value supported by anyscalar_to_real(3f).
!!           This at least includes REAL, INTEGER, and DOUBLEPRECISION.
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program feet_to_meters_demo
!!    use M_units, only : feet_to_meters
!!    implicit none
!!       write(*,*)'With REAL array input    ', feet_to_meters([ -1.0, 0.0, 1.0 ,1.0/12.0])
!!       write(*,*)'With INTEGER array input ', feet_to_meters([ -1,   0,   1   ])
!!       write(*,*)'With DOUBLEPRECISION     ', feet_to_meters(-1.0d0),feet_to_meters(0.0d0),feet_to_meters(1.0d0)
!!    end program feet_to_meters_demo
!!
!!   Results
!!
!!     With REAL array input     -0.304800004   0.00000000  0.304800004  2.54000016E-02
!!     With INTEGER array input  -0.304800004   0.00000000  0.304800004
!!     With DOUBLEPRECISION      -0.304800004   0.00000000  0.304800004
!===================================================================================================================================
elemental function feet_to_meters(feet)
character(len=*),parameter::ident="@(#)M_units::feet_to_meters(3f): converts a measurement in feet to meters"
class(*),intent(in)           :: feet                           ! the input length in feet.
real                          :: feet_to_meters                 ! OUTPUT, the corresponding length in meters.
   real                       :: feet_local
   feet_local=anyscalar_to_real(feet)
   feet_to_meters = 0.0254 * 12.0 * feet_local
end function feet_to_meters
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!    meters_to_feet - [M_units]converts a measurement in meters to feet
!!##SYNOPSIS
!!
!!    elemental real function meters_to_feet(meters)
!!
!!     class(*),intent(in) :: meters
!!##DESCRIPTION
!!    Converts feet to meters using the formula:
!!
!!     feet= meters/12.0/0.0254
!!##OPTIONS
!!    feet   any standard scalar value supported by anyscalar_to_real(3f).
!!           This at least includes REAL, INTEGER, and DOUBLEPRECISION.
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program meters_to_feet_demo
!!    use M_units, only : meters_to_feet
!!    implicit none
!!       write(*,*)'With REAL array input    ', meters_to_feet([ -1.0, 0.0, 1.0 ])
!!       write(*,*)'With INTEGER array input ', meters_to_feet([ -1,   0,   1   ])
!!       write(*,*)'With DOUBLEPRECISION     ', meters_to_feet(-1.0d0),meters_to_feet(0.0d0),meters_to_feet(1.0d0)
!!    end program meters_to_feet_demo
!!
!!   Results
!!
!!     With REAL array input      -3.28083992       0.00000000       3.28083992
!!     With INTEGER array input   -3.28083992       0.00000000       3.28083992
!!     With DOUBLEPRECISION       -3.28083992       0.00000000       3.28083992
!===================================================================================================================================
elemental function meters_to_feet(meters)
character(len=*),parameter::ident="@(#)M_units::meters_to_feet(3f): converts a measurement in meters to feet"
class(*),intent(in)           :: meters                         ! the input length in meters.
real                          :: meters_to_feet                 ! OUTPUT, the corresponding length in feet.
   real                       :: meters_local
   meters_local=anyscalar_to_real(meters)
   meters_to_feet = meters_local/12.0/0.0254
end function meters_to_feet
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!     cartesian_to_spherical(3f) - [M_units] convert Cartesian coordinates to ISO polar coordinates
!!##SYNOPSIS
!!
!!    subroutine cartesian_to_spherical(x,y,z,radius,inclination,azimuth)
!!
!!     real,intent(in)  :: x,y,z
!!     real,intent(out) :: radius,inclination,azimuth
!!
!!##DESCRIPTION
!!
!!     Convert a cartesian point <X,Y,Z> to ISO 8000-2:2009 polar coordinates <radius,
!!     inclination, azimuth> with angles in radians using the formulas
!!
!!       radius=sqrt(x**2+y**2+z**2)
!!       inclination=acos(z/radius)
!!       azimuth=atan2(y,x)
!!
!!##OPTIONS
!!    X  The distance along the x-axis
!!    Y  The distance along the y-axis
!!    Z  The distance along the z-axis
!!
!!##RESULTS
!!
!!    RADIUS       The radial distance from the origin (O) to the point (P)
!!    INCLINATION  The zenith angle in radians between the zenith reference direction
!!                 (z-axis) and the line OP
!!    AZIMUTH      The azimuth angle in radians between the azimuth reference direction
!!                 (x-axis) and the orthogonal projection of the line OP of the
!!                 reference plane (x-y plane).
!!
!!##EXAMPLES
!!
!!   examples of usage
!!
!!    program demo_cartesian_to_spherical
!!    use M_units, only : cartesian_to_spherical
!!    implicit none
!!    real    :: x,y,z
!!    real    :: r,i,a
!!    integer :: ios
!!    INFINITE: do
!!       read(*,*,iostat=ios) x, y, z
!!       if(ios.ne.0)exit INFINITE
!!       call cartesian_to_spherical(x,y,z,r,i,a)
!!       write(*,*)'x=',x,' y=',y,' z=',z,'radius=',r,'inclination=',i,'azimuth=',a
!!    enddo INFINITE
!!    end program demo_cartesian_to_spherical
!===================================================================================================================================
subroutine cartesian_to_spherical(x,y,z,radius,inclination,azimuth)
implicit none
character(len=*),parameter::ident="@(#)M_units::cartesian_to_spherical(3f): convert Cartesian coordinates to ISO polar coordinates"
real,intent(in)  :: x,y,z
real,intent(out) :: radius,inclination,azimuth
   radius=sqrt(x**2+y**2+z**2)
   if(radius.eq.0)then
      inclination=0.0
      azimuth=0.0
   else
      inclination=acos(z/radius)
      azimuth=atan2(y,x)
   endif
end subroutine cartesian_to_spherical
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!     spherical_to_cartesian(3f) - [M_units] convert ISO polar coordinates to Cartesian coordinates
!!##SYNOPSIS
!!
!!    subroutine spherical_to_cartesian(radius,inclination,azimuth,x,y,z)
!!
!!     real,intent(in) :: radius,inclination,azimuth
!!     real,intent(out)  :: x,y,z
!!
!!##DESCRIPTION
!!
!!     Convert ISO 8000-2:2009 polar coordinate <radius, inclination, azimuth> with
!!     angles in radians to cartesian point <X,Y,Z> using the formulas
!!
!!       x=radius*sin(inclination)*cos(azimuth)
!!       y=radius*sin(inclination)*sin(azimuth)
!!       z=radius*cos(inclination)
!!
!!##OPTIONS
!!
!!    RADIUS       The radial distance from the origin (O) to the point (P)
!!    INCLINATION  The zenith angle in radians between the zenith reference direction
!!                 (z-axis) and the line OP
!!    AZIMUTH      The azimuth angle in radians between the azimuth reference direction
!!                 (x-axis) and the orthogonal projection of the line OP of the
!!                 reference plane (x-y plane).
!!
!!##RESULTS
!!
!!    X  The distance along the x-axis
!!    Y  The distance along the y-axis
!!    Z  The distance along the z-axis
!!
!!##EXAMPLES
!!
!!   examples of usage
!!
!!    program demo_spherical_to_cartesian
!!    use M_units, only : spherical_to_cartesian
!!    implicit none
!!    real    :: x,y,z
!!    real    :: r,i,a
!!    integer :: ios
!!    INFINITE: do
!!       read(*,*,iostat=ios) x, y, z
!!       if(ios.ne.0)exit INFINITE
!!       call spherical_to_cartesian(r,i,a,x,y,z)
!!       write(*,*)'x=',x,' y=',y,' z=',z,'radius=',r,'inclination=',i,'azimuth=',a
!!    enddo INFINITE
!!    end program demo_spherical_to_cartesian
!===================================================================================================================================
subroutine spherical_to_cartesian(radius,inclination,azimuth,x,y,z)
implicit none
character(len=*),parameter::ident="@(#)M_units::spherical_to_cartesian(3f): convert spherical coordinates to cartesian coordinates"
real,intent(in) :: radius,inclination,azimuth
real,intent(out)  :: x,y,z
   if(radius.eq.0)then
      x=0.0
      y=0.0
      z=0.0
   else
      x=radius*sin(inclination)*cos(azimuth)
      y=radius*sin(inclination)*sin(azimuth)
      z=radius*cos(inclination)
   endif
end subroutine spherical_to_cartesian
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!     cartesian_to_polar(3f) - [M_units] convert Cartesian coordinates to polar coordinates
!!##SYNOPSIS
!!
!!    subroutine cartesian_to_polar(x,y,radius,inclination)
!!
!!     real,intent(in)  :: y,z
!!     real,intent(out) :: radius,inclination
!!
!!##DESCRIPTION
!!
!!     Convert a cartesian point <X,Y,Z> to polar coordinates <radius,
!!     inclination> with angles in radians using the formulas
!!
!!       radius=sqrt(x**2+y**2)
!!       inclination=atan2(y,x)
!!
!!##OPTIONS
!!    X  The distance along the x-axis
!!    Y  The distance along the y-axis
!!
!!##RESULTS
!!
!!    RADIUS       The radial distance from the origin (O) to the point (P)
!!    INCLINATION  The inclination angle in radians between the inclination reference direction
!!                 (x-axis) and the orthogonal projection of the line OP of the
!!                 reference plane (x-y plane).
!!
!!##EXAMPLES
!!
!!   examples of usage
!!
!!    program demo_cartesian_to_polar
!!    use M_units, only : cartesian_to_polar
!!    implicit none
!!    real    :: x,y
!!    real    :: r,i
!!    integer :: ios
!!    INFINITE: do
!!       read(*,*,iostat=ios) x, y
!!       if(ios.ne.0)exit INFINITE
!!       call cartesian_to_polar(x,y,r,i)
!!       write(*,*)'x=',x,' y=',y,'radius=',r,'inclination=',i
!!    enddo INFINITE
!!    end program demo_cartesian_to_polar
!===================================================================================================================================
subroutine cartesian_to_polar(x,y,radius,inclination)
implicit none
character(len=*),parameter::ident="@(#)M_units::cartesian_to_polar(3f): convert Cartesian coordinates to polar coordinates"
real,intent(in)  :: x,y
real,intent(out) :: radius,inclination
   radius=sqrt(x**2+y**2)
   if(radius.eq.0)then
      inclination=0.0
   else
      inclination=atan2(y,x)
   endif
end subroutine cartesian_to_polar
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!     polar_to_cartesian(3f) - [M_units] convert polar coordinates to Cartesian coordinates
!!##SYNOPSIS
!!
!!    subroutine polar_to_cartesian(radius,inclination,x,y)
!!
!!     real,intent(in) :: radius,inclination
!!     real,intent(out)  :: x,y
!!
!!##DESCRIPTION
!!
!!     Convert polar coordinate <radius, inclination > with
!!     angles in radians to cartesian point <X,Y> using the formulas
!!
!!       x=radius*cos(inclination)
!!       y=radius*sin(inclination)
!!
!!##OPTIONS
!!
!!    RADIUS       The radial distance from the origin (O) to the point (P)
!!    INCLINATION  The INCLINATION angle in radians between the inclination reference direction
!!                 (x-axis) and the orthogonal projection of the line OP of the
!!                 reference plane (x-y plane).
!!
!!##RESULTS
!!
!!    X  The distance along the x-axis
!!    Y  The distance along the y-axis
!!
!!##EXAMPLES
!!
!!   examples of usage
!!
!!    program demo_polar_to_cartesian
!!    use M_units, only : polar_to_cartesian
!!    implicit none
!!    real    :: x,y
!!    real    :: r,i
!!    integer :: ios
!!    INFINITE: do
!!       read(*,*,iostat=ios) x, y
!!       if(ios.ne.0)exit INFINITE
!!       call polar_to_cartesian(r,i,x,y)
!!       write(*,*)'x=',x,' y=',y,'radius=',r,'inclination=',i
!!    enddo INFINITE
!!    end program demo_polar_to_cartesian
!===================================================================================================================================
subroutine polar_to_cartesian(radius,inclination,x,y)
implicit none
character(len=*),parameter::ident="@(#)M_units::polar_to_cartesian(3f): convert polar coordinates to cartesian coordinates"
real,intent(in) :: radius,inclination
real,intent(out)  :: x,y
   if(radius.eq.0)then
      x=0.0
      y=0.0
   else
      x=radius*cos(inclination)
      y=radius*sin(inclination)
   endif
end subroutine polar_to_cartesian
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module m_units
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
