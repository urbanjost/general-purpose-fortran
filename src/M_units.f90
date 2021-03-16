










!>
!!##NAME
!!    M_units(3fm) - [M_units::INTRO] convert between various physical units
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!
!!    procedure definitions:
!!
!!       elemental real function c2f(celsius)
!!       elemental real function f2c(fahrenheit)
!!
!!       elemental real|double function r2d(radians)
!!       elemental real|double function d2r(degrees)
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
!!       elemental double function norm_angle_rad(radians)
!!       elemental real|double function norm_angle_360(radians)
!!
!!       subroutine cartesian_to_spherical(x,y,z,radius,inclination,azimuth)
!!       subroutine spherical_to_cartesian(radius,inclination,azimuth,x,y,z)
!!       subroutine cartesian_to_polar(x,y,radius,inclination)
!!       subroutine polar_to_cartesian(radius,inclination,x,y)
!!
!!##CONSTANTS SYNOPSIS
!!
!!       function nan()
!!       function inf()
!!       function is_nan()
!!       function is_even()
!!
!!   Simple constants:
!!
!!     e,gamma,euler,golden_ratio,pi
!!     deg_per_rad, rad_per_deg
!!     c__m_per_sec, c__ft_per_sec
!!
!!##DESCRIPTION
!!
!!    M_units(3fm) is a Fortran module that collects together basic
!!    procedures that are used to convert between various physical units
!!    and common named constants.
!!
!!    The input values may be any standard scalar value supported by
!!    the anyscalar_to_real(3f) function (real,integer,doubleprecision) within
!!    the range allowed by the function.
!!
!!##PROCEDURES
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
!!    Normalize angles into specific ranges
!!
!!     o elemental double function norm_angle_rad(angle_in_radians)
!!     o elemental real|double function norm_angle_360(angle_in_degrees)
!!
!!    Coordinates
!!
!!     o cartesian_to_spherical:  Convert cartesian coordinates to spherical
!!     o spherical:  Convert spherical coordinates to cartesian
!!     o cartesian_to_polar:  Convert cartesian coordinates to polar
!!     o polar:  Convert polar coordinates to cartesian
!!
!!   Note that your compiler is less likely to inline small procedures in a
!!   module than it would statement functions or CONTAINED functions.
!!
!!##CONSTANTS
!!
!!   "c__m_per_sec"   Speed of light in a vacuum
!!   "c__ft_per_sec"  Speed of light in a vacuum
!!   "deg_per_rad"
!!   "rad_per_deg"
!!   "e"              The base of the natural logarithm system. "e"
!!                    was named in honor of Euler, but is known as Napier's constant.
!!   "euler"
!!   "gamma"          The Euler-Mascheroni constant is often denoted by
!!                    a lower-case Gamma.
!!   "golden_ratio"
!!
!!   "pi"             The ratio of the circumference of a circle to the diameter of the circle
!!
!!##NOTES
!!
!!   Gamma is defined as
!!
!!    Gamma = limit ( M -> Infinity ) ( Sum ( 1 <= N <= M ) 1 / N ) - Log ( M )
!!
!!##EXAMPLES
!!
!!   Simple usage example:
!!
!!    program demo_M_units
!!    use M_units, only : r2d, d2r
!!    use M_units, only : f2c, c2f
!!    use M_units, only : sind, cosd, tand
!!    use M_units, only : asind, acosd, atand, atan2d
!!    !!
!!    use M_units, only : pi8=>pi
!!    use M_units, only : e,euler,golden_ratio,deg_per_rad,rad_per_deg
!!    use M_units, only : c__m_per_sec, c__ft_per_sec
!!    !!
!!    implicit none
!!    real pi
!!    pi=pi8
!!    write(*,*)r2d([0.0,PI/4.0,PI/2.0,3.0*PI/2.0,PI])
!!    write(*,*)d2r([0.0,45.0,90.0,135.0,180.0])
!!    write(*,*)f2c([-40.0,32.0,212.0])
!!    write(*,*)c2f([-40.0,0.0,100.0])
!!    write(*,*)PI
!!    write(*,*)E
!!    !!
!!    write(*,101) "Napier's constant (e) is about ",e
!!    write(*,101) "The Euler-Mascheroni constant (euler or gamma) is about ",euler
!!    write(*,101) "pi (pi) is about ",pi8
!!    write(*,101) "The Golden Ratio (golden_ratio) is about ",golden_ratio
!!    write(*,101) "Deg_Per_Rad is about ",Deg_Per_Rad
!!    write(*,101) "Rad_Per_Deg is about ",Rad_Per_Deg
!!    !!
!!    write(*,101) "Speed of light in a vacuum (m/sec)       ", c__m_per_sec
!!    write(*,101) "Speed of light in a vacuum (ft/sec)      ", c__ft_per_sec
!!    !!
!!    101 format(a,t57,g0)
!!    !!
!!    end program demo_M_units
!!
!!   Results:
!!
!!    >   0.00000000       45.0000000       90.0000000       270.000000       180.000000
!!    >   0.00000000      0.785398185       1.57079637       2.35619450       3.14159274
!!    >  -40.0000000       0.00000000       100.000000
!!    >  -40.0000000       32.0000000       212.000000
!!    >   3.14159274
!!    >   2.7182818284590451
!!    >Napier's constant (e) is about                          2.7182818284590451
!!    >The Euler-Mascheroni constant (euler or gamma) is about 0.57721566490153287
!!    >pi (pi) is about                                        3.1415926535897931
!!    >The Golden Ratio (golden_ratio) is about                1.6180339887498949
!!    >Deg_Per_Rad is about                                    57.295779513082323
!!    >Rad_Per_Deg is about                                    0.17453292519943295E-001
!!    >Speed of light in a vacuum (m/sec)                      299792458.00000000
!!    >Speed of light in a vacuum (ft/sec)                     983571056.00000000
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_units
use M_anything,only : anyscalar_to_real, anyscalar_to_double
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
!  normalize angles
      public norm_angle_rad
      public norm_angle_360
!  convert between degrees and radians
      public d2r
      public r2d
!  distance
      public feet_to_meters
      public meters_to_feet
!  weight
      public pounds_to_kilograms
!  convert between Celsius and Fahrenheit
      public f2c
      public c2f
!  convert between coordinate systems
      public cartesian_to_spherical
      public spherical_to_cartesian
      public cartesian_to_polar
      public polar_to_cartesian
!  tables
      public symbol2atomnum ! return atomic number given element symbol name
      public atomnum2symbol ! return element symbol given atomic number

      public test_suite_M_units
!===================================================================================================================================
      public is_even
      public is_nan
      public inf
      interface inf
         module procedure inf32, inf64, inf128
      end interface inf

      public nan
      interface nan
         module procedure nan32, nan64, nan128
      end interface nan
!===================================================================================================================================
!  constants

doubleprecision, parameter, private :: eighth_circle_rad_d  = atan(1.0d0)            ! pi/4
doubleprecision, parameter, private :: quarter_circle_rad_d = 2*eighth_circle_rad_d  ! pi/2
doubleprecision, parameter, private :: half_circle_rad_d    = 4*eighth_circle_rad_d  ! pi
doubleprecision, parameter, private :: circle_rad_d         = 8*eighth_circle_rad_d  ! 2pi

real, parameter, private :: eighth_circle_rad_r  = atan(1.0)              ! pi/4
real, parameter, private :: quarter_circle_rad_r = 2*eighth_circle_rad_r  ! pi/2
real, parameter, private :: half_circle_rad_r    = 4*eighth_circle_rad_r  ! pi
real, parameter, private :: circle_rad_r         = 8*eighth_circle_rad_r  ! 2pi

!===================================================================================================================================
integer, public, parameter :: DP = selected_real_kind(15)
real(kind=DP), public, parameter ::              &
!---------------------!------------------------------------------------------------
                      ! velocity of light in a vacuum
   c__m_per_sec       = 2.99792458d+8,                                            & ! m/sec
   c__ft_per_sec      = 9.83571056d+8,                                            & ! ft/sec
!---------------------!------------------------------------------------------------
                      ! "e" is the base of the natural logarithm system.
                      ! "e" was named in honor of Euler, but is known as Napier's constant.
   e                  = 2.71828182845904523536028747135266249775724709369995d+00, &
!---------------------!------------------------------------------------------------
   euler              = 0.577215664901532860606512090082402431042d+00,            &
!---------------------!------------------------------------------------------------
                      ! The Euler-Mascheroni constant is often denoted by a lower-case Gamma.  Gamma is defined as
                      ! Gamma = limit ( M -> Infinity ) ( Sum ( 1 <= N <= M ) 1 / N ) - Log ( M )
   gamma              = 0.577215664901532860606512090082402431042d+00,            &
!---------------------!------------------------------------------------------------
   pi                 = 3.14159265358979323846264338327950288419716939937510d0,   &
!---------------------!------------------------------------------------------------
                      ! for two values A+B is to A as A is to B
   Golden_Ratio       = 1.6180339887498948482045868_DP,                           &
!---------------------!------------------------------------------------------------
   Deg_Per_Rad        = 57.2957795130823208767981548_DP,                          &
   Rad_Per_Deg        = 0.01745329251994329576923691_DP,                          &
   degrees_to_radians = PI / 180.0D+00,                                           &
!---------------------!------------------------------------------------------------
   end=99999    ! END OF CONSTANTS
!===================================================================================================================================

   interface norm_angle_360                                  ! a Generic Interface in a module with PRIVATE specific procedures
      module procedure norm_angle_360_real, norm_angle_360_double
      module procedure norm_angle_360_integer
   end interface

   interface r2d
      module procedure r2d_d, r2d_r, r2d_i
   end interface

   interface d2r
      module procedure d2r_d
      module procedure d2r_r
      module procedure d2r_i
   end interface

contains
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!    c2f(3f) - [M_units:TEMPERATURE] convert Celsius to Fahrenheit
!!    (LICENSE:PD)
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
!!    program demo_c2f
!!    use M_units, only : c2f
!!    implicit none
!!       write(*,*)'With REAL array input    ', c2f([ -40.0, 0.0, 100.0 ])
!!       write(*,*)'With INTEGER array input ', c2f([ -40,   0,   100   ])
!!       write(*,*)'With DOUBLEPRECISION     ', c2f(-40.0d0),c2f(0.0d0),c2f(100.0d0)
!!    end program demo_c2f
!!
!!   Results
!!
!!    With REAL array input      -40.0000000       32.0000000       212.000000
!!    With INTEGER array input   -40.0000000       32.0000000       212.000000
!!    With DOUBLEPRECISION       -40.0000000       32.0000000       212.000000
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function c2f(celsius)
! ident_1="@(#)M_units::c2f(3f): Convert celsius to fahrenheit"
class(*),intent(in)           :: celsius        ! celsius value to convert to fahrenheit
   real                       :: celsius_local
   celsius_local=anyscalar_to_real(celsius)
   c2f=(celsius_local+40.0)*9.0/5.0 - 40.0         ! do the conversion
end function c2f
!***********************************************************************************************************************************
!>
!!##NAME
!!    f2c(3f) - [M_units:TEMPERATURE] convert Fahrenheit to Celsius
!!    (LICENSE:PD)
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
!!    program demo_f2c
!!    use M_units, only :  f2c
!!    implicit none
!!       write(*,*)'With REAL array input    ', f2c([ -40.0,32.0, 212.0 ])
!!       write(*,*)'With INTEGER array input ', f2c([ -40,  32,   212   ])
!!       write(*,*)'With DOUBLEPRECISION     ', f2c(-40.0d0),f2c(32.0d0),f2c(212.0d0)
!!    end program demo_f2c
!!
!!   Results
!!
!!    With REAL array input      -40.0000000       0.00000000       100.000000
!!    With INTEGER array input   -40.0000000       0.00000000       100.000000
!!    With DOUBLEPRECISION       -40.0000000       0.00000000       100.000000
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function f2c(fahrenheit)
! ident_2="@(#)M_units::f2c(3f): Convert fahrenheit to celsius"
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
!!    r2d(3f) - [M_units:TRIGONOMETRY] convert radians to degrees
!!    (LICENSE:PD)
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
!!    program demo_r2d
!!    use M_units, only :  r2d
!!    use M_units, only : pi8=>pi
!!    implicit none
!!    real :: pi=real(pi8)
!!       write(*,*)'With REAL array input    ', r2d([ 0.0, PI/4.0, PI/2.0, 3.0*PI/2.0, PI ])
!!       write(*,*)'With INTEGER array input ', r2d([0,1,2,3,4])
!!       write(*,*)'With DOUBLEPRECISION     ', r2d(0.0d0),r2d(PI/4.0d0),r2d(PI/2.0d0),r2d(3.0d0*PI/2.0d0),r2d(PI)
!!    end program demo_r2d
!!
!!   Results
!!
!!     With REAL array input       0.00000000       45.0000000       90.0000000       270.000000       180.000000
!!     With INTEGER array input    0.00000000       57.2957802       114.591560       171.887344       229.183121
!!     With DOUBLEPRECISION        0.00000000       45.0000000       90.0000000       270.000000       180.000000
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
elemental doubleprecision function r2d_i(iradians)

! ident_3="@(#)M_units::r2d_i(3f): Convert radians to degrees"

doubleprecision,parameter :: DEGREE=0.017453292519943d0 ! radians
integer,intent(in)           :: iradians        ! input radians to convert to degrees
   r2d_i=dble(iradians)/DEGREE ! do the conversion
end function r2d_i
!-----------------------------------------------------------------------------------------------------------------------------------
elemental doubleprecision function r2d_d(radians)

! ident_4="@(#)M_units::r2d_d(3f): Convert radians to degrees"

doubleprecision,parameter :: DEGREE=0.017453292519943d0 ! radians
doubleprecision,intent(in)           :: radians        ! input radians to convert to degrees
   r2d_d=radians / DEGREE ! do the conversion
end function r2d_d
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function r2d_r(radians)

! ident_5="@(#)M_units::r2d_r(3f): Convert radians to degrees"

doubleprecision,parameter :: DEGREE=0.017453292519943d0 ! radians
real,intent(in)           :: radians        ! input radians to convert to degrees
   r2d_r=radians / DEGREE ! do the conversion
end function r2d_r
!***********************************************************************************************************************************
!>
!!##NAME
!!    d2r(3f) - [M_units:TRIGONOMETRY] convert degrees to radians
!!    (LICENSE:PD)
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
!!    program demo_d2r
!!    use M_units, only :  d2r
!!    implicit none
!!       write(*,*)'With REAL array input    ', d2r([0.0,45.0,90.0,135.0,180.0])
!!       write(*,*)'With INTEGER array input ', d2r([0,  45,  90,  135,  180  ])
!!       write(*,*)'With DOUBLEPRECISION     ', &
!!       & d2r(0.0d0),d2r(45.0d0),d2r(90.0d0),d2r(135.0d0),d2r(180.0d0)
!!    end program demo_d2r
!!
!!   Results
!!
!!    With REAL array input    0.00000 0.785398185 1.57079637 2.35619450 3.14159274
!!    With INTEGER array input 0.00000 0.785398185 1.57079637 2.35619450 3.14159274
!!    With DOUBLEPRECISION     0.00000 0.785398185 1.57079637 2.35619450 3.14159274
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function d2r_r(degrees)

! ident_6="@(#)M_units::d2r_r(3f): Convert degrees to radians"

doubleprecision,parameter :: RADIAN=57.2957795131d0 ! degrees
real,intent(in)           :: degrees                ! input degrees to convert to radians
   d2r_r=dble(degrees)/RADIAN                       ! do the unit conversion
end function d2r_r
!-----------------------------------------------------------------------------------------------------------------------------------
elemental doubleprecision function d2r_d(degrees)

! ident_7="@(#)M_units::d2r_d(3f): Convert degrees to radians"

doubleprecision,parameter :: RADIAN=57.2957795131d0 ! degrees
doubleprecision,intent(in) :: degrees               ! input degrees to convert to radians
   d2r_d=degrees/RADIAN                             ! do the unit conversion
end function d2r_d
!-----------------------------------------------------------------------------------------------------------------------------------
elemental doubleprecision function d2r_i(idegrees)

! ident_8="@(#)M_units::d2r_i(3f): Convert degrees to radians"

doubleprecision,parameter :: RADIAN=57.2957795131d0 ! degrees
integer,intent(in) :: idegrees                      ! input degrees to convert to radians
   d2r_i=dble(idegrees)/RADIAN                      ! do the unit conversion
end function d2r_i
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!    sind(3f) - [M_units:TRIGONOMETRY] calculate sine of value in degrees
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    elemental real function sind(degrees)
!!
!!     class(*),intent(in) :: degrees
!!##DESCRIPTION
!!    Calculate sine of input value in degrees
!!
!!##OPTIONS
!!    degrees    any standard scalar value supported by anyscalar_to_double(3f)
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_sind
!!    use M_units, only :  sind
!!    implicit none
!!       write(*,*)'With REAL array input    ', sind([ 0.0, 15.0, 30.0, 45.0, &
!!                                            & 60.0, 75.0, 90.0, 180.0, 270.0 ])
!!       write(*,*)'With INTEGER array input ', sind([0,15,30,45,60,75,90,180,270])
!!       write(*,*)'With DOUBLEPRECISION     ',           &
!!          & sind(0.0d0),sind(15.0d0),sind(90.0/3.0d0),  &
!!          & sind(90.0/2.0d0),sind(60.0d0),sind(75.0d0), &
!!          & sind(90.0d0),sind(180.0d0),sind(270.0d0)
!!    end program demo_sind
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
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function sind(angle_in_degrees)

! ident_9="@(#)M_units::sind(3f): sin(3f) with degrees as input instead of radians"

class(*),intent(in)  :: angle_in_degrees
real                 :: angle_in_degrees_local
   angle_in_degrees_local=anyscalar_to_double(angle_in_degrees)
   sind=sin(angle_in_degrees_local*degrees_to_radians)
end function sind
!***********************************************************************************************************************************
!>
!!##NAME
!!    cosd(3f) - [M_units:TRIGONOMETRY] calculate cosine of value in degrees
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    elemental real function cosd(degrees)
!!
!!     class(*),intent(in) :: degrees
!!##DESCRIPTION
!!    Calculate cosine of input value in degrees
!!
!!##OPTIONS
!!    degrees    any standard scalar value supported by anyscalar_to_double(3f).
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_cosd
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
!!    end program demo_cosd
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
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function cosd(angle_in_degrees)

! ident_10="@(#)M_units::cosd(3f): cos(3f) with degrees as input instead of radians"

class(*),intent(in) :: angle_in_degrees
real                :: angle_in_degrees_local
   angle_in_degrees_local=anyscalar_to_double(angle_in_degrees)
   cosd=cos(angle_in_degrees_local*degrees_to_radians)
end function cosd
!***********************************************************************************************************************************
!>
!!##NAME
!!    tand(3f) - [M_units:TRIGONOMETRY] calculate tangent of value in degrees
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! elemental real function tand(degrees)
!!
!!     class(*),intent(in) :: degrees
!!##DESCRIPTION
!!    Calculate tangent of input value in degrees
!!
!!##OPTIONS
!!    degrees    any standard scalar value supported by anyscalar_to_double(3f).
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_tand
!!    use M_units, only :  tand
!!    implicit none
!!       write(*,*)'With REAL array input    ', &
!!         & tand([ 0.0, 15.0, 30.0, 45.0, 60.0, 75.0, 90.0, 180.0, 270.0 ])
!!       write(*,*)'With INTEGER array input ', &
!!         & tand([0,15,30,45,60,75,90,180,270])
!!       write(*,*)'With DOUBLEPRECISION     ', &
!!         & tand(0.0d0),tand(15.0d0),tand(90.0/3.0d0),tand(90.0/2.0d0),&
!!         & tand(60.0d0),tand(75.0d0),&
!!         & tand(90.0d0),tand(180.0d0),tand(270.0d0)
!!    end program demo_tand
!!   Results:
!!
!!    With REAL array input    0.00000000   0.267949194   0.577350259
!!    1.00000000    1.73205078    3.73205090    1.63312395E+16
!!    -1.22464685E-16  5.44374649E+15
!!    With INTEGER array input  0.00000000   0.267949194   0.577350259
!!    1.00000000    1.73205078    3.73205090    1.63312395E+16
!!    -1.22464685E-16  5.44374649E+15
!!    With DOUBLEPRECISION    0.00000000   0.267949194   0.577350259
!!    1.00000000    1.73205078    3.73205090    1.63312395E+16
!!    -1.22464685E-16  5.44374649E+15
!!
!!   Results
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function tand(angle_in_degrees)

! ident_11="@(#)M_units::tand(3f): tan(3f) with degrees as input instead of radians"

class(*),intent(in) :: angle_in_degrees
real                :: angle_in_degrees_local
   angle_in_degrees_local=anyscalar_to_double(angle_in_degrees)
   tand=tan(angle_in_degrees_local*degrees_to_radians)
end function tand
!***********************************************************************************************************************************
!>
!!##NAME
!!    asind(3f) - [M_units:TRIGONOMETRY] calculate arcsine of value in degrees
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    elemental real function asind(value)
!!
!!     class(*),intent(in) :: value
!!##DESCRIPTION
!!    Calculate arcsine of input value in degrees. It converts the input
!!    value to radians from degrees and calls asin(3f).
!!
!!##OPTIONS
!!    value    any standard scalar value supported by anyscalar_to_double(3f)
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_asind
!!    use M_units, only :  asind
!!    implicit none
!!       write(*, *)asind([ 0.0, 0.258819044, 0.5, 0.707106829, 0.866025448,  &
!!                       & 0.965925813, 1.0, -8.74227766E-08, -1.0 ])
!!    end program demo_asind
!!
!!   Results
!!
!!       0.0 15.0  30.0 45.0000038  60.00 75.0 90.0 -5.00895612E-06  -90.0
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function asind(x)

! ident_12="@(#)M_units::asind(3f): asin(3f) with degrees as output instead of radians"

class(*),intent(in) :: x
real                :: x_local
   x_local=anyscalar_to_double(x)
   asind=asin(x_local)/degrees_to_radians
end function asind
!***********************************************************************************************************************************
!>
!!##NAME
!!    acosd(3f) - [M_units:TRIGONOMETRY] calculate arccosine of value in degrees
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    elemental real function acosd(value)
!!
!!     class(*),intent(in) :: value
!!##DESCRIPTION
!!    Calculate arcsine of input value in degrees. It converts the input value
!!    from degrees to radians and calls acos(3f).
!!
!!##OPTIONS
!!    value    any standard scalar value supported by anyscalar_to_double(3f)
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_acosd
!!    use M_units, only :  acosd, cosd
!!    implicit none
!!       write(*, *)       cosd(0.0),cosd(45.0),cosd(120.0),cosd(180.0),cosd(720.0)
!!       write(*, *)acosd([cosd(0.0),cosd(45.0),cosd(120.0),cosd(180.0),cosd(720.0) ])
!!    end program demo_acosd
!!
!!   Results
!!
!!       1.00000000      0.707106769     -0.500000000      -1.00000000       1.00000000
!!       0.00000000       45.0000000       120.000000       180.000000       0.00000000
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function acosd(x)

! ident_13="@(#)M_units::acosd(3f): calculate arc-cos of angle in degrees"

class(*),intent(in) :: x
real                :: x_local
   x_local=anyscalar_to_double(x)
   acosd=acos(x_local)/degrees_to_radians
end function acosd
!***********************************************************************************************************************************
!>
!!##NAME
!!    atand(3f) - [M_units:TRIGONOMETRY] calculate arctangent of value in degrees
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    elemental real function atand(value)
!!
!!     class(*),intent(in) :: value
!!##DESCRIPTION
!!    Calculate arctangent of input value in degrees. It calls atan(3f) and
!!    converts the output to degrees from radians.
!!
!!##OPTIONS
!!    value    any standard scalar value supported by anyscalar_to_double(3f)
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!    program demo_atand
!!    use M_units, only :  atand, tand
!!    implicit none
!!       write(*, *)       tand(0.0),tand(45.0),tand(120.0),tand(180.0),tand(720.0)
!!       write(*, *)atand([tand(0.0),tand(45.0),tand(120.0),tand(180.0),tand(720.0) ])
!!    end program demo_atand
!!
!!   Results:
!!
!!       0.00000000       1.00000000      -1.73205078      -1.22464685E-16  -4.89858741E-16
!!       0.00000000       45.0000000      -60.0000000      -7.01670955E-15  -2.80668382E-14
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function atand(x)

! ident_14="@(#)M_units::atand(3f): result is arc-tangent of angle in degrees"

class(*),intent(in) :: x
real                :: x_local
   x_local=anyscalar_to_double(x)
   atand=atan(x_local)/degrees_to_radians
end function atand
!***********************************************************************************************************************************
!>
!!##NAME
!!    atan2d(3f) - [M_units:TRIGONOMETRY] calculate arctangent of the complex number X + i Y
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    elemental real function atan2d(x,y)
!!
!!     class(*),intent(in) :: x
!!     class(*),intent(in) :: y
!!##DESCRIPTION
!!    Calculate arctangent of arctangent of the complex number
!!
!!       X + i Y.
!!
!!    in degrees. It calls atan2(3f) and converts the output from radians to degrees.
!!
!!##OPTIONS
!!    X    any standard scalar value supported by anyscalar_to_double(3f)
!!    Y    any standard scalar value supported by anyscalar_to_double(3f)
!!
!!##EXAMPLE
!!
!!        Sample program:
!!
!!           program demo_atan2d
!!           use M_units, only : atan2d
!!           real(4) :: x = 1.e0_4, y = 0.5e0_4
!!             write(*,*)atan2d(y,x)
!!           end program demo_atan2d
!!   Results:
!!
!!       26.5650501
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function atan2d(x,y)

! ident_15="@(#)M_units::atan2d(3f): calculate arc-tangent of angle in degrees"

class(*),intent(in) :: x
class(*),intent(in) :: y
real                :: x_local
real                :: y_local
   x_local=anyscalar_to_double(x)
   y_local=anyscalar_to_double(y)
   atan2d=atan2(x_local,y_local)/degrees_to_radians
end function atan2d
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!    feet_to_meters(3f) - [M_units:LENGTH] converts a measurement in feet to meters
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    elemental doubleprecision function feet_to_meters(feet)
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
!!    program demo_feet_to_meters
!!    use M_units, only : feet_to_meters
!!    implicit none
!!       write(*,*)'With REAL array input    ', &
!!               & feet_to_meters([ -1.0, 0.0, 1.0 ,1.0/12.0])
!!       write(*,*)'With INTEGER array input ', &
!!               & feet_to_meters([ -1,   0,   1   ])
!!       write(*,*)'With DOUBLEPRECISION     ', &
!!               & feet_to_meters(-1.0d0), &
!!               & feet_to_meters(0.0d0), &
!!               & feet_to_meters(1.0d0)
!!    end program demo_feet_to_meters
!!
!!   Results
!!
!!     With REAL array input     -0.304800004   0.00000000  0.304800004  2.54000016E-02
!!     With INTEGER array input  -0.304800004   0.00000000  0.304800004
!!     With DOUBLEPRECISION      -0.304800004   0.00000000  0.304800004
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental function feet_to_meters(feet)
! ident_16="@(#)M_units::feet_to_meters(3f): converts a measurement in feet to meters"
class(*),intent(in)           :: feet                           ! the input length in feet.
doubleprecision               :: feet_to_meters                 ! OUTPUT, the corresponding length in meters.
doubleprecision               :: feet_local
   feet_local=anyscalar_to_double(feet)
   !!feet_to_meters = 0.0254 * 12.0 * feet_local
   feet_to_meters = 0.3048d0 * feet_local

end function feet_to_meters
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!    meters_to_feet(3f) - [M_units:LENGTH] converts a measurement in meters to feet
!!    (LICENSE:PD)
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
!!    program demo_meters_to_feet
!!    use M_units, only : meters_to_feet
!!    implicit none
!!       write(*,*)'With REAL array input    ', meters_to_feet([ -1.0, 0.0, 1.0 ])
!!       write(*,*)'With INTEGER array input ', meters_to_feet([ -1,   0,   1   ])
!!       write(*,*)'With DOUBLEPRECISION     ', meters_to_feet(-1.0d0),meters_to_feet(0.0d0),meters_to_feet(1.0d0)
!!    end program demo_meters_to_feet
!!
!!   Results
!!
!!     With REAL array input      -3.28083992       0.00000000       3.28083992
!!     With INTEGER array input   -3.28083992       0.00000000       3.28083992
!!     With DOUBLEPRECISION       -3.28083992       0.00000000       3.28083992
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental function meters_to_feet(meters)
! ident_17="@(#)M_units::meters_to_feet(3f): converts a measurement in meters to feet"
class(*),intent(in)           :: meters                         ! the input length in meters.
doubleprecision               :: meters_to_feet                 ! OUTPUT, the corresponding length in feet.
   doubleprecision            :: meters_local
   meters_local=anyscalar_to_double(meters)
   meters_to_feet = meters_local/12.0d0/0.0254d0
end function meters_to_feet
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!     cartesian_to_spherical(3f) - [M_units:TRIGONOMETRY] convert Cartesian coordinates to ISO polar coordinates
!!     (LICENSE:PD)
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
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine cartesian_to_spherical(x,y,z,radius,inclination,azimuth)
implicit none
! ident_18="@(#)M_units::cartesian_to_spherical(3f): convert Cartesian coordinates to ISO polar coordinates"
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
!!     spherical_to_cartesian(3f) - [M_units:TRIGONOMETRY] convert ISO polar coordinates to Cartesian coordinates
!!     (LICENSE:PD)
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
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine spherical_to_cartesian(radius,inclination,azimuth,x,y,z)
implicit none
! ident_19="@(#)M_units::spherical_to_cartesian(3f): convert spherical coordinates to cartesian coordinates"
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
!!     cartesian_to_polar(3f) - [M_units:TRIGONOMETRY] convert Cartesian coordinates to polar coordinates
!!     (LICENSE:PD)
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
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine cartesian_to_polar(x,y,radius,inclination)
implicit none
! ident_20="@(#)M_units::cartesian_to_polar(3f): convert Cartesian coordinates to polar coordinates"
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
!!     polar_to_cartesian(3f) - [M_units:TRIGONOMETRY] convert polar coordinates to Cartesian coordinates
!!     (LICENSE:PD)
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
!!       write(*,'(g0)',advance='no')'Enter radius and inclination(in radians):'
!!       read(*,*,iostat=ios) r, i
!!       if(ios.ne.0)exit INFINITE
!!       call polar_to_cartesian(r,i,x,y)
!!       write(*,*)'x=',x,' y=',y,'radius=',r,'inclination=',i
!!    enddo INFINITE
!!    end program demo_polar_to_cartesian
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine polar_to_cartesian(radius,inclination,x,y)
implicit none
! ident_21="@(#)M_units::polar_to_cartesian(3f): convert polar coordinates to cartesian coordinates"
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
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!    atomnum2symbol(3f) - [M_units:SYMBOLS] return element symbol given atomic number
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine atomnum2symbol(atomnum,symbol)
!!
!!    integer,intent(in)           :: atomnum
!!    character(len=2),intent(out) :: symbol
!!
!!##DESCRIPTION
!!    Given an atomic number in the range of 1 to 109 return the corresponding element symbol name
!!
!!##OPTIONS
!!    atomnum    an atomic number from 1 to 109
!!
!!##RETURNS
!!    symbol     two-character symbol name corresponding to atomic number ATOMNUM
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_atomnum2symbol
!!    use M_units, only :  atomnum2symbol
!!    implicit none
!!    character(len=2)  :: name
!!    integer           :: i
!!    do i=1,109
!!       call atomnum2symbol(i,name)
!!       write(*,*)i,name
!!    enddo
!!    end program demo_atomnum2symbol
!!
!!   Results:
!!
!!        >   1 H
!!        >   2 He
!!        >   3 Li
!!        >   4 Be
!!        >   5 B
!!        >   6 C
!!        >   7 N
!!        >   8 O
!!        >   9 F
!!        >  10 Ne
!!        >  11 Na
!!        >  12 Mg
!!        >  13 Al
!!        >  14 Si
!!        >  15 P
!!        >  16 S
!!        >  17 Cl
!!        >  18 Ar
!!        >  19 K
!!        >  20 Ca
!!        >  21 Sc
!!        >  22 Ti
!!        >  23 V
!!        >  24 Cr
!!        >  25 Mn
!!        >  26 Fe
!!        >  27 Co
!!        >  28 Ni
!!        >  29 Cu
!!        >  30 Zn
!!        >  31 Ga
!!        >  32 Ge
!!        >  33 As
!!        >  34 Se
!!        >  35 Br
!!        >  36 Kr
!!        >  37 Rb
!!        >  38 Sr
!!        >  39 Y
!!        >  40 Zr
!!        >  41 Nb
!!        >  42 Mo
!!        >  43 Tc
!!        >  44 Ru
!!        >  45 Rh
!!        >  46 Pd
!!        >  47 Ag
!!        >  48 Cd
!!        >  49 In
!!        >  50 Sn
!!        >  51 Sb
!!        >  52 Te
!!        >  53 I
!!        >  54 Xe
!!        >  55 Cs
!!        >  56 Ba
!!        >  57 La
!!        >  58 Ce
!!        >  59 Pr
!!        >  60 Nd
!!        >  61 Pm
!!        >  62 Sm
!!        >  63 Eu
!!        >  64 Gd
!!        >  65 Tb
!!        >  66 Dy
!!        >  67 Ho
!!        >  68 Er
!!        >  69 Tm
!!        >  70 Yb
!!        >  71 Lu
!!        >  72 Hf
!!        >  73 Ta
!!        >  74 W
!!        >  75 Re
!!        >  76 Os
!!        >  77 Ir
!!        >  78 Pt
!!        >  79 Au
!!        >  80 Hg
!!        >  81 Tl
!!        >  82 Pb
!!        >  83 Bi
!!        >  84 Po
!!        >  85 At
!!        >  86 Rn
!!        >  87 Fr
!!        >  88 Ra
!!        >  89 Ac
!!        >  90 Th
!!        >  91 Pa
!!        >  92 U
!!        >  93 Np
!!        >  94 Pu
!!        >  95 Am
!!        >  96 Cm
!!        >  97 Bk
!!        >  98 Cf
!!        >  99 Es
!!        > 100 Fm
!!        > 101 Md
!!        > 102 No
!!        > 103 Lr
!!        > 104 Rf
!!        > 105 Db
!!        > 106 Sg
!!        > 107 Bh
!!        > 108 Hs
!!        > 109 Mt
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine atomnum2symbol(atomnum,symbol)
implicit none
! ident_22="@(#)M_units::atomnum2symbol(3f): return element symbol given atomic number"
integer,intent(in)           :: atomnum
character(len=2),intent(out) :: symbol
integer,parameter            :: nelements=109
character(len=2),save        :: symbols(nelements)

data symbols/                                                 &
& 'H ', 'He', 'Li', 'Be', 'B ', 'C ', 'N ', 'O ', 'F ', 'Ne', &
& 'Na', 'Mg', 'Al', 'Si', 'P ', 'S ', 'Cl', 'Ar', 'K ', 'Ca', &
& 'Sc', 'Ti', 'V ', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', &
& 'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', 'Y ', 'Zr', &
& 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 'In', 'Sn', &
& 'Sb', 'Te', 'I ', 'Xe', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd', &
& 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er', 'Tm', 'Yb', &
& 'Lu', 'Hf', 'Ta', 'W ', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg', &
& 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th', &
& 'Pa', 'U ', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'Es', 'Fm', &
& 'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Bh', 'Hs', 'Mt'/

   if(atomnum.lt.1.or.atomnum.gt.nelements)then
      write(*,*)'*atomnum2symbol* atomic number out of range (1 to 109) ',atomnum
      symbol='  '
   else
      symbol=symbols(atomnum)
   endif
end subroutine atomnum2symbol
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!    symbol2atomnum(3f) - [M_units:SYMBOLS] return atomic number given element symbol name
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine symbol2atomnum(symbol,atomnum)
!!
!!    character(len=2),intent(in) :: symbol
!!    integer,intent(out)         :: atomnum
!!
!!##DESCRIPTION
!!    Given a two-character element symbol name return the corresponding atomic number
!!
!!##OPTIONS
!!    symbol     two-character symbol name corresponding to atomic number ATOMNUM
!!
!!##RETURNS
!!    atomnum    an atomic number from 1 to 109
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_symbol2atomnum
!!    use M_units, only :  symbol2atomnum
!!    implicit none
!!    integer           :: atomnum
!!    character(len=2)  :: name
!!    name='Ne'
!!    call symbol2atomnum(name,atomnum)
!!    write(*,*)atomnum,name
!!    end program demo_symbol2atomnum
!!
!!   Results:
!!
!!    10 Ne
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine symbol2atomnum(symbol,atomnum)
implicit none
! ident_23="@(#)M_units::symbol2atomnum(3f): return atomic number given element symbol name"
character(len=2),intent(in) :: symbol
integer,intent(out)         :: atomnum
integer,parameter           :: nelements=109
integer                     :: i
character(len=2),save       :: symbols(nelements)

data symbols/                                                 &
& 'H ', 'He', 'Li', 'Be', 'B ', 'C ', 'N ', 'O ', 'F ', 'Ne', &
& 'Na', 'Mg', 'Al', 'Si', 'P ', 'S ', 'Cl', 'Ar', 'K ', 'Ca', &
& 'Sc', 'Ti', 'V ', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', &
& 'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', 'Y ', 'Zr', &
& 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 'In', 'Sn', &
& 'Sb', 'Te', 'I ', 'Xe', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd', &
& 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er', 'Tm', 'Yb', &
& 'Lu', 'Hf', 'Ta', 'W ', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg', &
& 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th', &
& 'Pa', 'U ', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'Es', 'Fm', &
& 'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Bh', 'Hs', 'Mt'/

FINDIT: block
   do i = 1,nelements
      if ( (symbol(1:1) .eq. symbols(i)(1:1)) .and. (symbol(2:2) .eq. symbols(i)(2:2)) )then
         atomnum=i
         exit FINDIT
      endif
   enddo
   write(*,*)'*symbol2atomnum* error: symbol not found :',symbol
   atomnum=0
endblock FINDIT
end subroutine symbol2atomnum
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
!>
!!##NAME
!!    pounds_to_kilograms(3f) - [M_units:MASS] - converts a measurement in pounds-mass to kilograms.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental function pounds_to_kilograms ( pounds )
!!
!!     class(*),intent(in) ::  pounds
!!     doubleprecision     :: pounds_to_kilograms
!!
!!##DESCRIPTION
!!    Converts pounds-mass to kilograms using the formula:
!!
!!       kilograms = 0.45359237 * pounds
!!
!!    The pound (or more properly pound-mass) is a unit of mass used in the
!!    Imperial, United States Customary, and other systems of measurement. A
!!    number of different definitions have been used; the most common today
!!    is the international avoirdupois pound, which is legally defined as
!!    exactly 0.45359237 kilograms. The international standard symbol for
!!    the avoirdupois pound is lb (from the Roman "libra"); an alternative
!!    symbol is lbm).
!!
!!##OPTIONS
!!    POUNDS  The weight in pounds.
!!            POUNDS may be any standard scalar value supported by anyscalar_to_double(3f).
!!            This at least includes REAL, INTEGER, and DOUBLEPRECISION.
!!##RETURN
!!    POUNDS_TO_KILOGRAMS   the corresponding weight in kilograms.
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_pounds_to_kilograms
!!    use M_units, only : pounds_to_kilograms
!!    implicit none
!!       write(*,*)'REAL            ', pounds_to_kilograms(1.0)
!!       write(*,*)'INTEGER array   ', pounds_to_kilograms([ 0, 1, 100, 200 ])
!!       write(*,*)'DOUBLEPRECISION ', pounds_to_kilograms(1.0d0)
!!    end program demo_pounds_to_kilograms
!!
!!   Typical Results
!!
!!     REAL              0.45359237000000002
!!     INTEGER array     0.0000000000000000    0.45359237000000002
!!                      45.359237000000000    90.718474000000001
!!     DOUBLEPRECISION   0.45359237000000002
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental function pounds_to_kilograms ( pounds )
! ident_24="@(#)M_units::pounds_to_kilograms(3f): converts a measurement in pounds to kilograms."
class(*),intent(in) :: pounds
   doubleprecision  :: pounds_to_kilograms
   doubleprecision  :: pounds_local
   pounds_local=anyscalar_to_double(pounds)
   pounds_to_kilograms = 0.45359237d0 * pounds_local
end function pounds_to_kilograms
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    norm_angle_rad(3f) - [M_units:TRIGONOMETRY] Return input angle given in radians as angle between 0 and 2pi
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    elemental function norm_angle_rad(ang) result(rev)
!!
!!    class(*), intent(in) :: ang
!!    doubleprecision      :: rev
!!##DESCRIPTION
!!    Given an input angle specified in radians, normalize the value to fall in the
!!    range 0 to 2*pi radians.
!!##OPTIONS
!!    ang  Input angle (radians). May be INTEGER, REAL, or DOUBLEPRECISION
!!##RESULTS
!!    rev  Return input angle (radians) normalized to range 0>= REV <=2*pi radians
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_norm_angle_rad
!!    use M_units, only : norm_angle_rad
!!    implicit none
!!    real,parameter :: PI=4*atan(1.0)
!!    real           :: val
!!    integer        :: i
!!    do i=-2,2
!!       val=i*2*pi;   write(*,*)val,norm_angle_rad(val)
!!       val=i*pi;     write(*,*)val,norm_angle_rad(val)
!!       write(*,*)
!!    enddo
!!    write(*,*)norm_angle_rad([-PI/8.0,-PI/4.0,-PI/2.0,-PI,-0.0,PI/8.0,PI/4.0,PI/2.0,PI,0.0])
!!    end program demo_norm_angle_rad
!!
!!   Results:
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental function norm_angle_rad(ang)

! ident_25="@(#)M_units::norm_angle_rad(3fp): Return input angle (given in radians) as doubleprecision angle between 0 and 2pi radians"
class(*), intent(in) :: ang
doubleprecision      :: ang_local
doubleprecision      :: norm_angle_rad
   ang_local=anyscalar_to_double(ang)
   norm_angle_rad = ang_local - dble(floor(ang_local/circle_rad_d)) * circle_rad_d
end function norm_angle_rad
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    norm_angle_360(3f) - [M_units:TRIGONOMETRY] Return input angle given in degrees as angle between 0 and 360
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental function norm_angle_360(ang) result(rev)
!!
!!     doubleprecision, intent(in) :: ang
!!     doubleprecision             :: rev
!!        or
!!     real, intent(in) :: ang
!!     real             :: rev
!!        or
!!     integer, intent(in) :: ang
!!     integer             :: rev
!!
!!##DESCRIPTION
!!    Given an input angle in degrees, normalize the value to fall in the
!!    range 0 to 360  degrees.
!!
!!##OPTIONS
!!    ang  Input angle (degrees)
!!
!!##RESULTS
!!    rev  Return input angle (degrees) normalized to range 0 to 360 degrees
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_norm_angle_360
!!    use M_units, only : norm_angle_360
!!    implicit none
!!    real,parameter :: PI=360
!!    real           :: val
!!    integer        :: i
!!    do i=-2,2
!!       val=i*2*pi;   write(*,*)val,norm_angle_360(val)
!!       val=i*pi;     write(*,*)val,norm_angle_360(val)
!!       val=i*pi/2;   write(*,*)val,norm_angle_360(val)
!!       write(*,*)
!!    enddo
!!    end program demo_norm_angle_360
!!
!!   Results:
!!
!!      -1440.00000       0.00000000
!!      -720.000000       0.00000000
!!      -360.000000       0.00000000
!!
!!      -720.000000       0.00000000
!!      -360.000000       0.00000000
!!      -180.000000       180.000000
!!
!!       0.00000000       0.00000000
!!       0.00000000       0.00000000
!!       0.00000000       0.00000000
!!
!!       720.000000       0.00000000
!!       360.000000       0.00000000
!!       180.000000       180.000000
!!
!!       1440.00000       0.00000000
!!       720.000000       0.00000000
!!       360.000000       0.00000000
!!
!!##LICENSE
!!    MIT License
elemental function norm_angle_360_class(ang)

! ident_26="@(#)M_units:: norm_angle_360_class(3fp): Returns angle in degrees between 0 and 360"

class(*),intent(in) :: ang
doubleprecision     :: ang_local
doubleprecision     :: norm_angle_360_class
ang_local=anyscalar_to_double(ang)
   norm_angle_360_class = ang_local - dble(floor(ang_local/360.d0)) * 360.d0
end function norm_angle_360_class
!===================================================================================================================================
elemental function norm_angle_360_double(ang)

! ident_27="@(#)M_units:: norm_angle_360_double(3fp): Returns angle in degrees between 0 and 360"

doubleprecision,intent(in) :: ang
doubleprecision            :: norm_angle_360_double
   norm_angle_360_double = norm_angle_360_class(ang)
end function norm_angle_360_double
!===================================================================================================================================
elemental function norm_angle_360_real(ang)

! ident_28="@(#)M_units:: norm_angle_360_real(3fp): Returns angle in degrees between 0 and 360"

real,intent(in) :: ang
real            :: norm_angle_360_real
   norm_angle_360_real = norm_angle_360_class(ang)
end function norm_angle_360_real
!===================================================================================================================================
elemental function norm_angle_360_integer(ang)

! ident_29="@(#)M_units:: norm_angle_360_integer(3fp): Returns angle in degrees between 0 and 360"

integer,intent(in) :: ang
integer            :: norm_angle_360_integer
   norm_angle_360_integer = norm_angle_360_class(ang)
end function norm_angle_360_integer
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    inf(3f) - [M_units] return an inf (Infinity)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function inf(value)
!!    use,intrinsic :: iso_fortran_env, only: real32, real64, real128
!!    real(kind=real32|real64|real128) :: inf
!!    real(kind=real32|real64|real128) :: value
!!##DESCRIPTION
!!    Return an inf (Infinity) value. The type returned will be the same
!!    kind as the passed value.
!!##OPTIONS
!!    value  A real value whose kind is used to define the kind of the
!!           returned value.
!!##RETURNS
!!    inf    returns an inf value ("Infinity") on platforms that support
!!           inf values. The kind is determined by the kind of the input
!!           value.
!!##EXAMPLE
!!
!!    Sample program
!!
!!      program demo_inf
!!      use,intrinsic :: iso_fortran_env, only: real32, real64, real128
!!      use M_units, only : inf
!!      implicit none
!!      real(kind=real32)  :: r32
!!      real(kind=real64)  :: r64
!!      real(kind=real128) :: r128
!!      character(len=256) :: message
!!      integer            :: ios
!!         r32=inf(0.0_real32)
!!         r64=inf(0.0_real64)
!!         r128=inf(0.0_real128)
!!         write(*,*,iomsg=message,iostat=ios)r32,r64,r128
!!         if(ios.ne.0)write(*,*)trim(message)
!!         write(*,'(z0)',iomsg=message,iostat=ios)r32,r64,r128
!!         if(ios.ne.0)write(*,*)trim(message)
!!         write(*,'(g0)',iomsg=message,iostat=ios)r32,r64,r128
!!         if(ios.ne.0)write(*,*)trim(message)
!!         write(*,'(f3.1)',iomsg=message,iostat=ios)r32,r64,r128
!!         if(ios.ne.0)write(*,*)trim(message)
!!         write(*,'(f2.1)',iomsg=message,iostat=ios)r32,r64,r128
!!         if(ios.ne.0)write(*,*)trim(message)
!!      end program demo_inf
!!   Results:
!!
!!     Infinity Infinity Infinity
!!    7F800000
!!    7FF0000000000000
!!    7FFF0000000000000000000000000000
!!    Inf
!!    Inf
!!    Inf
!!    Inf
!!    Inf
!!    Inf
!!    **
!!    **
!!    **
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function inf32(value)
use,intrinsic :: iso_fortran_env, only: real32
implicit none

! ident_30="@(#)M_units:: inf32(3fp): Returns an inf (Infinity) of type real32"

character(len=3),save :: STRING='inf'
real(kind=real32) :: inf32,value
   read(STRING,*)inf32
end function inf32
!===================================================================================================================================
function inf64(value)
use,intrinsic :: iso_fortran_env, only: real64
implicit none

! ident_31="@(#)M_units:: inf64(3fp): Returns an inf (Infinity) of type real64"

character(len=3),save :: STRING='inf'
real(kind=real64) :: inf64,value
   read(STRING,*)inf64
end function inf64
!===================================================================================================================================
function inf128(value)
use,intrinsic :: iso_fortran_env, only: real128
implicit none

! ident_32="@(#)M_units:: inf128(3fp): Returns an inf (Infinity) of type real128"

character(len=3),save :: STRING='inf'
real(kind=real128) :: inf128,value
   read(STRING,*)inf128
end function inf128
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!>
!!##NAME
!!    nan(3f) - [M_units] return a NaN (Not a number)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function nan(value)
!!    use,intrinsic :: iso_fortran_env, only: real32, real64, real128
!!    real(kind=real32|real64|real128) :: nan
!!    real(kind=real32|real64|real128) :: value
!!##DESCRIPTION
!!    Return a NaN (Not a number) value. The type returned will be the same
!!    kind as the passed value.
!!
!!    At some point, the IEEE interface should work, with something like
!!
!!     use,intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
!!     use,intrinsic :: ieee_arithmetic, only : ieee_support_nan
!!
!!     if(IEEE_SUPPORT_NAN(x))then        ! Are IEEE NaNs supported?
!!        x=IEEE_VALUE(x,ieee_quiet_nan)  ! Generate an IEEE value.
!!     endif
!!
!!##OPTIONS
!!    value  A real value whose kind is used to define the kind of the
!!           returned value.
!!##RETURNS
!!    nan    returns a Nan value ("Not a number") on platforms that support
!!           NaN values. The kind is determined by the kind of the input
!!           value.
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_nan
!!    use,intrinsic :: iso_fortran_env, only: real32, real64, real128
!!    use M_units, only : nan
!!    implicit none
!!    real(kind=real32)  :: r32
!!    real(kind=real64)  :: r64
!!    real(kind=real128) :: r128
!!    character(len=256) :: message
!!    integer            :: ios
!!
!!       r32=nan(0.0_real32)
!!       r64=nan(0.0_real64)
!!       r128=nan(0.0_real128)
!!
!!       ! examples printing the NaN values
!!       ! list directed format
!!       write(*,*,iomsg=message,iostat=ios)r32,r64,r128
!!       if(ios.ne.0)write(*,*)trim(message)
!!       ! hexadecimal format to show different kinds
!!       write(*,'(*(z0,1x))',iomsg=message,iostat=ios)r32,r64,r128
!!       if(ios.ne.0)write(*,*)trim(message)
!!       ! G0 format
!!       write(*,'(*(g0,1x))',iomsg=message,iostat=ios)r32,r64,r128
!!       if(ios.ne.0)write(*,*)trim(message)
!!       ! if a specific numeric field is used
!!       write(*,'(*(f3.1,1x))',iomsg=message,iostat=ios)r32,r64,r128
!!       if(ios.ne.0)write(*,*)trim(message)
!!       ! if format is less than three characters
!!       write(*,'(*(f2.1,1x))',iomsg=message,iostat=ios)r32,r64,r128
!!       if(ios.ne.0)write(*,*)trim(message)
!!
!!       ! an option to terminate a program when a NaN is encountered
!!       ! (if X is NaN the comparison with 0. is always false.)
!!       if (.not.(r32<=0.0) .and. .not.(r32>=0.0))then
!!          write(*,*)'found nan'
!!          stop
!!       endif
!!
!!       ALT1: block
!!          integer :: x = 2143289344
!!          print *, transfer(x, 1.0)    ! prints "nan" on i686
!!       endblock ALT1
!!
!!    end program demo_nan
!!
!!   Results:
!!
!!    NaN      NaN              NaN
!!    7FC00000 7FF8000000000000 7FFF8000000000000000000000000000
!!    NaN NaN NaN
!!    NaN NaN NaN
!!    ** ** **
!!     found nan
!!
!!##SEE ALSO
!!      IS_NAN(3f)
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function nan32(value)
use,intrinsic :: iso_fortran_env, only: real32
implicit none

! ident_33="@(#)M_units:: nan32(3fp): Returns a NAN (Not a number) of type real32"

character(len=3),save :: STRING='NaN'
real(kind=real32) :: nan32,value
   read(STRING,*)nan32
   ! (if X is NaN the comparison with 0. is always false.)
   !if ( (nan32<=0.0_real32) .or. (nan32>=0.0_real32) )then
   !   write(*,*)'nan(3f) did not produce a nan'
   !   stop
   !endif
end function nan32
!===================================================================================================================================
function nan64(value)
use,intrinsic :: iso_fortran_env, only: real64
implicit none

! ident_34="@(#)M_units:: nan64(3fp): Returns a NAN (Not a number) of type real64"

character(len=3),save :: STRING='NaN'
real(kind=real64) :: nan64,value
   read(STRING,*)nan64
   ! (if X is NaN the comparison with 0. is always false.)
   !if ( (nan64<=0.0_real64) .or. (nan64>=0.0_real64) )then
   !   write(*,*)'nan(3f) did not produce a nan'
   !   stop
   !endif
end function nan64
!===================================================================================================================================
function nan128(value)
use,intrinsic :: iso_fortran_env, only: real128
implicit none

!$@(#) M_units:: nan128(3fp): Returns a NAN (Not a number) of type real128

character(len=3),save :: STRING='NaN'
real(kind=real128) :: nan128,value
   read(STRING,*)nan128
   ! (if X is NaN the comparison with 0. is always false.)
   !if ( (nan128<=0.0_real128) .or. (nan128>=0.0_real128) )then
   !   write(*,*)'nan(3f) did not produce a nan'
   !   stop
   !endif
end function nan128
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    is_even(3f) - [M_units] determine if integer is even
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    elemental pure logical is_even(int)
!!
!!     integer(kind=int8|int16|int32|int64) :: int
!!##DESCRIPTION
!!     Determine if an integer is even or not.
!!##OPTIONS
!!     int      The integer to test
!!##RETURNS
!!     is_even  logical value is .true. if the input value INT is even
!!##EXAMPLE
!!
!!   simple example
!!
!!     program demo_is_even
!!     use M_units, only : is_even
!!        write(*,*)is_even(0)
!!        write(*,*)is_even(-1)
!!        write(*,*)is_even(-2)
!!        write(*,*)is_even(+1)
!!        write(*,*)is_even(+2)
!!        write(*,*)is_even([10,11,17,19,22])
!!        write(*,*)(is_even(i),i=-10,10)
!!     end program demo_is_even
!!   Expected output
!!     T
!!     F
!!     T
!!     F
!!     T
!!     T F F F T
!!     T F T F T F T F T F T F T F T F T F T F T
!!##LICENSE
!!    Public Domain
elemental pure function is_even(ival)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64

! ident_35="@(#)M_units::is_even(3f): determine if integer is  even"

class(*),intent(in) :: ival
logical             :: is_even
select type(ival)
   type is (integer(kind=int8))  ; is_even = mod(ival, 2_int8)   == 0_int8
   type is (integer(kind=int16)) ; is_even = iand(ival, 1_int16) == 0_int16
   type is (integer(kind=int32)) ; is_even = iand(ival, 1_int32) == 0_int32
   type is (integer(kind=int64)) ; is_even = mod(ival, 2_int64)  == 0_int64
   end select
end function is_even
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    is_nan(3f) - [M_units] determine if integer is a Nan (Not a Number) value
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    elemental pure logical is_nan(value)
!!
!!     integer(kind=real32|real64|real128|complex) :: value
!!##DESCRIPTION
!!     Determine if a real or complex value is a NaN (Not a Number) value
!!##OPTIONS
!!     value   The value to test
!!##EXAMPLE
!!
!!   simple example
!!
!!     program demo_is_nan
!!     use M_units, only : is_nan
!!     real :: x
!!     character(len=*),parameter   :: linei='Infinity'
!!     character(len=*),parameter   :: line='NaN'
!!     character(len=:),allocatable :: readable
!!     real,parameter :: arr(*)=[-100.0,100.0,huge(0.0)]
!!        readable=linei
!!        read(readable,*)x
!!        write(*,*)is_nan(x),x   ! note Infinity is not a Nan
!!        write(*,*)is_nan(-x),-x
!!        readable=line
!!        read(readable,*)x
!!        write(*,*)is_nan(x),x
!!        write(*,*)x==x,x  ! note Nan is never equal to another value
!!        write(*,*)is_nan(arr),arr
!!     end program demo_is_nan
!!
!!   Expected results
!!
!!     F         Infinity
!!     F        -Infinity
!!     T              NaN
!!     F              NaN
!!     F F F  -100.000000       100.000000       3.40282347E+38
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental pure function is_nan(x)
!!use IEEE_EXCEPTIONS, only : ieee_support_nan ! is IEEE NaNs supported?
use,intrinsic :: IEEE_ARITHMETIC, only : IEEE_IS_NAN       ! Determine if value is IEEE Not-a-Number.
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128

! ident_36="@(#)M_units::is_nan(3f): determine if value is  IEEE Not-a-Number"

class(*),intent(in) :: x
logical             :: is_nan
   select type(x)
      type is (real(kind=real32));      is_nan=ieee_is_nan(x)
      type is (real(kind=real64));      is_nan=ieee_is_nan(x)
      type is (real(kind=real128));     is_nan=ieee_is_nan(x)
      type is (complex);                is_nan=ieee_is_nan(real(x)).and.ieee_is_nan(aimag(x))
      !!type is (complex);                is_nan=ieee_is_nan(x%re).and.ieee_is_nan(x%im)
   end select
end function is_nan
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_units()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
use M_msg,   only : str

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
   call test_norm_angle_360_double()
   call test_norm_angle_360_integer()
   call test_norm_angle_360_real()
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
use M_anything,only : anyscalar_to_real, anyscalar_to_double
USE M_Compare_Float_Numbers
use M_verify, only : accdig
use M_verify, only : unit_check
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
 call unit_check(label,stat,label,v1,v2,trim(message),'accuracy=',acurcy,'asked for',int(significant_digits)-2,'digits')
end subroutine testit_p
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nan()
use,intrinsic :: iso_fortran_env, only: real32, real64, real128
real(kind=real32) :: r32
real(kind=real64) :: r64
real(kind=real128) :: r128
   call unit_check_start('nan',msg='')
   ! (if X is NaN the comparison with 0.0 is always false.)
   r32=nan(0.0_real32)
   call unit_check('nan',.not.(r32<=0.0_real32) .and. .not.(r32>=0.0_real32),msg='real32')

   r64=nan(0.0_real64)
   call unit_check('nan',.not.(r64<=0.0_real64) .and. .not.(r64>=0.0_real64),msg='real64')

   r128=nan(0.0_real128)
   call unit_check('nan',.not.(r128<=0.0_real128) .and. .not.(r128>=0.0_real128),msg='real128')

   call unit_check_done('nan',msg='')
end subroutine test_nan
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_is_even()
logical,parameter     :: t=.true.
logical,parameter     :: f=.false.
   call unit_check_start('is_even',msg='')
   call unit_check('is_even', all(is_even([-10, 0, 1, 2, 3]).eqv.[t,t,f,t,f]), '-10, 0, 1, 2, 3')
   call unit_check_done('is_even',msg='')
end subroutine test_is_even
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_is_nan()
character(len=3),save :: line='NaN'
real                  :: x
logical,parameter     :: t=.true.
logical,parameter     :: f=.false.
   call unit_check_start('is_nan',msg='')
   read(line,*)x
call unit_check('is_nan', all(is_nan([x, 0.0,-0.0,-x,-100.0,100.0,huge(0.0)]).eqv.[t,f,f,t,f,f,f]),  &
        & 'checking',x,0,-x,-100.0,100.0,huge(0.0))
   call unit_check_done('is_nan',msg='')
end subroutine test_is_nan
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_inf()
   call unit_check_start('inf',msg='')
   !!call unit_check('inf', 0.eq.0, 'checking', 100)
   call unit_check_done('inf',msg='')
end subroutine test_inf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_acosd()

   call unit_check_start('acosd',msg='')
   !!call unit_check('acosd', 0.eq.0, 'checking', 100)
   call unit_check_done('acosd',msg='')
end subroutine test_acosd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_asind()

   call unit_check_start('asind',msg='')
   !!call unit_check('asind', 0.eq.0, 'checking', 100)
   call unit_check_done('asind',msg='')
end subroutine test_asind
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_atan2d()

   call unit_check_start('atan2d',msg='')
   !!call unit_check('atan2d', 0.eq.0, 'checking', 100)
   call unit_check_done('atan2d',msg='')
end subroutine test_atan2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_atand()

   call unit_check_start('atand',msg='')
   !!call unit_check('atand', 0.eq.0, 'checking', 100)
   call unit_check_done('atand',msg='')
end subroutine test_atand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_atomnum2symbol()

   call unit_check_start('atomnum2symbol',msg='')
   !!call unit_check('atomnum2symbol', 0.eq.0, 'checking', 100)
   call unit_check_done('atomnum2symbol',msg='')
end subroutine test_atomnum2symbol
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_c2f()

   call unit_check_start('c2f',msg='')
   call testit_p('c2f',     c2f(0.0)   ,  32.0,message='')
   call testit_p('c2f',     c2f(100.0) , 212.0,message='')
   call testit_p('c2f',     c2f(-40.0) , -40.0,message='')
   call unit_check_done('c2f',msg='')
end subroutine test_c2f
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cartesian_to_polar()

   call unit_check_start('cartesian_to_polar',msg='')
   !!call unit_check('cartesian_to_polar', 0.eq.0, 'checking', 100)
   call unit_check_done('cartesian_to_polar',msg='')
end subroutine test_cartesian_to_polar
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cartesian_to_spherical()

use M_verify,  only : accdig
implicit none
real    :: x=10.0,y=10.0,z=10.0
real    :: radius,inclination,azimuth
real    :: acurcy
integer :: ind1,ind2,ind3
   call unit_check_start('cartesian_to_spherical',msg='')

   ! 10,10,10 -> 17.32, 0.9553, 0.7854
   call cartesian_to_spherical(x,y,z,radius,inclination,azimuth)
   call accdig(radius,      17.3205090,   5.0,acurcy,ind1)
   call accdig(inclination,  0.955316663 ,5.0,acurcy,ind2)
   call accdig(azimuth,      0.785398185 ,5.0,acurcy,ind3)
   call unit_check('cartesian_to_spherical',all([ind1,ind2,ind3].eq.0),x,y,z,'to',radius,inclination,azimuth)

   call unit_check_done('cartesian_to_spherical') ! if got here without being stopped assume passed test
end subroutine test_cartesian_to_spherical
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cosd()

real, allocatable :: values(:)
integer           :: i
values=[0.0, 30.0, 45.0, 60.0, 92.0, 120.0, 135.0, 150.0, 180.0, 210.0, 240.0, 273.0, 300.0, 330.0, 360.0, -45.0]
   call unit_check_start('cosd',msg='')
   do i=1,size(values)
      call testit_p('cosd', cosd(values(i)), cos(d2r(values(i))),message=str('value=',values(i)) )
   enddo
   call unit_check_done('cosd',msg='')

!  unit_check:       cosd  FAILED:cosd 6.12323426E-17 -4.37113883E-08 value= 90.0000000 accuracy= 0.00000000 asked for 6 digits
!  unit_check:       cosd  FAILED:cosd -1.83697015E-16 1.19248806E-08 value= 270.000000 accuracy= 0.00000000 asked for 6 digits
end subroutine test_cosd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2r()

   call unit_check_start('d2r',msg='')

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

   call unit_check_done('d2r',msg='')

end subroutine test_d2r
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_f2c()

use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
   call unit_check_start('f2c',msg='')
   call testit_p('f2c',     f2c(32.0)  ,   0.0,message='')
   call testit_p('f2c',     f2c(212.0) , 100.0,message='')
   call testit_p('f2c',     f2c(-40.0) , -40.0,message='')
   call unit_check_done('f2c',msg='')
end subroutine test_f2c
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_feet_to_meters()
doubleprecision, parameter :: f2m=0.3048d0
   call unit_check_start('feet_to_meters',msg=' 0.3048')

   call unit_check('feet_to_meters', &
      & all(abs(feet_to_meters([ -1.0, 0.0, 1.0 ,1.0/12.0])- [-f2m, 0.0d0, f2m, 0.0254d0]).lt.0.00001),'real')
   call unit_check('feet_to_meters', &
      & all(abs(feet_to_meters([ -1,   0,   1   ])- [-f2m, 0.0d0, f2m]).lt.0.00001),'integer')
   call unit_check('feet_to_meters', &
      & all(abs([feet_to_meters(-1.0d0),feet_to_meters(0.0d0),feet_to_meters(1.0d0)]-[-f2m, 0.0d0, f2m]).lt.0.00001),'double')

   call unit_check_done('feet_to_meters',msg='')
end subroutine test_feet_to_meters

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_meters_to_feet()
   doubleprecision, parameter :: m2f=3.2808398950131233595d0

   call unit_check_start('meters_to_feet',msg='3.2808398950131233595')

   call unit_check('meters_to_feet', &
     & all(abs(meters_to_feet([ -1.0, 0.0, 1.0 ])-[-m2f,0.0d0,m2f]).lt.0.00001d0),msg='real')
   call unit_check('meters_to_feet', &
     & all(abs(meters_to_feet([ -1,   0,   1   ])-[-m2f,0.0d0,m2f]).lt.0.00001d0) ,msg='integer')
   call unit_check('meters_to_feet', &
     & all(abs([meters_to_feet(-1d0),meters_to_feet(0.0d0),meters_to_feet(1.0d0)]-[-m2f,0.0d0,m2f]).lt.0.00001d0),msg='double')

   call unit_check_done('meters_to_feet',msg='')
end subroutine test_meters_to_feet
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norm_angle_360_double()

   call unit_check_start('norm_angle_360_double',msg='')
   !!call unit_check('norm_angle_360_double', 0.eq.0, 'checking', 100)
   call unit_check_done('norm_angle_360_double',msg='')
end subroutine test_norm_angle_360_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norm_angle_360_integer()

   call unit_check_start('norm_angle_360_integer',msg='')
   !!call unit_check('norm_angle_360_integer', 0.eq.0, 'checking', 100)
   call unit_check_done('norm_angle_360_integer',msg='')
end subroutine test_norm_angle_360_integer
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norm_angle_360_real()

   call unit_check_start('norm_angle_360_real',msg='')
   !!call unit_check('norm_angle_360_real', 0.eq.0, 'checking', 100)
   call unit_check_done('norm_angle_360_real',msg='')
end subroutine test_norm_angle_360_real
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_norm_angle_rad()

   call unit_check_start('norm_angle_rad',msg='')
   !!call unit_check('norm_angle_rad', 0.eq.0, 'checking', 100)
   call unit_check_done('norm_angle_rad',msg='')
end subroutine test_norm_angle_rad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_polar_to_cartesian()

   call unit_check_start('polar_to_cartesian',msg='')
   !!call unit_check('polar_to_cartesian', 0.eq.0, 'checking', 100)
   call unit_check_done('polar_to_cartesian',msg='')
end subroutine test_polar_to_cartesian
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pounds_to_kilograms()

   call unit_check_start('pounds_to_kilograms',msg='')
   call unit_check('pounds_to_kilograms',abs(pounds_to_kilograms(1.0)-0.45359237).lt.0.00001,'real')
   call unit_check('pounds_to_kilograms',any(abs(pounds_to_kilograms([ 0, 1, 100, 200 ])-&
      &[0.0, 0.45359237, 45.359237,90.718474]).lt.0.00001),'integer')
   call unit_check('pounds_to_kilograms',abs(pounds_to_kilograms(1.0d0)-0.45359237).lt.0.00001,'double')
   call unit_check_done('pounds_to_kilograms',msg='')
end subroutine test_pounds_to_kilograms
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_r2d()

real              :: x=real(PI)
doubleprecision   :: d=PI

   call unit_check_start('r2d',msg='')

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

   call unit_check_done('r2d',msg='')
end subroutine test_r2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sind()

real, allocatable :: values(:)
integer           :: i
   values=[0.0, 30.0, 45.0, 60.0, 90.0, 120.0, 135.0, 150.0, 181.0, 210.0, 240.0, 270.0, 300.0, 330.0, 362.0, -45.0]
   call unit_check_start('sind',msg='')
   do i=1,size(values)
      call testit_p('sind',   sind(values(i))             ,  sin(d2r(values(i))),message=str('value=',values(i))  )
   enddo
   call unit_check_done('sind',msg='')
! unit_check:       sind  FAILED:sind 1.22464685E-16 -8.74227766E-08 value= 180.000000 accuracy= 0.00000000 asked for 6 digits
! unit_check:       sind  FAILED:sind -2.44929371E-16 1.74845553E-07 value= 360.000000 accuracy= 0.00000000 asked for 6 digits
end subroutine test_sind
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_spherical_to_cartesian()
use M_verify,  only : accdig
implicit none
real    :: x,y,z
real    :: radius,inclination,azimuth
real    :: acurcy
integer :: ind1,ind2,ind3
   call unit_check_start('spherical_to_cartesian',msg='')

   radius=17.32; inclination=0.9553; azimuth=0.7854
   x=-9999; y=-9999; z=-9999;
   call spherical_to_cartesian(radius,inclination,azimuth,x,y,z)

   call accdig(x,10.0,4.0,acurcy,ind1)
   call accdig(y,10.0,4.0,acurcy,ind2)
   call accdig(z,10.0,4.0,acurcy,ind3)

   call unit_check('spherical_to_cartesian',all([ind1,ind2,ind3].eq.0),radius,inclination,azimuth,'to',x,y,z)

   call unit_check_done('spherical_to_cartesian',msg='')
end subroutine test_spherical_to_cartesian
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_symbol2atomnum()

   call unit_check_start('symbol2atomnum',msg='')
   !!call unit_check('symbol2atomnum', 0.eq.0, 'checking', 100)
   call unit_check_done('symbol2atomnum',msg='')
end subroutine test_symbol2atomnum
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tand()
real, allocatable :: values(:)
integer                      :: i
   values=[0.0,30.0,45.0,60.0,92.0,120.0,135.0,150.0,183.0,210.0,240.0,273.0,300.0, 330.0, 362.0, -45.0]
   call unit_check_start('tand',msg='')
   do i=1,size(values)
      call testit_p('tand', tand(values(i)), tan(d2r(values(i))),message=str('value=',values(i)))
   enddo
   call unit_check_done('tand',msg='')
! unit_check:       tand  FAILED:tand 1.63312395E+16 -22877332.0 value= 90.0000000 accuracy= -8.85361290 asked for 6 digits
! unit_check:       tand  FAILED:tand -1.22464685E-16 8.74227766E-08 value= 180.000000 accuracy= 0.00000000 asked for 6 digits
! unit_check:       tand  FAILED:tand 5.44374649E+15 -83858280.0 value= 270.000000 accuracy= -7.81235218 asked for 6 digits
! unit_check:       tand  FAILED:tand -2.44929371E-16 1.74845553E-07 value= 360.000000 accuracy= 0.00000000 asked for 6 digits
end subroutine test_tand
!===================================================================================================================================
end subroutine test_suite_M_units
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_units
!***********************************************************************************************************************************
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!***********************************************************************************************************************************
