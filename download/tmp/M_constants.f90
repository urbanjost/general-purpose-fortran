!>
!!##NAME
!!     M_constants(3fm) - common constants
!!##SYNOPSIS
!!
!!     use M_constants, only : e,gamma
!!##DESCRIPTION
!!   o "e"       the base of the natural logarithm system.  "e" was named in honor of Euler, but is known as Napier's constant.
!!   o "gamma"   The Euler-Mascheroni constant is often denoted by a lower-case Gamma.  Gamma is defined as
!!
!!                Gamma = limit ( M -> Infinity ) ( Sum ( 1 <= N <= M ) 1 / N ) - Log ( M )
!!   o "pi"      The ratio of the circumference of a circle to the diameter of the circle
!!##EXAMPLES
!!
!!   Sample usage
!!
!!    program demo_constants
!!    use M_constants, only : e,euler,pi,golden_ratio,deg_per_rad,rad_per_deg
!!    implicit none
!!       write(*,101) "Napier's constant (e) is about ",e
!!       write(*,101) "The Euler-Mascheroni constant (euler or gamma) is about ",euler
!!       write(*,101) "pi (pi) is about ",pi
!!       write(*,101) "The Golden Ratio (golden_ratio) is about ",golden_ratio
!!       write(*,101) "Deg_Per_Rad is about ",Deg_Per_Rad
!!       write(*,101) "Rad_Per_Deg is about ",Rad_Per_Deg
!!    101 format(a,t52,g0)
!!    end program demo_constants
!!
!!   Results:
!!
!!    Napier's constant (e) is about                     2.7182818284590451
!!    The Euler-Mascheroni constant (gamma) is about     .57721566490153287
!!    pi (pi) is about                                   3.1415926535897931
!!    The Golden Ratio (golden_ratio) is about           1.6180339887498949
!!    Deg_Per_Rad is about                               57.295779513082323
!!    Rad_Per_Deg is about                               .17453292519943295E-001
!===================================================================================================================================
module M_constants
implicit none
private
character(len=*),parameter :: ident1='@(#)M_constants(3fm): A collection of commonly used constants (pi, e, gamma, ...)'
!doubleprecision,public,parameter :: &
integer, public, parameter :: DP = selected_real_kind(15)
real(kind=DP), public, parameter ::              &
!==================================================================================================================================
                ! "e" is the base of the natural logarithm system.
                ! "e" was named in honor of Euler, but is known as Napier's constant.
   e            = 2.71828182845904523536028747135266249775724709369995d+00, &

                ! The Euler-Mascheroni constant is often denoted by a lower-case Gamma.  Gamma is defined as
                ! Gamma = limit ( M -> Infinity ) ( Sum ( 1 <= N <= M ) 1 / N ) - Log ( M )
   euler        = 0.577215664901532860606512090082402431042d+00,            &
   gamma        = 0.577215664901532860606512090082402431042d+00,            &

   pi           = 3.14159265358979323846264338327950288419716939937510d0,   &

                ! for two values A+B is to A as A is to B
   Golden_Ratio = 1.6180339887498948482045868_DP,                           &

   Deg_Per_Rad  = 57.2957795130823208767981548_DP,                          &
   Rad_Per_Deg  = 0.01745329251994329576923691_DP,                          &

   end=99999    ! END OF CONSTANTS
!==================================================================================================================================
! for unit conversions
!doubleprecision,parameter,public :: c                       2.997925+8          ! m/sec
!doubleprecision,parameter,public :: g                       9.80665             ! m/sec2
!doubleprecision,parameter,public :: e                       1.6021917-19        ! coul
!doubleprecision,parameter,public :: mercury                 1.33322+5           ! kg/m2-sec2
!doubleprecision,parameter,public :: year                    365.24219879        ! day fuzz
!doubleprecision,parameter,public :: lb                      .45359237           ! kg
!doubleprecision,parameter,public :: inch                    2.54                ! cm
!doubleprecision,parameter,public :: nmile                   1852                ! m
!doubleprecision,parameter,public :: acre                    4840                ! yd2
!doubleprecision,parameter,public :: gallon                  231                 ! in3
!doubleprecision,parameter,public :: imperial                1.20095             ! gallons
!doubleprecision,parameter,public :: au                      1.49597871+11
!doubleprecision,parameter,public :: mole                    6.022169+23
!doubleprecision,parameter,public :: brgallon                277.420             ! in3
!doubleprecision,parameter,public :: cal                     4.1868              ! joule
!doubleprecision,parameter,public :: atmosphere              1.01325+5           ! nt/m2
!doubleprecision,parameter,public :: atomicmassunit          1.66044-27          ! kg
!doubleprecision,parameter,public :: carat                   205                 ! mg
!doubleprecision,parameter,public :: faraday                 9.652+4             ! coul
!doubleprecision,parameter,public :: fathom                  6                   ! ft
!doubleprecision,parameter,public :: fermi                   1e-15               ! m
!==================================================================================================================================
end module M_constants
