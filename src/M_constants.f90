module M_constants
!>
!!##NAME
!!    M_constants(3f) - [M_constants:INTRO] Useful constants
!!
!!##SYNOPSIS
!!
!!    use M_constants,  only : uc
!!
!!       ! real128 constants
!!       uc%qp%pi
!!       uc%qp%gamma
!!       uc%qp%e
!!       uc%qp%golden_ratio
!!       uc%qp%euler
!!
!!       ! real64 constants
!!       uc%dp%pi
!!       uc%dp%gamma
!!       uc%dp%e
!!       uc%dp%golden_ratio
!!       uc%dp%euler
!!
!!       ! real32 constants
!!       uc%sp%pi
!!       uc%sp%gamma
!!       uc%sp%e
!!       uc%sp%golden_ratio
!!       uc%sp%euler
!!
!!    use M_constants,  only : f
!!       ! formats
!!
!!       f%all
!!
!!##DESCRIPTION
!!  Useful universal constants, physical constants, formats, ...
!!
!!
!!  UNIVERSAL CONSTANTS
!!
!!      "e"              The base of the natural logarithm system. "e"
!!                       was named in honor of Euler, but is known as
!!                       Napier's constant.
!!      "euler"
!!      "gamma"          The Euler-Mascheroni constant is often denoted by
!!                       a lower-case Gamma.
!!      "golden_ratio"   In mathematics, two quantities are in the golden
!!                       ratio if their ratio is the same as the ratio of
!!                       their sum to the larger of the two quantities. so
!!                       for a > b > 0, (a + b)/ a = a/b where the Greek
!!                       letter phi often denotes the golden ratio.
!!
!!      "pi"             The ratio of the circumference of a circle to the
!!                       diameter of the circle
!!
!!  PHYSICAL CONSTANTS
!!
!!
!!      "deg_per_rad"
!!      "rad_per_deg"
!!      "c__m_per_sec"   Speed of light in a vacuum
!!      "c__ft_per_sec"  Speed of light in a vacuum
!!
!!##NOTES
!!
!!   Gamma is defined as
!!
!!    Gamma = limit ( M -> Infinity ) ( Sum ( 1 <= N <= M ) 1 / N ) - Log ( M )
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program test_universal_constants
!!     use M_constants, only : uc, f
!!     ! give a local name to a constant
!!     real,parameter :: e=uc%sp%e ! universal constant, single-precision, e
!!     character(len=*),parameter :: all=f%all
!!
!!     ! just use it
!!     print f%all, 'gamma=',uc%qp%gamma    ! universal constant, quad-precision, gamma
!!     print all, 'e=',e
!!
!!     ! or rename it with ASSOCIATE
!!     associate (gamma => uc%dp%gamma)
!!        print all,'gamma=',gamma
!!     end associate
!!
!!     end program test_universal_constants
use,intrinsic :: iso_fortran_env, only : real32, real64, real128
use,intrinsic :: iso_fortran_env, only : int8,   int16,  int32,  int64
use,intrinsic :: iso_fortran_env, only : r4 => real32, r8 => real64, r16 => real128
implicit none
private

!!integer, public, parameter :: DP = selected_real_kind(15)

real(kind=real128),parameter :: &

!------------------------!-----------------------------------------------------------------------------
   pi                      = 3.141592653589793238462643383279502884197169399375105820974944592307_real128, &
!------------------------!-----------------------------------------------------------------------------
                         ! The Euler-Mascheroni constant is often denoted by a lower-case Gamma.  Gamma is defined as
                         ! Gamma = limit ( M -> Infinity ) ( Sum ( 1 <= N <= M ) 1 / N ) - Log ( M )
   gamma                   = 0.577215664901532860606512090082402431042_real128,        &
!------------------------!-----------------------------------------------------------------------------
                         ! "e" is the base of the natural logarithm system.
                         ! "e" was named in honor of Euler, but is known as Napier's constant.
   e                       = 2.71828182845904523536028747135266249775724709369995_real128, &
!------------------------!-----------------------------------------------------------------------------
                         ! for two values A+B is to A as A is to B
   Golden_Ratio            = 1.6180339887498948482045868_real128,                      &
!------------------------!-----------------------------------------------------------------------------
   euler                   = 0.577215664901532860606512090082402431042_real128,        &
!------------------------!-----------------------------------------------------------------------------
                         ! velocity of light in a vacuum
   c__m_per_sec            = 2.99792458e+8_real128,                                    & ! m/sec
   c__ft_per_sec           = 9.83571056e+8_real128,                                    & ! ft/sec
!------------------------!-----------------------------------------------------------------------------
   Deg_Per_Rad             = 57.2957795130823208767981548_real128,                     &
   Rad_Per_Deg             = 0.01745329251994329576923691_real128,                     &
   degrees_to_radians      = PI/ 180.0_real128,                                    &
!------------------------!-----------------------------------------------------------------------------

end_of_constants = huge(0.0_real128)

type r128; real(r16) :: pi,gamma,e,golden_ratio,euler; endtype r128
type r64;  real(r8)  :: pi,gamma,e,golden_ratio,euler; endtype r64
type r32;  real(r4)  :: pi,gamma,e,golden_ratio,euler; endtype r32

type rall
   type(r128) :: qp
   type(r64)  :: dp
   type(r32)  :: sp
end type rall

type(rall),parameter,public :: uc=rall( &
& r128(pi=     pi,    gamma=     gamma,    e=     e,    golden_ratio=     golden_ratio,    euler=     euler),      &
&  r64(pi=real(pi,r8),gamma=real(gamma,r8),e=real(e,r8),golden_ratio=real(golden_ratio,r8),euler=real(euler,r8) ), &
&  r32(pi=real(pi,r4),gamma=real(gamma,r4),e=real(e,r4),golden_ratio=real(golden_ratio,r4),euler=real(euler,r4) )  &
& )

type fmt
   character(len=128) :: all='(*(g0,1x))'
end type fmt

type(fmt),public,parameter  :: f=fmt( )

end module M_constants
