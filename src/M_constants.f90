module M_constants
use, intrinsic :: iso_fortran_env, only : real32, real64, real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
real(kind=real128),parameter :: &
!------------------------!-----------------------------------------------------------------------------
   pi128                 = 3.141592653589793238462643383279502884197169399375105820974944592307_real128, &
!------------------------!------------------------------------------------------------
                         ! The Euler-Mascheroni constant is often denoted by a lower-case Gamma.  Gamma is defined as
                         ! Gamma = limit ( M -> Infinity ) ( Sum ( 1 <= N <= M ) 1 / N ) - Log ( M )
   gamma128              = 0.577215664901532860606512090082402431042_real128,        &
!------------------------!-----------------------------------------------------------------------------
                         ! "e" is the base of the natural logarithm system.
                         ! "e" was named in honor of Euler, but is known as Napier's constant.
   e128                  = 2.71828182845904523536028747135266249775724709369995_real128, &
!------------------------!-----------------------------------------------------------------------------
                         ! for two values A+B is to A as A is to B
   Golden_Ratio128       = 1.6180339887498948482045868_real128,                      &
!------------------------!-----------------------------------------------------------------------------
   euler128              = 0.577215664901532860606512090082402431042_real128,        &
!------------------------!-----------------------------------------------------------------------------
                         ! velocity of light in a vacuum
   c__m_per_sec128       = 2.99792458e+8_real128,                                    & ! m/sec
   c__ft_per_sec128      = 9.83571056e+8_real128,                                    & ! ft/sec
!------------------------!-----------------------------------------------------------------------------
   Deg_Per_Rad128        = 57.2957795130823208767981548_real128,                     &
   Rad_Per_Deg128        = 0.01745329251994329576923691_real128,                     &
   degrees_to_radians128 = PI128 / 180.0_real128,                                    &
!------------------------!-----------------------------------------------------------------------------
end_constants=0.0_real128  ! END OF CONSTANTS
private end_constants
!===================================================================================================================================
!!integer, public, parameter :: DP = selected_real_kind(15)
end module M_constants
!===================================================================================================================================
module M_constants_32
use M_constants
private
real(kind=real32),public,parameter  :: pi=real(pi128,kind=real32)
real(kind=real32),public,parameter  :: gamma=real(gamma128,kind=real32)
real(kind=real32),public,parameter  :: e=real(e128,kind=real32)
real(kind=real32),public,parameter  :: golden_ratio=real(golden_ratio128,kind=real32)
real(kind=real32),public,parameter  :: euler=real(euler128,kind=real32)
end module M_constants_32
!===================================================================================================================================
module M_constants_64
use M_constants
private
real(kind=real64),public,parameter  :: pi=real(pi128,kind=real64)
real(kind=real64),public,parameter  :: gamma=real(gamma128,kind=real64)
real(kind=real64),public,parameter  :: e=real(e128,kind=real64)
real(kind=real64),public,parameter  :: golden_ratio=real(golden_ratio128,kind=real64)
real(kind=real64),public,parameter  :: euler=real(euler128,kind=real64)
end module M_constants_64
!===================================================================================================================================
module M_constants_128
use M_constants, only : pi=>           pi128
use M_constants, only : gamma=>        gamma128
use M_constants, only : e=>            e128
use M_constants, only : golden_ratio=> golden_ratio128
use M_constants, only : euler=>        euler128
end module M_constants_128
!===================================================================================================================================
