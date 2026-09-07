      program demo_acospi
      use, intrinsic :: iso_fortran_env, only : real32,real64,real128
      implicit none
      character(len=*),parameter :: all='(*(g0,1x))'
      real(kind=real64) :: x , d2r
      real(kind=real64),parameter :: &
      & PI = 3.14159265358979323846264338327950288419716939937510_real64

         ! basics
          x = PI/4.0_real64
          print all,'acospi(',x,') is ', acospi(x)

         ! acospi(-1) should be PI
          write(*,*) acospi(-1.0_real64)
          d2r=acospi(-1.0_real64)/180.0_real64
          print all,'90 degrees is ', d2r*90.0_real64, ' radians'
         ! elemental
          print all,'elemental',acospi([-1.0,-0.5,0.0,0.50,1.0])
         !
          print *,'-1.0',acospi( -1.0 )
          print *,' 0.0',acospi(  0.0 )
          print *,' 1.0',acospi(  1.0 )

      end program demo_acospi
