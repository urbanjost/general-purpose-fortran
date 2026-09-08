      program demo_acos
      use, intrinsic :: iso_fortran_env, only : real32,real64,real128
      implicit none
      character(len=*),parameter :: all='(*(g0,1x))'
      real(kind=real64) :: x , d2r

         ! basics
          x = 0.866_real64
          print all,'acos(',x,') is ', acos(x)

         ! acos(-1) should be PI
          print all,'for reference', new_line('a'), &
          &'PI ~= 3.14159265358979323846264338327950288419716939937510'
          write(*,*) acos(-1.0_real64)
          d2r=acos(-1.0_real64)/180.0_real64
          print all,'90 degrees is ', d2r*90.0_real64, ' radians'
         ! elemental
          print all,'elemental',acos([-1.0,-0.5,0.0,0.50,1.0])
         ! complex
          print *,'complex',acos( (-1.0,  0.0) )
          print *,'complex',acos( (-1.0, -1.0) )
          print *,'complex',acos( ( 0.0, -0.0) )
          print *,'complex',acos( ( 1.0,  0.0) )

      end program demo_acos
