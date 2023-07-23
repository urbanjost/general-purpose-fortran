      program demo_aint
      use, intrinsic :: iso_fortran_env, only : sp=>real32, dp=>real64
      implicit none
      real(kind=dp) :: x8
         print *,'basics:'
         print *,' just chops off the fractional part'
         print *,  aint(-2.999), aint(-2.1111)
         print *,' if |x| < 1 a positive zero is returned'
         print *,  aint(-0.999), aint( 0.9999)
         print *,' input may be of any real kind'
         x8 = 4.3210_dp
         print *, aint(-x8), aint(x8)
         print *,'elemental:'
         print *,aint([ &
          &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
          &  0.0,   &
          &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])
      end program demo_aint
