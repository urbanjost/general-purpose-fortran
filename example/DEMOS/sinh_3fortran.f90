      program demo_sinh
      use, intrinsic :: iso_fortran_env, only : &
      & real_kinds, real32, real64, real128
      implicit none
      real(kind=real64) :: x = - 1.0_real64
      real(kind=real64) :: nan, inf
      character(len=20) :: line

        ! basics
         print *, sinh(x)
         print *, (exp(x)-exp(-x))/2.0

        ! sinh(3) is elemental and can handle an array
         print *, sinh([x,2.0*x,x/3.0])

         ! a NaN input returns NaN
         line='NAN'
         read(line,*) nan
         print *, sinh(nan)

         ! a Inf input returns Inf
         line='Infinity'
         read(line,*) inf
         print *, sinh(inf)

         ! an overflow returns Inf
         x=huge(0.0d0)
         print *, sinh(x)

      end program demo_sinh
