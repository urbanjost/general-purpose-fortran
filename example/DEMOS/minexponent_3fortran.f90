      program demo_minexponent
      use, intrinsic :: iso_fortran_env, only : real32, real64, real128
      implicit none
      real(kind=real32) :: x
      real(kind=real64) :: y
          print *, minexponent(x), maxexponent(x)
          print *, minexponent(y), maxexponent(y)
      end program demo_minexponent
