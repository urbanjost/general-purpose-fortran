      program demo_erf
      use, intrinsic :: iso_fortran_env, only : real32, real64, real128
      implicit none
      real(kind=real64) :: x = 0.17_real64
          write(*,*)x, erf(x)
      end program demo_erf
