          program demo_atan
          use, intrinsic :: iso_fortran_env, only : real_kinds, &
          & real32, real64, real128
          implicit none
          real(kind=real64) :: x = 2.866_real64
             x = atan(x)
          end program demo_atan
