          program demo_tan
          use, intrinsic :: iso_fortran_env, only : real_kinds, &
          & real32, real64, real128
          implicit none
          real(kind=real64) :: x = 0.165_real64
            x = tan(x)
          end program demo_tan
