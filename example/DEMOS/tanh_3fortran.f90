          program demo_tanh
          use, intrinsic :: iso_fortran_env, only : real_kinds, real32, real64, real128
          implicit none
            real(kind=real64) :: x = 2.1_real64
            x = tanh(x)
          end program demo_tanh
