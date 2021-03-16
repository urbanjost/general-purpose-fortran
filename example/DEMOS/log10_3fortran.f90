           program demo_log10
           use, intrinsic :: iso_fortran_env, only : real_kinds, &
           & real32, real64, real128
           implicit none
           real(kind=real64) :: x = 10.0_real64
             x = log10(x)
           end program demo_log10
