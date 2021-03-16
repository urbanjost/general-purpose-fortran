           program demo_sqrt
           use, intrinsic :: iso_fortran_env, only : real_kinds, &
           & real32, real64, real128
           implicit none
             real(kind=real64) :: x = 2.0_real64
             complex :: z = (1.0, 2.0)
             x = sqrt(x)
             z = sqrt(z)
           end program demo_sqrt
