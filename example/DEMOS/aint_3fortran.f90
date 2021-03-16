           program demo_aint
           use, intrinsic :: iso_fortran_env, only : real_kinds, &
           & real32, real64, real128
           implicit none
           real ::  x4
           real(kind=real64) :: x8
              x4 = 1.234E0_4
              x8 = 4.321_real64
              print *, aint(x4), dint(x8)
              x8 = aint(x4,kind=real64)
           end program demo_aint
