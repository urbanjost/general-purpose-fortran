           program demo_anint
           use, intrinsic :: iso_fortran_env, only : real_kinds, &
           & real32, real64, real128
           implicit none
           real(kind=real32) :: x4
           real(kind=real64) :: x8
              x4 = 1.234E0_real32
              x8 = 4.321_real64
              print *, anint(x4), dnint(x8)
              x8 = anint(x4,kind=real64)
           end program demo_anint
