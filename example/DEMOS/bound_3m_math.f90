           program demo_bound
           use,intrinsic :: iso_fortran_env, only : &
            & int8, int16, int32, int64, &
            & real32, real64, real128, dp=>real64
           use M_math, only : bound
           implicit none
              write(*, *)bound(1, 3, 10)
              write(*, *)bound(1, -30, 10)
              write(*, *)bound(1, 30, 10)
              write(*, *)bound(1, [-11,0,6,11,22], 10)
              write(*, *)bound(-11.11, 5.5, 9.999)
           end program demo_bound
