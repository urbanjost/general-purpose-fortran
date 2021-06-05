           program demo_aimag
           use, intrinsic :: iso_fortran_env, only : real_kinds, &
           & real32, real64, real128
           implicit none
           complex(kind=real32) z4
           complex(kind=real64) z8
              z4 = cmplx(1.e0, 2.e0)
              z8 = cmplx(3.e0_real64, 4.e0_real64,kind=real64)
              print *, aimag(z4), aimag(z8)
              ! an elemental function can be passed an array
              print *
              print *, [z4,z4/2.0,z4+z4,z4**3]
              print *
              print *, aimag([z4,z4/2.0,z4+z4,z4**3])
           end program demo_aimag
