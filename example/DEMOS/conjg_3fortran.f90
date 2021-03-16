           program demo_conjg
           use, intrinsic :: iso_fortran_env, only : real_kinds, &
           & real32, real64, real128
           implicit none
           complex :: z = (2.0, 3.0)
           complex(kind=real64) :: dz = (&
           &  1.2345678901234567_real64, &
           & -1.2345678901234567_real64)
               z= conjg(z)
               print *, z
               dz = conjg(dz)
               print *, dz
           end program demo_conjg
