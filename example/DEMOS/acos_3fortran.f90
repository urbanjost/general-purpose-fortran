          program demo_acos
          use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128
          implicit none
          real(kind=real64) :: x = 0.866_real64
          real(kind=real64),parameter :: D2R=acos(-1.0_real64)/180.0_real64
            write(*,*)'acos(',x,') is ', acos(x)
            write(*,*)'90 degrees is ', d2r*90.0_real64, ' radians'
            write(*,*)'180 degrees is ', d2r*180.0_real64, ' radians'
            write(*,*)'for reference &
            &PI= 3.14159265358979323846264338327950288419716939937510'
           end program demo_acos
