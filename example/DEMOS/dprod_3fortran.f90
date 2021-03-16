           program demo_dprod
           use, intrinsic :: iso_fortran_env, only : real_kinds, &
           & real32, real64, real128
           implicit none
           integer,parameter :: dp=kind(0.0d0)
           real :: x = 5.2
           real :: y = 2.3
           real(kind=dp) :: dd
              dd = dprod(x,y)
              print *, dd, x*y, kind(x), kind(dd), kind(dprod(x,y))
              ! interesting comparisons
              print *, 52*23
              print *, 52*23/100.0
              print *, 52*23/100.0d0

              !! common extension is to take doubleprecision arguments
              !! and return higher precision
              bigger: block
              doubleprecision :: xx = 5.2d0
              doubleprecision :: yy = 2.3d0
              real(kind=real128) :: ddd
              !ddd = dprod(xx,yy)
              !print *, ddd, xx*yy, kind(xx), kind(ddd), kind(dprod(xx,yy))
              endblock bigger

           end program demo_dprod
