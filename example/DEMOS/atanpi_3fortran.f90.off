      program demo_atanpi
      use, intrinsic :: iso_fortran_env, only : real32, real64
      implicit none
      character(len=*),parameter :: all='(*(g0,1x))'
      real(kind=real64) :: x, y
          x=2.866_real64
          print all, atanpi(x)

          print all, atanpi( 2.0d0, 2.0d0),atanpi( 2.0d0, 2.0d0)*180
          print all, atanpi( 2.0d0,-2.0d0),atanpi( 2.0d0,-2.0d0)*180
          print all, atanpi(-2.0d0, 2.0d0),atanpi(-2.0d0, 2.0d0)*180
          print all, atanpi(-2.0d0,-2.0d0),atanpi(-2.0d0,-2.0d0)*180

      end program demo_atanpi
