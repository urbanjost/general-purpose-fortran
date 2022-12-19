      program demo_acosh
      use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
      implicit none
      real(kind=dp), dimension(3) :: x = [ 1.0d0, 2.0d0, 3.0d0 ]
         write (*,*) acosh(x)
      end program demo_acosh
