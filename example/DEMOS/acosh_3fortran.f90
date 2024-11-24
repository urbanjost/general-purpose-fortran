      program demo_acosh
      use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
      implicit none
      real(kind=dp), dimension(3) :: x = [ 1.0_dp, 2.0_dp, 3.0_dp ]
         if( any(x.lt.1) )then
            write (*,*) ' warning: values < 1 are present'
         endif
         write (*,*) acosh(x)
      end program demo_acosh
