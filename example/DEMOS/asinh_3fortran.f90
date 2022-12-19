      program demo_asinh
      use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
      implicit none
      real(kind=dp), dimension(3) :: x = [ -1.0d0, 0.0d0, 1.0d0 ]

         ! elemental
          write (*,*) asinh(x)

      end program demo_asinh
