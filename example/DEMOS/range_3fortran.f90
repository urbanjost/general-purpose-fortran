      program demo_range
      use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
      implicit none
      real(kind=sp)    :: x(2)
      complex(kind=dp) :: y
         print *, precision(x), range(x)
         print *, precision(y), range(y)
      end program demo_range
