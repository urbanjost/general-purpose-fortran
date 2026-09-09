      program demo_int
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      implicit none
      integer :: i = 42
      complex :: z = (-3.7, 1.0)
      real :: x=-10.5, y=10.5

         print *, int(x), int(y)

         print *, int(i)

         print *, int(z), int(z,8)
         ! elemental
         print *, int([-10.9,-10.5,-10.3,10.3,10.5,10.9])
         ! note int(3) truncates towards zero

         ! CAUTION:
         ! a number bigger than a default integer can represent
         ! produces an incorrect result and is not required to
         ! be detected by the program.
         x=real(huge(0))+1000.0
         print *, int(x),x
         ! using a larger kind
         print *, int(x,kind=int64),x

         print *, int(&
         & B"111111111111111111111111111111111111111111111111111111111111111",&
         & kind=int64)
         print *, int(O"777777777777777777777",kind=int64)
         print *, int(Z"7FFFFFFFFFFFFFFF",kind=int64)

         ! elemental
         print *
         print *,int([ &
         &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
         &  0.0,   &
         &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

      end program demo_int
