      program demo_dim
      use, intrinsic :: iso_fortran_env, only : real64
      implicit none
      integer           :: i
      real(kind=real64) :: x

         ! basic usage
          i = dim(4, 15)
          x = dim(4.321_real64, 1.111_real64)
          print *, i
          print *, x

         ! elemental
          print *, dim([1,2,3],2)
          print *, dim([1,2,3],[3,2,1])
          print *, dim(-10,[0,-10,-20])

      end program demo_dim
