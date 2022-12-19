      program demo_all
      implicit none
      logical,parameter :: T=.true., F=.false.
      logical bool
        ! basic usage
         ! is everything true?
         bool = all([ T,T,T ])
         bool = all([ T,F,T ])
         print *, bool

        ! by a dimension
         ARRAYS: block
         integer :: a(2,3), b(2,3)
          ! set everything to one except one value in b
          a = 1
          b = 1
          b(2,2) = 2
          ! now compare those two arrays
          print *,'entire array :', all(a ==  b )
          print *,'compare columns:', all(a ==  b, dim=1)
          print *,'compare rows:', all(a ==  b, dim=2)
        end block ARRAYS

      end program demo_all
