      program demo_transfer
      use,intrinsic :: iso_fortran_env, only : int32, real32
      integer(kind=int32) :: i = 2143289344
      real(kind=real32)   :: x
      character(len=10)   :: string
      character(len=1)    :: chars(10)
         x=transfer(i, 1.0)    ! prints "nan" on i686
         ! the bit patterns are the same
         write(*,'(b0,1x,g0)')x,x ! create a NaN
         write(*,'(b0,1x,g0)')i,i

         ! a string to an array of characters
         string='abcdefghij'
         chars=transfer(string,chars)
         write(*,'(*("[",a,"]":,1x))')string
         write(*,'(*("[",a,"]":,1x))')chars
      end program demo_transfer
