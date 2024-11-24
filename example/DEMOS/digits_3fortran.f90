      program demo_digits
      implicit none
      character(len=*),parameter :: all='(*(g0:,1x))'
      integer                    :: i = 12345
      real                       :: x = 3.143
      doubleprecision            :: y = 2.33d0
         print all, 'default integer:        ', digits(i)
         print all, 'default real:           ', digits(x)
         print all, 'default doubleprecision:', digits(y)
      end program demo_digits
