           program demo_digits
           implicit none
               integer :: i = 12345
               real :: x = 3.143
               doubleprecision :: y = 2.33d0
               print *,'default integer:        ', digits(i)
               print *,'default real:           ', digits(x)
               print *,'default doubleprecision:', digits(y)
           end program demo_digits
