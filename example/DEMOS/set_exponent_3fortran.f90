           program demo_setexp
           implicit none
             real :: x = 178.1387e-4
             integer :: i = 17
             print *, set_exponent(x, i), fraction(x) * radix(x)**i
           end program demo_setexp
