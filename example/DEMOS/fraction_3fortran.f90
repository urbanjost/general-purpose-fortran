           program demo_fraction
           implicit none
             real :: x
             x = 178.1387e-4
             print *, fraction(x), x * radix(x)**(-exponent(x))
           end program demo_fraction
