      program demo_fraction
      implicit none
      real :: x
         x = 178.1387e-4
         print *, fraction(x), x * real(radix(x))**(-exponent(x))
         x = 10.0
         print *, fraction(x)
         print *, fraction(x) * 2**4
      end program demo_fraction
