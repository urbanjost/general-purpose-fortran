      program demo_exponent
      implicit none
      real :: x = 1.0
      integer :: i
         i = exponent(x)
         print *, i
         print *, exponent(0.0)
         print *, exponent([10.0,100.0,1000.0,-10000.0])
         print *, 2.0**[1.0,10.0,100.0,-10000.0]
         print *, exponent(huge(0.0))
         print *, exponent(tiny(0.0))
      end program demo_exponent
