           program demo_exponent
           implicit none
             real :: x = 1.0
             integer :: i
             i = exponent(x)
             print *, i
             print *, exponent(0.0)
           end program demo_exponent
