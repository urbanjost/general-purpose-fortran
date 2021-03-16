           program demo_scale
           implicit none
             real :: x = 178.1387e-4
             integer :: i = 5
             print *, scale(x,i), x*radix(x)**i
           end program demo_scale
