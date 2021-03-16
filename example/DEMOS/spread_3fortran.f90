           program demo_spread
           implicit none
             integer :: a = 1, b(2) = [ 1, 2 ]
             write(*,*) spread(a, 1, 2)            ! "1 1"
             write(*,*) spread(b, 1, 2)            ! "1 1 2 2"
           end program demo_spread
