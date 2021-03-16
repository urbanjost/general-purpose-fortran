           program demo_iand
           implicit none
             integer :: a, b
             data a / z'f' /, b / z'3' /
             write (*,*) iand(a, b)
           end program demo_iand
