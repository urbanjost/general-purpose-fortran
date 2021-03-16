           program demo_transfer
           implicit none
             integer :: x = 2143289344
             print *, transfer(x, 1.0)    ! prints "nan" on i686
           end program demo_transfer
