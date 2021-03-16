           program demo_maxexponent
           implicit none
             real(kind=4) :: x
             real(kind=8) :: y

             print *, minexponent(x), maxexponent(x)
             print *, minexponent(y), maxexponent(y)
           end program demo_maxexponent
