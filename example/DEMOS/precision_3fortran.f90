           program demo_precision
           implicit none
             real(kind=4) :: x(2)
             complex(kind=8) :: y

             print *, precision(x), range(x)
             print *, precision(y), range(y)
           end program demo_precision
