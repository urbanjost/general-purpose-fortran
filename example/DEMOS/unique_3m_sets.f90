     program demo_unique
     use M_sets, only: unique
     character(len=*),parameter :: g='(*(g0,1x))'
     integer,allocatable        :: A(:)

        write(*,g) 'UNIQUE','Find the unique elements of vector A.'
         A = [10, -10, 0, 1, 2, 3, 3, 2, 1, -10]
         write(*,g) 'A=', A
         write(*,g) unique(A)
         write(*,g) unique(A, setOrder='stable')

     end program demo_unique
