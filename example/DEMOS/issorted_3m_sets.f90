     program demo_issorted
     use M_sets, only: issorted
     character(len=*),parameter :: g='(*(g0,1x))'
     integer,allocatable        :: A(:)

        write(*,g) 'ISSORTED','Find the issorted elements of vector A.'
         A = [10, -10, 0, 1, 2, 3, 3, 2, 1, -10]
         write(*,g) 'A=', A
         write(*,g) issorted(A)
         A = [-10, 10, 100, 201]
         write(*,g) 'A=', A
         write(*,g) issorted(A)

     end program demo_issorted
