     program demo_isequal
     use M_sets, only: isequal
     character(len=*),parameter :: g='(*(g0,1x))'
     integer,allocatable        :: A(:)
     integer,allocatable        :: B(:)

     write(*,g) 'isequal','Find if A is equal to B. '
         A = [10, -10, 0, 1, 2, 3, 3, 2, 1,-10]
         B = [10, -10, 0, 1, 2, 3, 3, 2, 1, 10]
         write(*,g) 'A=', A
         write(*,g) 'B=', B
         write(*,g) isequal(A,B)
     write(*,g) 'isequal','Find if A is equal to A. '
         write(*,g) isequal(A,A)

     end program demo_isequal
