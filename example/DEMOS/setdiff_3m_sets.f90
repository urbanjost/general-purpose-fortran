     program demo_setdiff
     use M_sets, only: setdiff
     character(len=*),parameter :: g='(*(g0,1x))'
     integer, allocatable      :: A(:)
     integer, allocatable      :: B(:)

        write(*,g) 'SETDIFF','Find the values in A that are not in B.'
         A=[3, 6, 2, 1, 5, 1, 1]
         B=[2, 4, 6]
         write(*,g) 'A=', A
         write(*,g) 'B=', B
         write(*,g) setdiff(A, B)
         write(*,g) setdiff([4, 1, 3, 2, 5], [2, 1], 'sorted')
         write(*,g) setdiff([4, 1, 3, 2, 5], [2, 1], 'stable')

     end program demo_setdiff
