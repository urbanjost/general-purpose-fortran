     program demo_union
     use M_sets, only: union
     character(len=*),parameter :: g='(*(g0,1x))'
     integer,allocatable        :: A(:)
     integer,allocatable        :: B(:)

        write(*,g) 'UNION', 'Find the union of vectors A and B.'
        A=[5, 7, 1]
        B=[3, 1, 1]
        write(*,g) 'A=', A
        write(*,g) 'B=', B
        write(*,g) union(A,B)

        A=[5, 5, 3]
        B=[1, 2, 5]
        write(*,g) 'A=', A
        write(*,g) 'B=', B
        write(*,g) union(A, B, 'sorted')
        write(*,g) union(A, B, 'stable')

     end program demo_union
