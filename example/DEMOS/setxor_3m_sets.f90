     program demo_setxor
     use M_sets, only: setxor
     character(len=*),parameter :: g='(*(g0,1x))'
     integer, allocatable       :: A(:)
     integer, allocatable       :: B(:)

        write(*,g) 'SETXOR','Find values of A and B not in their intersection.'
        A = [5,1,3,3,3]
        B = [4,1,2]
        write(*,g) 'A=', A
        write(*,g) 'B=', B
        write(*,g) setxor(A,B)
        write(*,g) setxor(A,B,'stable')

     end program demo_setxor
