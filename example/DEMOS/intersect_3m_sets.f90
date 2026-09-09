    program demo_intersect
    use M_sets, only: unique, intersect, union, setdiff, ismember, setxor
    character(len=*),parameter :: g='(*(g0,1x))'
    integer, allocatable       :: A(:)
    integer, allocatable       :: B(:)

       write(*,g) 'INTERSECT', 'Find the values common to both A and B.'
        A=[7, 1, 7, 7, 4]
        B=[7, 0, 4, 4, 0]
        write(*,g) 'A=', A
        write(*,g) 'B=', B
        write(*,g) intersect(A, B)
        write(*,g) intersect(A, B, setOrder='stable')
    end program demo_intersect
