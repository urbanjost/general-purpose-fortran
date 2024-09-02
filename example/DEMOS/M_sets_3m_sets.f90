     program demo_M_sets
     use M_sets, only: unique, intersect, union, setdiff, ismember, setxor, issorted
     character(len=*),parameter :: g='(*(g0,1x))'
     integer, allocatable      :: A(:)
     integer, allocatable      :: B(:)
     integer, allocatable      :: C(:)

        write(*,g) 'UNIQUE','Find the unique elements of vector A.'
         A = [10, -10, 0, 1, 2, 3, 3, 2, 1, -10]
         write(*,g) 'A=', A
         write(*,g) unique(A)
         write(*,g) unique(A, setOrder='stable')
        write(*,g) 'UNION', 'Find the union of vectors A and B.'
         call setab( [5, 7, 1], [3, 1, 1] )
         write(*,g) union(A,B)
         call setab( [5, 5, 3], [1, 2, 5] )
         write(*,g) union(A, B, 'sorted')
         write(*,g) union(A, B, 'stable')
        write(*,g) 'INTERSECT', 'Find the values common to both A and B.'
         call setab( [7, 1, 7, 7, 4], [7, 0, 4, 4, 0] )
         write(*,g) intersect(A, B)
         write(*,g) intersect(A, B, setOrder='stable')
        write(*,g) 'SETDIFF','Find the values in A that are not in B.'
         call setab( [3, 6, 2, 1, 5, 1, 1], [2, 4, 6] )
         write(*,g) setdiff(A, B)
         call setab( [4, 1, 3, 2, 5], [2, 1])
         write(*,g) setdiff(A, B, 'sorted')
         write(*,g) setdiff(A, B, 'stable')
        write(*,g) 'ISMEMBER', 'Determine which elements of A are also in B.'
         call setab( [5,3,4,2], [2,4,4,4,6,8] )
         write(*,g) ismember(A,B)
        write(*,g) 'SETXOR','Find values of A and B not in their intersection.'
         call setab( [5,1,3,3,3], [4,1,2] )
         write(*,g) setxor(A,B)
         write(*,g) setxor(A,B,'stable')

         write(*,g) 'ISSSORTED','confirm whether array is sorted in ascending order or not'
         call setab([1,2,3,4,5],[5,4,3,2,1])
         write(*,g) issorted(A)
         write(*,g) issorted(B)

     contains
     subroutine setab(ain,bin)
     integer,intent(in) :: ain(:)
     integer,intent(in) :: bin(:)
        A=ain
        B=bin
        write(*,g) 'A=', A
        write(*,g) 'B=', B
     end subroutine setab

     end program demo_M_sets
