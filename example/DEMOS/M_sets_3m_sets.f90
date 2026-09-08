     program demo_M_sets
     use M_sets, only: &
     & unique, intersect, union, setdiff, setxor, bool, &
     & ismember, issorted, isequal
     character(len=*),parameter :: all='(*(g0,1x))'
     character(len=*),parameter :: nl=new_line('A')
     integer, allocatable      :: A(:)
     integer, allocatable      :: B(:)
     integer, allocatable      :: C(:)

        A = [10, -10, 0, 1, 2, 3, 3, 2, 1, -10]
        !
        print all                                                   ,nl, &
        'UNIQUE','Find the unique elements of vector A.'            ,nl, &
        'A=', A                                                     ,nl, &
        'sorted=',unique(A)                                         ,nl, &
        'stable=',unique(A, setOrder='stable')

        A=[5, 7, 1]
        B=[3, 1, 1]
        !
        print all                                                   ,nl, &
        'UNION', 'Find the union of vectors A and B.'               ,nl, &
        'A=', A                                                     ,nl, &
        'B=', B                                                     ,nl, &
        'sorted=',union(A, B, 'sorted')                             ,nl, &
        'stable=',union(A, B, 'stable')

        A=[7, 1, 7, 7, 4]
        B=[7, 0, 4, 4, 0]
        !
        print all                                                   ,nl, &
        'INTERSECT', 'Find the values common to both A and B.'      ,nl, &
        'A=', A                                                     ,nl, &
        'B=', B                                                     ,nl, &
        'sorted=',intersect(A, B)                                   ,nl, &
        'stable=',intersect(A, B, setOrder='stable')

        A=[3, 6, 2, 1, 5, 1, 1]
        B=[2, 4, 6]
        !
        print all                                                   ,nl, &
        'SETDIFF','Find the values in A that are not in B.'         ,nl, &
        'A=', A                                                     ,nl, &
        'B=', B                                                     ,nl, &
        'sorted=',setdiff(A, B, 'sorted')                           ,nl, &
        'stable=',setdiff(A, B, 'stable')

        A=[5,3,4,2]
        B=[2,4,4,4,6,8]
        !
        print all                                                   ,nl, &
        'ISMEMBER','Determine which elements of A are also in B.'   ,nl, &
        'A=', A                                                     ,nl, &
        'B=', B                                                     ,nl, &
        'in A and B=',ismember(A,B)

        A=[5,1,3,3,3]
        B=[4,1,2]
        !
        print all                                                   ,nl, &
        'SETXOR'                                                       , &
        'Find values of A and B not in their intersection.'         ,nl, &
        'A=', A                                                     ,nl, &
        'B=', B                                                     ,nl, &
        'sorted=',setxor(A,B)                                       ,nl, &
        'stable=',setxor(A,B,'stable')

        A=[1,2,3,4,5]
        B=[5,4,3,2,1]
        !
        print all                                                   ,nl, &
        'ISSSORTED'                                                    , &
        'confirm whether array is sorted in ascending order or not' ,nl, &
        'A=', A                                                     ,nl, &
        'B=', B                                                     ,nl, &
        'is A sorted?',issorted(A)                                  ,nl, &
        'is B sorted?',issorted(B)

        A=[1,2,3,4,5]
        B=[1,2,3,4,5]
        !
        print all                                                   ,nl, &
        'ISEQUAL'                                                     , &
        'confirm whether sets have same elements in same order    ' ,nl, &
        'A=', A                                                     ,nl, &
        'B=', B                                                     ,nl, &
        'is A equal to B?',isequal(A,B)                             ,nl, &
        'is B equal to -B?',isequal(A,-B)

        A=[1,2,3,4,5]
        B=[5,2,3,4,1]
        !
        print all                                                   ,nl, &
        'BOOL'                                                         , &
        'if logical expression is true, 0 if false.'                ,nl, &
        'A=', A                                                     ,nl, &
        'B=', B                                                     ,nl, &
        'is A(i) = B(i) ?',bool(A==B)                               ,nl, &
        'how many elements are the same?',sum(bool(A==B))

     end program demo_M_sets
