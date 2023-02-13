       program demo_aton

        use M_strings, only: aton
        implicit none
        character(len=14),allocatable :: strings(:)
        doubleprecision               :: dv
        integer                       :: iv
        real                          :: rv
        integer                       :: i

        ! different strings representing INTEGER, REAL, and DOUBLEPRECISION
        strings=[&
        &' 10.345       ',&
        &'+10           ',&
        &'    -3        ',&
        &'    -4.94e-2  ',&
        &'0.1           ',&
        &'12345.678910d0',&
        &'              ',& ! Note: will return zero without an error message
        &'1 2 1 2 1 . 0 ',& ! Note: spaces will be ignored
        &'WHAT?         ']  ! Note: error messages will appear, zero returned

        do i=1,size(strings)
           write(*,'(a)',advance='no')'STRING:',strings(i)
           if(aton(strings(i),iv)) write(*,'(g0)',advance='no')':INTEGER ',iv
           if(aton(strings(i),rv)) write(*,'(g0)',advance='no')':INTEGER ',rv
           if(aton(strings(i),dv)) write(*,'(g0)',advance='no')':INTEGER ',dv
        enddo

        end program demo_aton
