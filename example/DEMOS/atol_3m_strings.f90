       program demo_atol

        use iso_fortran_env, only: wp => int64
        use M_strings, only: atol
        implicit none
        character(len=14),allocatable :: strings(:)
        integer(kind=wp)              :: iv
        integer                       :: i

        ! different strings representing whole numbers
        strings=[&
        &'+10           ',&
        &'    -3        ',&
        &'              ',& ! Note: will return zero without an error message
        &'1 2 1 2 1 . 0 ',& ! Note: will just read first value
        &'WHAT?         ']  ! Note: will return zero without an error message

        do i=1,size(strings)
           iv=atol(strings(i))
           write(*,'(*(g0,1x))')'STRING:',strings(i),':VALUE:',iv
        enddo

        end program demo_atol
