     program demo_isdigit

     use M_strings, only : isdigit, isspace, switch
     implicit none
     character(len=10),allocatable :: string(:)
     character(len=1),allocatable  :: chars(:)
     character(len=*),parameter    :: g='(*(g0,1x))'
     integer                       :: i

        string=[&
        & '1 2 3 4 5 ' ,&
        & 'letters   ' ,&
        & '1234567890' ,&
        & 'both 8787 ' ]

        ! if string is nothing but digits and whitespace return .true.

        print g,'using ISDIGIT(3) and ISSPACE(3):'
        do i=1,size(string)
           ! convert to array of single characters
           chars=switch(string(i))
           print g, 'For string[',string(i),']', &
           & all( isdigit(chars) .or. isspace(chars) )
        enddo

        ! ALTERNATIVE using VERIFY(3)
        ! the Fortran intrinsic function VERIFY(3) returns a position just
        ! not a logical like C, which can be useful for complex comparisons
        print g,'using VERIFY(3):'
        do i=1,size(string)
           print g, 'For string[',string(i),']', &
           & verify(string(i), "01234567890 ") == 0
        enddo

     end program demo_isdigit
