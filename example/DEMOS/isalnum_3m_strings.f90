     program demo_isalnum

     use M_strings, only : isalnum, isspace, switch
     implicit none
     character(len=10),allocatable :: string(:)
     character(len=1),allocatable  :: letters(:)
     integer                       :: i
        string=[&
        & '1 2 3 4 5 ' ,&
        & 'letters   ' ,&
        & '1234567890' ,&
        & '<02468>   ' ,&
        & 'has dot.  ' ,&
        & 'both 8787 ' ]
        ! if string is all letters, digits and whitespace return .true.
        do i=1,size(string)
           letters=switch(string(i))
           write(*,'(*(g0))') 'For string['//string(i)//'] ', &
              all( isalnum(letters) .or. isspace(letters) )
        enddo

     end program demo_isalnum
