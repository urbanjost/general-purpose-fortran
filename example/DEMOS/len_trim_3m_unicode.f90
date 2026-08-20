     program demo_len_trim
     use M_unicode, only : ut=>unicode_type, assignment(=)
     use M_unicode, only : len,len_trim
     use M_unicode, only : write(formatted)
     implicit none
     type(ut) :: string
     integer  :: i
     ! basic usage
        string=" how long is this string?     "
        print '(DT)',  string
        print *, 'untrimmed length=',len(string)
        print *, 'trimmed length=',len_trim(string)
        !
        ! print string, then print substring of string
        string='xxxxx   '
        write(*,'(*(DT))')string,string,string
        i=len_trim(string)
        print '(*(DT))',string%sub(1,i),string%sub(1,i),string%sub(1,i)
        !
        ! elemental example
        ele:block
        ! an array of strings may be used
        type(ut),allocatable :: tablet(:)
        tablet=[ &
        & ut(' how long is this string?     '),&
        & ut('and this one?')]
           write(*,*)'untrimmed length=  ',len(tablet)
           write(*,*)'trimmed length=    ',len_trim(tablet)
           write(*,*)'sum trimmed length=',sum(len_trim(tablet))
        endblock ele
        !
     end program demo_len_trim
