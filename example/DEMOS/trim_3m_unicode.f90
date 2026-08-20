     program demo_trim
     use M_unicode, only : ut=>unicode_type, assignment(=)
     use M_unicode, only : trim, len
     use M_unicode, only : write(formatted)
     implicit none
     type(ut)                   :: str
     type(ut), allocatable      :: strs(:)
     character(len=*),parameter :: brackets='( *("[",DT,"]":,1x) )'
     integer                    :: i
        !
        str='   trailing    '
        print brackets, str,trim(str) ! trims it
        !
        str='   leading'
        print brackets, str,trim(str) ! no effect
        !
        str='            '
        print brackets, str,trim(str) ! becomes zero length
        print *,  len(str), len(trim('               '))
        !
        strs=[ut("Z "),ut(" a b c"),ut("ABC   "),ut("")]
        !
        write(*,*)'untrimmed:'
        print brackets, (strs(i), i=1,size(strs))
        print brackets, strs
        !
        write(*,*)'trimmed:'
        ! everything prints trimmed
        print brackets, (trim(strs(i)), i=1,size(strs))
        print brackets, trim(strs)
        !
     end program demo_trim
