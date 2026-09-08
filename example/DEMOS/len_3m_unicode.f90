     program demo_len
     use M_unicode, only : assignment(=), ut=>unicode_type, len
     use M_unicode, only : write(formatted)
     implicit none
     type(ut)             :: string
     type(ut),allocatable :: many_strings(:)
     integer                        :: ii
     ! BASIC USAGE
       string='Noho me ka hau’oli' ! (Be happy.)
       ii=len(string)
       write(*,'(DT,*(g0))')string, ' LEN=', ii
     !
       string=' How long is this allocatable string? '
       write(*,'(DT,*(g0))')string, ' LEN=', len(string)
     !
     ! STRINGS IN AN ARRAY MAY BE OF DIFFERENT LENGTHS
       many_strings = [ ut('Tom'), ut('Dick'), ut('Harry') ]
       write(*,'(*(g0,1x))')'length of elements of array=',len(many_strings)
     !
       write(*,'(*(g0))')'length from type parameter inquiry=',string%len()
     !
     ! LOOK AT HOW A PASSED STRING CAN BE USED ...
       call passed(ut(' how long? '))
     !
     contains
     !
     subroutine passed(str)
     type(ut),intent(in) :: str
        ! you can query the length of the passed variable
        ! when an interface is present
        write(*,'(*(g0))')'length of passed value is ', len(str)
     end subroutine passed
     !
     end program demo_len
