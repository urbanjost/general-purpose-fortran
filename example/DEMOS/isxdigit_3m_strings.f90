           program demo_isxdigit
           use M_strings, only : isxdigit
           implicit none
           integer                    :: i
           character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
              write(*,'(40(a))')'ISXDIGIT: ',pack( string, isxdigit(string) )
           end program demo_isxdigit
