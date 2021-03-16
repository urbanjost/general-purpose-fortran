           program demo_isalpha
           use M_strings, only : isalpha
           implicit none
           integer                    :: i
           character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
              write(*,'(40(a))')'ISGRAPH: ',pack( string, isalpha(string) )
           end program demo_isalpha
