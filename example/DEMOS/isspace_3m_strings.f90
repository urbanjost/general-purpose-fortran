           program demo_isspace
           use M_strings, only : isspace
           implicit none
           integer                    :: i
           character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
              write(*,'(20(g0,1x))')'ISSPACE: ', &
              & ichar(pack( string, isspace(string) ))
           end program demo_isspace
