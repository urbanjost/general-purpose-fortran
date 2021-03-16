           program demo_isblank
           use M_strings, only : isblank
           implicit none
           integer                    :: i
           character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
              write(*,'(*(g0,1x))')'ISXBLANK: ',&
              & ichar(pack( string, isblank(string) ))
           end program demo_isblank
