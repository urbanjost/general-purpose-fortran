           program demo_isupper
           use M_strings, only : isupper
           implicit none
           integer                    :: i
           character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
              write(*,'(10(g0,1x))')'ISUPPER: ', &
              & ichar(pack( string, isupper(string) ))
              write(*,'(10(g0,1x))')'ISUPPER: ', &
              & pack( string, isupper(string) )
           end program demo_isupper
