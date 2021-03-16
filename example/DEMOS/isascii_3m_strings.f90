           program demo_isascii
           use M_strings, only : isascii
           implicit none
           integer                    :: i
           character(len=1),parameter :: string(*)=[(char(i),i=0,255)]
              write(*,'(10(g0,1x))')'ISASCII: ', &
              & ichar(pack( string, isascii(string) ))
           end program demo_isascii
