           program demo_islower
           use M_strings, only : islower
           implicit none
           integer                    :: i
           character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
              write(*,'(15(g0,1x))')'ISLOWER: ', &
              & ichar(pack( string, islower(string) ))
              write(*,'(15(g0,1x))')'ISLOWER: ', &
              & pack( string, islower(string) )
           end program demo_islower
