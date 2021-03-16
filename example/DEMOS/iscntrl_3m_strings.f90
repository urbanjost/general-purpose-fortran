           program demo_iscntrl
           use M_strings, only : iscntrl
           implicit none
           integer                    :: i
           character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
              write(*,'(20(g0,1x))')'ISCNTRL: ', &
              & ichar(pack( string, iscntrl(string) ))
           end program demo_iscntrl
