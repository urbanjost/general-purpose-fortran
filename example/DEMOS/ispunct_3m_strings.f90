           program demo_ispunct
           use M_strings, only : ispunct
           implicit none
           integer                    :: i
           character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
              write(*,'(20(g0,1x))')'ISPUNCT: ', &
              & ichar(pack( string, ispunct(string) ))
              write(*,'(20(g0,1x))')'ISPUNCT: ', &
              & pack( string, ispunct(string) )
           end program demo_ispunct
