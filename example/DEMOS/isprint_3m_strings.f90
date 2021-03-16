          program demo_isprint
          use M_strings, only : isprint
          implicit none
          integer                    :: i
          character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
             write(*,'(40(a))')'ISPRINT: ',pack( string, isprint(string) )
          end program demo_isprint
