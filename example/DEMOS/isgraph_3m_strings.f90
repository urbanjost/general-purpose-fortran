          program demo_isgraph
          use M_strings, only : isgraph
          implicit none
          integer                    :: i
          character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
             write(*,'(40(a))')'ISGRAPH: ',pack( string, isgraph(string) )
          end program demo_isgraph
