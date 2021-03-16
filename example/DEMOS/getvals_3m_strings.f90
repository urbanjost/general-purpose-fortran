             program demo_getvals
             use M_strings, only: getvals
             implicit none
             integer,parameter  :: longest_line=256
             character(len=longest_line) :: line
             real               :: values(longest_line/2+1)
             integer            :: ios,icount,ierr
             INFINITE: do
                read(*,'(a)',iostat=ios) line
                if(ios.ne.0)exit INFINITE
                call getvals(line,values,icount,ierr)
                write(*,'(4(g0,1x))')'VALUES=',values(:icount)
             enddo INFINITE
             end program demo_getvals
