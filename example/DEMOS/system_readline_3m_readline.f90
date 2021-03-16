          program demo_system_readline
             use m_readline, only : system_readline
             implicit none
             character(len=256) :: line
             integer            :: cstat
             character(len=256) :: sstat

             write(*,*)' ____________________________________________________________'
             write(*,*)'  Your input lines are now editable using the GNU'
             write(*,*)'  readline(3C) procedure. By default, up-arrow and'
             write(*,*)'  down-arrow go thru the history lines; left and right arrow'
             write(*,*)'  keys and delete and just typing characters let you do'
             write(*,*)'  simple editing. Far more input control is available.'
             write(*,*)'  See the browser pages and man(1) pages for readline(3c).'
             write(*,*)' ____________________________________________________________'
             write(*,*)' Enter text and then edit it. "q" quits; "h" display history:'

             do
                call system_readline(line,'readline>') ! read editable input line
                if(line.eq.'q') stop
                call execute_command_line(trim(line),cmdstat=cstat,cmdmsg=sstat)
             enddo
          end program demo_system_readline
