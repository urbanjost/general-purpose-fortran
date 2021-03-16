          program demo_xterm_get_pencolor
          use M_xterm, only : xterm_get_pencolor
          character(len=:),allocatable :: cache
          do i=0,15
             cache=xterm_get_pencolor(i)
             write(*,'(i4.4,1x,a)')i,cache
          enddo
          end program demo_xterm_get_pencolor
