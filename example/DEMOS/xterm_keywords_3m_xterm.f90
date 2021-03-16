           program demo_xterm_keywords
           use M_xterm, only : xterm_keywords
           implicit none
              call xterm_keywords('iconify')
              write(*,*)'do some stuff'
              call xterm_keywords('uniconify')
              call xterm_keywords('raise')
           end program demo_xterm_keywords
