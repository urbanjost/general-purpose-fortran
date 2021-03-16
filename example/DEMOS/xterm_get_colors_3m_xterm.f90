          program demo_xterm_get_colors
          use M_xterm, only : xterm_get_colors
          character(len=:),allocatable :: cache

          cache=xterm_get_colors('background')
          write(*,'("BACKGROUND:",a)')cache

          cache=xterm_get_colors('foreground')
          write(*,'("FOREGROUND:",a)')cache

          cache=xterm_get_colors('cursor')
          write(*,'("CURSOR    :",a)')cache

          cache=xterm_get_colors('highlight')
          write(*,'("HIGHLIGHT :",a)')cache

          end program demo_xterm_get_colors
