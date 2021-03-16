          program demo_xterm_colors
          use M_xterm, only : xterm_colors
          call xterm_colors('background','gray')
          call xterm_colors('foreground','black')
          call xterm_colors('cursor','red')
          call xterm_colors('highlight','blue')
          end program demo_xterm_colors
