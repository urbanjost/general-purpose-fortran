          program demo_xterm_pencolor
          use M_xterm, only : xterm_pencolor
          call xterm_pencolor(0,'gray')
          call xterm_pencolor(1,'rgb:000/fff/000')
          call xterm_pencolor(2,'#FF00FF')
          end program demo_xterm_pencolor
