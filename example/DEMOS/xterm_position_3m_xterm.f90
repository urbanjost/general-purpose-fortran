          program demo_xterm_position
          use M_xterm, only : xterm_position
          implicit none
          integer :: right, down
          call xterm_position(down=200,right=100)
          end program demo_xterm_position
