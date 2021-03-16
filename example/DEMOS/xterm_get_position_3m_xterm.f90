          program demo_xterm_get_position
          use M_xterm, only : xterm_get_position
          implicit none
          integer :: iright, idown
             call xterm_get_position(iright,idown)
             write(*,*)'right=',iright,' down=',idown
          end program demo_xterm_get_position
