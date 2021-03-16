          program demo_xterm_get_geometry
          use M_xterm, only : xterm_get_geometry
          implicit none
          integer :: irows, icols
             call xterm_get_geometry(irows,icols)
             write(*,*)'rows=',irows,' cols=',icols
          end program demo_xterm_get_geometry
