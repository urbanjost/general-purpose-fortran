          program demo_xterm_geometry
          use M_xterm, only : xterm_geometry
          implicit none
          integer :: ios
          integer :: rows, cols
          write(*,'(a)',advance='no')'Enter rows and columns: '
          read(*,*,iostat=ios)rows,cols
          if(ios.eq.0)then
             call xterm_geometry(rows,cols)
          endif
          end program demo_xterm_geometry
