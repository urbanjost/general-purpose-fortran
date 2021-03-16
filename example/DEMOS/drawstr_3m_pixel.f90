             program demo_drawstr
             use M_pixel
             use :: M_writegif, only : writegif
             implicit none
             call prefsize(400,400)
             call vinit()
             call ortho2(-1.0,1.0,-1.0,1.0)
             ! by default the drawing surface is
             ! a square ranging from -1 to 1 in both
             ! the X and Y axis
             write(*,*)D_BLACK, D_GREEN, D_RED

             call color(D_BLACK)    ! set current color to black
             call clear()           ! clear to current color

             ! SET COMMON TEXT ATTRIBUTES
             call color(D_GREEN)    ! we want to draw in green
             call circle(0.0,0.0,1.0)
             call font('futura.m')  ! set font
             call textsize(0.1,0.1) ! font size

             ! DRAW A STRING
             call move2(-1.0, 0.0)
             call drawstr('Hello')  ! draw string at current position
             ! note that current position is now at end of this string

             ! CHANGE SOME TEXT ATTRIBUTES AGAIN
             call linewidth(20)     ! set line width
             call color(D_RED)      ! change color
             call textang(45.0)     ! change text angle

             call drawstr(' World!')! draw string at current position
             !! render pixel array to a file
             call writegif('drawstr.3m_pixel.gif',P_pixel,P_colormap)
             !! display graphic assuming display(1) is available
             call execute_command_line('display drawstr.3m_pixel.gif')

             call vexit()           !  wrap up and exit graphics mode

             end program demo_drawstr
