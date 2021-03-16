             program demo_drawstr
             use M_draw
             call vinit('')
             ! by default the drawing surface is
             ! a square ranging from -1 to 1 in both
             ! the X and Y axis

             call color(D_BLACK)    ! set current color to black
             call clear()           ! clear to current color

             ! SET COMMON TEXT ATTRIBUTES
             call color(D_GREEN)    ! we want to draw in green
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
             idum=getkey()          ! pause

             call vexit()           !  wrap up and exit graphics mode

             end program demo_drawstr
