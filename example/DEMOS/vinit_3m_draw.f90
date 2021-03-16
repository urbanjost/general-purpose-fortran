             program demo_vinit
             use M_draw
             use ISO_C_BINDING
             integer :: ios
             character(len=50) :: device

             ! read in device name and start graphics mode
             print*,'Enter output device:'
             read(*,'(a)',iostat=ios)device
             if(ios.ne.0)device=' '
             call vinit(device)
             ! by default the drawing surface is
             ! a square ranging from -1 to 1 in both
             ! the X and Y axis

             ! set font to large hardware font
             call font('large')

             ! set current color to black
             call color(D_BLACK)

             ! clear to current color
             call clear()

             ! we want to draw in green
             call color(D_GREEN)

             ! draw a horizontal line at y = 0
             call move2(-1.0, 0.0)
             call draw2(1.0, 0.0)

             ! pause for some input
             idum=getkey()

             ! draw a line along x = 0
             call move2(0.0, 0.0)
             call draw2(0.0, 1.0)

             ! move to the middle of the screen
             call move2(0.0, 0.0)

             ! draw 'Hello' starting at the origin
             call drawstr('Hello')

             ! pause again
             idum=getkey()

             !  wrap up and exit graphics mode
             call vexit()

             end program demo_vinit
