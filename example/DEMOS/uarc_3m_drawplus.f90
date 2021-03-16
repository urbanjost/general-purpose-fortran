          program demo_uarc
          use :: M_draw
          use :: M_drawplus, only : uarc
          integer :: i, idum
          call prefsize(400,400)
          call vinit(' ')
          call color(D_BLACK)
          call clear()
          call linewidth(300)
          call ortho2(-10.0,10.0,-10.0,10.0)
          ! draw square with rounded corners, filled in
          ! yellow, outlined in blue
          call color(D_YELLOW)
          call polyfill(.true.)
          do i=1,2
             call makepoly()
             call move2(-9.0,-7.0)
             call draw2(-9.0, 7.0)
             call uarc(-7.0,7.0,-90.0)
             call draw2(7.0,9.0)
             call uarc( 7.0,7.0,-90.0)
             call draw2(9.0,-7.0)
             call uarc( 7.0,-7.0,-90.0)
             call draw2( -7.0,-9.0)
             call uarc(-7.0,-7.0,-90.0)
             call closepoly()
             call color(D_BLUE)
             call polyfill(.false.)
          enddo
          idum=getkey()
          call vexit()
          end program demo_uarc
