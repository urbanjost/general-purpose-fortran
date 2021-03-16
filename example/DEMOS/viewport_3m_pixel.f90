          program demo_viewport
          use :: M_pixel
          use :: M_writegif, only : writegif
          implicit none
             call prefsize(400, 400) ! set up drawing surface
             call vinit()
             call color(7)
             call linewidth(40)
             call clear()
             call ortho2(-88.0, 88.0, -88.0, 88.0)
             ! draw the same circle, just changing viewport

             call viewport(   0.0, 200.0,   0.0, 200.0 ); call draw_circle(1)
             call viewport( 200.0, 400.0,   0.0, 200.0 ); call draw_circle(2)
             call viewport(   0.0, 200.0, 200.0, 400.0 ); call draw_circle(3)
             call viewport( 200.0, 400.0, 200.0, 400.0 ); call draw_circle(4)
             call viewport( 250.0, 350.0, 150.0, 300.0 ); call draw_circle(5)

             call writegif('viewport.3m_pixel.gif',P_pixel,P_colormap)
             !call execute_command_line('display viewport.3m_pixel.gif')
             call vexit()
          contains
          subroutine draw_circle(icolor)
          integer,intent(in) :: icolor
             call color(0)
             call rect(-88.0,-88.0,88.0,88.0)
             call color(icolor)
             call makepoly()
             call circle(0.0,0.0,88.0)
             call closepoly()
          end subroutine draw_circle
          end program demo_viewport
