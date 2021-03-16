          program demo_circle
          use M_pixel
          use M_writegif, only : writegif
          implicit none
             !! set up drawing surface
             call prefsize(400,400)
             call vinit()
             call ortho2(left=-100.0, right=100.0, bottom=-100.0, top=100.0)
             call color(3)
             call clear()
             call color(4)
             call linewidth(200)
             !! draw some circles
             call circle(0.0, 0.0, 90.0)
             call color(1)
             call circle(0.0, 0.0, 40.0)
             call color(2)
             call circle(-25.0, 25.0, 20.0)
             call circle(-25.0,-25.0, 20.0)
             call circle( 25.0, 25.0, 20.0)
             call circle( 25.0,-25.0, 20.0)
             !! render the pixel map
             call writegif('circle.3m_pixel.gif',P_pixel,P_colormap)
             !! display the graphic assuming display(1) is available
             call execute_command_line('display circle.3m_pixel.gif')
             !! exit graphics mode
             call vexit()
          end program demo_circle
