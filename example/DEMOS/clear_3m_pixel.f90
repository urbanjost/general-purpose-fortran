          program demo_clear
          use :: M_pixel
          use :: M_writegif, only : writegif
          implicit none
          real,parameter :: x=400.0, y=400.0
             call prefsize(int(x), int(y)) ! set up drawing surface
             call vinit()
             call color(1)
             call linewidth(300)
             ! clear a circle and rectangle in default window and viewport
             call rect(0.0,0.0,x,y)
             call circle(x/2.0,y/2.0,x/2.0)
             ! now clear screen to current color
             call color(3)
             call clear()
             ! gif should be blank
             call writegif('clear.3m_pixel.gif',P_pixel,P_colormap)
             call execute_command_line('display clear.3m_pixel.gif')
             call vexit()
          end program demo_clear
