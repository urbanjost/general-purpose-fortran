          program demo_drawchar
          use M_pixel
          use M_writegif_animated, only : write_animated_gif
          implicit none
          integer,parameter :: isize=600
          integer           :: movie(32:124,0:isize-1,0:isize-1)
          integer           :: i
          !! set up environment
          call prefsize(isize,isize)
          call vinit()
          call ortho2(-100.0,100.0,-100.0,100.0)
          call textsize(150.0,150.0)
          call centertext(.true.)

          do i=33,124
             !! draw reference circle and crosshairs
             call linewidth(100)
             call color(0)
             call clear()
             call color(4)
             call circle(0.0,0.0,75.0)
             call move2(-75.0,0.0)
             call draw2(75.0,0.0)
             call move2(0.0,-75.0)
             call draw2(0.0,75.0)
             call color(7)
             call linewidth(200)
             call textang(3.0*i)
             call move2(0.0,0.0)
             call drawchar(char(i))
             movie(i,:,:)=P_pixel
          enddo
          call vexit()
          !! write to file and display with display(1)
          call write_animated_gif('drawchar.3m_pixel.gif',movie,P_colormap)
          call execute_command_line('display drawchar.3m_pixel.gif')
          end program demo_drawchar
