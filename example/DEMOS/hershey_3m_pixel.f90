          program demo_hershey
          use M_pixel
          use M_writegif_animated, only : write_animated_gif
          implicit none
          integer,parameter :: isize=600
          integer,parameter :: topsym=432
          integer           :: movie(1:topsym,0:isize-1,0:isize-1)
          integer           :: i
          !! set up environment
             call prefsize(isize,isize)
             call vinit()
             call ortho2(-150.0,150.0,-150.0,150.0)

             !! draw all characters using hershey numeric strings
             do i=1,topsym
                !! draw reference circle and crosshairs
                call color(0)
                call clear()

                call color(4)
                call linewidth(100)
                call circle(0.0,0.0,75.0)
                call move2(-75.0,0.0)
                call draw2(75.0,0.0)
                call move2(0.0,-75.0)
                call draw2(0.0,75.0)

                call centertext(.true.)
                call color(7)
                call linewidth(500)
                call textang(3.0*i)
                call textang(0.0)
                call move2(0.0,0.0)
                call textsize(150.0,150.0)
                call drawstr('\',i+1000,'\',nospace=.true.)

                call centertext(.false.)
                call color(1)
                call move2(-120.0,120.0)
                call textsize(10.0,10.0)
                call linewidth(40)
                call drawstr(i+1000,' ')
                movie(i,:,:)=P_pixel
             enddo
             call vexit()
             !! write to file and display with display(1)
             call write_animated_gif('hershey.3m_pixel.gif',&
             & movie,P_colormap,delay=40)
             !call execute_command_line('display hershey.3m_pixel.gif')
          end program demo_hershey
