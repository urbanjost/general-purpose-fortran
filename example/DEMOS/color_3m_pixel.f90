           program demo_color
           use M_pixel
           use M_writegif, only : writegif
           implicit none
           real    :: b=0.5
           real    :: y1,y2,ym,x1,x2
           real    :: width=50.0/8.0,width2
           integer :: i
              !! set up long bar as plotting area
              call prefsize(1000,200)
              call vinit()
              call ortho2(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
              call textsize( 3.5, 4.0)
              call font('DUPLEX')
              call centertext(.true.)
              call linewidth(90)
              y1=-5
              y2=5
              ym=0
              x1=-25+.05*width
              ! draw colored rectangle and a circle and label center of circle
              ! and repeat from colors 0 to 7.
              width2=width*0.95
              do i=0,7
                 call color(i)
                 x2=x1+width2
                 call makepoly()
                 call rect(x1,y1,x2,y2)
                 call closepoly()
                 call color(i+1)
                 call move2((x1+x2)/2.0,ym)
                 call drawstr(i)     ! convert number to string and draw it
                 call circle((x1+x2)/2.0, ym, (x2-x1)/2.10)
                 x1=x1+width
              enddo
              ! write plot as GIF file
              call writegif('color.3m_pixel.gif',P_pixel,P_colormap)
              call vexit()
              ! use system to display GIF file
              call execute_command_line('display color.3m_pixel.gif')
           end program demo_color
