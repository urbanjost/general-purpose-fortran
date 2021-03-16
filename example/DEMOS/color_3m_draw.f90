           program demo_color
           use M_draw
           real    :: b=0.5
           real    :: y1,y2,ym,x1,x2
           real    :: width=50.0/8.0,width2
           integer :: i
           integer :: ipaws
              !! set up long bar as plotting area
              call prefsize(1000,200)
              call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
              call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
              call textsize( 3.5, 4.0)
              call font('futura.m')
              call centertext(.true.)
              call linewidth(90)
              y1=-5
              y2=5
              ym=0
              x1=-25+.05*width
              ! draw colored rectangle and a circle and label center of circle
              ! and repeat from colors 0 to 7.
              width2=width*0.95
              call linewidth(40)
              do i=0,7
                 call color(i)
                 x2=x1+width2
                 call polyfill(.true.)
                 call makepoly()
                 call rect(x1,y1,x2,y2)
                 call closepoly()
                 call color(mod(i+1,7)+1)
                 call move2((x1+x2)/2.0,ym)
                 call print(i)     ! convert number to string and draw it
                 call polyfill(.false.)
                 call circle((x1+x2)/2.0, ym, (x2-x1)/2.10)
                 x1=x1+width
              enddo
              ipaws=getkey()
              call vexit()
           end program demo_color
