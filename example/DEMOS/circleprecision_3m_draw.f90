          program demo_circleprecision
          use M_draw
          use M_draw,    only  : D_BLACK,   D_WHITE
          use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
          use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
          real    :: b=0.5
          real    :: y1,y2,ym,x1,x2
          real    :: width=50.0/8.0,width2
          integer,parameter :: ivals(*)=[3,5,7,10,20,30,60,100]
          integer :: i
          integer :: ipaws
             !! set up long bar as plotting area
             call prefsize(1000,200)
             call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
             call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
             call textsize( 2.5/2.0, 3.0/2.0)
             call font('futura.l')
             call centertext(.true.)
             call linewidth(30)
             call color(D_GREEN)
             y1=-5
             y2=5
             ym=0
             x1=-25+.05*width
             ! draw colored rectangle and a circle and label center of circle repeat
             width2=width*0.95
             do i=1,size(ivals)
                x2=x1+width2
                call move2((x1+x2)/2.0,ym)
                call circleprecision(ivals(i))
                call print(ivals(i))     ! convert number to string and draw it
                call circle((x1+x2)/2.0, ym, (x2-x1)/2.10)
                x1=x1+width
             enddo
             ipaws=getkey()
             call vexit()
          end program demo_circleprecision
