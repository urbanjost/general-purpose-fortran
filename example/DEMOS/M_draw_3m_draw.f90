          program demo_M_draw
          use M_draw
          use M_draw,    only  : D_BLACK,   D_WHITE
          use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
          use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
          use M_units,    only : cosd, sind
          implicit none
          integer  :: ipaws
          real     :: x1, y1
          integer  :: icolor
          integer  :: i,j

             ! initialize image
             call prefsize(400,400)  ! set size before starting
             call vinit(' ')         ! start graphics using device $M_DRAW_DEVICE
             call textsize(10.0,10.0)
             call mapcolor( 0,   255,255,255 )  !white
             call mapcolor( 1,   255,  0,  0 )  !red
             call mapcolor( 2,     0,255,  0 )  !green
             call mapcolor( 3,   255,255,  0 )  !yellow
             call mapcolor( 4,     0,  0,255 )  !blue
             call mapcolor( 5,   255,  0,255 )  !magenta
             call mapcolor( 6,     0,255,255 )  !cyan
             call mapcolor( 7,     0,  0,  0 )  !black
             call mapcolor( 8,     0,155,  0 )
             call mapcolor( 9,   155,155,155 )
             call mapcolor(10,   155,255,255 )
             call mapcolor(11,   155,155,  0 )
             call mapcolor(12,     0,  0,155 )
             call mapcolor(13,   155,  0,155 )
             call mapcolor(14,     0,155,155 )
             call mapcolor(15,   100,100,100 )
             call mapcolor(16,   155,100,100 )
             call color(D_BLACK)
             call clear()            ! clear to color 0
             call color(D_WHITE)

             ! map area of virtual world to specified device area
             ! notice Y-axis for viewport is zero at TOP
             ! define the virtual world area we want to work in
             call page(0.0,  400.0,    0.0, 400.0)
             ! the drawing routines use these world units

             ! put some colored boxes into pixmap by address
             ! so show how the pixel map can be edited easily with
             ! other routines that can manipulate a pixel array.
             ! The P_pixel array was created when vinit(3f) was called
             call polyfill(.true.)
             icolor=1
             do i=0,3
                do j=0,3
                   x1=j*100.0
                   y1=i*100.0
                   icolor=icolor+1
                   call color(icolor)
                   call rect(x1,y1,x1+100.0,y1+100.0)
                enddo
             enddo
             call polyfill(.false.)

             ! draw polar grids
             call linewidth(100)

             call linewidth(100)
             call color(14)
             call target(200.0,200.0,200.0)

             call linewidth(75)
             call color(0)
             call target(100.0,200.0,50.0)

             ! draw some lines
             call color(D_RED)
             call linewidth(200)
             call line(1.0,1.0,400.0,400.0)

             call color(D_BLUE)
             call linewidth(250)
             call line(350.0,200.0,350.0,300.0)

             ! print some text
             call color(1)
             call linewidth(125)
             call font('futura.l')
             call hershey(40.0, 40.0,35.0,'Hello World',0.0)
             call color(7)
             call linewidth(25)
             call hershey(40.0, 80.0,35.0,'Hello World',0.0)
             call linewidth(100)
             call hershey(40.0,120.0,35.0,'Hello World',30.0)

             call hershey(  40.0,350.0,35.0,'Hello World',0.0)
             call font('futura.m')
             call hershey(  40.0,310.0,35.0,'Hello World',0.0)
             call font('times.r')
             call hershey( 350.0,400.0,35.0,'Hello World',90.0)
             call linewidth(50)
             call font('times.i')
             call hershey(200.0,120.0,15.0,'Hello World',20.0)

             ipaws=getkey()
             call vexit()

             contains

             subroutine target(xc,yc,rc)
             use M_units,    only : cosd, sind
             real     :: xc,yc,rc
             integer  :: i
             real     :: x,y
                do i=0,360,10
                   x=rc*cosd(i)
                   y=rc*sind(i)
                   call line(xc,yc,xc+x,yc+y)
                enddo
                do i=1,int(rc),10
                   call circle(xc,yc,real(i))
                enddo
             end subroutine target

             subroutine line(x1,y1,x2,y2)
             real,intent(in) :: x1,y1,x2,y2
             call move2(x1,y1)
             call draw2(x2,y2)
             end subroutine line

             subroutine hershey(x,y,height,itext,theta)
             real,intent(in)               :: x,y
             real,intent(in)               :: height
             character(len=*),intent(in)   :: itext
             real,intent(in)               :: theta
             call move2(x,y)
             call textang(theta)
             call textsize(height,height)
             call drawstr(itext)
             end subroutine hershey
       end program demo_M_draw
