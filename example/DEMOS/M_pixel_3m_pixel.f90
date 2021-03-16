          program demo_M_pixel
          use M_pixel
          use M_writegif, only :  writegif
          use M_pixel,    only : cosd, sind
          implicit none

          integer  :: i
          integer  :: j
          integer  :: icolor

             ! initialize image
             call prefsize(400,400)  ! set size before starting
             call vinit()            ! start graphics
             call clear(0)           ! clear to color 0

             ! put some colored boxes into pixmap by address
             ! so show how the pixel map can be edited easily with
             ! other routines that can manipulate a pixel array.
             ! The P_pixel array was created when vinit(3f) was called
             icolor=1
             do i=1,4
                do j=1,4
                   P_pixel((i-1)*100+1+3:i*100-3,(j-1)*100+1+3:j*100-3)=icolor
                   icolor=icolor+1
                enddo
             enddo

             ! map area of virtual world to pixel array
             ! notice Y-axis for viewport is zero at TOP
                ! viewport(left, right, bottom, top)
             call viewport(0.0,  400.0,  400.0, 0.0)
             ! define the virtual world area we want to work in
                 !ortho2(left, right, bottom,   top)
             call ortho2(0.0,  400.0,    0.0, 400.0)
             ! the drawing routines use these world units

             ! draw polar grids
             call linewidth(100)
             call color(14)
             call target(200.0,200.0,200.0)

             call linewidth(75)
             call color(0)
             call target(100.0,200.0,50.0)

             ! draw some lines
             call color(1)
             call linewidth(200)
             call line(1.0,1.0,400.0,400.0)

             call color(4)
             call line(350.0,200.0,350.0,300.0)

             ! print some text
             call color(1)
             !call hershey(x,y,height,itext,theta,ntext)
             call linewidth(125)
             call hershey(40.0, 40.0,35.0,'Hello World',0.0,11)
             call color(7)
             call linewidth(25)
             call hershey(40.0, 80.0,35.0,'Hello World',0.0,11)
             call linewidth(100)
             call hershey(40.0,120.0,35.0,'Hello World',30.0,11)

             call hershey( 40.0,350.0,35.0,'\COMPLEX\Hello World',0.0,20)
             call hershey( 40.0,310.0,35.0,'\DUPLEX\Hello World',0.0,19)
             call hershey( 350.0,400.0,35.0,'\ITALIC\Hello World',90.0,19)
             call linewidth(50)
             call hershey(200.0,120.0,15.0,'\SIMPLEX\Hello World',20.0,20)

             ! change background color directly
             where (P_pixel.eq.0) P_pixel=9
             ! write standard gif file
             call writegif('M_pixel.3m_pixel.gif',P_pixel,P_ColorMap)

              contains

                    subroutine target(xc,yc,rc)
                    use M_pixel,    only : cosd, sind
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
              end program demo_M_pixel
