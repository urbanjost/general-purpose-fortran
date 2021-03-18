          program demo_width
             use M_calcomp
             implicit none
             integer :: i,n, ii
             real    :: angle, rad, xx, yy
             call plots(0.0,10.0,0.0,10.0)
             call plot(5.0,5.0,-3)
             angle=0.0
             n=30
             do i=1,n
                ii=360/n
                call width(i*10)
                rad=0.0174533*angle
                xx=5.0*cos (rad)
                yy=5.0*sin (rad)
                call plot( 0.0, 0.0,3)
                call plot(  xx,  yy,2)
                angle=angle+360.0/n
             enddo
             call plot(0.0,0.0,999)
          end program demo_width
