          program demo_fit
          use M_calcomp, only : plots, plot, newpen, width, fit
          use M_calcomp, only : black ,red ,green ,yellow
          use M_calcomp, only : purple ,magenta ,cyan ,white
          implicit none
          integer,parameter  :: MOVE=3, DRAW=2
          integer            :: i
          real               :: x(3)=[-3.0,1.0,4.4],y(3)=[3.2,1.0,-4.0]
          call plots(0.0,10.0,0.0,10.0)      ! initialize graphics
          call plot(5.0,5.0,-3)              ! set origin
          call newpen(green)
          call crosshair(0.0,0.0,2.0)        ! draw a crosshair at origin point <0,0>
          call width(30); call newpen(red)
          do i=1,size(x)                     ! mark the points
             call crosshair(x(i),y(i),0.2)
          enddo
          x=[-3.0, 1.0, 4.4]
          y=[ 3.2, 1.0,-4.0]
          call width(80); call newpen(yellow)
          call fit(x(2),y(2),x(3),y(3),x(1),y(1))   ! draw in wrong order
          call width(40);call newpen(magenta)
          call fit(x(2),y(2),x(1),y(1),x(3),y(3))   ! draw in wrong order
          call width(140); call newpen(green)
          call fit(x(1),y(1),x(2),y(2),x(3),y(3))   ! draw in right order to get fit
          call plot(0.0,0.0,999)                    ! terminate graphics and pause
          contains
          subroutine crosshair(x,y,s)
          real,intent(in) :: x,y,s
              call plot(x+s,y    ,MOVE)
              call plot(x-s,y    ,DRAW)
              call plot(x    ,y+s,MOVE)
              call plot(x    ,y-s,DRAW)
          end subroutine crosshair
          end program demo_fit
