          program demo_circl
          use M_calcomp, only : plots, plot, newpen, circl
          implicit none
          character(len=:),allocatable :: lines(:)
          integer                      :: i
          integer                      :: ipen
          real                         :: xstart, ystart
          real                         :: start_angle, finish_angle
          real                         :: start_radius, finish_radius
          real                         :: dash_code
          integer,parameter            :: MOVE=3, DRAW=2
          lines=[character(len=80) :: &
          '#--------#--------#--------#--------#--------#--------#--------#', &
          '# xstart ! ystart !strt_ang! end_ang! start_r!  end_r !dashcode ', &
          '# BIG CIRCLES                                                   ', &
          '!4.50    ! 2.5    !0.0     !360.0   !2.00    !2.00    !0.0     !', &
          '!4.00    ! 2.5    !0.0     !360.0   !1.50    !1.50    !0.0     !', &
          '!3.50    ! 2.5    !0.0     !360.0   !1.00    !1.00    !0.0     !', &
          '# LONG SPIRAL                                                   ', &
          '!5.00    !-2.5    !0.0     !1440.0  !2.50    !0.25    !0.0     !', &
          '# SPIRAL WITH DASHED LINE                                       ', &
          '!-1.75   ! 2.5    !0.0     !360.0   !0.75    !0.25    !1.0     !', &
          '# CIRCULAR ARC                                                  ', &
          '!-2.50   !-2.5    !0.0     !180.0   !0.85    !0.85    !0.0     !', &
          '!-2.50   !-2.5    !-45.0   !-90.0   !0.85    !0.85    !0.0     !', &
          '#--------#--------#--------#--------#--------#--------#--------#']
          call plots(0.0,10.0,0.0,10.0)      ! initialize graphics
          call plot(5.0,5.0,-3) ! set origin
          ! draw a crosshair at origin point <0,0>
          call crosshair(0.0,0.0,2.0)
          ! draw some circles using center and radius
          call circle(-2.5,-2.5,2.5)
          call circle( 2.5, 2.5,2.5)
          call circle( 2.5,-2.5,2.5)
          call circle(-2.5, 2.5,2.5)
          ! box around 10x10 area
          call plot(-5.0,-5.0, MOVE)
          call plot( 5.0,-5.0, DRAW)
          call plot( 5.0, 5.0, DRAW)
          call plot(-5.0, 5.0, DRAW)
          call plot(-5.0,-5.0, DRAW)
          ! call the values from the table
          do i = 1,size(lines)
             write(*,'(a)')lines(i)
             ipen=mod(i,8)
             ipen=merge(5,ipen,ipen.eq.0)
             call newpen(ipen)
             if(index(lines(i),'#').ne.0)cycle
             read(lines(i),'(7(1x,f8.3))') xstart,ystart, &
                start_angle,finish_angle, &
                start_radius,finish_radius, &
                dash_code
             call circl(xstart,ystart, &
                start_angle,finish_angle, &
                start_radius,finish_radius, &
                dash_code)
          enddo
          call plot(0.0,0.0,999) ! end graphics
          contains
          subroutine crosshair(x,y,s)
          real,intent(in) :: x,y
          real,intent(in) :: s
              call plot(x+s,y    ,MOVE)
              call plot(x-s,y    ,DRAW)
              call plot(x    ,y+s,MOVE)
              call plot(x    ,y-s,DRAW)
          end subroutine crosshair
          subroutine circle(x,y,r)
          real,intent(in) :: x,y  ! center
          real,intent(in) :: r    ! radius
             call crosshair(x,y,0.2)
             call circl(x+r,y,0.0,360.0,r,r,0.0)
          end subroutine circle
          end program demo_circl
