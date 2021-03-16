          program demo_elips
          use M_calcomp, only : plots, plot, newpen, factor, nframe, elips
          implicit none
          character(len=:),allocatable :: lines(:)
          integer                  :: i
          real                     :: x, y
          real                     :: a
          real                     :: major_axis_length, minor_axis_length
          real                     :: start_angle, finish_angle
          integer                  :: ipen
          integer,parameter        :: END=999, MOVE=3, DRAW=2
          lines=[character(len=80) :: &
          '#---------------------------------------------------------------------#-',&
          '#  move over in the x direction the same amount you change axis length  ',&
          '#---------#---------#---------#---------#---------#---------#---------#-',&
          '|6.5      !8.0      !0.5      !0.7      ! 0.0     ! 0.0     ! 360.0   !3',&
          '|6.6      !8.0      !0.6      !0.6      ! 0.0     ! 0.0     ! 360.0   !3',&
          '|6.7      !8.0      !0.7      !0.5      ! 0.0     ! 0.0     ! 360.0   !3',&
          '|6.8      !8.0      !0.8      !0.4      ! 0.0     ! 0.0     ! 360.0   !3',&
          '|6.9      !8.0      !0.9      !0.3      ! 0.0     ! 0.0     ! 360.0   !3',&
          '|7.0      !8.0      !1.0      !0.2      ! 0.0     ! 0.0     ! 360.0   !3',&
          '#---------#---------#---------#---------#---------#---------#---------#-',&
          '#  different end angles of different signs                              ',&
          '#---------#---------#---------#---------#---------#---------#---------#-',&
          '|5.0      !8.0      !1.0      !0.2      ! 0.0     ! 0.0     !  45.0   !3',&
          '|3.0      !8.0      !1.0      !0.2      ! 0.0     ! 0.0     ! -60.0   !3',&
          '#---------#---------#---------#---------#---------#---------#---------#-',&
          '# circles                                                               ',&
          '#---------#---------#---------#---------#---------#---------#---------#-',&
          '#---------#---------#---------#---------#---------#---------#---------#-',&
          '#  end of values to call ELIPS(3f) with                                 ',&
          '#---------------------------------------------------------------------#-']
             call plots(0.0,10.0,0.0,10.0)
             do i=1,size(lines)
                write(*,'(a)')lines(i)
                if( lines(i).eq.' ' .or. index(lines(i),'#').ne.0 )cycle
                read(lines(i),9012) x,y, &
                                & major_axis_length,minor_axis_length, &
                                & a, &
                                & start_angle,finish_angle, &
                                & ipen
                9012 format(7(1X,F9.3),1X,I1)
                call elips(x,y, &
                                & major_axis_length,minor_axis_length, &
                                & a, &
                                & start_angle,finish_angle, &
                                & ipen)
             enddo
             call nframe()
             call plot(0.0,0.0,END)
          end program demo_elips
