          program demo_newpen
          use M_calcomp
          character(len= 4)  :: ICHR3='ANG='
          character(len= 4)  :: ICHR4=', H='
          character(len= 19) :: ICHR5='ANGULAR LETTER TEST'
          call plots(0.0,10.0,0.0,10.0)
          ! PLOT ANGULAR LETTER TEST
          call plot(4.5,5.5,-3)
          angle=0.0
          height=0.105
          do i=1,8
             call newpen(i)
             rad=0.0174533*angle
             xx=0.5*cos (rad)
             yy=0.5*sin (rad)
             call symbol( xx  , yy  ,height,ichr3,inteq,angle, 4)
             call number(999.0,999.0,height,angle ,angle,-1)
             call symbol(999.0,999.0,height,ichr4,inteq,angle, 4)
             call number(999.0,999.0,height,height,angle, 3)
             height=height+0.035
             angle=angle+45.0
          enddo
          call newpen(1)
          call symbol(-1.4,4.0,0.14,ichr5,inteq,0.0,19)
          call plot( 4.5, 5.0,3)
          call plot(-4.5, 5.0,2)
          call plot(-4.5,-5.5,2)
          call plot( 4.5,-5.5,2)
          call plot( 4.5, 5.0,2)
          !call plot( 6.5,-5.5,-3)
          call plot(0.0,0.0,999)
          end program demo_newpen
