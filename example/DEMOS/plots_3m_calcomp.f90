          program demo_plots
          use M_calcomp, only : plots, plot, newpen, width, rect
          use M_calcomp, only : black ,red ,green ,yellow
          use M_calcomp, only : purple ,magenta ,cyan ,white
          use M_calcomp, only : MOVE, DRAW, END
          implicit none
          call plots(0.0,10.0,0.0,10.0)          ! initialize graphics
          call width(80)
          call rect(0.0,0.0,10.0,10.0,0.0,GREEN) ! outline plot area
          call plot(5.0,5.0,-3)                  ! set origin
          call newpen(RED)                       ! make X across area
          call plot(-5.0,-5.0, MOVE)
          call plot( 5.0, 5.0, DRAW)
          call plot(-5.0, 5.0, MOVE)
          call plot( 5.0,-5.0, DRAW)
          call plot( 0.0, 0.0, END)
          end program demo_plots
