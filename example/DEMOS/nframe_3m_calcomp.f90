              program demo_nframe
              use M_calcomp
              implicit none
              !
              ! Perform initialization
              call plots(0.0,10.0,0.0,10.0)
              !
              ! Establish origin for first plot (Negative Y values up to -0.5 are
              ! now permitted also)
              call plot( 0.0, 0.5, -3)
              !
              ! Draw a box inside of which all lines will appear
              ! but notice plot frame size now includes the offset plus this box size
              ! Plot frame size = maximum coordinate value used + offset
              ! Plot frame size in the X-direction is 8 inches
              ! Plot frame size in the Y-direction is 9.5 inches (0.5 offset in PLOT
              ! call above
              call plot( 8.0, 0.0, 2)
              call plot( 8.0, 9.0, 2)
              call plot( 0.0, 9.0, 2)
              call plot( 0.0, 0.0, 2)
              !
              ! Calls to generate first plot go here
              ! .
              ! .
              ! .
              ! Terminate first plot
              call nframe()
              !
              ! Establish origin for second plot
              call plot(1.0, 2.0, -3)
              ! Plot frame size in the X-direction is 6 inches
              ! Plot frame size in the Y-direction is 6 inches
              call plot(5.0, 0.0, 2)
              call plot(5.0, 4.0, 2)
              call plot(0.0, 4.0, 2)
              call plot(0.0, 0.0, 2)
              !
              ! Calls to generate second plot go here
              ! .
              ! .
              ! .
              ! Close the plot file
              call plot(0.0, 0.0, 999)
              end program demo_nframe
