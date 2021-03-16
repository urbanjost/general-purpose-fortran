          program demo_state
          use M_pixel
          implicit none
             call prefsize(640,400)
             call vinit()
             call state()
             call vexit()
          end program demo_state
