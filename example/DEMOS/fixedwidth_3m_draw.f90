          program demo_fixedwidth
             use M_draw
             implicit none
             real,parameter :: x1=0.0,  x2=40.0,  y1=0.0,  y2=4.0
             real,parameter :: scl=3*0.7
             integer :: idum
          ! set up display
             call prefsize(1000,100)
             call vinit(' ')
             call page(x1,x2,y1,y2)
          ! set font appearance
             call linewidth(200)
             call font("times.rb")
          ! draw a string using proportional and fixed spacing
             call move2(x1+0.3,y1+0.4)
             call textsize(0.8*scl,1.2*scl)
             call color(1)
             call fixedwidth(.false.)
             call drawstr("fixedwidth(.false.)")
             call textsize(0.6*scl,1.2*scl)
             call color(2)
             call fixedwidth(.true.)
             call drawstr(" fixedwidth(.true.)")
          ! wrap up
             idum=getkey()
             call vexit()
          end program demo_fixedwidth
