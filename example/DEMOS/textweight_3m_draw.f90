          program demo_textweight
          use M_draw
          implicit none
          real,parameter :: w=40.0
          integer        :: key
             call prefsize(600,600)
             call vinit(' ')
             call color(D_BLACK)
             call clear()
             call color(D_YELLOW)
             call page(-w,w,-w,w)
             call font("times.rb")
             call linewidth(180)
             call textsize(15.0,15.0)
             call centertext(.true.)
             call linewidth(0);call color(D_BLUE)
             call move2(0.0, W/2.0)

             call textweight(0)
             call drawstr('NORMAL')

             call linewidth(0);call color(D_MAGENTA)
             call move2(0.0, 0.0-W/2.0)

             call textweight(1)
             call drawstr('BOLD')

             call vflush()
             key=getkey()
             call vexit()
          end program demo_textweight
