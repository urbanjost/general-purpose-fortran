          program demo_textslant
          use M_draw
          implicit none
          real    :: x1, x2, y1, y2
          real    :: scl
          integer :: key
             call prefsize(1200,300)
             call vinit(' ')
             call color(D_BLACK)
             call clear()
             x1=0.0; x2=40.0; y1=0.0; y2=10.0; scl=3*0.7
             call page(x1,x2,y1,y2)
             call font("times.rb")
             call linewidth(180)
             call textsize(0.8*scl,1.2*scl)
             call move2( x1+.3,y1+.4)
             call color(D_RED); call textslant(0.0);  call drawstr("textslant(0.0); ")
             call color(D_GREEN); call textslant(-1.0); call drawstr(" textslant(-1.0);")
             call color(D_BLUE); call textslant(1.0);  call drawstr(" textslant(1.0);")
             call textsize(0.8*scl,1.2*3*scl)
             call move2(x1+.3,y1+3+.4)
             call color(D_MAGENTA); call textslant(1.0); call drawstr(" textslant(1.0);")
             call textsize(0.8*scl,1.2*scl)
             call color(D_CYAN); call textslant(0.3); call drawstr(" textslant(0.3);")
             call color(D_WHITE); call textslant(0.5); call drawstr(" textslant(0.5);")
             call vflush()
             key=getkey()
             call vexit()
          end program demo_textslant
