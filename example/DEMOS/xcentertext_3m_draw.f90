          program demo_xcentertext
          use M_draw
          implicit none
          real    :: x1, x2, y1, y2
          real    :: scl, ax, bx
          integer :: key
             call prefsize(1200,120)
             call vinit(' ')
             x1=0; x2=40; y1=0; y2=4; scl=1.9
             call page(x1,x2,y1,y2)
             call textsize(0.9*scl,1.4*scl)
             call font("times.rb")
             call linewidth(200)
             AX=(x1+x2)/2+4; BX=y1+1.3
             call move2(AX,BX)
             call xcentertext()
             call color(D_GREEN)
             call drawstr("xcentertext()")
             call color(D_RED)
             call move2(AX-1.0,BX)
             call draw2(AX+1.0,BX)
             call move2(AX,BX-1.0)
             call draw2(AX,BX+1.0)
             call vflush()
             key=getkey()
             call vexit()
          end program demo_xcentertext
