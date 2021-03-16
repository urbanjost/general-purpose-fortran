           program demo_boxtext
           use M_draw,     only : vinit,vexit,prefsize,vgetdev,clear,page
           use M_draw,     only : centertext,polyfill,font,linewidth,color
           use M_draw,     only : getkey
           use M_draw,     only : color,rect,boxtext
           use M_draw,     only : D_BLACK,   D_WHITE
           use M_draw,     only : D_RED,     D_GREEN,    D_BLUE
           use M_draw,     only : D_YELLOW,  D_MAGENTA,  D_CYAN
           implicit none
           real              :: x1=0.0,    x2=40.0,    y1=0.0,    y2=7.0
           real              :: xmin=1.0,  xmax=39.0,  ymin=1.0,  ymax=6.0
           integer           :: idum
              call prefsize(int(x2-x1)*25,int(y2-y1)*25)
              call vinit(' ')
              call page(x1,x2,y1,y2)
              call centertext(.true.)
              call font("times.rb")
              call color(D_GREEN)
              call clear()
              call linewidth(200)
              call color(D_CYAN); call polyfill(.false.); call rect(xmin,ymin,xmax,ymax)
              call color(D_WHITE); call polyfill(.true.);  call rect(xmin,ymin,xmax,ymax)
              call color(D_BLACK)
              call boxtext(xmin,ymin,xmax-xmin,ymax-ymin,"This text is in the box")
              idum=getkey()
              call vexit()
           end program demo_boxtext
