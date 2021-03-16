          program demo_textjustify
          use M_draw
          use M_draw,    only  : D_BLACK,   D_WHITE
          use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
          use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
          use iso_c_binding
          implicit none
          real :: x1=-20.0, x2=20.0, y1=-20.0, y2=20.0
             call prefsize(int(x2-x1)*30,int(y2-y1)*30)
             !!call voutput('|ppmtogif >images/textjustify.3M_draw.gif')
             !!call vinit('p6')
             call vinit(' ')
             call page(x1,x2,y1,y2)
             call clear()
             call textsize(0.9, 1.4)
             call font("times.rb")
             call linewidth(20)
             call seejustify( "right|top",           iany([d_right,d_top]),           -10.0, -10.0 )
             call seejustify( "right|ycentered",     iany([d_right,d_ycentered]),     -10.0,   0.0 )
             call seejustify( "right|bottom",        iany([d_right,d_bottom]),        -10.0, +10.0 )
             call seejustify( "xcentered|top",       iany([d_xcentered,d_top]),         0.0, -10.0 )
             call seejustify( "xcentered|ycentered", iany([d_xcentered,d_ycentered]),   0.0,   0.0 )
             call seejustify( "xcentered|bottom",    iany([d_xcentered,d_bottom]),      0.0, +10.0 )
             call seejustify( "left|top",            iany([d_left,d_top]),            +10.0, -10.0 )
             call seejustify( "left|ycentered",      iany([d_left,d_ycentered]),      +10.0,   0.0 )
             call seejustify( "left|bottom",         iany([d_left,d_bottom]),         +10.0, +10.0 )
             call vexit()
          contains
             subroutine seejustify(string,justify,x,y)
                implicit none
                real                    :: x, y
                real                    :: height, width
                integer(kind=c_short)   :: justify
                character(len=*)        :: string
                character(kind=c_char)  :: byte
                call color(D_RED)
                call move2(x-1.0,y); call draw2(x+1.0,y); call move2(x,y-1.0); call draw2(x,y+1.0)
                call circle(x,y,5.0)
                call color(D_BLUE)
                call move2(x,y)
                byte=char(justify)
                call textjustify(byte)
                call drawstr(string)
                call color(D_WHITE)
                call rmove2(-strlength(string),0.0)
                call rdraw2(+strlength(string),0.0)
                call getfontsize(width, height)
                call rmove2(0.0,height)
                call rmove2(-strlength(string),0.0)
                call rdraw2(+strlength(string),0.0)
             end subroutine seejustify
          end program demo_textjustify
