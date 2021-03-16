          program demo_font
          use :: M_draw
          use M_draw,    only  : D_BLACK,   D_WHITE
          use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
          use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
          real    :: left
          real    :: baseline=80.0
          integer :: icolor=1
          integer :: ipaws
             !! set up drawing surface
             call prefsize(400, 400)
             call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
             call page(-100.0, 100.0, -100.0, 100.0)
             call color(D_WHITE)
             call clear()
             call textsize(10.0, 10.0)
             !! place a vertical line along the edge
             call color(D_RED)
             call move2(-90.0, -90.0)
             call draw2(-90.0, 90.0)
             !! make a centered title at top a bit bolder and bigger
             call xcentertext()
             call textsize(13.0, 13.0)
             call linewidth(90)
             left=0
             call nextline('Font Samples')
             !! print the font samples
             left=-90
             call linewidth(40)
             call textsize(10.0, 10.0)
             call centertext(.false.)
             icolor=icolor-1
             call nextline('DEFAULT (ie. futura.l)')
             icolor=icolor-1
             call nextline('now call font(3f) ...')
             call nextline('A nice SIMPLEX font, futura.l')
             call nextline('A COMPLEX font, times.r')
             call nextline('ITALIC letters,  times.i')
             call nextline('DUPLEX is in between, futura.m')
             ipaws=getkey()
             call vexit()
          contains
          subroutine nextline(string)
          character(len=*) :: string
          !! reduce some duplicate code; very specific to this EXAMPLE
             integer :: iend
             iend=index(string,',')  ! if comma, assume font name found
             write(*,*)'FONT=',string(iend+1:),iend
             if(iend.ne.0)call font(trim(adjustl(string(iend+1:)))) ! change font
             icolor=icolor+1         ! set pen color
             call color(icolor)
             baseline=baseline-20    ! move down before drawing line
             call move2(left, baseline)
             call drawstr(string)    ! draw string
          end subroutine nextline

          end program demo_font
