          program demo_font
          use :: M_pixel
          use :: M_writegif, only : writegif
          implicit none
          real    :: left
          real    :: baseline=80.0
          integer :: icolor=1
             !! set up drawing surface
             call prefsize(400, 400)
             call vinit()
             call viewport(0.0, 400.0, 400.0, 0.0)
             call ortho2(-100.0, 100.0, -100.0, 100.0)
             call color(7)
             call clear()
             call textsize(10.0, 10.0)
             !! place a vertical line along the edge
             call color(1)
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
             call linewidth(0)
             call textsize(10.0, 10.0)
             call centertext(.false.)
             icolor=icolor-1
             call nextline('DEFAULT (ie. futura.l)')
             icolor=icolor-1
             call nextline('now call font(3f) ...')
             call nextline('SIMPLEX, or futura.l')
             call nextline('COMPLEX, or times.r')
             call nextline('ITALIC, or times.i')
             call nextline('DUPLEX, or futura.m')
             call writegif('font.3m_pixel.gif',P_pixel,P_colormap)
             !call execute_command_line('display font.3m_pixel.gif')
             call vexit()
          contains
          subroutine nextline(string)
          character(len=*) :: string
          !! reduce some duplicate code; very specific to this example
          integer :: iend
             iend=index(string,',')  ! if comma, assume font name found
             if(iend.ne.0)call font(string(:iend-1)) ! change font
             icolor=icolor+1         ! set pen color
             call color(icolor)
             baseline=baseline-20    ! move down before drawing line
             call move2(left, baseline)
             call drawstr(string)    ! draw string
          end subroutine nextline

          end program demo_font
