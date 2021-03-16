          program demo_strlength
          use :: M_pixel
          use :: M_writegif, only : writegif
          implicit none
          real    :: left
          real    :: baseline
          integer :: icolor=0
          real    :: texth=10.0
             !! set up drawing surface
             call prefsize(800, 400)
             call vinit()
             call viewport(0.0, 800.0, 400.0, 0.0)
             call ortho2(-100.0, 300.0, -100.0, 100.0)
             call color(7)
             call clear()
             call linewidth(30)
             call textsize(texth, texth)
             call xcentertext()
             call color(1)

             baseline=85.0
             call move2(0.0,baseline)
             call drawstr('If I Can Stop One Heart')
             baseline= baseline-texth*1.20
             call move2(0.0,baseline)
             call drawstr('by Emily Dickinson')
             call centertext(.false.)

             texth=8.5
             baseline=baseline-texth*1.50
             call textsize(texth, texth)
             left=-90.0

             call nextline('If I can stop one heart from breaking,')
             call nextline('I shall not live in vain;')
             call nextline('If I can ease one life the aching,')
             call nextline('Or cool one pain,')
             call nextline('Or help one fainting robin')
             call nextline('Unto his nest again,')
             call nextline('I shall not live in vain.')

             call writegif('strlength.3m_pixel.gif',P_pixel,P_colormap)
             call execute_command_line('display strlength.3m_pixel.gif')
             call vexit()
          contains
          subroutine nextline(string)
          character(len=*) :: string
          real :: xx
          !! reduce some duplicate code; very specific to this example
             call color(icolor)
             baseline=baseline-texth*1.5    ! move down before drawing line
             call makepoly()
             xx=strlength(string)
             call rect(left,baseline-texth*0.3,left+xx,baseline+texth)
             call closepoly()
             call color(7)
             call move2(left, baseline)
             call drawstr(string)    ! draw string
             icolor=icolor+1         ! set pen color
          end subroutine nextline

          end program demo_strlength
