       program demo_vexit
       use M_draw, only: vinit, vexit, voutput, circle, linewidth, color
       use M_draw,    only  : D_BLACK,   D_WHITE
       use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
       use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
       implicit none
       integer :: i

       ! assuming you have the NetPBM package installed
       ! set up output to create a GIF file called one.gif
       call voutput('|ppmtogif >vexit.3m_draw.gif')

       call vinit('p6') ! start graphics
       ! default window is -1 <= x <= 1, -1 <= y <= 1
       ! default viewport is left bottom justified square
       ! so essentially you have a square drawing surface
       ! with the point <0,0> at the center of the square
       ! with X and Y ranging from -1 to 1.

       call color(D_RED)
       call linewidth(100)
       ! this loop draws outside the current window
       ! but by default clipping is on
       do i=1,30
          call circle((real(i)-1)*0.04,0.0,1.0/real(i))
       enddo

       call vexit() ! exited graphics so can start again

       ! start second graphics session with new output
       ! device and output file
       call voutput('|ppmtogif >vexit2.3m_draw.gif')
       call vinit('p6')

       do i=10,1,-1
          call color(i)
          call circle(0.0,0.0,1.0/real(i))
       enddo

       call vexit()

       end program demo_vexit
