program testit
!(LICENSE:PD)
use M_draw, only : vinit, prefsize, clear, getkey, textsize, font, color, linewidth, textjustify
use M_draw, only : vflush, vexit, move2, circle, drawstr, draw2
use M_draw, only : d_right, d_top, d_ycentered, d_bottom, d_xcentered, d_top, d_ycentered, d_left 
use M_draw, only : page
use iso_c_binding
implicit none
real              :: x1=-20, x2=20, y1=-20, y2=20
integer           :: idum
real              :: scl=1.0
character(len=50) :: device

   print*,'Enter output device:'
   read(*,'(a)')device
   select case(trim(device))
   case('x11','X11','p3')
      call prefsize(int(x2-x1)*15,int(y2-y1)*15)
   end select
   call vinit(device)
   call page(x1,x2,y1,y2)
   call clear()
   idum=getkey()
   call textsize(0.9*scl, 1.4*scl)
   call font("times.rb")
   call color(2)
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
   call vflush()
   idum=getkey()
   call vexit()
contains
   subroutine seejustify(string,justify,x,y)
   implicit none
   real                    ::  x, y
   integer(kind=c_short)   ::  justify
   character(len=*)        ::  string
   character(kind=c_char)  ::  byte
      call color(1)
      call move2(x-1.0,y); call draw2(x+1.0,y); call move2(x,y-1.0); call draw2(x,y+1.0)
      call circle(x,y,5.0)
      call color(2)
      call move2(x,y)
      !call textjustify(transfer(justify,byte))
      byte=char(justify)
      !write(*,*)string,justify,ichar(byte)
      call textjustify(byte)
      call drawstr(string)
   end subroutine seejustify
      
end program testit
