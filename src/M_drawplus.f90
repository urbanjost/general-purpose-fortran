










!>
!!##NAME
!!    M_drawplus(3f) - [M_drawplus] Additional routines using the M_DRAW graphics library
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!  Usage:
!!
!!      use M_drawplus, only : spirograph ,call_draw ,draw_interpret
!!      use M_drawplus, only : arrowhead ,plain_rect ,rdpnt ,rdbox ,barcode
!!      use M_drawplus, only : uconic ,ellipse ,uarc ,polyline2, seefont
!!      use M_drawplus, only : smoot, ismoo,ismoo1,ismoo2,ismoo3,perin
!!      use M_drawplus, only : ismoo,ismoo1,ismoo2,ismoo3, perin
!!
!!##DESCRIPTION
!!    M_drawplus(3f) is a collection of higher level graphic routines that call the
!!    base M_draw(3f) graphics routines.
!!
!!##LIBRARY FUNCTION DESCRIPTIONS
!!
!!    DEVICE ROUTINES
!!
!!    ROUTINES FOR SETTING UP WINDOWS
!!
!!    CLIPPING ROUTINES
!!
!!    COLOR ROUTINES
!!
!!    INTERACTIVE ROUTINES
!!
!!        subroutine rdbox(returnx1,returny1,returnx2,returny2,ikey)
!!        reads two points and outline defined box and return points
!!        subroutine rdpnt(oldx,oldy,sx,sy,ibut)
!!        reads coordinates of point locator clicked at
!!
!!    FLUSHING
!!
!!    PROJECTION AND VIEWPORT ROUTINES
!!
!!    MATRIX STACK ROUTINES
!!
!!    VIEWPOINT ROUTINES
!!
!!    MOVE ROUTINES
!!
!!    LINESTYLE ROUTINES
!!
!!    DRAW ROUTINES
!!
!!        subroutine polyline2(arrx(:),arry(:))
!!
!!        subroutine arrowhead(x1,y1,x2,y2,size,idraw)
!!
!!    ARCS AND CIRCLES
!!
!!        subroutine uarc
!!
!!    CURVE ROUTINES
!!
!!        subroutine uconic(x,y,p,e,theta1,theta2,orientation)
!!
!!        subroutine ellipse(xpage,ypage,rmaj,rmin,angle,th0,thf,ipen)
!!
!!        subroutine spirograph(xcenter,ycenter,sunr0,planet0,offset0,radius,ilines,ang,angs,ifill)
!!
!!        subroutine smoot, ismoo,ismoo1,ismoo2,ismoo3,perin
!!
!!    RECTANGLES AND GENERAL POLYGON ROUTINES
!!
!!        subroutine plain_rect(x1,y1,x2,y2)
!!
!!    TEXT ROUTINES
!!
!!        subroutine seefont(fontin)
!!
!!    TRANSFORMATIONS ROUTINES
!!
!!    PATCH ROUTINES
!!
!!    POINT ROUTINES
!!
!!    OBJECT ROUTINES
!!
!!    DOUBLE BUFFERING
!!
!!    POSITION ROUTINES
!!
!!    HIGH LEVEL ROUTINES
!!
!!        subroutine barcode
!!
!!        subroutine call_draw
!!
!!        subroutine draw_interpret
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
module M_drawplus
use ISO_C_BINDING
use M_draw
use M_journal, only : journal

implicit none
private

public  :: spirograph
public  :: call_draw
public  :: draw_interpret
public  :: arrowhead
public  :: plain_rect
public  :: rdpnt
public  :: rdbox

public  :: barcode
public  :: uconic
public  :: ellipse
public  :: uarc
public  :: polyline2
integer :: ismoo,ismoo1,ismoo2,ismoo3
real    :: perin
public  :: smoot, ismoo,ismoo1,ismoo2,ismoo3,perin

public  :: seefont

private :: arc2

public  :: test_suite_M_drawplus

interface polyline2
   module procedure :: polyline2_i, polyline2_r
end interface polyline2
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    arrowhead(3f) - [M_drawplus] Draw arrow head (for text boxes and line markers)
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!     subroutine arrowhead(xpoint,ypoint,xstart,ystart,size,idraw)
!!
!!      real,intent(in)    :: xpoint,ypoint
!!      real,intent(in)    :: xstart,ystart
!!      real,intent(in)    :: size
!!      integer,intent(in) :: idraw
!!
!!##DESCRIPTION
!!    given line segment
!!
!!      START --> POINT
!!
!!    draw an arrow head of overall length SIZE measured along the line segment.
!!    The arrow head is 2/3 SIZE wide and the indent is 1/3 SIZE.
!!
!!    if IDRAW is 0, draw line from x3 to START to P3 and leave current
!!    position at POINT.
!!
!!     >                o START
!!     >                |
!!     >                |
!!     >                |
!!     >                |
!!     >                |  1/3*size
!!     >                |<------->
!!     >                |
!!     >                |
!!     >   P1  o .      |      . o P2   ---------
!!     >        \  .    |   .   /         ^
!!     >         \   .  | .    /          |
!!     >          \     o P3  / -------   |
!!     >           \         /     ^     SIZE
!!     >            \       /      |      |
!!     >             \     /    2/3*SIZE  |
!!     >              \   /        V      V
!!     >               \ /      -----------------
!!     >                o POINT
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_arrowhead
!!    use :: M_draw
!!    use :: M_drawplus, only : arrowhead
!!    implicit none
!!    real :: xpoint,ypoint, xstart,ystart
!!    real :: PI, ang, size
!!    integer :: i, idraw, idum
!!    call prefsize(600,600)
!!    call vinit('')
!!    call ortho2(-10.0,10.0,-10.0,10.0)
!!    call linewidth(100)
!!    call color(D_WHITE)
!!    call clear()
!!    xstart=0.0
!!    ystart=0.0
!!    size=1.00
!!    idraw=0
!!
!!    call color(D_RED)
!!    xpoint=9.0
!!    ypoint=0.0
!!    call arrowhead(xpoint,ypoint,xstart,ystart,size,idraw)
!!
!!    call color(D_GREEN)
!!    xpoint=9.0
!!    ypoint=9.0
!!    call arrowhead(xpoint,ypoint,xstart,ystart,size,idraw)
!!
!!    call color(D_BLACK)
!!    idraw=-1
!!    PI=4.0*atan(1.0)
!!    ang=PI/2.0
!!    do i=1,20+1
!!       xpoint=9.0*cos(ang)
!!       ypoint=9.0*sin(ang)
!!       call arrowhead(xpoint,ypoint,xstart,ystart,size,idraw)
!!       ang=ang+PI/20.0
!!    enddo
!!    idum=getkey()
!!    call vexit()
!!    end program demo_arrowhead
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
subroutine arrowhead(xpoint,ypoint,xstart,ystart,size,idraw)
use M_draw
implicit none

! ident_1="@(#) M_drawplus arrowhead(3f) Draw arrow head (for text boxes and line markers)"

real,intent(in)    :: xpoint,ypoint
real,intent(in)    :: xstart,ystart
real,intent(in)    :: size
integer,intent(in) :: idraw

   real            :: xdel,ydel
   real            :: hyp,hyp2,hyp3
   real            :: ang
   real            :: adder
   real            :: x2,y2,x3,y3,x4,y4

   xdel=xpoint-xstart
   ydel=ypoint-ystart
   hyp=sqrt(xdel**2+ydel**2)                 ! length from START to POINT
   hyp2=hyp-size*2.0/3.0                     ! length from START to P3
   hyp3=sqrt((hyp-size)**2+(size/3.0)**2)    ! length from START to P1

   ang=atan2(ydel,xdel)                      ! angle of line START-->POINT
   adder=atan2(size/3.0,hyp-size)            ! angle P1-->START-->P3

   x2=xstart+hyp3*cos(ang+adder)             ! calculate P2 from START
   y2=ystart+hyp3*sin(ang+adder)

   x3=xstart+hyp2*cos(ang)                   ! calculate P3 from START
   y3=ystart+hyp2*sin(ang)

   x4=xstart+hyp3*cos(ang-adder)             ! calculate P4 from START
   y4=ystart+hyp3*sin(ang-adder)

   call pushattributes()
      call polyfill(.true.)                  ! fill arrowhead
      call makepoly()
         call move2(xpoint,ypoint)
         call draw2(x2,y2)
         call draw2(x3,y3)
         call draw2(x4,y4)
         call draw2(xpoint,ypoint)
      call closepoly()
      call polyfill(.false.)                 ! ASSUME ORIGINALLY OFF!
   call popattributes()

   if(idraw.eq.0)then      ! draw line from back of arrowhead (P3) to START
      call move2(x3,y3)
      call draw2(xstart,ystart)
      call move2(xpoint,ypoint)
   endif
end subroutine arrowhead
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plain_rect(3f) - [M_drawplus] - draw a simple rectangle that does not act as a polygon
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!     subroutine plain_rect(x1,y1,x2,y2)
!!
!!      real,intent(in) :: x1
!!      real,intent(in) :: y1
!!      real,intent(in) :: x2
!!      real,intent(in) :: y2
!!##DESCRIPTION
!!    The M_draw(3fm) routine rect(3f) is treated as a polygon. This simple routine ensures just the
!!    outline of the box is draw regardless of whether polygon fill or hatchfill mode is on.
!!##OPTIONS
!!    X1,Y1   coordinates of a corner of the box
!!    X2,Y2   coordinates of opposite corner of the box
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
subroutine plain_rect(x1,y1,x2,y2)
use M_draw
implicit none

! ident_2="@(#) M_xyplot plain_rect(3fp) Draw a rectangle; gets around problem with filled boxes in M_DRAW"

real,intent(in) :: x1
real,intent(in) :: y1
real,intent(in) :: x2
real,intent(in) :: y2

      call move2(x1,y1)
      call draw2(x2,y1)
      call draw2(x2,y2)
      call draw2(x1,y2)
      call draw2(x1,y1)

end subroutine plain_rect
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!       polyline2(3f) - [M_drawplus] - draw an unclosed polyline in the XY plane
!!       (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!     subroutine polyline2(arrx,arry)
!!
!!      integer,intent(in)          :: arrx(:)
!!      integer,intent(in),optional :: arry(:)
!!
!!##DESCRIPTION
!!    Given either a single array composed of pairs <x(i),y(i)> of values
!!    defining points or an X and Y array move to first point and draw to
!!    remaining points using current line style.
!!
!!##OPTIONS
!!    ARRX    If ARRY is present, an array of X values
!!    ARRY    An optional array of Y values
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_polyline2
!!      use M_draw
!!      use M_drawplus, only : polyline2
!!      implicit none
!!      integer :: ipaws
!!         call prefsize(300,300)
!!         call vinit(' ')
!!         call ortho2(-2.0,2.0,-2.0,2.0)
!!         call color(2)
!!         call linewidth(100)
!!         call polyline2([-0.5,-0.5, -0.5,+0.5, +0.5,+0.5, +0.5,-0.5])
!!         call color(4)
!!         call polyline2( [-1,-1,+1,+1,-1] , &  ! X values
!!                       & [-1,+1,+1,-1,-1] )    ! Y values
!!         ipaws=getkey()
!!         call vexit()
!!      end program demo_polyline2
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
subroutine polyline2_i(arrx,arry)
integer,intent(in)          :: arrx(:)
integer,intent(in),optional :: arry(:)
integer                     :: i
integer                     :: isizex
integer                     :: isizey
integer                     :: ipairs
! assuming nice data in x,y pairs
if(present(arry))then    ! two arrays means X array and Y array
   isizex=size(arrx)
   isizey=size(arry)
   ipairs=min(isizex,isizey)
   if(ipairs.gt.0)then
      call move2(real(arrx(1)),real(arry(1)))
   endif
   do i=2,ipairs
      call draw2(real(arrx(i)),real(arry(i)))
   enddo
else                      ! one array means array is <x1,y1>, <x2,y2>, <x3,y3>, ...
   isizex=size(arrx)
   isizey=0
   ipairs=isizex/2
   if(ipairs.gt.0)then
      call move2(real(arrx(1)),real(arrx(2)))
   endif
   do i=3,ipairs*2,2
      call draw2(real(arrx(i)),real(arrx(i+1)))
   enddo
endif

end subroutine polyline2_i
!===================================================================================================================================
subroutine polyline2_r(arrx,arry)
real,intent(in)          :: arrx(:)
real,intent(in),optional :: arry(:)
integer                  :: i
integer                  :: isizex
integer                  :: isizey
integer                  :: ipairs
! assuming nice data in x,y pairs
if(present(arry))then    ! two arrays means X array and Y array
   isizex=size(arrx)
   isizey=size(arry)
   ipairs=min(isizex,isizey)
   if(ipairs.gt.0)then
      call move2(arrx(1),arry(1))
   endif
   do i=2,ipairs
      call draw2(arrx(i),arry(i))
   enddo
else                      ! one array means array is <x1,y1>, <x2,y2>, <x3,y3>, ...
   isizex=size(arrx)
   isizey=0
   ipairs=isizex/2
   if(ipairs.gt.0)then
      call move2(arrx(1),arrx(2))
   endif
   do i=3,ipairs*2,2
      call draw2(arrx(i),arrx(i+1))
   enddo
endif

end subroutine polyline2_r
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!     uarc(3f) - [M_drawplus] create circular arc, leaving CP at end of arc
!!     (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!     subroutine uarc(x,y,angle)
!!
!!      real,intent(in) :: x
!!      real,intent(in) :: y
!!      real,intent(in) :: angle
!!
!!##DESCRIPTION
!!     given center point and angle in degrees, draw circular arc from
!!     current point with counterclockwise angles positive.
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_uarc
!!    use :: M_draw
!!    use :: M_drawplus, only : uarc
!!    integer :: i, idum
!!    call prefsize(400,400)
!!    call vinit(' ')
!!    call color(D_BLACK)
!!    call clear()
!!    call linewidth(300)
!!    call ortho2(-10.0,10.0,-10.0,10.0)
!!    ! draw square with rounded corners, filled in
!!    ! yellow, outlined in blue
!!    call color(D_YELLOW)
!!    call polyfill(.true.)
!!    do i=1,2
!!       call makepoly()
!!       call move2(-9.0,-7.0)
!!       call draw2(-9.0, 7.0)
!!       call uarc(-7.0,7.0,-90.0)
!!       call draw2(7.0,9.0)
!!       call uarc( 7.0,7.0,-90.0)
!!       call draw2(9.0,-7.0)
!!       call uarc( 7.0,-7.0,-90.0)
!!       call draw2( -7.0,-9.0)
!!       call uarc(-7.0,-7.0,-90.0)
!!       call closepoly()
!!       call color(D_BLUE)
!!       call polyfill(.false.)
!!    enddo
!!    idum=getkey()
!!    call vexit()
!!    end program demo_uarc
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
subroutine uarc(x,y,angle)
!-----------------------------------------------------------------------------------------------------------------------------------
! given center point and angle in degrees, go from current point
! with counterclockwise angles positive.
!-----------------------------------------------------------------------------------------------------------------------------------
use M_draw

! ident_3="@(#) M_drawplus create circular arc leaving CP at end of arc"

real,intent(in) :: x
real,intent(in) :: y
real,intent(in) :: angle
!-----------------------------------------------------------------------------------------------------------------------------------
real :: xpass, ypass  ! copies of x and y
real :: ang1
real :: ang2
real :: d2r
integer :: nsegs
real :: radius
real :: xnow
real :: ynow

   ! let compiler know I am not changing input values
   xpass=x
   ypass=y

   call getgp2(xnow,ynow)
   radius=sqrt((xnow-xpass)**2+(ynow-ypass)**2)
   ang1=atan2(ynow-ypass,xnow-xpass)*180.0/3.14159265359
   ang2=ang1+angle
!
!!      reduce angles to range of 0 to 360
!!!     ang1=amod(amod(ang1,360.0)+360.0,360.0)
!!      ang2=amod(amod(ang2,360.0)+360.0,360.0)
!
   call arc2(xpass,ypass,radius,ang1,ang2)
!
!!      update current position to end of arc
!!!     rang2=ang2/180.0*3.14159265359
!!!!    xend=xpass+radius*cos(rang2)
!!!     yend=ypass+radius*sin(rang2)
!!      call move2(xend,yend)
!
      end subroutine uarc
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    arc2(3fp) - [M_drawplus] draw an arc
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!     subroutine arc2(x, y, radius, startang, endang)
!!
!!      real,intent(in) :: x
!!      real,intent(in) :: y
!!      real,intent(in) :: radius
!!      real,intent(in) :: startang
!!      real,intent(in) :: endang
!!
!!##DESCRIPTION
!!    M_DASH arc(3f) routine does a move so it cannot be included in a
!!    polygon very easily. This routine is called by M_drawplus(3fm) version of
!!    uarc(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
subroutine arc2(x, y, radius, startang, endang)
use M_draw

! ident_4="@(#) M_drawplus arc2(3f) Like M_draw(3f) arc(3f) routine without move at end so can be in a polygon"

real,intent(in) :: x
real,intent(in) :: y
real,intent(in) :: radius
real,intent(in) :: startang
real,intent(in) :: endang
!----------------------------------------------------------------------------------------------------------------------------------!
real     :: cx, cy, dx, dy
real     :: deltang, cosine, sine, angle
integer  :: i
integer  :: numsegs
real     :: ang1
real     :: ang2
real     :: d2r
integer  :: nsegs
real     :: xnow
real     :: ynow
   D2R=3.14159265359/180.0
   angle = startang * D2R
   nsegs=100
   numsegs = abs(endang - startang) / 360.0 * nsegs + 0.5
   deltang = (endang - startang) * D2R / numsegs
   cosine = cos(deltang)
   sine = sin(deltang)
!  calculates initial point on arc
   cx = x + radius * cos(angle)
   cy = y + radius * sin(angle)
!  assume you are already at this point so can be used in polygons
!  call move2(cx, cy)
   do i=1,numsegs
      dx = cx - x
      dy = cy - y
      cx = x + dx * cosine - dy * sine
      cy = y + dx * sine + dy * cosine
      call draw2(cx, cy)
   enddo
end subroutine arc2
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    ellipse(3f) - [M_drawplus] draws an ellipse or elliptical arc.
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!     subroutine ellipse (XPAGE,YPAGE,RMAJ,RMIN,ANGLE,THO,THF,IPEN)
!!
!!##DESCRIPTION
!!    ellipse(3f) is a FORTRAN subroutine which draws an ellipse or
!!    elliptical arc.
!!
!!##OPTIONS
!!    XPAGE,YPAGE  are the coordinates, in inches, of the starting point of
!!                 the ellipse or arc.
!!
!!    RMAJ,RMIN    are the lengths, in inches, of the semimajor and
!!                 semiminor axes, respectively.
!!
!!    ANGLE        is the angle of the major axis, in degrees.
!!
!!    THO,THF      are the angles, in degrees with respect to ANGLE, of the
!!                 arc's starting and ending points.
!!     IPEN        is the code that moves the pen to the arc's starting
!!                 point. If the value of IPEN is:
!!
!!                   3, the pen is up for the move;
!!                   2, the pen is down for the move.
!!
!!    COMMENTS
!!
!!    THO and THF may be positive or negative. If THO is less than THF, the arc is
!!    drawn in a counterclockwise direction; if THO is greater than THF, the arc is
!!    drawn in a clockwise direction.
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
subroutine ellipse(xpage,ypage,rmaj,rmin,angle,th0,thf,ipen)
use M_draw
use ISO_C_BINDING
implicit none

! ident_5="@(#) M_drawplus m_draw ellipse(3f) draw ellipse or elliptical arc"

real,intent(in)    :: xpage
real,intent(in)    :: ypage
real,intent(in)    :: rmaj
real,intent(in)    :: rmin
real,intent(in)    :: angle
real,intent(in)    :: th0
real,intent(in)    :: thf
integer,intent(in) :: ipen

real    :: ab
real    :: absq
real    :: alp
real    :: bsq
real    :: d
real    :: dthe
real    :: fctr
integer :: i
integer :: n
real    :: st
real    :: the0
real    :: the_n
real    :: thef
real    :: xc
real    :: xdum
real    :: xf
real    :: yc
real    :: ydum
real    :: yf

   if ((abs(rmaj)+abs(rmin)).eq.0)then
      if(ipen.eq.2)then
         call draw2(xpage,ypage)
      else
         call move2(xpage,ypage)
      endif
      return
   endif

   alp = angle/57.2958
   the0 = th0  / 57.2958
   thef = thf  / 57.2958
   d=rmaj*rmin/sqrt((rmaj*sin(the0))**2+(rmin*cos(the0))**2)
   xc = xpage - d * cos(the0 + alp)
   yc = ypage - d * sin(the0 + alp)
   bsq=rmin*rmin
   absq=rmaj*rmaj-bsq
   ab=rmaj*rmin

   if(ipen.eq.2)then
      call draw2(xpage,ypage)
   else
      call move2(xpage,ypage)
   endif

   call getgp2(xdum,ydum) !
   fctr=1.0
   dthe = 0.03/(abs(rmaj)+abs(rmin))/fctr
   n  =int((thef - the0)/dthe)

   if(n.lt.0)then
      n = -n
      dthe = -dthe
   elseif(n.eq.0)then
      n = 1
      dthe = -dthe
   endif

   the_n = the0 + dthe

   do i=1,n
      st=sin(the_n)
      d=ab/sqrt(absq*st*st+bsq)
      xf=xc+d*cos(the_n+alp)
      yf=yc+d*sin(the_n+alp)
      call draw2(xf,yf)
      the_n = the_n + dthe
   enddo

   st=sin(thef)
   d=ab/sqrt(absq*st*st+bsq)
   xf=xc+d*cos(thef+alp)
   yf=yc+d*sin(thef+alp)
   call draw2(xf,yf)

end subroutine ellipse
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    uconic(3f) - [M_drawplus] general conic sections
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!     uconic(x,y,p,e,theta1,theta2,orientation)
!!
!!      real, intent=(in) :: x
!!      real, intent=(in) :: y
!!      real, intent=(in) :: p
!!      real, intent=(in) :: e
!!      real, intent=(in) :: theta1
!!      real, intent=(in) :: theta2
!!      real, intent=(in) :: orientation
!!
!!##DESCRIPTION
!!
!!    UCONIC() allows the user to draw a generalized conic section having
!!    a given focus, directrix, eccentricity, and starting and ending sweep
!!    positions. The conic sections that may be drawn are
!!
!!       CALL UCONIC (X,Y,P,E,THETA1,THETA2,ORIENTATION)
!!
!!    (X,Y) are used to specify the coordinates of the focus of the conic
!!    section; P is the distance from the focus to the directrix; E is the
!!    eccentricity, THETA1 and THETA2 represent the initial and final angles
!!    through which the conic section is to be drawn. Parameters E and P
!!    affect the generation of the conic section in the following manner:
!!
!!##OPTIONS
!!         x  x-coordinate of the focus of the conic section.
!!         y  y-coordinate of the focus of the conic section.
!!
!!         p  Distance from the focus to the directrix.
!!
!!              o P.GT.0 Defines the position of the focus to be to the right of (or
!!                below) the directrix.
!!              o P.LT.0 Indicates that the focus is positioned to the left of (or
!!                above) the directrix.
!!
!!         e  eccentricity.
!!
!!              o E=0 Circular arc with a center at (X,Y) at radius P/2. The arc
!!                will subtend the angular range defined by THETA1 and THETA2.
!!              o 0.LT.ABS(E).LT.1 ELLIPSE.
!!              o ABS(E)=1 PARABOLA.
!!              o ABS(E).GT.1 HYPERBOLA
!!              o E.GT.0 Indicates that the major axis of the conic section is to
!!                be oriented parallel to the X-axis.
!!              o E.LT.0 Specifies that the major axis is to be oriented along
!!                the Y-axis.
!!
!!         theta1    Initial sweep position in current angular units.
!!
!!         theta2    Terminal sweep position in current angular units.
!!
!!         orientation  angle of conic axis in degrees
!!
!!         COMMENTS
!!         The basis for uconic is the generalized conic equation
!!
!!            R=(E*P)/(1-E*COS(THETA))
!!
!!         By suitably modifying the values of the parameters P and E, all types
!!         of conic sections can be created. The following describes the effects
!!         induced by different values of P of E.
!!
!!           P.GT.0       the focus is to the right (below the directrix).
!!
!!           P.LT.0       the focus is to the left (above the directrix).
!!
!!           P = 0        the conic will be a point at x,y.
!!
!!           E = 0        the conic is a circular arc with center at x,y
!!                        and radius subtending the angular range from the
!!                        theta1 to theta2.
!!
!!           0.LT.ABS
!!           (E).LT.1     the conic is an ellipse.
!!
!!           ABS(E)=1     the conic is a parabola.
!!
!!           ABS(E).GT.1  the conic is a hyperbola.
!!
!!           E.GT.0       the conic is oriented along the x-axis
!!
!!           E.LT.0       the conic is oriented along the y-axis
!!
!!
!!         Conic sections may be rotated to any angle by defining a suitable user
!!         coordinate system.
!!
!!         The circle is a degenerate case not fully handled by the generalized
!!         conic equation. For completeness a circle with arbitrarily assigned
!!         radius of will be generated when E has a value of zero.
!!
!!         If an angle of orientation of the conic is specified, then the conic
!!         section will be oriented as specified around the point of intersection
!!         of the directrix and the semi-major axis.
!!
!!         In three dimensional applications, the conic section drawn by uconic
!!         will lie in the current xy plane.
!!
!!##AUTHOR
!!    o Heavily based on a GCS routine written by Major Gabriel.
!!    o John S. Urban
!!##LICENSE
!!    MIT License
subroutine uconic(x,y,p,e,th1,th2,rangle)
!    procedure draws all or part of a generalized conic section having a
!    given focus, directrix, and eccentricity and an angular span from
!    th1 to th2
!
!    basis for uconic is general conic equation  r=ep/1-e*cos(theta)
!    plotting in uconic is done in a cartesian coordinate system
!    computations are performed in degrees
!
!    procedures called:  move2, draw2, uarc, translate, d2r
!
!    calling sequence:
!       call uconic(x,y,p,e,th1,th2)
!    where
!    x,y are the coordinates of the focus of the conic section
!    p is the distance from the focus to the directrix
!             p>0       focus to the right(below) the directrix
!             p<0       focus to the left(above) the directrix
!             p=0       the conic will be a point at x,y
!    e is the eccentricity
!             e=0       the conic is a circular arc with center at x,y and
!                       radius p/2 subtending the angular range from th1 to th2
!             0<|e|<1   conic is an ellipse
!             |e|=1     conic is a parabola
!             |e|>1     conic is a hyperbola
!             e>0       conic is oriented along the x-axis
!             e<0       conic is oriented along the y-axis
!   th1      initial angular location from which the conic is to start
!   th2      final angular location where the conic will end
!   rangle   rotation angle
!
!    local variables
!        ep   the product of the eccentricity(e) and the distance from the focus
!             to the directrix
!        g    the denominator in the general conic equation
!        r    the radius
!        deg  the number of three degree line segments required to draw figure
!        n    the total number of three degree segments
!        d    the fractional part of three degree segment to complete the figure
!        an   the angular span of the circular arc
!-----------------------------------------------------------------------------------------------------------------------------------
use M_draw
use ISO_C_BINDING
use m_units, only: d2r

! ident_6="@(#) M_drawplus m_draw uconic(3f) general conic sections"

real :: an
real :: angdel
real :: anginc
real :: e
real :: ep
real :: finer
real :: g
integer :: i
integer :: itune
integer :: j
integer :: ltune
integer :: n
real :: p
real :: r
real :: rangle
real :: ro
real :: s
real :: t
real :: th1
real :: th2
real :: theta
real :: theta1
real :: theta2
real :: thetal
real :: x
real :: xx
real :: y
real :: yy
!-----------------------------------------------------------------------------------------------------------------------------------
! if distance between focus and directrix is zero plot a point
   if(p.eq.0.)then
      call point2(x,y)
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   theta2=th2
   theta1 = th1
   thetal=theta2                             ! save last location
   anginc=3.                                 ! set initial arc segment angle
   an=theta2-theta1                          ! determine the angular span of the arc
   finer=abs(2.0*p*e)                        ! compute fine tuning threshold
   call translate(x,y,0.0)                   ! create uconic coordinate system
   n=(theta2-theta1)/anginc
   anginc=(theta2-theta1)/float(n)           ! update angle increment to leave small angle at end

!  if the eccentricity is equal to zero plot a circle ( or circular arc )
88 continue
   if(e.eq.0.)then                           ! circle or circular arc section
      r = p
!     determine where the initial point will be and move to that point
      theta2=theta1+rangle
      xx=r*cos(d2r(theta2))
      yy=r*sin(d2r(theta2))
      call move2(xx,yy)
!     since the window has been modified the user"s x,y coordinate is the (0.,0.,) coordinate in the new window.
      call uarc(0.,0.,an)                    ! draw the arc or circle with the center at (0.,0.,) for an angular span of an.
      call translate(-x,-y,0.0)              ! restore the coordinate system
      return
   elseif(e.lt.0.) then                     ! determine where the initial point will be and move to that point
      g = -1.0 + e * sin(theta1 * .01745329)
   else
      g =  1.0 - e * cos(theta1 * .01745329)
   endif

   ep=e*p
   if(g.ne.0.)then
      r = ep / g
!     save the previous value of !r!
      ro=r
      theta2=theta1+rangle
      xx=r*cos(d2r(theta2))
      yy=r*sin(d2r(theta2))
      call move2(xx,yy)
   else
!     correct to avoid starting at infinity
      n = n - 1
      theta1=theta1+anginc
      go to 88
   endif

!     construct the conic(hyperbola,parabola, or ellipse)
   do i=1,n
!     prepare fine tuning loop
      angdel=anginc
      itune=1

!     branch if fine tuning not required
      if(abs(r).gt.finer)then           ! set loop for fine tuning
         itune=min1(10.,abs(r)/finer+1.)
         angdel=anginc/float(itune)
      endif

!     start fine tuning loop
      do ltune=1,itune
!        save previous values of r and theta2
         s=r
         t=theta2
!        add anginc degrees to the previous value of theta in determining the next segment to be drawn
         theta=theta1+anginc*float(i-1)+angdel*float(ltune)

!        determine the eccentricity so the appropriate denominator for the general conic equation and axis orientation can be used
         if(e.lt.0.) then
            g = -1.0 + e * sin(theta * .01745329)
         else
            g =  1.0 - e * cos(theta * .01745329)
         endif

         if(g.eq.0.) then
            if(abs(e).gt.1.0) then
               j=2
            endif
         elseif(g.eq.2.)then
            theta2=theta+rangle
            xx=r*cos(d2r(theta2))
            yy=r*sin(d2r(theta2))
            call move2(xx,yy)
            j=1
         else
!           solve the conic equation for !r!
            r = ep / g
!           solve for the polar coordinate theta2=theta+ the angle of rotation
            theta2=theta+rangle
!           eliminate the asymptotes
            if(r*ro.gt.0.)then
!              draw the segment using !r! and theta2 as the polar coordinates
               xx=r*cos(d2r(theta2))
               yy=r*sin(d2r(theta2))
               call draw2(xx,yy)
            else
               xx=r*cos(d2r(theta2))
               yy=r*sin(d2r(theta2))
               call move2(xx,yy)
            endif
         endif
         ro=r
      enddo
   enddo

!  construct the last segment of the conic
!  if !d! is equal to zero , back-up to the last segment drawn since a single point cannot have a terminator
   if(theta.eq.thetal)then
         xx=s*cos(d2r(t))
         yy=s*sin(d2r(t))
         call move2(xx,yy)
   endif
!  determine the eccentricity
   theta=thetal

   if(e.lt.0.) then
         g = -1.0 + e * sin(theta * .01745329)
   else
         g = 1.0 - e * cos(theta * .01745329)
   endif
   if(g.ne.0.) then
         r = ep / g
         theta2=theta+rangle
         xx=r*cos(d2r(theta2))
         yy=r*sin(d2r(theta2))
         call draw2(xx,yy)
   endif
   call translate(-x,-y,0.0) ! restore the coordinate system
end subroutine uconic
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
! @(#) Fortran MODULE interface to M_DRAW C routines using Fortran ISO_C_BINDING interface
!             3. Interoperating with C
!
! Any entity involved in interoperating with C must be such that equivalent
! declarations of it may be made in the two languages.
!
! Enforced within the Fortran program by requiring all such entities to
! be interoperable.
!
! We will explain in turn what this requires for types, variables,
! and procedures.
!
! They are all requirements on the syntax so that the compiler knows at
! compile time whether an entity is interoperable.
!
!      3.1 Interoperability of intrinsic types
!
! Intrinsic module ISO_C_BINDING contains named constants holding kind
! type parameter values.  For example:
!
!    C_INT                       int
!    C_SHORT                     short int
!    C_LONG                      long int
!    C_FLOAT                     float
!    C_DOUBLE                    double
!    C_LONG_DOUBLE               long double
!    C_FLOAT_COMPLEX             float _Complex
!    C_BOOL                      _Bool
!    C_CHAR                      char
!
! Lack of support is indicated with a negative value.
!
!       3.2 Interoperability of derived types
!
! For a derived type to be interoperable, it must be given the BIND
! attribute explicitly:
!
!        TYPE, BIND(C) :: MYTYPE
!         :
!        END TYPE MYTYPE
!
! Each component must have interoperable type and type parameters, must
! not be a pointer, and must not be allocatable. This allows Fortran and
! C types to correspond.
!
!         3.3 Interoperability of variables
!
! A scalar Fortran variable is interoperable if it is of interoperable
! type and type parameters, and is neither a pointer nor allocatable.
!
! An array Fortran variable is interoperable if it is of interoperable
! type and type parameters, and is of explicit shape or assumed size. It
! interoperates with a C array of the same type, type parameters and shape,
! but with reversal of subscripts.
!
! For example, a Fortran array declared as
!          INTEGER :: A(18, 3:7, *)
! is interoperable with a C array declared as
!          int b[][5][18]
!
!
!       3.4 Interoperability with C pointers
!
! For interoperating with C pointers (addresses), the module contains a
! derived type C_PTR that is interoperable with any C pointer type and a
! named constant C_NULL_PTR.
!
! The module also contains the procedures:
!
!    C_LOC(X) returns the C address of X.
!
!    C_ASSOCIATED (C_PTR1[, C_PTR2]) is an
!   inquiry function that is like ASSOCIATED.
!
!   C_F_POINTER (CPTR, FPTR [, SHAPE])) is a
!   subroutine that constructs a Fortran pointer
!   from a scalar of type C_PTR.
!
!        3.5 Interoperability of procedures
!
! A new attribute, VALUE, has been introduced for scalar dummy
! arguments. Does copy-in without copy-out.
!
! A Fortran procedure is interoperable if it has an explicit interface
! and is declared with the BIND attribute:
!
!  FUNCTION FUNC(I, J, K, L, M), BIND(C)
!
! All the dummy arguments must be interoperable.  For a function, the
! result must be scalar and interoperable.
!
!                 3.6 Binding labels
!
! The Fortran procedure has a `binding label',
! which has global scope and is the name by which
! it is known to the C processor.
! By default, it is the lower-case version of the
! Fortran name.
! An alternative binding label may be specified:
!   FUNCTION FUNC(I, J, K, L, M) BIND(C, NAME='C_Func')
!
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
! hide logicals from C
! trim and append null to intent(in) character strings
subroutine barcode(XCORNER_IN,YCORNER_IN,XSMALL_IN,XLARGE_IN,YSIZE_IN,STRING)
implicit none

! ident_7="@(#) M_drawplus barcode(3f) draw 3-of-9 bar code"

! void barcode( float xcorner, float ycorner, float xsmall, float xlarge, float ysize, char *string);
   interface
      subroutine barcode_F(XCORNER,YCORNER,XSMALL,XLARGE,YSIZE,STRING) bind(C,NAME='barcode')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: XCORNER
         real(KIND=C_FLOAT),intent(in),value :: YCORNER
         real(KIND=C_FLOAT),intent(in),value :: XSMALL
         real(KIND=C_FLOAT),intent(in),value :: XLARGE
         real(KIND=C_FLOAT),intent(in),value :: YSIZE
         character(KIND=C_CHAR) :: STRING(*)
      end subroutine barcode_F
   end interface

   real,intent(in) :: XCORNER_IN
   real,intent(in) :: YCORNER_IN
   real,intent(in) :: XSMALL_IN
   real,intent(in) :: XLARGE_IN
   real,intent(in) :: YSIZE_IN

   real(KIND=C_FLOAT) :: XCORNER
   real(KIND=C_FLOAT) :: YCORNER
   real(KIND=C_FLOAT) :: XSMALL
   real(KIND=C_FLOAT) :: XLARGE
   real(KIND=C_FLOAT) :: YSIZE

   character*(*) STRING

   XCORNER=XCORNER_IN
   YCORNER=YCORNER_IN
   XSMALL=XSMALL_IN
   XLARGE=XLARGE_IN
   YSIZE=YSIZE_IN

   !write(*,*)'*barcode* ',xcorner,ycorner,xsmall,xlarge,ysize,string
   call barcode_F(XCORNER,YCORNER,XSMALL,XLARGE,YSIZE,STRING(:len_trim(STRING))//achar(0))
end subroutine barcode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     draw_interpret(3f) - [M_drawplus] call interpreter for testing M_draw(3fm) routines
!!     (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!     subroutine draw_interpret(array,delimiters)
!!
!!      character(len=*) :: array(*)
!!      character(len=*) :: delimiters
!!
!!##DESCRIPTION
!!     takes an array of strings and treats them as requests for simple calls
!!     to the call_draw(3f) routine. This allows for creating simple
!!     graphics and for testing the M_draw(3fm) module.
!!
!!     The M_calc(3fm) module is used to evaluate number parameters. The
!!     "set" directive is added to allow easily declaring variables for the
!!     calculator.
!!
!!##OPTIONS
!!     ARRAY       character array containing strings representing basic
!!                 M_draw(3fm) procedures.
!!     DELIMITERS  character(s) used to separate commands in the array.
!!
!!##EXAMPLES
!!
!!     Sample program
!!
!!      program demo_draw_interpret
!!      use M_drawplus, only : draw_interpret
!!      character(len=:),allocatable :: draw_cmds(:)
!!
!!      ! $FILTER variable -varname DRAW_CMDS
!!
!!      draw_cmds=[ character(len=128) ::                                         &
!!      'set N=11; prefsize 600 200; vinit ;page -15 15 -5 5                    ',&
!!      'textsize .3 .3; linewidth 150/3; color 0; clear                        ',&
!!      'color  1;  spirograph  -10  0  N  1  N  5  1000  0  0  0               ',&
!!      'polyhatch 1; hatchang 45.0 ; hatchpitch 0.3 # turn on polygon hatching ',&
!!      'color  2;  spirograph   10  0  N  1  N  5  1000  0  0  2               ',&
!!      '                                                                       ',&
!!      'vflush; getkey ; vexit                                                 ',&
!!      '']
!!
!!      ! $FILTER END
!!
!!      call draw_interpret(draw_cmds,delimiters=';')
!!      end program demo_draw_interpret
!!
!!##SEE ALSO
!!     call_draw(3f), M_draw(3fm), M_drawplus(3fm)
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
subroutine draw_interpret(array,delimiters)
use M_strings, only: split
implicit none
character(len=:),allocatable    :: array(:)
character(len=:),allocatable    :: cmds(:) ! output array of tokens
logical                         :: found
integer                         :: iend
integer                         :: i,j
character(len=*)                :: delimiters
!-----------------------------------------------------------------------------------------------------------------------------------
   do j=1,size(array)
      call split(array(j),cmds,delimiters=trim(delimiters))
      do i=1,size(cmds)
         cmds(i)=adjustl(cmds(i))
         iend=scan(cmds(i),' #')-1
         if(iend.le.0)iend=len_trim(cmds(i))
         if(iend.ne.0)then
            if(cmds(i)(:1).eq.'#') cycle
            cmds(i)=trim(cmds(i))//' '
            call call_draw(cmds(i)(:iend),cmds(i)(iend+2:),found)
            if(.not.found)then
               write(*,'(*(a))')'ERROR: ',trim(cmds(i)(:iend)),' [',trim(cmds(i)(iend+2:)),']',' not found'
            endif
         endif
      enddo
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine draw_interpret
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!     call_draw(3f) - [M_drawplus] Given a string representing a M_draw procedure and parameters call the routine
!!     (LICENSE:MIT)
!!##SYNOPSIS
!!
!!     subroutine call_draw(verb,parameters,found)
!!
!!      character(len=*),intent(in)  :: verb
!!      character(len=*),intent(in)  :: parameters
!!      logical,intent(out)          :: found
!!
!!##DESCRIPTION
!!    Used to allow input files to directly call arbitrary low-level graphics procedures.
!!    This is a simple interpreter for M_graph(3fm) routines.
!!
!!##OPTIONS
!!    verb         name of M_draw(3fm) routine to call
!!    parameters   string representing options to pass to
!!                 the routine specified by the verb.
!!                 Numeric values are evaluated using the
!!                 M_calc(3fm) module to allow expressions.
!!##RETURNED
!!    found        returns .TRUE. if the verb was found,
!!                 otherwise .FALSE.
!!
!!##EXAMPLE
!!
!!    Simple Example
!!
!!       program demo_call_draw
!!          use M_drawplus, only : call_draw
!!          use M_io, only : read_line
!!          implicit none
!!          character(len=:),allocatable :: line
!!          logical                      :: found
!!          integer                      :: iend
!!          INFINITE: do while (read_line(line)==0)
!!             line=adjustl(line)
!!             iend=scan(line,' #;')-1
!!             if(iend.le.0)iend=len_trim(line)
!!             if(iend.ne.0)then
!!                line=line//' '
!!                call call_draw(line(:iend),line(iend+2:),found)
!!                if(.not.found)then
!!                   write(*,*)'ERROR: ',line(:iend),'[',line(iend+1:),']',' not found'
!!                endif
!!             endif
!!          enddo INFINITE
!!       end program demo_call_draw
!!
!!    Sample
!!
!!        demo_call_draw <<eof
!!        prefsize 400 400
!!        vinit X11
!!        circleprecision 100
!!        color 1
!!        circle 0 0 A=1.0
!!        color 2
!!        circle 0 0 A=A-.1
!!        color 3
!!        circle 0 0 A=A-.1
!!        color 4
!!        circle 0 0 A=A-.1
!!        color 5
!!        circle 0 0 A=A-.1
!!        color 6
!!        circle 0 0 A=A-.1
!!        color 7
!!        getkey LET
!!        vexit
!!        eof
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
subroutine call_draw(verb,parameters,found)
use,intrinsic :: iso_fortran_env
use M_calculator,      only : stuffa, stuff, iclen_calc
use M_calculator,      only : snum0, inum0, rnum0, strgar2
use M_verify,           only : debug, io_debug
use M_strings,         only : delim
use M_draw
!!implicit real   (a-h,o-z)
implicit none

! ident_8="@(#) M_drawplus call_draw(3f) parse most M_draw(3fm) routines positionally"

! parse most M_draw(3fm) routines positionally
! simplistic, does not handle quoted parameters directly, just parses on space
!
character(len=*),intent(in)  :: parameters
character(len=*),intent(in)  :: verb
logical,intent(out)          :: found

character(len=255)           :: x1,x2,x3,x4,x5,x6,x7,x8,x9,x10
character(len=1)             :: ch
character(len=1024)          :: ctemp1
integer,parameter            :: IHM=10
character(len=255)           :: array(IHM)
character(len=iclen_calc)    :: temp1
integer                      :: i
integer,parameter            :: isize=2000
integer                      :: il(IHM) ,icount, ilast
integer                      :: is(IHM), ie(IHM)
real                         :: numbrs(isize)
real                         :: points(3,1000)
real                         :: points2(2,1000)
integer                      :: idum
integer                      :: ierr
integer                      :: inums
integer                      :: ival
integer                      :: n2
real                         :: rval
real                         :: value0
real                         :: value1
real                         :: value2
real                         :: value3
real                         :: value4
!-----------------------------------------------------------------------------------------------------------------------------------
   is=0
   ie=0
   found=.true.
   array=' '
   call delim(parameters,array,int(IHM),icount,is,ie,ilast,' ')
   il=ie-is+1 ! lengths of character strings, except 1 if empty string


   if(debug)then
      write(io_debug,*)'*juifvg* PARAMETERS=',trim(parameters)
      write(io_debug,*)'*juifvg* ICOUNT=',icount
      write(io_debug,*)'*juifvg* IL=',IL
      do i=1,icount
         write(io_debug,'(a,i0,a,a,/)')'*juifvg* ARRAY=',i,' ',trim(array(i))
         write(io_debug,'(a,i0,a,a,/)')'*juifvg* IS:IE=',i,' ',trim(parameters(is(i):ie(i)))
      enddo
   endif

   x1=  array(01)(:il(01))
   x2=  array(02)(:il(02))
   x3=  array(03)(:il(03))
   x4=  array(04)(:il(04))
   x5=  array(05)(:il(05))
   x6=  array(06)(:il(06))
   x7=  array(07)(:il(07))
   x8=  array(08)(:il(08))
   x9=  array(09)(:il(09))
   x10= array(10)(:il(10))

!-----------------------------------------------------------------------------------------------------------------------------------
   select case(verb)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('arc')              ; call arc(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4),rnum0(x5))
   case('backface')         ; call backface(iflogic(snum0(x1)))
   case('backfacedir')      ; call backfacedir(iflogic(snum0(x1)))
   case('bottomjustify')    ; call bottomjustify()
   case('boxfit')           ; call boxfit(rnum0(x1),rnum0(x2),inum0(x3))
   case('callobj')          ; call callobj(inum0(x1))
   case('centertext')       ; call centertext(iflogic(snum0(x1)))
   case('circle')           ; call circle(rnum0(x1),rnum0(x2),rnum0(x3))
   case('circleprecision')  ; call circleprecision(inum0(x1))
   case('clear')            ; call clear()
   case('clipping')         ; call clipping(iflogic(snum0(x1)))
   case('closeobj')         ; call closeobj()
   case('closepoly')        ; call closepoly()
   case('color')            ; call color(inum0(x1))
   case('dashcode')         ; call dashcode(rnum0(x1))
   case('delobj')           ; call delobj(inum0(x1))
   case('draw')             ; call draw(rnum0(x1),rnum0(x2),rnum0(x3))
   case('expandviewport')   ; call expandviewport()
   case('fixedwidth')       ; call fixedwidth(iflogic(snum0(x1)))
   case('font')             ; call font(snum0(x1))
   case('frontbuffer')      ; call frontbuffer()
   case('hatchang')         ; call hatchang(rnum0(x1))
   case('hatchpitch')       ; call hatchpitch(rnum0(x1))
   case('leftjustify')      ; call leftjustify()
   case('linestyle')        ; call linestyle(x1)
   case('linewidth')        ; call linewidth(inum0(x1))
   case('loadobj')          ; call loadobj(inum0(x1),snum0(x2))
   case('lookat')           ; call lookat(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4),rnum0(x5),rnum0(x6),rnum0(x7))
   case('makeobj')          ; call makeobj(inum0(x1))
   case('makepoly')         ; call makepoly()
   case('mapcolor')         ; call mapcolor(inum0(x1),inum0(x2),inum0(x3),inum0(x4))
   case('move')             ; call move(rnum0(x1),rnum0(x2),rnum0(x3))
   case('ortho')            ; call ortho(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4),rnum0(x5),rnum0(x6))
   case('ortho2')           ; call ortho2(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4))
   case('perspective')      ; call perspective(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4))
   case('point')            ; call point(rnum0(x1),rnum0(x2),rnum0(x3))
   case('point2')           ; call point2(rnum0(x1),rnum0(x2))
   case('polarview')        ; call polarview(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4))
   case('polyfill')         ; call polyfill(iflogic(snum0(x1)))
   case('polyhatch')        ; call polyhatch(iflogic(snum0(x1)))
   case('popattributes')    ; call popattributes()
   case('popdev')           ; call popdev()
   case('popmatrix')        ; call popmatrix()
   case('popviewport')      ; call popviewport()
   case('prefposition')     ; call prefposition(inum0(x1),inum0(x2))
   case('prefsize')         ; call prefsize(inum0(x1),inum0(x2))
   case('printattribs')     ; call printattribs(snum0(x1))
   case('printvdevice')     ; call printvdevice(snum0(x1))
   case('pushattributes')   ; call pushattributes()
   case('pushdev')          ; call pushdev(x1)
   case('pushmatrix')       ; call pushmatrix()
   case('pushviewport')     ; call pushviewport()
   case('rdraw')            ; call rdraw(rnum0(x1),rnum0(x2),rnum0(x3))
   case('rdraw2')           ; call rdraw2(rnum0(x1),rnum0(x2))
   case('rect')             ; call rect(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4))
   case('rightjustify')     ; call rightjustify()
   case('rmove')            ; call rmove(rnum0(x1),rnum0(x2),rnum0(x3))
   case('rmove2')           ; call rmove2(rnum0(x1),rnum0(x2))
   case('rsdraw2')          ; call rsdraw2(rnum0(x1),rnum0(x2))
   case('rsmove2')          ; call rsmove2(rnum0(x1),rnum0(x2))
   case('saveobj')          ; call saveobj(inum0(x1),snum0(x2))
   case('scale')            ; call scale(rnum0(x1),rnum0(x2),rnum0(x3))
   case('sdraw2')           ; call sdraw2(rnum0(x1),rnum0(x2))
   case('sector')           ; call sector(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4),rnum0(x5))
   case('smove2')           ; call smove2(rnum0(x1),rnum0(x2))
   case('swapbuffers')      ; call swapbuffers()
   case('textang')          ; call textang(rnum0(x1))
   case('textjustify')      ; !  call textjustify()
   case('textsize')         ; call textsize(rnum0(x1),rnum0(x2))
   case('topjustify')       ; call topjustify()
   case('translate')        ; call translate(rnum0(x1),rnum0(x2),rnum0(x3))
   case('unexpandviewport') ; call unexpandviewport()
   case('up')               ; call up(rnum0(x1),rnum0(x2),rnum0(x3))
   case('vexit')            ; call vexit()
   case('vflush')           ; call vflush()
   case('viewport')         ; call viewport(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4))
   case('vinit')            ; call vinit(snum0(x1))
   case('vnewdev')          ; call vnewdev(snum0(x1))
   case('voutput')          ; call voutput(snum0(x1))
   case('vsetflush')        ; call vsetflush(iflogic(snum0(x1)))
   case('window')           ; call window(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4),rnum0(x5),rnum0(x6))
   case('xcentertext')      ; call xcentertext()
   case('ycentertext')      ; call ycentertext()
   case('patchbasis')       ; write(*,*)'*call_draw* patchbasis not implemented'
   case('patchprecision')   ; write(*,*)'*call_draw* patchprecision not implemented'
   case('patchcurves')      ; write(*,*)'*call_draw* patchcurves not implemented'
   case('rpatch')           ; write(*,*)'*call_draw* rpatch not implemented'
   case('patch')            ; write(*,*)'*call_draw* patch not implemented'
   case('curvebasis')       ; write(*,*)'*call_draw* curvebasis not implemented'
   case('curveprecision')   ; write(*,*)'*call_draw* curveprecision not implemented'
   case('rcurve')           ; write(*,*)'*call_draw* rcurve not implemented'
   case('curve')            ; write(*,*)'*call_draw* curve not implemented'
   case('curven')           ; write(*,*)'*call_draw* curven not implemented'
   ! M_drawplus
   case('invokeobj')        ; call invokeobj(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4),rnum0(x5),  &
                                            & rnum0(x6),rnum0(x7),rnum0(x8),rnum0(x9),inum0(x10) )
   case('biggest_ortho2')   ; call page(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4))
   case('page')             ; call page(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4))
   case('pop')              ; call pop()
   case('push')             ; call push()
   case('spirograph')       ; call spirograph(rnum0(x1),rnum0(x2), &             ! xcenter, ycenter
                                            & rnum0(x3),rnum0(x4),rnum0(x5), &   ! sunr0, planet0, offset0
                                            & rnum0(x6), &                       ! radius
                                            & inum0(x7), &                       ! ilines
                                            & rnum0(x8), &                       ! ang
                                            & rnum0(x9), &                       ! angs
                                            & inum0(x10) )                       ! ifill
!-----------------------------------------------------------------------------------------------------------------------------------
   case('vgetdev')
     call vgetdev(ctemp1)
     if(x1.ne.' ')then
        call stuffa(x1,ctemp1,'')
     else
        call stuffa('$VFUNCTION',ctemp1,'')
     endif
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getdepth')
     value1=getdepth()
     if(icount.ge.1)then               ! if extra option assume it is variable name to store into
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')  ! use VFUNCTION to return M_draw(3fm) function values (not generic, causes nesting problems)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getkey')
     value1=getkey()
     if(icount.ge.1)then             ! if no variable specified for return, simply pause
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('checkkey')
     value1=checkkey()
     if(icount.ge.1)then             ! if variable specified for return store returned ordinal
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getstring')
     value1=getstring(inum0(x1),ctemp1)
     call stuff('VFUNCTION',value1,'')  ! use VFUNCTION to return M_draw(3fm) function values (not generic, causes nesting problems)
     if(icount.ge.2)then
        call stuffa(x2,ctemp1,'')
     else
        call stuffa('$VFUNCTION',ctemp1,'')
     endif
!-----------------------------------------------------------------------------------------------------------------------------------
   case('set')
     do i=1,icount
        value0=rnum0(array(i))
     enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   case('locator')
     value0=locator(value1,value2)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
     if(icount.ge.3)then
        call stuff(x3,value0,'')
     endif
     call stuff('VFUNCTION',value0,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('slocator')
     value0=slocator(value1,value2)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
     if(icount.ge.3)then
        call stuff(x3,value0,'')
     endif
     call stuff('VFUNCTION',value0,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getviewport')
     call getviewport(value1,value2,value3,value4)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
     call stuff(x3,value3,'')
     call stuff(x4,value4,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getaspect')
     value1 =  getaspect()
     if(icount.ge.1)then               ! if extra option assume it is variable name to store into
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getfactors')
     call getfactors(value1,value2)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getdisplaysize')
     call getdisplaysize(value1,value2)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('move2') ! move2 x1 y1 x2 y2 ...xn yn ---> move2(x1,x2);draw2(x2,y2);draw2(x3,y3);....draw2(xn,yn)
     !call move2(rnum0(x1),rnum0(x2))
     call strgar2(parameters,isize,numbrs,inums,' :',ierr)
     call move2(numbrs(1),numbrs(2))
     do i=3,inums,2     ! sloppy assumes icount divisible by two, big enough.
        call draw2(numbrs(i),numbrs(i+1))
     enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   case('draw2') ! draw2 x1 y1 x2 y2 ...xn yn ---> draw2(x1,x2);draw2(x2,y2);draw2(x3,y3);....draw2(xn,yn)
     !call draw2(rnum0(x1),rnum0(x2))
     call strgar2(parameters,isize,numbrs,inums,' :',ierr)
     do i=1,inums,2     ! sloppy assumes icount divisible by two, big enough
        call draw2(numbrs(i),numbrs(i+1))
     enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   case('poly2') ! poly2 followed by points that define polygon
!    poly2 x1 y1 x2 y2 ...xn yn ---> poly2(n,x(),y())
     call strgar2(parameters,isize,numbrs,inums,' :',ierr)
     n2=0
     do i=1,inums,2     ! sloppy assumes icount divisible by two, big enough.; use new features of fortran to resize
        n2=n2+1
        points2(1,n2)=numbrs(i)
        points2(2,n2)=numbrs(i+1)
     enddo
     call poly2(n2,points2)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('poly') ! poly followed by points that define polygon
!    poly x1 y1 z1 x2 y2 z2 ...xn yn zn ---> poly(n,x(),y(),z())
     call strgar2(parameters,isize,numbrs,inums,' :',ierr)
     n2=0
     do i=1,inums,3     ! sloppy assumes icount divisible by three, big enough.
        n2=n2+1
        points(1,n2)=numbrs(i)
        points(2,n2)=numbrs(i+1)
        points(3,n2)=numbrs(i+2)
     enddo
     call poly(n2,points)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('numchars') ! wants a parameter, unlike actual function
     value1=real(numchars())
     if(icount.ge.1)then               ! if extra option assume it is variable name to store into
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getcharsize')
     if(icount.eq.2)then ! if icount equal 2 character was a blank
        call getcharsize(' ',value1,value2)
        call stuff(x1,value1,'')
        call stuff(x2,value2,'')
     else
        ch(1:1)=snum0(x1)
        call getcharsize(ch,value2,value3)
        call stuff(x2,value2,'')
        call stuff(x3,value3,'')
     endif
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getfontsize')
     call getfontsize(value1,value2)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('drawchar')
     if(icount.le.0)then
        call drawchar(' ')
     else
        ch(1:1)=snum0(x1)
        call drawchar(ch)
     endif
!-----------------------------------------------------------------------------------------------------------------------------------
   case('drawstr')
     if(x1(1:1).eq.'$'.or.x1(1:1).eq.'"')then
        call drawstr( trim(snum0(parameters)))
     else
        call drawstr(trim(parameters))
     endif
!-----------------------------------------------------------------------------------------------------------------------------------
   case('strlength')
     temp1=trim(parameters)
     if(temp1(1:1).eq.'$'.or.temp1(1:1).eq.'"')then
        temp1=snum0(temp1)
     endif
     value1=strlength(temp1)
     call stuff('VFUNCTION',value1,'')  ! use VFUNCTION to return M_draw(3fm) function values (not generic, causes nesting problems)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('boxtext')
     temp1=trim(parameters(is(5):))
     if(temp1(1:1).eq.'$'.or.temp1(1:1).eq.'"')then ! if a string expression
        temp1=snum0(temp1)
     endif
     call boxtext(rnum0(x1),rnum0(x2),rnum0(x3),rnum0(x4),trim(temp1))
!-----------------------------------------------------------------------------------------------------------------------------------
   case('rotate')
     temp1=snum0(x2)
     if(temp1(1:1).eq.' ')temp1='z'
     ! parameter must be a single character for ISO_C_BINDING, at least with gfortran(1)
     ch(1:1)=snum0(x2)
     call rotate(rnum0(x1),ch)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('genobj')
     value1 = genobj()
     if(icount.ge.1)then
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getopenobj')
     value1 = getopenobj()
     if(icount.ge.1)then
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('isobj')
     if(isobj(inum0(x1)))then
        value1=0
     else
        value1=1
     endif
     if(icount.ge.2)then               ! if extra option assume it is variable name to store into
        call stuff(x2,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')  ! use VFUNCTION to return M_draw(3fm) function values (not generic, causes nesting problems)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('backbuffer')
     value1=backbuffer()
     if(icount.ge.1)then               ! if extra option assume it is variable name to store into
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getgp')
     call getgp(value1,value2,value3)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
     call stuff(x3,value3,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getgp2')
     call getgp2(value1,value2)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('sgetgp2')
     call sgetgp2(value1,value2)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('textslant')
     rval=rnum0(x1)
     call textslant(rval)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('textweight')
     ival=inum0(x1)
     call textweight(ival)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getgpt')
     call getgpt(value1,value2,value3,value4)
     call stuff(x1,value1,'')
     call stuff(x2,value2,'')
     call stuff(x3,value3,'')
     call stuff(x4,value4,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case('getfontdec')
     value1 = getfontdec()
     if(icount.ge.1)then
        call stuff(x1,value1,'')
     endif
     call stuff('VFUNCTION',value1,'')
!-----------------------------------------------------------------------------------------------------------------------------------
   case default
      found=.false.
!-----------------------------------------------------------------------------------------------------------------------------------
   end select
end subroutine call_draw
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
logical function iflogic(string)
use M_calculator, only : inum0
use M_strings,    only : lower

! ident_9="@(#) M_drawplus iflogic(3fp) evaluate string in calculator and return false if value is zero"

character(len=*)           :: string
   select case(lower(string))
   case('.true.','t','.t.','true')
      iflogic=.true.
   case('.false.','f','.f.','false')
      iflogic=.false.
   case default
   if(inum0(string).eq.0)then    ! evaluate string and test if result is zero
      iflogic=.false.            ! result was zero, so return .false.
   else
      iflogic=.true.             ! result was not zero, so return .true.
   endif
   end select
end function iflogic
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!! spirograph(3f) - [M_drawplus] draw a hypotrochoid
!! (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!     subroutine spirograph(xcenter,ycenter,sunr0,planet0,offset0,radius,ilines,ang,angs,ifill)
!!
!!      real,intent(in)    :: xcenter, ycenter
!!      real,intent(in)    :: sunr0,planet0,offset0
!!      real,intent(in)    :: radius
!!      integer,intent(in) :: ilines
!!      real,intent(in)    :: ang
!!      real,intent(in)    :: angs
!!      integer,intent(in) :: ifill
!!
!!##DESCRIPTION
!!    Draw a hypotrochoid generated by a fixed point on a circle rolling inside a
!!    fixed circle. It has the parametric equations
!!
!!       x = (R+r)costheta-(r+rho)cos((R+r)/rtheta)
!!       y = (R+r)sintheta-(r+rho)sin((R+r)/rtheta)
!!
!!    where R is the radius of the fixed circle, r is the radius of the rotating
!!    circle, and rho is the offset of the edge of the rotating circle. The figure
!!    closes only if R, r, and rho are rational. The equations can also be written
!!
!!##OPTIONS
!!    xcenter,ycenter     center of curve
!!    sunr0               radii of sun, planet, and planet offset
!!    planet0             radii of sun, planet, and planet offset
!!    offset0             radii of sun, planet, and planet offset
!!    radius              radius to fit the shape to (no fit if radius is 0)
!!    ilines              number of points to sample along curve
!!    ang                 angle to rotate the shape by, to orientate it.
!!    angs                angle to start sampling points at; ccw is +; 0 is East
!!    ifill               1 make a filled polygon, 2 make a hatched polygon
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
!===================================================================================================================================
subroutine spirograph(xcenter,ycenter,sunr0,planet0,offset0,radius,ilines,ang,angs,ifill)
!
!     Make shapes for use as markers using hypocycloidal curves.
!     Huge variety of shapes can be generated using this routine.
!===================================================================================================================================
use M_draw

! ident_10="@(#) M_drawplus spirograph(3f) draw hypocycloidal curves"

real,parameter :: PI= 3.14159265358979323846264338327950288419716939937510
real,intent(in)    :: xcenter, ycenter      ! center of curve
real,intent(in)    :: sunr0,planet0,offset0 ! radii of sun, planet, and planet offset
real,intent(in)    :: radius                ! radius to fit the shape to (no fit if radius is 0)
integer,intent(in) :: ilines                ! number of points to sample along curve
real,intent(in)    :: ang                   ! angle to rotate the shape by, to orientate it.
real,intent(in)    :: angs                  ! angle to start sampling points at; ccw is +; 0 is East
integer,intent(in) :: ifill                 ! 1 make a filled polygon, 2 make a hatched polygon
real               :: ang1
real               :: con1
real               :: con2
real               :: factor
integer            :: i10
real               :: offset
real               :: planet
real               :: r
real               :: sunr
real               :: u
real               :: xpoin
real               :: xpoin1
real               :: ypoin
real               :: ypoin1

   sunr=sunr0
   offset=offset0
   planet=planet0

   if(ilines.eq.0) return
   if(planet.eq.0.0) return
   if(sunr.eq.0.0)   return

   if(radius.ne.0.and.sunr-planet+offset.ne.0)then
      factor=radius/(sunr-planet+offset)
      sunr=factor*sunr
      planet=factor*planet
      offset=factor*offset
   endif

   u=0.0+ang
   con1=PI*2.*(sunr/planet)/real(ilines)
   con2=(1.0-planet/sunr)*u
   xpoin1=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
   ypoin1=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)

   ang1=atan2(ypoin1,xpoin1)+angs
   r=sqrt(xpoin1**2+ypoin1**2)
   xpoin1=r*cos(ang1)+xcenter
   ypoin1=r*sin(ang1)+ycenter

   !call push()

   select case(ifill)
   case(0)
   case(1)
      call polyfill(.true.)
      call makepoly()
   case(2)
      call polyhatch(.true.)
      call makepoly()
   case(3:)
      call makepoly()
   case default
   end select

   call move2(xpoin1,ypoin1)
   do i10=1,ilines
      u=con1*i10+ang
      con2=(1.0-planet/sunr)*u
      if(con2.ge.2**24) con2=amod(con2,PI)
      xpoin=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
      ypoin=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)

      ang1=atan2(ypoin,xpoin)+angs
      r=sqrt(xpoin**2+ypoin**2)
      xpoin=r*cos(ang1)+xcenter
      ypoin=r*sin(ang1)+ycenter

      call draw2(xpoin,ypoin)
   enddo

   if(ifill.gt.0)then
     call closepoly()
     call polyfill(.false.)
   endif

   !call pop()

end subroutine spirograph
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE smoot(Xn,Yn,Ic)
   USE m_draw
   IMPLICIT NONE

!! public :: ismoo,ismoo1,ismoo2,ismoo3,perin

!$@(#) M_drawplus::smoot(3f): draw smooth curve thru set up points using spline-fitting technique

!
! 4.4  subroutine smoot
!      ----------------
! general description:
!
! smoot is a fortran procedure which draws a smooth curve through a
! set of data points.  it accomplishes this by using a modified
! spline-fitting technique.  the procedure receives a single
! coordinate pair on each call and accumulates the points until it has
! received a sufficient number to compute a pair of cubic parametric
! equations for a smooth curve.  this accumulation method requires the
! user to specify an initial and a terminal call to the procedure.
!
! the smoot procedure operates in either of two modes:  smooth mode
! and plot mode.
!
! calling sequence:
!
!      call smoot (xpage,ypage,ipen)
!
!      xpage,ypage     are the coordinates, in inches, of a single point through
!                      which the pen moves.
!
!      ipen            determines the mode and action of the smoot procedure.
!
! detailed description:
!
! the first call to smoot must use an ipen value of 0 or -1 to put
! smoot in the smooth mode.
!
! if ipen = 0, xpage,ypage define the initial point (p(1)) on the
! curve.  the smoothing function ends at the last point (p(n)).  an
! open curve is produced.
!
! if ipen = -1, xpage,ypage are used to define the initial point (p(1))
! on the curve.  the smoothing function continues from the last point
! (p(n)) back to the initial point (p(1)).  a closed curve is produced.
!
! smooth mode:
!
! when smoot is in the smooth mode, ipen performs the following
! functions:
!
!  ipen = -2       xpage,ypage are used to define points p(2), p(3),...,
!                  p(n-1), and a smoothed curve is drawn through the points
!                  on the curve.
!
!  ipen = -3       xpage,ypage are used to define points p(2), p(3),
!                  ...,p(n-1), and the pen, in the up position, is moved
!                  through these points.  the smoothing function is
!                  maintained.
!
!  ipen = 2 or 3   the call is treated as a normal call plot_in
!                  (xpage,ypage,ipen), and the point is not considered a
!                  point on the curve.  the point of departure from the
!                  curve is the next-to-last point received by smoot, not
!                  the last point.
!
! when the next call to smoot with ipen = -2 or -3 is received, the pen
! is repositioned to the point where it left the smooth curve.  the
! smooth curve is then continued as though the calls with ipen = 2 or 3
! had not occurred.
!
! ipen <=(-24) is used for the terminal call while smoot is in the
! smooth mode.  xpage,ypage represent p(n).  the curve is finished, and
! the procedure returns to the plot mode.
!
!  plot mode:
!
! smoot is in the plot mode after receiving a terminal call.  when in
! plot mode, ipen = +-2 or +-3, the call is treated as a normal call
! plot (xpage,ypage,ipen).
!
! comments:
!
! when smoot is called while it is in the smooth mode, the pen is not
! moved until three points on an open curve or four points on a closed
! curve have been received.  for subsequent calls to smoot, the actual
! pen position is the next-to-last point received.
!
! calls to other plotting procedures may be intermixed with calls to
! smoot.  point-of-departure restrictions are the same as noted in the
! smooth mode description above.
!
! the first call to smoot must be with ipen = 0 or -1.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcccccccc
!...procedure     smoot     v068a     07/30/69  product number 99013
!...copyright 1968 california computer products
!........10 char   plot      bcd       inches
!        the smooth routine simulates the plot routine with a new plot
!     mode (drawing a smooth curve to the new point).  the smooth mode
!     is initialized with the units digit of ic = 0 (for an open curve)
!     or = 1 (for a closed curve).
!                     the value of ic for smoothing is the negative of
!     the pen values for plotting.  there is, therefore, no re-origining
!     while smoothing.  using positive values for ic while smoothing
!     will be treated as a call to plot.  to end the curve and return to
!     the plot mode let ic be less than -23.
! earlier version of this procedure was
!     subroutine smooth (xn,yn,ic)
   REAL :: Xn
   REAL :: Yn
   INTEGER :: Ic
   INTEGER :: ismoo
   INTEGER :: ismoo1
   INTEGER :: ismoo2
   INTEGER :: ismoo3
   REAL :: perin
   REAL :: ax
   REAL :: ay
   REAL :: bx
   REAL :: by
   REAL :: d
   REAL :: d1
   REAL :: d2
   REAL :: d3
   REAL :: dv
   INTEGER :: i
   INTEGER :: ipc
   INTEGER :: irep
   INTEGER :: isw
   INTEGER :: jsw
   INTEGER :: kc
   INTEGER :: lc
   INTEGER :: mc
   INTEGER :: n
   INTEGER :: nc
   REAL :: pxn
   REAL :: pyn
   REAL :: sx1
   REAL :: sx2
   REAL :: sx3
   REAL :: sy1
   REAL :: sy2
   REAL :: sy3
   REAL :: t
   REAL :: uux1
   REAL :: uux2
   REAL :: uuy1
   REAL :: uuy2
   REAL :: ux1
   REAL :: ux2
   REAL :: uy1
   REAL :: uy2
   REAL :: vx2
   REAL :: vx3
   REAL :: vy2
   REAL :: vy3
   REAL :: x
   REAL :: x1
   REAL :: x2
   REAL :: x3
   REAL :: y
   REAL :: y1
   REAL :: y2
   REAL :: y3


   SAVE
   INTEGER :: spag_nextblock_1
   DATA nc/0/ , ipc/0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         kc = Ic - (Ic/10)*10
                        ! kc is last digit of ic, of same sign as ic
         lc = nc - ipc
         pxn = Xn
         pyn = Yn
         irep = 0
         IF ( kc<0 ) THEN
            IF ( kc+1<0 ) THEN
               IF ( ipc>0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Ic+24>0 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( ipc+1>=0 ) THEN
                  kc = nc + 2
                  ipc = 1
                  CALL plot_in(x3,y3,mc)
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( isw<1 ) THEN
                  IF ( isw+1==0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( isw==1 ) THEN
                  irep = 2
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( kc+1==0 ) THEN
               isw = 1
            ELSE
               isw = -1
            ENDIF
         ELSEIF ( kc==0 ) THEN
            isw = -1
         ELSE
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         jsw = -1
         nc = -kc/10*10
         x3 = pxn
         y3 = pyn
         mc = nc + 3
         spag_nextblock_1 = 2
      CASE (2)
         ipc = kc
         RETURN
      CASE (3)
         CALL plot_in(pxn,pyn,kc)
         RETURN
      CASE (4)
         irep = irep + 1
         kc = 1
         spag_nextblock_1 = 5
      CASE (5)
         IF ( iabs(jsw)/=1 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         x1 = x2
         y1 = y2
         x2 = x3
         y2 = y3
         x3 = pxn
         y3 = pyn
         IF ( (ipc+1)>=0 ) THEN
            vx3 = x3 - x2
            vy3 = y3 - y2
            d3 = vx3*vx3 + vy3*vy3
            sx1 = x2
            sx2 = x3
            sy1 = y2
            sy2 = y3
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( jsw==0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( jsw<0 ) THEN
            IF ( isw==0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( isw<0 ) THEN
               vx2 = x3 - x2
               vy2 = y3 - y2
               CALL reflx(vx3,vy3,vx2,vy2)
               d2 = vx2*vx2 + vy2*vy2
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSE
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         jsw = 1
         spag_nextblock_1 = 8
      CASE (8)
         vx2 = vx3
         vy2 = vy3
         vx3 = x3 - x2
         vy3 = y3 - y2
         spag_nextblock_1 = 9
      CASE (9)
         d2 = d3
         ux1 = ux2
         uy1 = uy2
         spag_nextblock_1 = 10
      CASE (10)
         d3 = vx3*vx3 + vy3*vy3
         ux2 = d2*vx3 + d3*vx2
         uy2 = d2*vy3 + d3*vy2
         dv = 1.0/sqrt(ux2*ux2+uy2*uy2+0.000001)
         ux2 = dv*ux2
         uy2 = dv*uy2
         IF ( (isw-jsw)<=0 ) THEN
            IF ( jsw<0 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( jsw==0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            t = 0.
            CALL getgp2(x,y)
                        ! d is current scaling factor (call where(x,y,d)
            d = 1.0/100.0
                  ! playing with this to see if controls number of points
            IF ( abs(x1-x)-0.01*d>=0 .OR. abs(y1-y)>=0.01*d ) CALL plot_in(x1,y1,mc)
            IF ( ipc+3/=0 ) THEN
               d = abs(ux1*vx2+uy1*vy2)
               d1 = d
               uux1 = d*ux1
               uuy1 = d*uy1
               d = abs(ux2*vx2+uy2*vy2)
               uux2 = d*ux2
               uuy2 = d*uy2
               d = d + d1
               ax = uux2 + uux1 - vx2 - vx2
               bx = vx2 - uux1 - ax
               ay = uuy2 + uuy1 - vy2 - vy2
               by = vy2 - uuy1 - ay
!        perin is number of points to interpolate per unit distance between
!        point1 and point2
               n = int(perin*d+1.0)
               d = 1.0/float(n)
               DO i = 1 , n
                  t = t + d
                  x = ((ax*t+bx)*t+uux1)*t + x1
                  y = ((ay*t+by)*t+uuy1)*t + y1
                  CALL plot_in(x,y,lc)
               ENDDO
            ENDIF
         ELSE
            jsw = 1
            sx3 = x3
            sy3 = y3
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
         IF ( irep<=0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         irep = irep - 1
         IF ( isw<0 ) THEN
            CALL reflx(vx3,vy3,vx2,vy2)
            x = vx3
            y = vy3
            vx3 = vx2
            vy3 = vy2
            vx2 = x
            vy2 = y
            x1 = x2
            y1 = y2
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( isw>0 ) THEN
            pxn = sx1
            pyn = sy1
            sx1 = sx2
            sy1 = sy2
            sx2 = sx3
            sy2 = sy3
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSE      ! isw.eq.0
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      END SELECT
   ENDDO SPAG_DispatchLoop_1
CONTAINS
!===================================================================================================================================
   SUBROUTINE reflx(Vx1,Vy1,Vx2,Vy2)

!$@(#) M_drawplus::reflx(3fp): internal routine called by SMOOT

      REAL , INTENT(IN) :: Vx1
      REAL , INTENT(IN) :: Vy1
      REAL , INTENT(OUT) :: Vx2
      REAL , INTENT(OUT) :: Vy2
      REAL ps
      REAL ds
      REAL ss
      REAL temp
      ps = Vy1*Vy1
      ds = Vx1*Vx1
      ss = ds + ps + 0.00001
      ds = ds - ps
      ps = 2.0*Vx1*Vy1
      temp = (ps*Vy2+Vx2*ds)/ss
      Vy2 = (ps*Vx2-Vy2*ds)/ss
      Vx2 = temp
   END SUBROUTINE reflx
!===================================================================================================================================
   SUBROUTINE plot_in(X,Y,Lc)
      USE m_draw

!$@(#) M_drawplus::plot_in(3fp): internal routine called by SMOOT

      REAL , INTENT(IN) :: X
      REAL , INTENT(IN) :: Y
      INTEGER , INTENT(IN) :: Lc
      IF ( abs(Lc)==3 .AND. Ismoo1/=1 ) CALL move2(X,Y)
      IF ( abs(Lc)==3 .AND. Ismoo1==1 ) CALL draw2(X,Y)
      IF ( abs(Lc)==2 ) CALL draw2(X,Y)
   END SUBROUTINE plot_in
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
END SUBROUTINE smoot
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    rdbox(3f) - [M_drawplus:locator] - reads two points and outline defined box and return points
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!     subroutine rdbox(returnx1,returny1,returnx2,returny2,ikey)
!!
!!      real,intent(out)    :: returnx1, returny1
!!      real,intent(out)    :: returnx2, returny2
!!      integer,intent(out) :: ikey
!!
!!##DESCRIPTION
!!
!!    In workstation windows click, hold down mouse, release at opposite
!!    corner to define the opposite corners of a box. The box is drawn
!!    using current drawing attributes. The IKEY value indicates which
!!    mouse was pressed.
!!
!!    Note that on a device such as a Tektronix terminal (or emulator,
!!    such as xterm(1)) where you have to wait for a keypress to get the
!!    position of the crosshairs LOCATOR(3f) returns 0 automatically on
!!    every second call. So RDBOX(3f) waits for the locator to return zero
!!    so that we know the mouse button has been released.
!!
!!    As the mouse is moved a dot is drawn at each point, leaving a trail
!!    marking the mouse motion. Simple directions are written to stdout,
!!    and when a box is defined the coordinates of the corners are printed:
!!
!!     *rdbox* READY
!!     *rdbox* RELEASE MOUSE AT OTHER CORNER
!!     corners are 0.311320752 0.584905684 and 0.311320752 0.584905684 ; and key is 4
!!
!!
!!##OPTIONS
!!
!!    RETURNX1,RETURNY1   return coordinates of first corner of box
!!
!!    RETURNX2,RETURNY2   return coordinates of opposite corner of box
!!
!!    IKEY                returns the identifier of the mouse that was pressed,
!!                        using the LOCATOR(3f) function from the M_DRAW(3fm) module.
!!
!!                        o A return value of 2 indicates the second mouse button
!!                          has been pressed.
!!
!!                        o A return value of 1 indicates the first mouse button
!!                          has been pressed.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_rdbox
!!      use M_drawplus, only : rdbox
!!      use M_draw
!!      implicit none
!!      real    :: x1, y1, x2, y2
!!      integer :: key
!!      call vinit(' ')
!!      call color(D_GREEN)
!!      do
!!         call rdbox(x1,y1,x2,y2,key)
!!         if(key.le.0)exit
!!         ! if the mouse is clicked twice without moving exit the loop
!!         if(x1.eq.x2 .and. y1.eq.y2)exit
!!         write(*,*)'corners are ',x1,y1,' and ',x2,y2,'; and key is ',key
!!         call move2(x1,y1)
!!         call draw2(x2,y2)
!!         call move2(x1,y2)
!!         call draw2(x2,y1)
!!      enddo
!!      call vexit()
!!      end program demo_rdbox
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
subroutine rdbox(returnx1,returny1,returnx2,returny2,ikey)
use M_journal, only : journal
use M_draw
implicit none

! ident_11="@(#) M_drawplus rdbox(3f) reads two points and outline defined box and return points"

real,intent(out)    :: returnx1, returny1
real,intent(out)    :: returnx2, returny2
integer,intent(out) :: ikey
logical             :: release1, release2
integer             :: ibt
real                :: con1
real                :: x, y

   call getgp2(returnx1,returny1)
   returnx2=returnx1
   returny2=returny1
   release1 = .false.                                ! assume pressing a key until a 0 is received, then assume ready to start
   release2 = .false.                                ! assume pressing a key until a 0 is received, then assume ready to start
   ikey=-1

   INFINITE: do
      call vflush()
      ibt = locator(x, y)                         ! note that this function SETS the x and y variables, which is not always portable
      if (ibt .eq. -1) then                       ! not an interactive device; could just ask for numbers?
         call journal('*rdbox* no locator device found')
         exit INFINITE
      else if (ibt .eq. 0 .and. (.not.release1)) then   ! haven't hit the null state yet
         release1 = .true.                              ! ready to wait for first keypress
         call journal('s','*rdbox* READY')
      else if (ibt .eq. 0 .and. release2) then          ! this is the second release
         returnx2=x
         returny2=y
         exit INFINITE
      else if (ibt .ne. 0 .and. (.not.release2)) then   ! this is the first read
         returnx1 = x
         returny1 = y
         ikey=ibt
         release2=.true.                           ! ready for release
         con1=(returnx1-returnx2)/4000.0           ! warning: con1 is an arbitrary number, trying to make a small move to make a dot
         call move2(x-con1,y)
         call draw2(x+con1,y)
         call move2(x,y-con1)
         call draw2(x,y+con1)
         call journal('s','*rdbox* RELEASE MOUSE AT OTHER CORNER')
      else if (release1) then                        ! make points as you move around; not as nice as rubber-banding but OK
         call point2(x,y)
      endif
   enddo INFINITE

   call journal('s',char(07))                        ! send bell character
end subroutine rdbox
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     rdpnt(3f) - [M_drawplus:locator] reads coordinates of point locator clicked at
!!     (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!     subroutine rdpnt(oldx,oldy,sx,sy,ibut)
!!
!!##DESCRIPTION
!!    Move to the initial point given, and then move to the next point selected by
!!    the locator, and return the new point location in world coordinates and the
!!    mouse or button number used to select the new point. Send a bell character
!!    to stdout upon return.
!!
!!    This routine simplifies the differences in use between mouse selections
!!    made on a Tektronix terminal or terminal emulator and other devices such as
!!    X11 Windows servers.
!!
!!##OPTIONS
!!
!!      OLDX, OLDY  initial point to move to
!!      SX, SY      new point selected by locator and draw to from initial point
!!      IBUT        mouse button value returned by LOCATOR(3f) procedure. If the
!!                  value is -1 no locator device is supported.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_rdpnt
!!    use M_drawplus, only : rdpnt
!!    use M_draw
!!    implicit none
!!    real    :: x1, y1, x2, y2
!!    integer :: key
!!    call vinit(' ')
!!    x1=0.0
!!    y1=0.0
!!     write(*,*)'click at the same point twice to end the loop'
!!    do
!!       call rdpnt(x1,y1,x2,y2,key)
!!       if(key.le.0)exit
!!       ! if the mouse is clicked twice without moving exit the loop
!!       if(x1.eq.x2 .and. y1.eq.y2)exit
!!       write(*,*)'points are ',x1,y1,' and ',x2,y2,'; and key is ',key
!!       call color(D_RED)
!!       call circle(x1,y1,0.03)
!!       call color(D_GREEN)
!!       call circle(x2,y2,0.05)
!!       x1=x2
!!       y1=y2
!!    enddo
!!    call vexit()
!!    end program demo_rdpnt
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
subroutine rdpnt(oldx,oldy,selected_x,selected_y,ibut)
use M_journal,  only : journal
use M_draw
implicit none

! ident_12="@(#) M_drawplus rdpnt(3f) reads coordinates of point locator clicked at"

! see M_DRAW locator(3f) description for more details
real,intent(in)     :: oldx, oldy
real,intent(out)    :: selected_x, selected_y
integer,intent(out) :: ibut
real    :: x, y
logical :: act
logical :: curpnt

   call move2(oldx,oldy)
   call draw2(oldx,oldy)
   act = .false.   ! have you activated a button since the read started?
   curpnt = .false.
   selected_x=oldx
   selected_y=oldy
!     locator returns whether a mouse button has been
!     pressed or not. In a device such as the tektronix
!     where you have to wait for a keypress to get the
!     position of the crosshairs locator returns 0
!     automatically on every second call. A return value
!     of 2 indicates the second mouse button has been pressed.
!     A return value of 1 indicates the first mouse button has
!     been pressed. We wait for the locator to return zero so
!     that we know the mouse button has been released.
   INFINITE: do
      call vflush()
      ibut = locator(x, y)
      if (ibut .eq. -1) then
         call journal('*rdpnt* no locator device found')
         exit INFINITE
      elseif(ibut .ge. 2 .and. act)then
         selected_x=x
         selected_y=y
         exit INFINITE
      elseif(ibut .eq. 0)then
         act = .true.
      elseif(act) then
         act = .false.
         if (ibut .eq. 1)then
            if (curpnt)then
               call move2(selected_x, selected_y)
               call draw2(x, y)
               curpnt = .false.
            else
               curpnt = .true.
            endif
            selected_x = x
            selected_y = y
            exit INFINITE
         endif
      endif
   enddo INFINITE
   call journal('s',char(07))       ! send bell character
end subroutine rdpnt
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    seefont(3f) - [M_drawplus] display font sample pages
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!     seefont(fontin)
!!
!!     character(len=128),intent(in) :: fontin
!!##DESCRIPTION
!!   The seefont(3f) routine displays pages of sample fonts.
!!
!!    o blank name: show sample of all fonts, then details on each one.
!!    o known font name: show chart on just that font
!!    o unknown name: show sample of all fonts and quit
!!    o pause between pages, in graphics area use q(uit) to quit,
!!      n(ext) to display the next font, and p(revious) for previous font.
!!      A numeric string shows font by number.
!!##OPTIONS
!!    fontin  name of font to display
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_seefont
!!    use M_draw
!!    use M_drawplus, only : seefont
!!    implicit none
!!    character(len=128) :: fontname
!!    integer            :: iwidth
!!       call prefsize(1000,800)
!!       call vinit(' ')
!!       call linewidth(20)
!!       call seefont(' ')
!!       call vexit()
!!    end program demo_seefont
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
subroutine seefont(fontin)
use M_journal, only : journal
use m_calculator,      only : stuff
use m_calculator,      only : dnum0
use M_draw
implicit none

! ident_13="@(#) display sample page of a font"

character(len=*),intent(in)   :: fontin
character(len=80)             :: line
integer,parameter             :: ifontnames=33
character(len=20)             :: fontname(0:ifontnames)
doubleprecision               :: rval8
integer                       :: BGCOLOR   ! background color
integer                       :: FCOLOR    ! font color
integer                       :: NCOLOR    ! number color
integer                       :: LCOLOR    ! little letter color
integer                       :: BCOLOR    ! box color
real                          :: back
integer                       :: i10
integer                       :: i20
integer                       :: i30
integer                       :: i35
integer                       :: i40
integer                       :: i80
integer                       :: i90
integer                       :: ibox
integer                       :: icols
integer                       :: icount
integer                       :: idelta
integer                       :: idum
integer                       :: iend
integer                       :: ilen
integer                       :: iordinal
integer                       :: irows
integer                       :: istart
real                          :: rleft
real                          :: step
real                          :: tdec
real                          :: tsize
real                          :: xmax
real                          :: xmin
real                          :: y
real                          :: ymax
real                          :: ymin

   fontname( 1)='astrology'
   fontname( 2)='cursive'
   fontname( 3)='cyrillic'
   fontname( 4)='futura.l'
   fontname( 5)='futura.m'
   fontname( 6)='gothic.eng'
   fontname( 7)='gothic.ger'
   fontname( 8)='gothic.ita'
   fontname( 9)='greek'
   fontname(10)='markers'
   fontname(11)='math.low'
   fontname(12)='math.upp'
   fontname(13)='meteorology'
   fontname(14)='music'
   fontname(15)='script'
   fontname(16)='symbolic'
   fontname(17)='times.g'
   fontname(18)='times.i'
   fontname(19)='times.ib'
   fontname(20)='times.r'
   fontname(21)='times.rb'
   fontname(22)='japanese'
   fontname(23)='small'
   fontname(24)='large'
   fontname(25)='orall_aa'
   fontname(26)='orall_ab'
   fontname(27)='orall_ac'
   fontname(28)='orall_ad'
   fontname(29)='orall_ae'
   fontname(30)='orall_af'
   fontname(31)='orall_ag'
   fontname(32)='orall_ah'
   fontname(33)='orall_ai'

   idum=backbuffer()
   !write(*,*)'bg, f, n, l, b'
   !read(*,*)bgcolor,fcolor,ncolor,lcolor,bcolor
   BGCOLOR=0
   FCOLOR=7
   NCOLOR=1
   LCOLOR=4
   BCOLOR=2
   iordinal=-1 ! initialize for when no page displayed
!-----------------------------------------------------------------------------------------------------------------------------------
   !  calculate ISTART and IEND and IDELTA (flag if to show one font)
   istart=-1
   iend=-1
   if(fontin.eq. ' ')then  ! if string is blank, show all fonts
      istart=1
      iend=ifontnames
   else                    ! look up string as a font name.
      ! display all font names as text(and see if input string matches one)
      do i20=0,ifontnames
         if(fontin.eq.fontname(i20))then ! if fontname matches
            istart=i20
            iend=i20
            exit
         endif
      enddo
      if(istart.lt.0)then ! no match to fontnames, so try string as a number
         rval8=dnum0(fontin)
         if(rval8.le.0.or.rval8.gt.ifontnames)then
            call journal('*seefont* unknown font name/number')
            do i90=1,ifontnames   ! list all font names
               line=fontname(i90)//'is font '
               call journal('sc',line(:len_trim(line)),i90)
            enddo
            return
         else ! got a number in range
            istart=int(rval8+0.5)
            istart=min(ifontnames,istart)
            istart=max(istart,0)
            iend=istart
         endif
      endif
   endif
   !  if istart is -1 or 0 no match, else a specific font was asked for
!-----------------------------------------------------------------------------------------------------------------------------------
   idelta=iend-istart
!-----------------------------------------------------------------------------------------------------------------------------------
   do i80=1,ifontnames   ! list all font names
      line=fontname(i80)//'is font '
      call journal('sc',line(:len_trim(line)),i80)
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
     icount=0  ! clear X11 key buffer on X11 on 1 (anybody else need this?)
100  continue  ! flush key presses in case someone has been clicking around
     idum=checkkey()
     icount=icount+1
     if(idum.gt.0.and.icount.lt.100)then
        !call journal('sc','*seefont* flushing ',idum)
        goto 100
     endif
!-----------------------------------------------------------------------------------------------------------------------------------
! draw sample page with a line of each font
   if(idelta.gt.0)then  ! doing all fonts
     idum=backbuffer()
     call makeobj(12345)
     call push()          ! save graphics environment
     call polyfill(.false.)
     call color(BGCOLOR)
     call clear()                   ! clear display area to background color
     icount=1
     irows=ifontnames
     icols=irows
     ! set window and viewport so each box 10 units on a side
     call page((-icols)*5.0,icols*5.0,-irows*5.0,irows*5.0)
     call color(FCOLOR)
     !call move2((-icols)*5.0,(-irows)*5.0)
     !call draw2(( icols)*5.0,( irows)*5.0)
     !call move2((-icols)*5.0,( irows)*5.0)
     !call draw2(( icols)*5.0,(-irows)*5.0)
     call centertext(.false.)
     call fixedwidth(.false.)
     tsize=4.3*2.8*0.85
     step=1.88
     call textsize(tsize,tsize*4.0/3.0)
     rleft=(-icols)*5.0+4.0
     y=irows*5.0+tsize*.5
     do i35=1,ifontnames
        call font('futura.l')
        tdec=getfontdec()
        y=y-step*tsize
        call move2(rleft,y)
        call rmove2(0.0,tdec+tsize/2.0)
        write(line,'(i3,'')'')')i35
        call drawstr(line(1:4))
        call drawstr(fontname(i35))
        call move2(rleft+8.3*tsize,y)
        call font(fontname(i35))      ! select text font for numbers
        tdec=getfontdec()
        call rmove2(0.0,tdec+tsize/2.0)
        call drawstr('@ABCZabcz012')
        if(y.lt.(-irows)*5.0+tsize*step.or.i35.eq.ifontnames)then
           y=irows*5.0+tsize*.5
           rleft=rleft+(2*icols*5.0)/2.0
        endif
     enddo

     call pop()  ! restore graphics environment
     call closeobj()
     call callobj(12345)
     call swapbuffers()
     call vflush()
     call journal('*seefont* enter q(uit) or n(ext) in graphic area')
     iordinal=getkey()
     if(iordinal.eq.113)then
        call stuff('PLTOBJECT',12345.0d0,'')
        return
     endif
     ! instructions for multi-font display
     call journal('*seefont* #=========================#')
     call journal('*seefont* | In graphics area press: |')
     call journal('*seefont* |    n for next font      |')
     call journal('*seefont* |    p for previous font  |')
     call journal('*seefont* |    q to quit            |')
     call journal('*seefont* #=========================#')

   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  do font-specific pages in detail
   irows=10
   icols=10
   icount=0   ! no infinite loops
60 continue   ! come here with new istart value if 'p' for previous
   do 10 i10=istart,iend
      idum=backbuffer()
      call makeobj(12345)
      icount=icount+1

      call push()          ! save graphics environment
      call polyfill(.false.)
      call color(BGCOLOR)            ! background color
      call clear()                   ! clear display area to background color

      ! lay out window with boxes are 10x10, with room above for 10x100 title
      ! The window value 0,0 is in the middle of the box area

      ! set window and viewport so each box 10 units on a side
                 ! xsmall,      xlarge,   ysmall,        ylarge
      call page((-icols)*5.0,icols*5.0,-irows*5.0-9.0,irows*5.0+10.0)
      call color(FCOLOR)
               ! x1,          y1,             x2,        y2
      call rect((-icols)*5.0, -irows*5.0-9.0, icols*5.0, irows*5.0+10.0)

      call centertext(.false.)       ! all text should be centered
      tsize=8.0

      y=irows*5.0+10.0-tsize
      call move2((-icols)*5.0+4.0,y)

      call font('times.rb')          ! select text font for numbers
      call textsize(tsize,tsize)
      ilen=len_trim(fontname(i10))
      write(line,'(a)')fontname(i10)(1:ilen)
      call drawstr(line)

      call textsize(tsize/2.0,tsize/2.0)
      call drawstr('(')
      write(line,'(i3)')i10
      if(i10.lt.10)then
         call drawstr(line(3:3))
      else
         call drawstr(line(2:3))
      endif
      call drawstr(')')
!-----------------------------------------------------------------------------------------------------------------------------------
!     draw the boxed letters
      ibox=33
      do i30=1,irows
         do i40=1,icols
            xmin=(i40-1)*10.0-icols*5.0
            xmax=xmin+10.0
            ymax=irows*5.0-(i30-1)*10.0
            ymin=ymax-10.0
            call color(BCOLOR)
            call rect(xmin,ymin,xmax,ymax)

            call color(FCOLOR)
            call centertext(.true.)    ! all text should be centered
            call textsize(5.5,5.5)
            call font(fontname(i10))   ! select text font for numbers
            write(line,'(a1)')char(ibox)
            call move2(xmin+5.0,ymin+5.0)
            call drawstr(line)

            call centertext(.false.)   ! all text should be centered
            call font('futura.m')      ! select text font for numbers
            call textsize(2.0,2.5)

            call color(NCOLOR)
            write(line,'(i3)')ibox
            call move2(xmin,ymin)
            call drawstr(line)

            call color(LCOLOR)
            write(line,'(a1)')char(ibox)
            back=strlength(line)
            call move2(xmax-back*2,ymin)
            call drawstr(line)

            ibox=ibox+1
            if(ibox.ge.128)goto 50
         enddo
      enddo
50    continue
!-----------------------------------------------------------------------------------------------------------------------------------
      call pop()           ! restore graphics environment
      call closeobj()
      call callobj(12345)
      call swapbuffers()
      call vflush()
      if(icount.gt.200) goto 999  ! been in here too long, assume a loop in a batch job
      if(idelta.le.0)goto 999  ! originally single font requested
      iordinal=getkey()
      if(iordinal.eq.113)then     ! quit on "q" key
         goto 999
      elseif(iordinal.eq.110)then ! next on "n" key but at end
      elseif(iordinal.eq.112)then ! back to previous on "p" key
         istart=i10-1
         if(istart.le.0)istart=ifontnames
         iend=ifontnames
         goto 60
      endif
10 continue

   if(iordinal.le.-1)goto 999  ! not an interactive graphics device so end
   istart=1
   iend=ifontnames
   go to 60 ! keep going until a q is entered or hit end, then quit
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue
   call stuff('PLTOBJECT',12345.0d0,'')
end subroutine seefont
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine test_suite_M_drawplus()
use M_verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_verify, only : unit_test_level
implicit none
!! setup
   call test_arrowhead()
   call test_barcode()
   call test_call_draw()
   call test_draw_interpret()
   call test_ellipse()
   call test_plain_rect()
   call test_polyline2_i()
   call test_polyline2_r()
   call test_rdbox()
   call test_rdpnt()
   call test_seefont()
   call test_smoot()
   call test_spirograph()
   call test_uarc()
   call test_uconic()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_arrowhead()
implicit none
   call unit_test_start('arrowhead',msg='')
   !!call unit_test('arrowhead', 0.eq.0, 'checking',100)
   call unit_test_done('arrowhead',msg='')
end subroutine test_arrowhead
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_barcode()
implicit none
   call unit_test_start('barcode',msg='')
   !!call unit_test('barcode', 0.eq.0, 'checking',100)
   call unit_test_done('barcode',msg='')
end subroutine test_barcode
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_call_draw()
implicit none
   call unit_test_start('call_draw',msg='')
   !!call unit_test('call_draw', 0.eq.0, 'checking',100)
   call unit_test_done('call_draw',msg='')
end subroutine test_call_draw
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_draw_interpret()
implicit none
   call unit_test_start('draw_interpret',msg='')
   !!call unit_test('draw_interpret', 0.eq.0, 'checking',100)
   call unit_test_done('draw_interpret',msg='')
end subroutine test_draw_interpret
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ellipse()
implicit none
   call unit_test_start('ellipse',msg='')
   !!call unit_test('ellipse', 0.eq.0, 'checking',100)
   call unit_test_done('ellipse',msg='')
end subroutine test_ellipse
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plain_rect()
implicit none
   call unit_test_start('plain_rect',msg='')
   !!call unit_test('plain_rect', 0.eq.0, 'checking',100)
   call unit_test_done('plain_rect',msg='')
end subroutine test_plain_rect
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_polyline2_i()
implicit none
   call unit_test_start('polyline2_i',msg='')
   !!call unit_test('polyline2_i', 0.eq.0, 'checking',100)
   call unit_test_done('polyline2_i',msg='')
end subroutine test_polyline2_i
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_polyline2_r()
implicit none
   call unit_test_start('polyline2_r',msg='')
   !!call unit_test('polyline2_r', 0.eq.0, 'checking',100)
   call unit_test_done('polyline2_r',msg='')
end subroutine test_polyline2_r
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rdbox()
implicit none
   call unit_test_start('rdbox',msg='')
   !!call unit_test('rdbox', 0.eq.0, 'checking',100)
   call unit_test_done('rdbox',msg='')
end subroutine test_rdbox
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rdpnt()
implicit none
   call unit_test_start('rdpnt',msg='')
   !!call unit_test('rdpnt', 0.eq.0, 'checking',100)
   call unit_test_done('rdpnt',msg='')
end subroutine test_rdpnt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_seefont()
implicit none
   call unit_test_start('seefont',msg='')
   !!call unit_test('seefont', 0.eq.0, 'checking',100)
   call unit_test_done('seefont',msg='')
end subroutine test_seefont
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_smoot()
implicit none
   call unit_test_start('smoot',msg='')
   !!call unit_test('smoot', 0.eq.0, 'checking',100)
   call unit_test_done('smoot',msg='')
end subroutine test_smoot
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_spirograph()
implicit none
   call unit_test_start('spirograph',msg='')
   !!call unit_test('spirograph', 0.eq.0, 'checking',100)
   call unit_test_done('spirograph',msg='')
end subroutine test_spirograph
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_uarc()
implicit none
   call unit_test_start('uarc',msg='')
   !!call unit_test('uarc', 0.eq.0, 'checking',100)
   call unit_test_done('uarc',msg='')
end subroutine test_uarc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_uconic()
implicit none
   call unit_test_start('uconic',msg='')
   !!call unit_test('uconic', 0.eq.0, 'checking',100)
   call unit_test_done('uconic',msg='')
end subroutine test_uconic
!===================================================================================================================================
end subroutine test_suite_M_drawplus
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end module M_drawplus
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
