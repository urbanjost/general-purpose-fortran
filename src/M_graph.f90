!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
module m_graph
implicit none
private
public   graph_init
public   graph
public   plot

private  axisc_
private  axislg_
private  clipit_
private  color_
private  dashcode_
private  dashscale_
private  draw_
private  gridll_
private  move_
private  newpen_
private  number_
private  plot_
private  range_
private  rect_
private  symbol_
private  translate_
private  trs_
private  viewport_
private  width_
!-----------------------------------------------------------------------------------------------------------------------------------
logical,save :: debug=.false.
!-----------------------------------------------------------------------------------------------------------------------------------
! plot coordinate system
real,save    :: TRANSLATEXQ                ! TRANSLATEXQ  SCALED ROTATED ORIGIN X VALUE
real,save    :: TRANSLATEYQ                ! TRANSLATEYQ  SCALED ROTATED ORIGIN Y VALUE
real,save    :: XMINQ,YMINQ,XMAXQ,YMAXQ
real,save    :: SCALEQ                     ! ZOOM SCALE FACTOR
real,save    :: XLASTSCALEQ                ! LAST SCALED, SHIFTED X VALUE
real,save    :: YLASTSCALEQ                ! LAST SCALED, SHIFTED Y VALUE
real,save    :: ANGLEQ                     ! PLOTTING ANGLE
real,save    :: VIEWPORTQ(4)               ! VIEWPORTQ(4) VIEWPORT PARAMETERS
!-----------------------------------------------------------------------------------------------------------------------------------
integer,save :: ILINEQ                     ! LINE TYPE
integer,save :: IWIDTHQ                    ! CURRENT LINE WIDTH
integer,save :: ICOLORQ                    ! CURRENT LINE COLOR
integer,save :: IDASHSCALEQ                ! DEFAULT LINE SCALING FACTOR
real,save    :: XSCALEQ,YSCALEQ,ZSCALEQ,AMINQ,ALPHQ,BETQ
!-----------------------------------------------------------------------------------------------------------------------------------
type :: grid
   real :: xmin, xmax, ymin, ymax
end type grid
!-----------------------------------------------------------------------------------------------------------------------------------
type :: plot
!     1  0<.<=NDP NUMBER OF POINTS PLOTTED [NDP]
!     2  0<.<=NDL NUMBER OF LINES PLOTTED [NDL]

!     3  -1/0/1   X AUTO SCALING: 0=AUTO,SMOOTH; 1=AUTO,NOSMOOTH; -1=USER
!     4  XMIN     USER SUPPLIED SCALE VALUE (USED IF F(3)<0)
!     5  XMAX     USER SUPPLIED SCALE VALUE (USED IF F(3)<0)

!     6  -1/0/1   Y AUTO SCALING: 0=AUTO,SMOOTH; 1=AUTO,NOSMOOTH; -1=USER
!     7  YMIN     USER SUPPLIED SCALE VALUE (USED IF F(6)<0)
!     8  YMAX     USER SUPPLIED SCALE VALUE (USED IF F(6)<0)

!     9  0/1      USE X VALUES WITH Y VALUES: 0=ONLY FIRST LINE OF
!                 X DATA USED FOR ALL Y LINES; 1=LINES OF X/Y USED

!     10 0/1      CONNECT PLOTTED POINTS: 0=YES; 1=NO
!     11 >=0      SYMBOL PLOTTED EVERY ()TH POINT: 0=NO SYMBOLS
!     12 >=0      SYMBOL SIZE [0.1]: 0=USE DEFAULT
!     13 >=0      SYMBOL NUMBER FOR FIRST DATA LINE

!     14 -8<.<8   SHOW ERROR BARS: 0=NO ERROR BARS
!                 WHEN NON-ZERO, CHANGES INTERPRETATION OF LINES
!                 POINTS EVERY THIRD LINE ARE PLOTTED.  RELATIVE
!                 ERROR (UPPER,LOWER) ARE THE FOLLOWING TWO LINES.
!                 PLOTTED RELATIVE ERROR COMPUTED AS SUM OF FIRST
!                 LINE PLUS SECOND/THIRD.
!          <0     AN X IS MADE AT THE CENTER POINT
!
!        ABS(.)  TYPE OF ERROR BAR
!        ------  ---------------------------------------------------
!          1   LINE CONNECTING 2ND/3RD RELATIVE ERRORS
!          2   1+HORIZONTAL BARS OF WIDTH EBAR AT 2ND/3RD REL ERR
!          3   1+VERTICAL BARS OF WIDTH EBAR AT 2ND/3RD REL ERR
!          4   2 LINES SPACED EBAR/2+HORIZONTAL BARS WIDTH=EBAR
!          5   2 LINES SPACED EBAR/2+VERTICAL BARS WIDTH=EBAR
!          6   VERTICAL RECTANGLE FROM 2ND/3RD WITH WIDTH=EBAR
!          7     RECTANGLE WITH CORNER AT 2ND/3RD REL ERROR
!
!     16 >=0   ERROR BAR SIZE [0.1]: 0=DEFAULT USED

!     17 0/1   VERTICAL DRAWN LINE FROM POINT TO REFERENCE VALUE:
!           0=NO; 1=YES
!     18    REFERENCE VALUE

!     19 -1/0/1   X AXIS TYPE: 0=LINEAR; 1=LOG AXIS, -1=NO AXIS
!     20 -1/0/1   Y AXIS TYPE: 0=LINEAR; 1=LOG AXIS, -1=NO AXIS
!     21 >=0      X AXIS LENGTH [7.0]: 0=DEFAULT USED
!     22 >=0      Y AXIS LENGTH [5.0]: 0=DEFAULT USED

!     23 >=0      X AXIS TICK PATTERN (SEE axisc_) [7.00]: 0=DEFAULT
!     24 >=0      Y AXIS TICK PATTERN (SEE axisc_) [5.00]: 0=DEFAULT

!     25 0/1      X AXIS TITLE SIDE OF AXIS: 0=BELOW; 1=ABOVE
!     26 0/1      Y AXIS TITLE SIDE OF AXIS: 0=LEFT; 1=RIGHT

!     27 0/1      X AXIS AUTO EXPONENT ENABLE: 0=ENABLE; 1=DISABLE
!     28 0/1      Y AXIS AUTO EXPONENT ENABLE: 0=ENABLE; 1=DISABLE

!     29 0/1      X AXIS TICK SIDE: 0=BELOW; 1=ABOVE
!     30 0/1      Y AXIS TICK SIDE: 0=LEFT; 1=RIGHT

!     31 0/1      X AXIS NUMBERS ORIENTATION: 0=HORIZONTAL; 1=VERTICAL
!     32 0/1      Y AXIS NUMBERS ORIENTATION: 0=VERTICAL; 1=HORIZONTAL

!     33 0/1      X AXIS NUMBERS/TITLE SHOWN: 0=SHOWN; 1=NOT SHOWN
!     34 0/1      Y AXIS NUMBERS/TITLE SHOWN: 0=SHOWN; 1=NOT SHOWN

!     35 0/1      TAKE LOG10(ABS(X VALUES)+1.E-34): 0=NO; 1=YES
!     36 0/1      TAKE LOG10(ABS(Y VALUES)+1.E-34): 0=NO; 1=YES

!     37 -1/0/1   ADD MIRROR X AXIS: 0=NO; 1=WITH LABELS; -1:W/O LABELS
!     38 -1/0/1   ADD MIRROR Y AXIS: 0=NO; 1=WITH LABELS; -1:W/O LABELS

!     39 >=0      X AXIS LABEL SIZE [0.15]: 0=USE DEFAULT
!     40 >=0      Y AXIS LABEL SIZE [0.15]: 0=USE DEFAULT

!     41 >=0      TOP TITLE CHARACTER SIZE [0.18]: 0=USE DEFAULT

!     42 0/1/2/3  GRID OPTION: 0=NO GRID; 1=SOLID; 2=DOTTED; 3=TICKED

!     43 -1/0/1   LINE/SYMBOL LEGEND: 0=NO LEGEND; 1=AUTO PLACE; -1=USER
!     44 X        USER SPECIFIED LOWER-LEFT CORNER OF LEGEND
!     45 Y        USER SPECIFIED LOWER-LEFT CORNER OF LEGEND
!     46 0/1      SHOW PLOT SYMBOL WITH LEGEND: 0=NO; 1=YES
!     47 0/1      SHOW LINE SEGMENT ON LEGEND: 0=NO; 1=YES
!     48 >=0      LEGEND CHARACTER HEIGHT [0.12]: 0=USE DEFAULT
!     49 >=0      LEGEND SEGMENT LENGTH [0.5]: 0=USE DEFAULT

!     50 -1/0/1   TOP TITLE JUSTIFY: 0=CENTER; -1:LEFT; 1:RIGHT

!     51 0/1      PLOT HORIZONTAL REFERENCE LINE: 0=NO; 1=YES

!     52 0/1      USE LINE TYPE ARRAY VALUES: 0=NO; 1=YES

!     53 0/1      USE COLOR ARRAY VALUES: 0=NO; 1=YES
!     54 >=0      COLOR INDEX 1: 0=COLOR 1 USED
!     55 >=0      LINETYPE INDEX 1
!     56 >=0      COLOR INDEX 2: 0=COLOR 1 USED
!     57 >=0      LINETYPE INDEX 2
!     ...         ... ETC ...
!
!     INDEX    COLOR USAGE        LINETYPE USAGE
!     -------  ------------       --------------
! 54    1      X AXIS              X AXIS
! 56    2      X AXIS NUMBERS      Y AXIS
! 58    3      X AXIS TITLE        TITLE
! 60    4      X AXIS EXPONENT     LEGEND TITLES
! 62    5      Y AXIS              ZERO REF LINETYPE
! 64    6      Y AXIS NUMBERS      ERROR BAR LINETYPES
! 66    7      Y AXIS TITLE LINE   1 LINETYPE
! 68    8      Y AXIS EXPONENT     ETC.
! 70    9      TITLE               ...
! 72   10      LEGEND TITLES       ...
! 74   11      ZERO REF LINE       ...
! 76   12      GRID COLOR          ...
! 78   13      LINE 1 COLOR        ...
!     ...      ... ETC.   ...      ...
!
contains

procedure :: graph => graph_one
procedure :: table => graph_table

end type plot
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine graph_one(this,x,y)
class(plot), intent(inout) :: this
real,intent(in) :: x(*)
real,intent(in) :: y(*)
!   call graph(x,y,ndp,ndl,f,c,nc)
end subroutine graph_one
!==================================================================================================================================!
subroutine graph_table(this,xx,yy)
class(plot), intent(inout) :: this
real,intent(in) :: xx(:)
real,intent(in) :: yy(:,:)
integer             :: ndp
integer             :: ndl ! number of data lines
character(len=80)   :: c(size(yy,dim=2)+3)
integer             :: nc(size(yy,dim=2)+3)
integer,parameter   :: nf=255
real                :: f(nf)
integer             :: i
   ndp=size(xx)
   ndl=size(yy,dim=2)
   call default()
   call graph(xx,yy,ndp,ndl,f,c,nc)
contains
subroutine default()
   f=0.0 !     zero out option array
   ! set up color and linetype in option array arbitrarily
   do i=55,nf,2
      f(i)=mod(i,7)
      f(i-1)=mod(i,7)
   enddo

    f(54)=7  ! X AXIS
    f(62)=7  ! Y AXIS
   f(56)=7  ! X AXIS NUMBERS
   f(64)=7  ! Y AXIS NUMBERS
   f(60)=7  ! X AXIS EXPONENT
   f(68)=7  ! Y AXIS EXPONENT

   f(58)=2  ! X AXIS TITLE
   f(66)=2  ! Y AXIS TITLE
   f(70)=2  ! TITLE

   f(72)=7  ! LEGEND TITLES

   f(74)=7  ! ZERO REF LINE
   f(76)=3  ! GRID COLOR

   f(3)=1
   f(6)=1
   f(52)=1
   f(53)=1
   f(32)=1 ! 0/1    Y AXIS NUMBERS ORIENTATION: 0=VERTICAL; 1=HORIZONTAL
   f(11)=1 !        SYMBOL PLOTTED EVERY ()TH POINT: 0=NO SYMBOLS

   f(42)=2 ! 0/1/2/3  GRID OPTION: 0=NO GRID; 1=SOLID; 2=DOTTED; 3=TICKED
   f(43)=1 ! -1/0/1   LINE/SYMBOL LEGEND: 0=NO LEGEND; 1=AUTO PLACE; -1=USER
   f(46)=1 ! 0/1      SHOW PLOT SYMBOL WITH LEGEND: 0=NO; 1=YES
   f(47)=1 ! 0/1      SHOW LINE SEGMENT ON LEGEND: 0=NO; 1=YES

   f(39)=0.12      !  >=0      X AXIS LABEL SIZE [0.15]: 0=USE DEFAULT
   f(40)=0.12      !  >=0      Y AXIS LABEL SIZE [0.15]: 0=USE DEFAULT
   f(41)=0.16      !  >=0      TOP TITLE CHARACTER SIZE [0.18]: 0=USE DEFAULT

   ! array of strings dimensioned c(3+ndp)
   c(1)='X-axis'   !   x axis title
   c(2)='Y-axis'   !   y axis title
   c(3)='Title'    !   top title

   ! default labels for curves (optionally used)
   do i=1,ndl
      write(c(i+3),'("curve ",i0)')i
   enddo

   ! array of string lengths in c() dimensioned nc(3+ndl)
   do i=1,3+ndl
      nc(i)=len_trim(c(i))
   enddo

end subroutine default

end subroutine graph_table
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    graph(3) - [M_graph::INTRO] Draw an XY graph
!!##SYNOPSIS
!!
!!    subroutine graph(x,y,ndp,ndl,f,c,nc)
!!##DESCRIPTION
!!
!!    This is an XY graph routine that is intended to be very self-contained and
!!    easily ported to various graphics libraries. This version is callable from
!!    the M_draw(3f) module.
!!
!!    It is being ported from the LONGLIB public domain library and will be
!!    changing. It is recommended that you rename the M_graph(3f) module source
!!    to freeze a copy at this point, as new versions will not necessarily be
!!    upward compatible. The underlying calls to M_graph(3f) should not require
!!    any changes.
!!
!!    graph(3f) is a general routine for plotting lines or scatterplots. Optionally,
!!    error bars and symbols can be added. graph(3f) differs in philosophy from other
!!    libDL MASTER routines in that plotting options are passed via an option array.
!!    The routine is designed to produce a reasonable plot with the option array set
!!    to all zeros. The output format is changed by initializing selected array
!!    elements to the values described below. This permits simple but flexible
!!    specification of the plot format.
!!
!!##OPTIONS
!!
!!    x     (R): x input array dimensioned x(ndp,ndl).
!!    y     (R): y input array dimensioned y(ndp,ndl).
!!    ndp   (i): number of data points per line
!!    ndl   (i): number of data lines
!!    f     (R): option array dimensioned at least f(53)
!!
!!               (described below)
!!    c     (C): array of strings dimensioned C(3+ndl)
!!
!!                c(1) : x axis title
!!                c(2) : y axis title
!!                c(3) : top title
!!                c(4) : line 1 legend (optionally used)
!!                c(5) : line 2 legend (optionally used)
!!                ...        ...
!!    nc    (i): array of string lengths dimensioned nc(3+ndl)
!!
!!                nc(1) : number of characters to use in C(1)
!!                nc(2) : number of characters to use in C(2)
!!                 ...       ...
!!
!!    The array elements of the option array f are interpreted according to the
!!    following table. Some parameters have default values (shown in square
!!    brackets). These are used when the input value is zero. A simple plot may be
!!    produced by setting all the elements of f to zero. Note that user specified
!!    input scaling factors should be powers of ten when the log axis specification
!!    is selected. An optional legend may be plotted. The legend consists of a column
!!    of lines and the labels from n(4:)
!!
!!      array  range of
!!      index  values   action for each value
!!     ------- -------- ------------------------
!!
!!       1    0<=.<=ndp number of points/line to plot [ndp]: 0=ndp used
!!       2    0<=.<=ndl number of lines to plot [nl]: 0=ndl used
!!       3     -1/0/1   x scale: 0=auto,smoothed; 1=auto,nosmooth; -1=user
!!       4      xmin    user supplied scale value (used if f(3)<0)
!!       5      xmax    user supplied scale value (used if f(3)<0)
!!       6     -1/0/1   y scale: 0=auto,smoothed; 1=auto,nosmooth; -1=user
!!       7      ymin    user supplied scale value (used if f(6)<0)
!!       8      ymax    user supplied scale value (used if f(6)<0)
!!       9      0/1     x value usage: 0=first line of x data array
!!                      used for all y lines; 1=lines of x,y paired
!!       10     0/1     connect plotted points: 0=yes; 1=no
!!       11     >=0     symbol plotted every ()th point: 0=no symbols
!!       12     >=0     line symbol size [0.1]: 0=use default
!!       13     >=0     symbol number for first data line, each line
!!                      then uses next symbol in sequence
!!       14     8<.<8   error bar option (see below): 0=no error bars
!!       16     >=0     error bar size [0.1]: 0=default used
!!       17     0/1     vertical line from points to reference value:
!!                      0=no; 1=yes
!!       18     rval    reference value
!!       19    -1/0/1   x axis type: 0=linear; 1=log axis, -1=no axis
!!       20    -1/0/1   y axis type: 0=linear; 1=log axis, -1=no axis
!!       21     >=0     x axis length [7.0]: 0=default used
!!       22     >=0     y axis length [5.0]: 0=default used
!!       23     >=0     x axis tick pattern (see axisc_) [7.00]: 0=default
!!       24     >=0     y axis tick pattern (see axisc_) [5.00]: 0=default
!!       25     0/1     x axis title side of axis: 0=below; 1=above
!!       26     0/1     y axis title side of axis: 0=left; 1=right
!!       27     0/1     x axis auto exponent enable: 0=enable; 1=disable
!!       28     0/1     y axis auto exponent enable: 0=enable; 1=disable
!!       29     0/1     x axis tick side: 0=below; 1=above
!!       30     0/1     y axis tick side: 0=left; 1=right
!!       31     0/1     x axis number orientation: 0=horizontal; 1=vertical
!!       32     0/1     y axis number orientation: 0=vertical; 1=horizontal
!!       33     0/1     x axis numbers/title: 0=shown; 1=not shown
!!       34     0/1     y axis numbers/title: 0=shown; 1=not shown
!!       35     0/1     use x=log10(abs(x values)+1.e-34): 0=no; 1=yes
!!       36     0/1     use y=log10(abs(y values)+1.e-34): 0=no; 1=yes
!!       37   -1/0/1    add mirror x axis: 0=no; 1=w/labels; -1:w/o labels
!!       38   -1/0/1    add mirror y axis: 0=no; 1=w/labels; -1:w/o labels
!!                      (mirrored axes placed on opposite from normal axis)
!!       39     >=0     x axis label size [0.15]: 0=use default
!!       40     >=0     y axis label size [0.15]: 0=use default
!!       41     >=0     top title character size [0.18]: 0=use default
!!       42   0/1/2/3   grid: 0=no grid; 1=solid; 2=dotted; 3=ticked
!!       43   -1/0/1    legend: 0=no legend; 1=right side; -1=user locate
!!       44     xval    user specified lower-left corner of legend
!!       45     yval    user specified lower-left corner of legend
!!       46     0/1     show plot symbol on legend: 0=no; 1=yes
!!       47     0/1     show line segment on legend: 0=no; 1=yes
!!       48     >=0     legend character height [0.12]: 0=use default
!!       49     >=0     legend line segment length [0.5]: 0=use default
!!       50   -1/0/1    top title justify: 0=center; -1:left; 1:right
!!       51    0/1      plot horizontal reference line: 0=no; 1=yes
!!       52    0/1      use linetype array values: 0=no; 1=yes
!!       53    0/1      use color array values: 0=no; 1=yes
!!       54    >=0      color index #1: 0=color value 1 used
!!       55    >=0      linetype index #1
!!       56    >=0      color index #2: 0=color value 1 used
!!       57    >=0      linetype index #2
!!       ...   ...      ... etc ...
!!
!!    The optional error bar specification, when non-zero, changes interpretation of
!!    lines. The first line (and every third line) is considered a "center" line. The
!!    second line specifies the relative error (to be added to the first line) used
!!    for plotting the tops of the error bars. The third line is used similarly to
!!    locate the bottoms of the error bars. When the error bar specification is
!!    negative the center line points are marked with a special "x" (in addition to
!!    any other option). The absolution value of the specification determines the type
!!    of error bar according to the following table.
!!
!!     value   type of error bar
!!     ------  ---------------------------------------------------
!!      1     line connecting relative errors
!!      2     1 + horizontal bars at relative errs
!!      3     1 + vertical bars at relative errs
!!      4     double line connecting rel. errs+horizontal bars
!!      5     double line connecting rel. errs+vertical bars
!!      6     vertical rectangle w/top and bottom rel. errs
!!      7     rectangle with corners at relative errors
!!
!!    The color and line type index (when enabled) are used according to the
!!    following table.
!!
!!     index #        color usage     linetype usage
!!     ---------      -------------   ----------------
!!       1              x axis         x axis
!!       2           x axis numbers    y axis
!!       3           x axis title      title
!!       4           x axis exponent   legend titles
!!       5              y axis         reference line
!!       6           y axis numbers    error bars
!!       7           y axis title      line #1 linetype
!!       8           y axis exponent   line #2 linetype
!!       9              title            etc.
!!      10             legend titles     ...
!!      11            reference line     ...
!!      12               grid            ...
!!      13             line #1 color     ...
!!      14             line #2 color     ...
!!      ...            ... etc. ...      ...
!!##EXAMPLES
!!
!!   Sample program that creates some simple curves and then
!!   prompts for index values and values in the options array
!!   to allow for testing various options.
!!
!!    program demo_graph
!!    use m_graph, only : graph, graph_init
!!    use M_draw
!!    implicit none
!!    integer,parameter            :: numlines=3
!!    integer,parameter            :: numpts=25
!!    integer,parameter            :: nf=255
!!    real                         :: x(numpts),y(numpts,numlines)
!!    real                         :: value
!!    real                         :: f(nf)
!!    character(len=80)            :: c(numlines+3)
!!    character(len=20)            :: device
!!    character(len=:),allocatable :: filename
!!    integer                      :: nc(numlines+3)
!!    integer                      :: ixsize
!!    integer                      :: iysize
!!    integer                      :: w, ndl, ndp
!!    integer                      :: i
!!    integer                      :: indx
!!       device='x11'
!!       ixsize=1200*.75*0.5
!!       iysize=900*.75*0.5
!!       w=40
!!       if(device.eq.'x11')then
!!          call prefposition(0,0)
!!          call prefsize(ixsize,iysize)
!!       else
!!          call prefsize(ixsize,iysize)
!!          !!call voutput(str(filename,'_',int(a),'x',int(b),'.',device,sep=''))
!!       endif
!!       call vinit(device)
!!       call vsetflush(.false.)
!!       call vflush()
!!       call linewidth(w)
!!
!!    !     fill some arrays with data we can plot
!!       DO i=1,25
!!          X(i)=i
!!          Y(i,1)=i**2+5.0
!!          Y(i,2)=i*20.0
!!          Y(i,3)=(-3.0)*(i/4.0)**3
!!       enddo
!!
!!       f=0.0 !     zero out option array
!!    !     set up color and linetype in option array
!!       do i=55,nf,2
!!          f(i)=mod(i,7)
!!          f(i-1)=mod(i,7)
!!       enddo
!!       f(52)=1
!!       f(53)=1
!!
!!       ndp=numpts! number of data points per line
!!       ndl=3     ! number of data lines
!!       !c        ! array of strings dimensioned c(3+numpts)
!!       c(1)='X-AXIS'  !  c(1) : x axis title
!!       c(2)='Y-AXIS'  !  c(2) : y axis title
!!       c(3)='TITLE'  !  c(3) : top title
!!       c(4)='LABEL 1'  !  c(n) : line N-3 legend (optionally used)
!!       c(5)='LABEL 2'  !  c(n) : line N-3 legend (optionally used)
!!       c(6)='LABEL 3'  !  c(n) : line N-3 legend (optionally used)
!!       !nc       ! array of string lengths in c() dimensioned nc(3+numpts)
!!       do i=1,6
!!          nc(i)=len_trim(c(i))
!!       enddo
!!       !f(39)= 0.25
!!       !f(40)= 0.25
!!       !f(42)= 3.00
!!       !f(43)=-1.00
!!    !  initialize graphics
!!       call graph_init(12.0,9.0, 1.00,1.00, 1.0)
!!       INFINITE: do
!!          CALL graph(X,Y,NDP,NDL,F,C,NC)
!!          call vflush()
!!    888   continue
!!          write(*,*)'Enter index and value for graph(3f) option array:'
!!          read(*,*,end=999,err=888)indx,value
!!          if(indx.lt.1)then
!!             exit INFINITE
!!          elseif(indx.gt.nf)then
!!             goto 888
!!          else
!!             f(indx)=value
!!             write(*,*)'indx ',indx,' now ',value
!!          endif
!!          call vflush()              ! flush graphics buffers
!!          call color(7)
!!          call clear()
!!          call color(0)
!!          call vflush()              ! flush graphics buffers
!!       enddo INFINITE
!!    999 continue
!!       call vflush()              ! flush graphics buffers
!!       call vexit()               ! close up plot package
!!       stop
!!    end program demo_graph
!!
!!##AUTHOR
!!    Based on [LONGLIB93.SOURCES.FORTRAN.MASTER]graph.FOR
!!
!! !     WRITTEN:  Version 1.0 SEPT. 1987 - DGL
!!                 Version 2.0 20-JAN-1994 17:13:12.80
!!                 Version 3.0 Wed Jun 12 10:34:11 MDT 1996
SUBROUTINE graph(X,Y,NDP,NDL,F,C,NC)

! ident_1="@(#) M_graph graph(3f) routine for plotting lines"

!
!     X  (R) X input array dimensioned X(NDP,NDL)
!            NOTE: depending on options selected, X
!            actually may be dimensioned X(NDP)
!     Y  (R) Y Input array dimensioned Y(NDP,NDL)
!     NDP   (I) number of points per line dimension
!     NDL   (I) number of lines dimension
!     F  (R) options array (described below)
!     C  (C) titles array
!             C(1) X axis title
!             C(2) Y axis title
!             C(3) chart title
!             C(4) legend text for line 1 (if legend enabled)
!             C(5) legend text for line 2 (if legend enabled)
!             ...   ...
!     NC (I) number of characters in title array
!            NC(1) number of characters in C(1)
!            NC(2) number of characters in C(2)
!            ...    ...
!            ETC.  NOTE: if NC(I) is zero C(I) is not plotted
!
!     the elements of F are interpreted according to the following:
!     default values are shown in square brackets
!
!   INDEX OF F VALUE    ACTION
!  ____________ _____   ___________________
!     1  0<.<=NDP NUMBER OF POINTS PLOTTED [NDP]
!     2  0<.<=NDL NUMBER OF LINES PLOTTED [NL]
!     3  -1/0/1   X AUTO SCALING: 0=AUTO,SMOOTH; 1=AUTO,NOSMOOTH; -1=USER
!     4  XMIN     USER SUPPLIED SCALE VALUE (USED IF F(3)<0)
!     5  XMAX     USER SUPPLIED SCALE VALUE (USED IF F(3)<0)
!     6  -1/0/1   Y AUTO SCALING: 0=AUTO,SMOOTH; 1=AUTO,NOSMOOTH; -1=USER
!     7  YMIN     USER SUPPLIED SCALE VALUE (USED IF F(6)<0)
!     8  YMAX     USER SUPPLIED SCALE VALUE (USED IF F(6)<0)
!     9  0/1      USE X VALUES WITH Y VALUES: 0=ONLY FIRST LINE OF
!                 X DATA USED FOR ALL Y LINES; 1=LINES OF X/Y USED
!     10 0/1      CONNECT PLOTTED POINTS: 0=YES; 1=NO
!     11 >=0      SYMBOL PLOTTED EVERY ()TH POINT: 0=NO SYMBOLS
!     12 >=0      SYMBOL SIZE [0.1]: 0=USE DEFAULT
!     13 >=0      SYMBOL NUMBER FOR FIRST DATA LINE
!     14 -8<.<8   SHOW ERROR BARS: 0=NO ERROR BARS
!                 WHEN NON-ZERO, CHANGES INTERPRETATION OF LINES
!                 POINTS EVERY THIRD LINE ARE PLOTTED.  RELATIVE
!                 ERROR (UPPER,LOWER) ARE THE FOLLOWING TWO LINES.
!                 PLOTTED RELATIVE ERROR COMPUTED AS SUM OF FIRST
!                 LINE PLUS SECOND/THIRD.
!          <0     AN X IS MADE AT THE CENTER POINT
!
!        ABS(.)  TYPE OF ERROR BAR
!        ------  ---------------------------------------------------
!          1   LINE CONNECTING 2ND/3RD RELATIVE ERRORS
!          2   1+HORIZONTAL BARS OF WIDTH EBAR AT 2ND/3RD REL ERR
!          3   1+VERTICAL BARS OF WIDTH EBAR AT 2ND/3RD REL ERR
!          4   2 LINES SPACED EBAR/2+HORIZONTAL BARS WIDTH=EBAR
!          5   2 LINES SPACED EBAR/2+VERTICAL BARS WIDTH=EBAR
!          6   VERTICAL RECTANGLE FROM 2ND/3RD WITH WIDTH=EBAR
!          7     RECTANGLE WITH CORNER AT 2ND/3RD REL ERROR
!
!     16 >=0   ERROR BAR SIZE [0.1]: 0=DEFAULT USED
!     17 0/1   VERTICAL DRAWN LINE FROM POINT TO REFERENCE VALUE:
!           0=NO; 1=YES
!     18    REFERENCE VALUE
!     19 -1/0/1   X AXIS TYPE: 0=LINEAR; 1=LOG AXIS, -1=NO AXIS
!     20 -1/0/1   Y AXIS TYPE: 0=LINEAR; 1=LOG AXIS, -1=NO AXIS
!     21 >=0      X AXIS LENGTH [7.0]: 0=DEFAULT USED
!     22 >=0      Y AXIS LENGTH [5.0]: 0=DEFAULT USED
!     23 >=0      X AXIS TICK PATTERN (SEE axisc_) [7.00]: 0=DEFAULT
!     24 >=0      Y AXIS TICK PATTERN (SEE axisc_) [5.00]: 0=DEFAULT
!     25 0/1      X AXIS TITLE SIDE OF AXIS: 0=BELOW; 1=ABOVE
!     26 0/1      Y AXIS TITLE SIDE OF AXIS: 0=LEFT; 1=RIGHT
!     27 0/1      X AXIS AUTO EXPONENT ENABLE: 0=ENABLE; 1=DISABLE
!     28 0/1      Y AXIS AUTO EXPONENT ENABLE: 0=ENABLE; 1=DISABLE
!     29 0/1      X AXIS TICK SIDE: 0=BELOW; 1=ABOVE
!     30 0/1      Y AXIS TICK SIDE: 0=LEFT; 1=RIGHT
!     31 0/1      X AXIS NUMBERS ORIENTATION: 0=HORIZONTAL; 1=VERTICAL
!     32 0/1      Y AXIS NUMBERS ORIENTATION: 0=VERTICAL; 1=HORIZONTAL
!     33 0/1      X AXIS NUMBERS/TITLE SHWON: 0=SHOWN; 1=NOT SHOWN
!     34 0/1      Y AXIS NUMBERS/TITLE SHWON: 0=SHOWN; 1=NOT SHOWN
!     35 0/1      TAKE LOG10(ABS(X VALUES)+1.E-34): 0=NO; 1=YES
!     36 0/1      TAKE LOG10(ABS(Y VALUES)+1.E-34): 0=NO; 1=YES
!     37 -1/0/1   ADD MIRROR X AXIS: 0=NO; 1=WITH LABELS; -1:W/O LABELS
!     38 -1/0/1   ADD MIRROR Y AXIS: 0=NO; 1=WITH LABELS; -1:W/O LABELS
!     39 >=0      X AXIS LABEL SIZE [0.15]: 0=USE DEFAULT
!     40 >=0      Y AXIS LABEL SIZE [0.15]: 0=USE DEFAULT
!     41 >=0      TOP TITLE CHARACTER SIZE [0.18]: 0=USE DEFAULT
!     42 0/1/2/3  GRID OPTION: 0=NO GRID; 1=SOLID; 2=DOTTED; 3=TICKED
!     43 -1/0/1   LINE/SYMBOL LEGEND: 0=NO LEGEND; 1=AUTO PLACE; -1=USER
!     44 X        USER SPECIFIED LOWER-LEFT CORNER OF LEGEND
!     45 Y        USER SPECIFIED LOWER-LEFT CORNER OF LEGEND
!     46 0/1      SHOW PLOT SYMBOL WITH LEGEND: 0=NO; 1=YES
!     47 0/1      SHOW LINE SEGMENT ON LEGEND: 0=NO; 1=YES
!     48 >=0      LEGEND CHARACTER HEIGHT [0.12]: 0=USE DEFAULT
!     49 >=0      LEGEND SEGMENT LENGTH [0.5]: 0=USE DEFAULT
!     50 -1/0/1   TOP TITLE JUSTIFY: 0=CENTER; -1:LEFT; 1:RIGHT
!     51 0/1      PLOT HORIZONTAL REFERENCE LINE: 0=NO; 1=YES
!     52 0/1      USE LINE TYPE ARRAY VALUES: 0=NO; 1=YES
!     53 0/1      USE COLOR ARRAY VALUES: 0=NO; 1=YES
!     54 >=0      COLOR INDEX 1: 0=COLOR 1 USED
!     55 >=0      LINETYPE INDEX 1
!     56 >=0      COLOR INDEX 2: 0=COLOR 1 USED
!     57 >=0      LINETYPE INDEX 2
!     ...         ... ETC ...
!
!     INDEX    COLOR USAGE LINETYPE USAGE
!     -------     ------------    --------------
!       1           X AXIS  X AXIS
!       2         X AXIS NUMBERS Y AXIS
!       3         X AXIS TITLE TITLE
!       4         X AXIS EXPONENT LEGEND TITLES
!       5      Y AXIS    ZERO REF LINETYPE
!       6         Y AXIS NUMBERS ERROR BAR LINETYPES
!       7         Y AXIS TITLE LINE 1 LINETYPE
!       8         Y AXIS EXPONENT     ETC.
!       9           TITLE    ...
!      10      LEGEND TITLES   ...
!      11      ZERO REF LINE   ...
!      12      GRID COLOR   ...
!      13      LINE 1 COLOR   ...
!     ...      ... ETC. ...      ...
!
CHARACTER(len=*) :: C(*)
integer  :: IC(4)
real     :: SMO(2)
real     :: csleg
real     :: dx
real     :: dy
real     :: ebar
real     :: f(*)
real     :: fx
real     :: fy
real     :: glen
integer  :: i
integer  :: icc
integer  :: icoff
integer  :: iloff
integer  :: ipen
integer  :: isym
integer  :: j
integer  :: jcol
integer  :: jconn
integer  :: jebar
integer  :: jgrid
integer  :: jlegli
integer  :: jlegnd
integer  :: jlegsy
integer  :: jline
integer  :: jsym
integer  :: jsymst
integer  :: jszref
integer  :: jtcntr
integer  :: jvline
integer  :: jxauto
integer  :: jxexpsc
integer  :: jxlgax
integer  :: jxlog
integer  :: jxnum
integer  :: jxnums
integer  :: jxrax
integer  :: jxticks
integer  :: jxtitle
integer  :: jxuse
integer  :: jyauto
integer  :: jyexpsc
integer  :: jylgax
integer  :: jylog
integer  :: jynum
integer  :: jynums
integer  :: jyrax
integer  :: jyticks
integer  :: jytitle
integer  :: k
integer  :: kebar
integer  :: ksym
integer  :: na
integer  :: nbar
integer  :: nc(*)
integer  :: ndl
integer  :: ndp
integer  :: ng
integer  :: nl
integer  :: np
real     :: symsiz
real     :: tcs
real     :: x(ndp,ndl)
real     :: x1
real     :: x2
real     :: x3
real     :: xcs
real     :: xleg
real     :: xlen
real     :: xm
real     :: xtick
real     :: xx
real     :: xy
real     :: y(ndp,ndl)
real     :: y1
real     :: y2
real     :: y3
real     :: ycs
real     :: yleg
real     :: ylen
real     :: ym
real     :: ytick
real     :: yx
real     :: zref
real     :: zval
!
!     ESTABLISH DEFAULTS
!
   np=ndp            ! NUMBER OF POINTS/LINE
   nl=ndl            ! NUMBER OF LINES
   xm=x(1,1)         ! MIN X VALUE
   xx=xm             ! MAX X VALUE
   ym=y(1,1)         ! MIN Y VALUE
   yx=ym             ! MAX Y VALUE
   fx=5.01           ! X AXIS NUMBER FORMAT
   fy=5.01           ! Y AXIS NUMBER FORMAT
   xlen=7.0          ! X AXIS LENGTH
   ylen=5.0          ! Y AXIS LENGTH
   xtick=7.0         ! X AXIS TICK PATTERN
   ytick=6.0         ! Y AXIS TICK PATTERN
   xcs=0.15          ! X AXIS TITLE SIZE
   ycs=0.15          ! Y AXIS TITLE SIZE
   tcs=0.18          ! OVERALL TITLE SIZE
   ebar=0.1          ! ERROR BAR SIZE
   symsiz=0.1        ! SYMBOL SIZE
   csleg=0.12        ! LEGEND CHARACTER SIZE
   glen=0.5          ! LEGEND LINE LENGTH
   xleg=xlen+0.3     ! DEFAULT LEGEND LOCATION
   yleg=0.5
!
!     READ OPTIONS ARRAY
!
   if (f(1).gt.0.0) np=f(1) ! NUMBER OF POINT/LINE
   if (np.gt.ndp) np=ndp
   if (f(2).gt.0.0) nl=f(2) ! NUMBER OF LINES
   if (nl.gt.ndl) nl=ndl
   jxauto=f(3)         ! AUTO SCALE X, 0=YES/SMOOTH,-1=NO/USER
!                   1=YES/NO SMOOTH
   if (jxauto.lt.0) then  ! INPUT X AXIS SCALE FACTORS
      xm=f(4)          ! MIN X (OTHERWISE RETURNED)
      xx=f(5)          ! MAX X
   endif
   jyauto=f(6)         ! AUTO SCALE Y, 0=YES/SMOOTH,-1=NO/USER
!                   1=YES/NO SMOOTH
   if (jyauto.lt.0) then  ! INPUT Y AXIS SCALE FACTORS
      ym=f(7)          ! MIN Y (OTHERWISE RETURNED)
      yx=f(8)          ! MAX Y
   endif
   jxuse=abs(f(9))     ! USE X LINES WITH Y LINES,0=NO,1=YES
   jconn=abs(f(10))    ! CONNECT POINTS WITH LINES,0=YES,1=NO
   jsym=abs(f(11))     ! PLOT SYMBOL EVERY JSYM PT,0=NO SYMS
   if (f(12).gt.0.0) symsiz=f(12) ! SYMBOL SIZE
   jsymst=abs(f(13))   ! STARTING PLOT SYMBOL NUMBER
   jebar=f(14)         ! TYPE OF ERROR BAR,0=NOT USED
!
!        |JEBAR|  ERROR BAR (CLINE IS LINE CONNECT 2ND/3RD REL. ERR)
!        -------  ---------------------------------------------------
!          1     CLINE
!          2     CLINE+HORIZONTAL BARS WIDTH=EBAR
!          3     CLINE+VERTICAL BARS WIDTH=EBAR
!          4     DOUBLE CLINE+HORIZONTAL BARS WIDTH=EBAR
!          5     DOUBLE CLINE+VERTICAL BARS WIDTH=EBAR
!          6     VERTICAL RECTANGLE WIDTH=EBAR
!          7       RECTANGLE DEFINED BY 2ND/3RD REL ERROR
!
!        NOTE: IF JBAR < 0 AN X (SIZE EBAR) IS USED TO MARK 1ST LINE
!
   kebar=iabs(jebar)
   nbar=1
   if (jebar.ne.0) nbar=3
   if (f(16).gt.0.0) ebar=f(16)  ! ERROR BAR SIZE
!                   1ST LINE DEFINES CENTER
!                   2ND IS LOWER REL. ERROR
!                   3RD IS UPPER REL. ERROR
   jvline=f(17)                  ! VERTICAL LINE TO ZVAL,0=NO,1=YES
   zval=f(18)                    ! ZERO REFERENCE VALUE
   jxlgax=f(19)                  ! X AXIS, LINEAR=0/LOG=1/NONE=-1
   jylgax=f(20)                  ! Y AXIS, LINEAR=0/LOG=1/NONE=-1
   if (f(21).gt.0.0) xlen=f(21)  ! X AXIS LENGTH
   if (f(22).gt.0.0) ylen=f(22)  ! Y AXIS LENGTH
   if (f(23).gt.0.0) xtick=f(23) ! X AXIS TICK PATTERN
   if (f(24).gt.0.0) ytick=f(24) ! Y AXIS TICK PATTERN
   jxtitle=abs(f(25))            ! X AXIS TITLE SIDE,0=CW,1=CCW
   if (jxtitle.eq.0) then
      jxtitle=-1
   else
      jxtitle=1
   endif
   jytitle=abs(f(26))            ! Y AXIS TITLE SIDE,0=CCW,1=CW
   if (jytitle.eq.0) then
      jytitle=1
   else
      jytitle=-1
   endif
   jxexpsc=abs(f(27))            ! X AXIS ENABLE AUTO EXPONENT,0=YES,1=NO
   jyexpsc=abs(f(28))            ! Y AXIS ENABLE AUTO EXPONENT,0=YES,1=NO
   jxticks=abs(f(29))            ! X AXIS TICKS TITLE SIDE,0=YES,1=NO
   jyticks=abs(f(30))            ! Y AXIS TICKS TITLE SIDE,0=YES,1=NO
   jxnums=abs(f(31))             ! X AXIS NUMS PARA AXIS, 0=YES,1=NO
   jynums=abs(f(32))             ! Y AXIS NUMS PARA AXIS, 0=YES,1=NO
   jxnum=abs(f(33))              ! X AXIS NUMS SHOWN, 0=YES,1=NO
   jynum=abs(f(34))              ! Y AXIS NUMS SHOWN, 0=YES,1=NO
   jxlog=abs(f(35))              ! TAKE LOG OF X VALUES, 0=NO,1=YES
   jylog=abs(f(36))              ! TAKE LOG OF Y VALUES, 0=NO,1=YES
   jxrax=f(37)                   ! ADD AXIS OPPOSITE, 0=YES,-1=W/L,1=NO LABELS
   jyrax=f(38)                   ! ADD AXIS OPPOSITE, 0=YES,-1=W/L,1=NO LABELS
   if (f(39).gt.0.0) xcs=f(39)   ! X AXIS TITLE SIZE
   if (f(40).gt.0.0) ycs=f(40)   ! Y AXIS TITLE SIZE
   if (f(41).gt.0.0) tcs=f(41)   ! OVERALL TITLE SIZE
   jgrid=f(42)                   ! GRID OPTION, 0=NONE,1=SOLID,2=DOTTED
   jlegnd=f(43)                  ! ADD LEGEND, 0=NONE,1=AUTO,-1=USER
   xleg=xlen+0.3                 ! DEFAULT LEGEND LOCATION
   yleg=0.5
   if (jlegnd.lt.0) xleg=f(44)   ! X LEGEND LOCATION
   if (jlegnd.lt.0) yleg=f(45)   ! Y LEGEND LOCATION
   jlegsy=f(46)                  ! USE SYMBOL ON LEGEND,0=NO,1=YES
   jlegli=f(47)                  ! USE LINETYPE ON LEGEND,0=NO,1=YES
   if (f(48).gt.0.0) csleg=f(48) ! LEGEND CHARACTER SIZE
   if (f(49).gt.0.0) glen=f(49)  ! LEGEND LINE LENGTH
   jtcntr=f(50)                  ! CENTER TITLE,0=YES,-1=LEFT,1=RIGHT
   jszref=f(51)                  ! SHOW ZERO REFERENCE,0=NO,1=YES
   zref=0.0
!
   jline=f(52)                   ! USE LINE TYPES,0=NO,1=YES
   jcol=f(53)                    ! USE COLORS,0=NO,1=YES
!
!     F(54)=FIRST COLOR
!     F(55)=FIRST LINETYPE
!     F(56)=SECOND COLOR
!     F(57)=SECOND LINETYPE
!     ...
!
   icoff=54
   iloff=55
!
!     COMPUTE SCALING
!
   if (jxauto.ge.0) then  ! AUTO SCALE X
      x1=abs(xm)
      x2=x1
      do i=1,np
         k=1
         if (jxuse.ne.0) k=nl
         do j=1,k,nbar
            xm=min(xm,x(i,j))
            x1=min(x1,abs(x(i,j)))
            xx=max(xx,x(i,j))
            x2=max(xx,abs(x(i,j)))
            if (jebar.ne.0.and.jxuse.ne.0) then
               xm=min(xm,x(i,j)+x(i+1,j))
               x1=min(x1,abs(x(i,j)+x(i+1,j)))
               xx=max(xx,x(i,j)+x(i+1,j))
               x2=max(x2,abs(x(i,j)+x(i+1,j)))
               xm=min(xm,x(i,j)+x(i+2,j))
               x1=min(x1,abs(x(i,j)+x(i+1,j)))
               xx=max(xx,x(i,j)+x(i+2,j))
               x2=max(x2,abs(x(i,j)+x(i+1,j)))
            endif
         enddo
      enddo
      if (jxlog.ne.0) then
         xm=alog10(x1+1.e-34)
         xx=alog10(x2+1.e-34)
      endif
      if (jxauto.gt.0.and.jxlgax.eq.0) then ! SMOOTH SCALING
         smo(1)=xm
         smo(2)=xx
         call range_(smo,xlen,2,1,1,xm,dx)
         xx=xm+xlen*dx
      endif
      if (jxlgax.ne.0) then   ! LOG AXIS SCALING
         if (xm.le.0.and.xm.ne.int(xm)) xm=xm-1.0
         xm=int(xm)
         if (xx.gt.0.and.xx.ne.int(xx)) xx=xx+1.0
         xx=int(xx)
      endif
      if (xx.eq.xm) xx=xm+1.0
!     RETURN SCALE FACTORS USED
      f(4)=xm
      f(5)=xx
   endif
   if (jyauto.ge.0) then  ! AUTO SCALE Y
      y1=abs(ym)
      y2=y1
      do i=1,np
         do j=1,nl,nbar
            ym=min(ym,y(i,j))
            y1=min(y1,abs(y(i,j)))
            yx=max(yx,y(i,j))
            y2=max(yx,abs(y(i,j)))
            if (jebar.ne.0) then
               ym=min(ym,y(i,j)+y(i+1,j))
               y1=min(y1,abs(y(i,j)+y(i+1,j)))
               yx=max(yx,y(i,j)+y(i+1,j))
               y2=max(y2,abs(y(i,j)+y(i+1,j)))
               ym=min(ym,y(i,j)+y(i+2,j))
               y1=min(y1,abs(y(i,j)+y(i+1,j)))
               yx=max(yx,y(i,j)+y(i+2,j))
               y2=max(y2,abs(y(i,j)+y(i+1,j)))
            endif
         enddo
      enddo
      if (jylog.ne.0) then
         ym=alog10(y1+1.e-34)
         yx=alog10(y2+1.e-34)
      endif
      if (jyauto.gt.0.and.jylgax.eq.0) then ! SMOOTH SCALING
         smo(1)=ym
         smo(2)=yx
         call range_(smo,ylen,2,1,1,ym,dy)
         yx=ym+ylen*dy
      endif
      if (jylgax.ne.0) then   ! LOG AXIS SCALING
         if (ym.le.0.and.ym.ne.int(ym)) ym=ym-1.0
         ym=int(ym)
         if (yx.gt.0.and.yx.ne.int(yx)) yx=yx+1.0
         yx=int(yx)
      endif
      if (yx.eq.ym) yx=ym+1.0
!     RETURN SCALE FACTORS USED
      f(7)=ym
      f(8)=yx
   endif
   dx=(xx-xm)/xlen
   dy=(yx-ym)/ylen
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   if(debug)write(*,*)'PRODUCE PLOT'
!
!     PRODUCE PLOT X AXIS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   if (jcol.ne.0) then  ! INITIALIZE COLOR AXIS ARRAY
      do i=1,4
         ic(i)=abs(f(2*i+icoff-2))
         if (ic(i).eq.0) ic(i)=1
      enddo
   endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   if (jline.ne.0) call newpen_(int(f(iloff)))
!
   if (jxexpsc.ne.0) xcs=-xcs
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   if (jxlgax.eq.0) then  ! LINEAR X AXIS
      x1=xlen
      if (jxticks.ne.0) x1=-xlen
      na=1000
      if (jxnum.ne.0) na=na+100
      if (jcol.ne.0) na=na+100000
      if (jxnums.eq.0) na=na-1000
      if (nc(1).gt.0) then ! USE INPUT X AXIS TITLE
         na=(na+nc(1))*jxtitle
         if(debug)write(*,*)'1) axisc_'
         call axisc_(0.,0.,c(1),na,x1, 0.0,xm,xx,xtick,xcs,fx,ic)
      else        ! NO X AXIS TITLE
         na=(na+1)*jxtitle
         if(debug)write(*,*)'2) axisc_'
         call axisc_(0.,0.,' ',na,x1, 0.0,xm,xx,xtick,xcs,fx,ic)
      endif
      if (jxrax.ne.0) then
         na=1000
         if (jxrax.gt.0) na=na+100
         if (jcol.ne.0) na=na+100000
         if (jxnums.eq.0) na=na-1000
         if (nc(1).gt.0) then! USE INPUT X AXIS TITLE
            na=(-(na+nc(1)))*jxtitle
            if(debug)write(*,*)'3) axisc_'
            call axisc_(0.,ylen,c(1),na,x1, 0.0,xm,xx,xtick,xcs,fx,ic)
         else        ! NO X AXIS TITLE
            na=(na+1)*jxtitle
            if(debug)write(*,*)'4) axisc_'
            call axisc_(0.,ylen,' ',na,x1, 0.0,xm,xx,xtick,xcs,fx,ic)
         endif
      endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   else if (jxlgax.eq.1) then ! LOG X AXIS
      x1=xlen
      if (jxticks.ne.0) x1=-xlen
      na=1000
      if (jxnum.ne.0) na=na+100
      if (jcol.ne.0) na=na+10000
      if (jxnums.eq.0) na=na-1000
      if (nc(1).gt.0) then ! USE INPUT X AXIS TITLE
         na=(na+nc(1))*jxtitle
         if(debug)write(*,*)'5) axislg_'
         call axislg_(0.,0.,c(1),na,x1, 0.0,xm,dx,ic)
      else        ! NO X AXIS TITLE
         na=(na+1)*jxtitle
         if(debug)write(*,*)'6) axislg_'
         call axislg_(0.,0.,' ',na,x1, 0.0,xm,dx,ic)
      endif
      if (jxrax.ne.0) then
         na=1000
         if (jxrax.gt.0) na=na+100
         if (jcol.ne.0) na=na+10000
         if (jxnums.eq.0) na=na-1000
         if (nc(1).gt.0) then! USE INPUT X AXIS TITLE
            na=(-(na+nc(1)))*jxtitle
            if(debug)write(*,*)'7) axislg_'
            call axislg_(0.,ylen,c(1),na,x1, 0.0,xm,dx,ic)
         else    ! NO X AXIS TITLE
            na=(-(na+1))*jxtitle
            if(debug)write(*,*)'8) axislg_'
            call axislg_(0.,ylen,' ',na,x1, 0.0,xm,dx,ic)
         endif
      endif
   endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PRODUCE PLOT Y AXIS
!
   if (jcol.ne.0) then  ! INITIALIZE COLOR AXIS ARRAY
      do i=1,4
         ic(i)=abs(f(2*i+icoff+6))
         if (ic(i).eq.0) ic(i)=1
      enddo
   endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PRODUCE PLOT Y AXIS
   if (jline.ne.0) call newpen_(int(f(iloff+2)))
!
   if (jyexpsc.ne.0) ycs=-ycs
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PRODUCE PLOT Y AXIS
   if (jylgax.eq.0) then  ! LINEAR Y AXIS
      x1=ylen
      if (jyticks.ne.0) x1=-ylen
      na=1000
      if (jynum.ne.0) na=na+100
      if (jcol.ne.0) na=na+100000
      if (jynums.eq.0) na=na-1000
      if (nc(2).gt.0) then ! USE INPUT Y AXIS TITLE
         na=(na+nc(2))*jytitle
         if(debug)write(*,*)'9) axisc_'
         call axisc_(0.,0.,c(2),na,x1, 90.0,ym,yx,ytick,ycs,fy,ic)
      else        ! NO Y AXIS TITLE
         na=(na+1)*jytitle
         if(debug)write(*,*)'10) axisc_'
         call axisc_(0.,0.,' ',na,x1, 90.0,ym,yx,ytick,ycs,fy,ic)
      endif
      if (jyrax.ne.0) then
         na=1000
         if (jyrax.gt.0) na=na+100
         if (jcol.ne.0) na=na+100000
         if (jynums.eq.0) na=na-1000
         if (nc(2).gt.0) then! USE INPUT Y AXIS TITLE
            na=(-(na+nc(2)))*jytitle
            if(debug)write(*,*)'11) axisc_'
            call axisc_(xlen,0.,c(2),na,x1, 90.0,ym,yx,ytick,ycs,fy,ic)
         else        ! NO Y AXIS TITLE
            na=(na+1)*jytitle
            if(debug)write(*,*)'12) axisc_'
            call axisc_(xlen,0.,' ',na,x1, 90.0,ym,yx,ytick,ycs,fy,ic)
         endif
      endif
   else if (jylgax.eq.1) then ! LOG Y AXIS
      x1=ylen
      if (jyticks.ne.0) x1=-ylen
      na=1000
      if (jynum.ne.0) na=na+100
      if (jcol.ne.0) na=na+10000
      if (jynums.eq.0) na=na-1000
      if (nc(2).gt.0) then ! USE INPUT Y AXIS TITLE
         na=(na+nc(2))*jytitle
         if(debug)write(*,*)'13) axislg_'
         call axislg_(0.,0.,c(2),na,x1, 90.0,xy,dy,ic)
      else        ! NO X AXIS TITLE
         na=(na+1)*jytitle
         if(debug)write(*,*)'14) axislg_'
         call axislg_(0.,0.,' ',na,x1, 90.0,ym,dy,ic)
      endif
      if (jyrax.ne.0) then
         na=1000
         if (jyrax.gt.0) na=na+100
         if (jcol.ne.0) na=na+10000
         if (jynums.eq.0) na=na-1000
         if (nc(2).gt.0) then! USE INPUT Y AXIS TITLE
            na=(-(na+nc(2)))*jytitle
            if(debug)write(*,*)'15) axislg_'
            call axislg_(xlen,0.,c(2),na,x1, 90.0,ym,dy,ic)
         else    ! NO Y AXIS TITLE
            na=(-(na+1))*jytitle
            if(debug)write(*,*)'16) axislg_'
            call axislg_(xlen,0.,' ',na,x1, 90.0,ym,dy,ic)
         endif
      endif
   endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PLOT GRID
!
   if (jgrid.ne.0) then
      if (jcol.ne.0) then
         icc=int(abs(f(icoff+22)))
         if (icc.lt.1) icc=1
         call color_(icc)
      endif
      ng=jgrid-1
      i=int(xtick)-1
      if (i.lt.1) i=1
      x1=xlen/float(i)
      if (jxlgax.gt.0) then
         i=1-int(xlen*dx-xm+0.001)
         x1=(-xlen)/float(i)
      endif
      j=int(ytick)-1
      if (j.lt.1) j=1
      y1=ylen/float(j)
      if (jylgax.gt.0) then
         j=1-int(ylen*dy-ym+0.001)
         y1=(-ylen)/float(j)
      endif
      if(debug)write(*,*)'17) gridll_'
      call gridll_(0.,0.,x1,y1,i,j,ng)
   endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     ADD TITLE
!
   if (nc(3).gt.0) then
      if (jline.ne.0) call newpen_(int(f(iloff+4)))
      if (jcol.ne.0) then
         icc=int(abs(f(icoff+16)))
         if (icc.lt.1) icc=1
         call color_(icc)
      endif
      icc=0
      x1=0.5*xlen
      y1=ylen+0.1+tcs*0.2
      if (jtcntr.lt.0) then
         icc=-1
         x1=0.0
         y1=y1-tcs*0.2
      endif
      if (jtcntr.gt.0) then
         icc=1
         x1=xlen
         y1=y1-tcs*0.2
      endif
      if (jxrax.lt.0) y1=y1+2.5*xcs
      if(debug)write(*,*)'18) TITLE'
      call symbol_(x1,y1,tcs,c(3), 0.0,nc(3),icc)
   endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PLOT DATA IN DESIRED FORM
!
   dx=1.0
   dy=1.0
   if (xx-xm.ne.0.0) dx=xlen/(xx-xm)
   if (yx-ym.ne.0.0) dy=ylen/(yx-ym)
   zref=(zref-ym)*dy
   if (jszref.ne.0) then ! SHOW ZERO REFERENCE LINE
      if (jline.ne.0) call newpen_(int(f(iloff+8)))
      if (jcol.ne.0) then
         icc=int(abs(f(icoff+20)))
         if (icc.lt.1) icc=1
         call color_(icc)
      endif
      if(debug)write(*,*)'19) ZERO REFERENCE LINE'
      call move_(0.0,zref)
      call draw_(xlen,zref)
   endif
!
   do i=1,nl,nbar
      if (jline.ne.0) call newpen_(int(f(2*(i-1)+iloff+12)))
      if (jcol.ne.0) then
         icc=int(abs(f(icoff+24+2*(i-1))))
         if (icc.lt.1) icc=1
         call color_(icc)
      endif
      ksym=mod(i+jsymst-1,17)
!
!     IF WE NEED CONNECTING LINES MAKE A CONNECT PASS
!
      ipen=3
      if (jconn.eq.0) then
         do j=1,np
            if (jxuse.ne.0) then
               x1=x(j,i)
            else
               x1=x(j,1)
            endif
            if (jxlog.ne.0) x1=alog10(abs(x1)+1.e-34)
            x1=(x1-xm)*dx
            y1=y(j,i)
            if (jylog.ne.0) y1=alog10(abs(y1)+1.e-34)
            y1=(y1-ym)*dy
            call plot_(x1,y1,ipen)
            ipen=2
         enddo
      endif
!
!     ANOTHER PASS FOR SYMBOLS, ETC.
!
      if (jconn.ne.0.and.jebar.eq.0.and.jvline.eq.0 .and.jsym.eq.0) cycle
      isym=0
      do j=1,np
         if (jxuse.ne.0) then
            x1=x(j,i)
         else
            x1=x(j,1)
         endif
         if (jxlog.ne.0) x1=alog10(abs(x1)+1.e-34)
         x1=(x1-xm)*dx
         y1=y(j,i)
         if (jylog.ne.0) y1=alog10(abs(y1)+1.e-34)
         y1=(y1-ym)*dy
         if (jsym.ne.0) then  ! SYMBOLS
            if (mod(isym,jsym).eq.0) call symbol_(x1,y1,symsiz,char(ksym),0.,-1,-1)
         endif
         if (jvline.ne.0) then  ! VERTICAL LINE
            call move_(x1,y1)
            call draw_(x1,zref)
         endif
         if (jebar.ne.0) then  ! ERROR BAR
            if (jline.ne.0) call newpen_(int(f(iloff+10)))
            if (jxuse.ne.0) then
               x2=x(j,i+1)+x(j,i)
               x3=x(j,i+2)+x(j,i)
               if (jxlog.ne.0) then
                  x2=alog10(abs(x2)+1.e-34)
                  x3=alog10(abs(x3)+1.e-34)
               endif
               x2=(x2-xm)*dx
               x3=(x3-xm)*dx
            else
               x2=x1
               x3=x1
            endif
            y2=y(j,i+1)+y(j,i)
            y3=y(j,i+2)+y(j,i)
            if (jylog.ne.0) then
               y2=alog10(abs(y2)+1.e-34)
               y3=alog10(abs(y3)+1.e-34)
            endif
            y2=(y2-ym)*dy
            y3=(y3-ym)*dy
            if (kebar.eq.2.or.kebar.eq.5) then ! ERROR BAR TOP
               call move_(x2,y2-ebar)
               call draw_(x2,y2+ebar)
            else if (kebar.eq.3.or.kebar.eq.4) then
               call move_(x2-ebar,y2)
               call draw_(x2+ebar,y2)
            endif
            if (kebar.lt.4) then ! CONNECT LINE
               call move_(x2,y2)
               call draw_(x3,y3)
            else if (kebar.eq.4) then ! DOUBLE CONN V LINE
               call move_(x2-ebar*0.5,y2)
               call draw_(x3-ebar*0.5,y3)
               call move_(x3+ebar*0.5,y3)
               call draw_(x2+ebar*0.5,y2)
            else if (kebar.eq.5) then ! DOUBLE CONN HLINE
               call move_(x2,y2-ebar*0.5)
               call draw_(x3,y3-ebar*0.5)
               call move_(x3,y3+ebar*0.5)
               call draw_(x2,y2+ebar*0.5)
            else if (kebar.eq.6) then ! VERTICAL RECT
               call rect_(x1-ebar,y2,x1+ebar,y3)
            else if (kebar.eq.7) then ! RECT
               call rect_(x2,y2,x3,y3)
            endif
            if (jebar.lt.0) then ! X MARK
               call move_(x1-ebar,y1-ebar)
               call draw_(x1+ebar,y1+ebar)
               call move_(x1-ebar,y1+ebar)
               call draw_(x1+ebar,y1-ebar)
            endif
            if (kebar.eq.2.or.kebar.eq.5) then ! BAR BOTTOM
               call move_(x3,y3-ebar)
               call draw_(x3,y3+ebar)
            else if (kebar.eq.3.or.kebar.eq.4) then
               call move_(x3-ebar,y3)
               call draw_(x3+ebar,y3)
            endif
         endif
         if (jsym.eq.0.and.jconn.eq.0.and.  jebar.eq.0.and.jvline.eq.0) then
            call move_(x1,y1) ! DOTS ONLY
            call draw_(x1,y1)
         endif
         isym=isym+1
      enddo
    enddo
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     ADD LEGEND
!
   if (jlegnd.ne.0) then
      if(debug)write(*,*)'20) ADD LEGEND'
      do i=1,nl
         x1=xleg+csleg*1.5
         y1=yleg+(i-1)*csleg*1.5+csleg*0.5
         if (jcol.ne.0) then
            icc=int(abs(f(icoff+24+2*(i-1))))
            if (icc.lt.1) icc=1
            call color_(icc)
         endif
         if (jlegli.ne.0) then
            if (jline.ne.0) call newpen_(int(f(2*(i-1)+iloff+12)))
            call move_(x1,y1)
            call draw_(x1+glen,y1)
            x1=x1+glen*0.5
         endif
         if (jlegsy.ne.0) then
            icc=i-1+jsymst
            call symbol_(x1,y1,symsiz, char(icc),0.,-1,-1)
         endif
         if (jlegli.ne.0) x1=x1+glen*0.5
         x1=x1+csleg*0.7
         y1=y1-csleg*0.5
         if (nc(i+3).gt.0) then
            if (jline.ne.0) call newpen_(int(f(iloff+6)))
            if (jcol.ne.0) then
               icc=int(abs(f(icoff+18)))
               if (icc.lt.1) icc=1
               call color_(icc)
            endif
            call symbol_(x1,y1,csleg, c(i+3),0.,nc(i+3),-1)
         endif
      enddo
   endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     FINISH UP
   call move_(0.,0.)   !PEN UP
end subroutine graph
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine draw_(xa,ya)
real :: xa
real :: ya
   call plot_(xa,ya,2)
end subroutine draw_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine move_(xa,ya)
real :: xa
real :: ya
   call plot_(xa,ya,3)
end subroutine move_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine translate_(xa,ya)
real :: xa
real :: ya
   call plot_(xa,ya,-3)
end subroutine translate_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine color_(ic)
use M_draw
integer :: ic
   if(ic.ge.0)then
      call color(ic)  ! change color
   endif
end subroutine color_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!      graph_init(3f) - [M_graph] initialize the longlib graphics plot routine graph(3f)
!!##SYNOPSIS
!!
!!   subroutine graph_init(xmax0,ymax0,vpx,vpy,zom)
!!
!!    real,intent(in) :: xmax0
!!    real,intent(in) :: ymax0
!!    real,intent(in) :: vpx
!!    real,intent(in) :: vpy
!!    real,intent(in) :: zom
!!
!!##DESCRIPTION
!!
!!    Routine to initialize the longlib graphics plot package
!!
!!    XMAX0,YMAX0  size in inches M_DRAW will simulate as the
!!                 display size for this library
!!
!!    VPX,VPY      coordinates of bottom left origin
!!    ZOM          zoom factor
subroutine graph_init(xmax0,ymax0,vpx,vpy,zom)
!     FORTRAN-77 VERSION:   DGL JULY, 1987
use M_draw

! ident_2="@(#) M_graph graph_init(3f) initialize the longlib graphics plot package"

real,intent(in)   :: xmax0
real,intent(in)   :: ymax0
real,intent(in)   :: vpx
real,intent(in)   :: vpy
real,intent(in)   :: zom
real              :: xmax
real              :: ymax
real              :: z
   Z=ZOM
   xmax=xmax0
   ymax=ymax0
   call page(0.0,xmax,0.0,ymax)
   XMINQ=0.0
   YMINQ=0.0
   XMAXQ=xmax
   YMAXQ=ymax

   call color(0)
   call clear()
   call color(7)
   call vflush()

   TRANSLATEXQ=VPX         ! ORIGIN X
   TRANSLATEYQ=VPY         ! ORIGIN Y

   SCALEQ=ABS(Z)      ! SCALE FACTOR
   IF (SCALEQ.LE.0.0) SCALEQ=1.0

   ANGLEQ=0.0       ! PLOTTING ANGLE ROTATION

   XLASTSCALEQ=0.0         ! LAST POINT PLOTTED
   YLASTSCALEQ=0.0

   ! set the VIEWPORTQ() ARRAY
   call viewport_(-999.0,999.0,-999.0,999.0)
   call color_(7) ! initialize line color
END SUBROUTINE graph_init
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE plot_(XPLOT0,YPLOT0,ISELECT0)
!
!     PLOT is the central routine for controlling the plotting of lines.
!     Any call to PLOT when graphics mode is not initialized is a dummy call.
!     By default the viewport will be the "usable size" of the display device
!     as set by "graph_init".
!     By default the lower left corner is (0,0)
!     Only non-blank page strips are output to the device.
!
!     The transformation of an input point (x,y) in plot units
!     to an output point (nx,ny) in plot surface units (ignoring clipping) is:
!         nx = (z * (x * cos( a ) - y * sin( a )) + ox) / rx
!         nx = (z * (x * sin( a ) + y * cos( a )) + oy) / ry
!
!     where
!
!         a is the relative plotting angle (expressed in radians)
!         z is the zoom scale factor
!         ox,oy is the scaled, rotated relative origin
!         rx,ry is the metafile resolution for each axis (inches/res unit)
!     (see DL_QUERY)
!
!     The relative plotting angle a is updated according to:
!         anew = aold + ain
!
!     and the relative origin (ox,oy) is updated according to:
!         oxnew = z * (xin * cos( a ) - yin * sin( a )) + oxold
!         oynew = z * (xin * sin( a ) + yin * cos( a )) + oyold
!
!     CALL PLOT_(XPLOT0,YPLOT0,ISELECT0)
!
!     XPLOT0,YPLOT0   (R): coordinate values
!     ISELECT0        (I): plot function selector
!
!     XPLOT0 YPLOT0 ISELECT0 COMMAND INTERPRETATION
!
!
!     COL    ANG      0: line color control
!                        x is the new line color
!  WHAT?                 if x &gt;= 0 then plotting angle becomes y
!  WHAT?                 should give plotting angle its own selector
!                        what is difference to +6?
!                     1: newpen dash scale code
!      X      Y       2: draw to (x,y) with 'pen down'
!      X      Y      -2: same as iselect0=2; but (x,y) becomes new origin
!      X      Y       3: move to (x,y) with 'pen up'
!      X      Y      -3: same as iselect0=3. (x,y) becomes new origin
!     XMAX  YMAX      4: upper right corner of viewport set to (x,y)
!     XMIN  YMIN     -4: lower left corner of viewport set to (x,y)
!      -      -       5: pick pen up at last point and flush
!                        PEN UNCHANGED
!     ANG     -      +6: CHANGE RELATIVE ROTATION ANGLE TO XPLOT0.
!                        PEN UNCHANGED
!     THICK   -       7: line thickness
!                        units in rasters or percent/10 of display surface?
!     STYLE   -       8: newpen line style (dash code)
!     X       Y      10: CLEAR SCREEN NEW PAGE
!                        are x and y used to set a new origin?
!                        what color, current position, ....?????
!     -      -       11: FLUSH AND PAUSE FOR "n" IN GRAPHICS AREA
!
!                  ????: ANY OTHER VALUE OF ISELECT0 IS TREATED AS A NOP
!     WHAT ABOUT?
!     THREE CALLS SETS COLOR TABLE ENTRY
!     N      -     2010    COLOR TABLE ENTRY SIZE
!     R      G     2011    COLOR TABLE DATA R AND G
!     B      C     2012    COLOR TABLE DATA B AND CODES
!
!     NO LONGER SETTING RESOLUTION AND CONVERTING UNITS TO INTEGERS
!
!     REMOVED -9 and 9, which are like 3 but "erase" (draw in background
!     color on monochrome devices, but otherwise rare feature any more)
! *************************************************************************
! after page eject, should next commands set line type, width, color?
! or should start where last clear left off for easier animation for display
! devices? CLEAR SHOULD BE SEPARATE FROM EJECT? EJECT RESETS, CLEAR JUST CLEARS?
! *************************************************************************
use M_draw
integer            :: i110
integer            :: i130
integer            :: i30
integer            :: icoltemp
integer            :: iselect
integer            :: iselect0
integer            :: ivalue
integer            :: ivta
integer            :: ivtb
integer,parameter  :: nselect=17
INTEGER            :: IDECODE(NSELECT)
real               :: dscale
real               :: range
real               :: xcon
real               :: xplot0
real               :: xtemp
real               :: xtemp1
real               :: ycon
real               :: yplot0
real               :: ytemp
real               :: ytemp1
!
!      SAVE /DL_COM/
      ! TRANSLATEXQ  SCALED ROTATED ORIGIN X VALUE
      ! TRANSLATEYQ  SCALED ROTATED ORIGIN Y VALUE
      ! SCALEQ       SCALE FACTOR (DL_SCALE)
      ! XLASTSCALEQ  LAST SCALED SHIFTED X VALUE
      ! YLASTSCALEQ  LAST SCALED SHIFTED Y VALUE
      ! ILINEQ       LINE TYPE
      ! IWIDTHQ      CURRENT LINE WIDTH
      ! ICOLORQ      CURRENT LINE COLOR
      ! IDASHSCALEQ  DEFAULT LINE SCALING FACTOR
      ! ANGLEQ       PLOTTING ANGLE
      ! VIEWPORTQ(4) VIEWPORT PARAMETERS
!
      CHARACTER*20 STRING

      DATA IDECODE/1,2,3,0,-2,-3,5,10,11,999,4,-4,6,7,8,12,13/
!
!#######################################################################
      ISELECT=ISELECT0             ! make mutable copy of select option
!#######################################################################
!     DECODE COMMAND
!
      DO I30=1,NSELECT
         IF (ISELECT.EQ.IDECODE(I30))THEN
!                  1,2,3, 0,-2,-3,5,10,11,999,4,-4,6,7,8,12,13
      if(debug)write(*,*)'*_plot_* ',xplot0,yplot0,iselect,i30,idecode(i30)
             GOTO (1,2,3,50,22,33,5,10,11,999,4, 4,6,7,8,12,13),I30
         ENDIF
      enddo
      WRITE(*,*)'# *PLOT* UNEXPECTED SELECTION ',ISELECT
      RETURN
!     NO LONGER SETTING RESOLUTION AND CONVERTING UNITS TO INTEGERS
!     THREE CALLS SETS COLOR TABLE ENTRY
!     N      -     2010    COLOR TABLE ENTRY SIZE
!     R      G     2011    COLOR TABLE DATA R AND G
!     B      C     2012    COLOR TABLE DATA B AND CODES
!
!     REMOVED -9 and 9, which are like 3 but "erase" (draw in background
!     color on monochrome devices, but otherwise rare feature any more)
!#######################################################################
50    CONTINUE  ! ISELECT=0 CHANGE COLOR TO XPLOT0
      IF (XPLOT0.GE.0) THEN
         ICOLORQ=INT(XPLOT0+0.5)  ! store color into common and make integer
         ICOLTEMP=ICOLORQ         ! hate passing things in common if can avoid
         CALL COLOR(ICOLTEMP)     ! change color
         if(debug)write(*,*)'color ',icoltemp
      ENDIF
      RETURN
! **********************************************************************
1     continue   ! dash code scale
          IDASHSCALEQ=XPLOT0
          ! PROBABLY SHOULD BE PERCENT OF MIN OF SIDES OF PAGE /10
          RANGE=MIN(XMAXQ-XMINQ,YMAXQ-YMINQ)
          DSCALE=RANGE/10000.0*IDASHSCALEQ
          DSCALE=MAX(DSCALE,RANGE/10000.0) ! would get very slow, loop if 0
          if(debug)WRITE(*,*)'dashcode ',DSCALE
          call dashcode(DSCALE)
      return
! **********************************************************************
7     continue  ! ISELECT=0 or 7 line thickness in rasters
        IF(INT(XPLOT0).GE.0)THEN
           IWIDTHQ=XPLOT0
           CALL LINEWIDTH(IWIDTHQ)
          if(debug)write(*,*)'linewidth ',iwidthq
        endif
      return
! **********************************************************************
8     CONTINUE          ! newpen line style
        ILINEQ=XPLOT0   ! store line style selection into common
        ! this could be an array index or goto
        select case(ilineq)
         case(1) ; string=' '         ! 1 SOLID
         case(2) ; string='00001'     ! 2 DOTTED
         case(3) ; string='1110111'   ! 3 LONG-DOT-LONG
         case(4) ; string='1100'      ! 4 MEDDASH-MEDDASH
         case(5) ; string='111000'    ! 5 LONGDASH-LONGDASH
         case(6) ; string='11101000'  ! 6 LONG-DOT-DOT-LONG
         case(7) ; string='1110111'   ! 7 LONG-SHORT-LONG
         case(8) ; string='10'        ! 8 SHORT-SHORT
         case(9) ; string='111010111' ! 9 LONG-DOT-DOT-DOT-LONG
         case(0) ; RETURN             ! 0 NO CHANGE
         case default
           WRITE(*,*)'# UNEXPECTED LINESTYLE VALUE=',ilineq
           RETURN
         end select
        if(debug)write(*,*)'linestyle ',string
        call linestyle(string)
      RETURN
!#######################################################################
4     CONTINUE  ! ISELECT= 4  SET UPPER RIGHT CORNER OF VIEW PORT
                ! ISELECT=-4  SET lower left CORNER OF VIEW PORT
!#######################################################################
      CALL trs_(XPLOT0,YPLOT0,XCON,YCON) ! convert call numbers to current plot coordinate system
      ! note that new viewport is in terms of current coordinate system
      ! MAKE AN ALTERNATE BASED ON ORIGINAL COORDINATES?
      IF (ISELECT.GT.0) THEN
         VIEWPORTQ(3)=XCON
         VIEWPORTQ(4)=YCON
      ELSE
         VIEWPORTQ(1)=XCON
         VIEWPORTQ(2)=YCON
      ENDIF
      RETURN
!#######################################################################
!     OBSOLESCENT
5     CONTINUE       ! ISELECT=5   PEN UP AND FLUSH AT CURRENT LOCATION
999   CONTINUE       ! ISELECT=999   END GRAPHICS MODE
      RETURN
!#######################################################################
6     CONTINUE  ! ISELECT=6  INCREMENT PLOTTING ANGLE BY XPLOT0
      ANGLEQ=ANGLEQ+XPLOT0
         if(debug)write(*,*)'#rotate ',xplot0
      RETURN
!#######################################################################
22    continue   ! -2
33    continue   ! -3
      CALL trs_(XPLOT0,YPLOT0,XCON,YCON) ! convert call numbers to current plot coordinate system
      TRANSLATEXQ=XCON    ! make scaled rotated input coordinates the new origin
      TRANSLATEYQ=YCON
      ISELECT=IABS(ISELECT0)
      goto 222
!#######################################################################
!     DRAW LINE SEGMENT  ISELECT=2,3 (and -2,-3)
2     CONTINUE
3     CONTINUE
      CALL trs_(XPLOT0,YPLOT0,XCON,YCON) ! convert call numbers to current plot coordinate system
222   CONTINUE
      ! check if point (xcon,ycon) is in viewport rectangle
      IVTA=inbox_(XCON,YCON, VIEWPORTQ(1),VIEWPORTQ(2),VIEWPORTQ(3),VIEWPORTQ(4))
      ! check if point (xlastscaleq,ylastscaleq) is in viewport rectangle
      IVTB=inbox_(XLASTSCALEQ,YLASTSCALEQ, VIEWPORTQ(1),VIEWPORTQ(2),VIEWPORTQ(3),VIEWPORTQ(4))
      IF (IOR(IVTA,IVTB).EQ.0) GOTO 333 ! LINE ENTIRELY VISIBLE
      IF (IAND(IVTA,IVTB).NE.0) THEN  ! LINE ENTIRELY INVISIBLE
         XLASTSCALEQ=XCON
         YLASTSCALEQ=YCON
         RETURN
      ENDIF
      IF (IVTB.NE.0) THEN   ! OLD POINT IS OUTSIDE WINDOW
         XTEMP1=XLASTSCALEQ
         YTEMP1=YLASTSCALEQ
         CALL clipit_(IVTB,XTEMP1,YTEMP1,XCON,YCON, VIEWPORTQ(1),VIEWPORTQ(2),VIEWPORTQ(3),VIEWPORTQ(4))
         IF (IVTB.NE.0) THEN  ! VECTOR DOES NOT INTERSECT
            XLASTSCALEQ=XCON
            YLASTSCALEQ=YCON
            RETURN
         ENDIF

      ENDIF
      XTEMP=XCON
      YTEMP=YCON
      ! clips a partially visible line segment
      IF (IVTA.NE.0)CALL clipit_(IVTA, XTEMP,YTEMP,XLASTSCALEQ,YLASTSCALEQ,VIEWPORTQ(1),VIEWPORTQ(2),VIEWPORTQ(3),VIEWPORTQ(4))
      XLASTSCALEQ=XCON
      YLASTSCALEQ=YCON
      IF (IVTA.NE.0) THEN
         RETURN
      ENDIF
      XCON=XTEMP
      YCON=YTEMP
! ----------------------------------------------------------------------
333   CONTINUE  ! draw clipped vector
! ----------------------------------------------------------------------
      IF(ISELECT.EQ.2)THEN
        CALL DRAW2(XCON,YCON)
        if(debug)write(*,*)'draw2 ',xcon,ycon
      ELSEIF(ISELECT.EQ.3)THEN
        CALL MOVE2(XCON,YCON)
        if(debug)write(*,*)'move2 ',xcon,ycon
      ELSE
        write(*,*)'*plot_* 2,3 internal error',xcon,ycon,iselect0
      ENDIF
      XLASTSCALEQ=XCON
      YLASTSCALEQ=YCON
      RETURN
!#######################################################################
10    CONTINUE  ! ISELECT=10  EJECT PAGE  clear_
         CALL VFLUSH()              ! flush graphics buffers
         CALL COLOR(7)
         CALL CLEAR()
         CALL COLOR(0)
         CALL VFLUSH()              ! flush graphics buffers
         if(debug)write(*,*)'color 0;clear;color 7'
         ! what gets reset
         ! should pen be set back to user pen color?
         ! should origin, pen width, pen color ... be reset?
         ! what should color of background be?
      RETURN
!#######################################################################
12    CONTINUE  ! ISELECT=10  EJECT PAGE  clear_2
         CALL VFLUSH()              ! flush graphics buffers
         CALL COLOR(7)
         CALL CLEAR()
         CALL COLOR(0)
      RETURN
!#######################################################################
11    CONTINUE  ! dl_pause

      WRITE(*,*)'#PAUSING -- click "n" on plot'
      CALL VFLUSH()              ! flush graphics buffers
      WRITE(*,*)CHAR(7)          ! send bell character
      do I110=1,1000 ! flush key buffer
         IF(CHECKKEY().EQ.0)exit
      enddo
      IVALUE=GETKEY()     ! wait till keypress is read in graphic window
      CALL VFLUSH()
      RETURN
!#######################################################################
13    CONTINUE  ! dl_pause2

      WRITE(*,*)'#PAUSING -- click "n" on plot'
      CALL VFLUSH()              ! flush graphics buffers
      WRITE(*,*)CHAR(7)          ! send bell character
      do I130=1,1000 ! flush key buffer
         IF(CHECKKEY().EQ.0)exit
      enddo
      IVALUE=GETKEY()     ! wait till keypress is read in graphic window
      CALL VFLUSH()
      if(debug)write(*,*)'vflush'
      RETURN
!#######################################################################
END SUBROUTINE plot_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!*==range_.f90 processed by SPAG 8.01RF 22:14 12 Dec 2024
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
SUBROUTINE range_(X,S,N,K,Ix,Xmin,Dx)
IMPLICIT NONE
!
!     CREATES SMOOTHED LINEAR SCALE FACTORS FROM INPUT DATA
!
!     X  (R) ARRAY OF DATA TO BE SCANNED FOR MAXIMUM AND MINIMUM
!            VALUES.
!     S  (R) LENGTH OVER WHICH THIS DATA IS TO BE PLOTTED.
!     N  (I) NUMBER OF DATA POINTS IN THE ARRAY X.
!     K  (I) REPEAT CYCLE OF MIXED ARRAY(NORMALLY 1).
!     IX (I) FIRST RELEVANT DATA POINT IN X
!     XMIN  (R) SMOOTHED MINIMUM AFTER CALL
!     DX (R) SMOOTHED INCREMENT AFTER CALL
!
!     TO USE SMOOTHED VALUES: XPLOTTED=(XVALUE-XM)/DX
!
INTEGER :: i
INTEGER :: idx
INTEGER :: Ix
INTEGER :: K
INTEGER :: N
INTEGER :: np
REAL :: Dx
REAL :: S
REAL :: si
REAL :: sj
REAL :: xi
REAL :: xmax
REAL :: Xmin
REAL :: xmm
REAL X(*) , q(6)
DATA q/1.0 , 2.0 , 4.0 , 5.0 , 8.0 , 10.0/
   np = N*K
   xmax = X(1)
   Xmin = xmax
   DO i = Ix , np , K
      xi = X(i)
      xmax = amax1(xmax,xi)
      Xmin = amin1(Xmin,xi)
   ENDDO
   xmm = Xmin
   IF ( S>0.0 ) THEN
      Dx = (xmax-Xmin)/S
      IF ( Dx>0.0 ) THEN
         sj = 0.0
         IF ( Dx<1.0 ) sj = -1.0
         idx = alog10(Dx) + sj
         Dx = Dx/(10.0**idx)
         SPAG_Loop_1_1: DO i = 1 , 6
            xi = q(i)
            IF ( xi>=Dx ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
         Dx = xi*(10.0**idx)
         si = 1.0
         sj = 0.0
         IF ( Xmin<0 ) THEN
            si = -1.0
            sj = -0.99999
            Xmin = -Xmin
         ELSEIF ( Xmin==0 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
         idx = alog10(Xmin) + sj
         Xmin = Xmin/(10.0**idx)
         Xmin = Xmin - sj
         Xmin = ifix(Xmin)*si*(10.0**idx)
         CALL spag_block_1
         RETURN
      ENDIF
   ENDIF
   Dx = 1.0
   Xmin = Xmin - 0.5
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     BEFORE EXIT, CHECK TO BE SURE THAT DATA IS CONTAINED WITHIN
!     THE LIMITS XMIN AND XMIN+DX*S.  IF NOT, RESET DX
!
      IF ( xmm<Xmin ) Xmin = xmm
      IF ( xmax>Xmin+Dx*S ) THEN
         IF ( S>0.0 ) Dx = (xmax-Xmin)/S
         IF ( Dx<=0.0 ) Dx = 1.0
      ENDIF
   END SUBROUTINE spag_block_1
END SUBROUTINE range_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!      symbol_(3f) - [M_graph] routine to plot characters and symbols
!!##SYNOPSIS
!!
!!       SUBROUTINE symbol_(X,Y,S,T,A,NN,IS)
!!##DESCRIPTION
!!
!!      Routine to plot characters and symbols
!!
!!      X,Y   string position. If x>998 or y>998 then plotting
!!            of the string is continued from the last symbol_(3f) call
!!
!!      S     height of the string to be printed
!!
!!      T     character variable containing the ascii text to be plotted
!!
!!      A     angle at which the string is to be plotted
!!            counter-clockwise from x axis
!!
!!      N     number of characters to use from T
!!
!!            note: plotting will terminate if an ASCII zero is
!!            encountered at any other position than the first character.
!!
!!            If N<0, a plot(x,y,2) will be executed prior to plotting
!!            the first character and ABS(N) characters will be plotted.
!!
!!            For N<2, the plot pen is left at the 1st character origin
!!            point; otherwise it is at the end of the last plotted
!!            vector in the last plotted character.
!!
!!      IS    centering option flag.
!!
!!            = -3  end coordinates of string (if it were to be
!!                  plotted will be returned in x,y where the input
!!                  (x,y) are the lower left corner of string. This
!!                  permits computation of the plotted length.
!!                  However, no plotting is done and the last position
!!                  variables are not changed.
!!            = -2  end coordinates of string are returned in x,y.
!!                  Initial (x,y) to be lower left corner of plotted
!!                  string. String is plotted.
!!            = -1  (x,y) to be lower left corner of plotted string
!!                  (x and y not altered) String is plotted.
!!            = 0   (x,y) to be center of plotted string
!!                  (x and y not altered) String is plotted.
!!            = 1   (x,y) to be lower right corner of plotted string
!!                  (x and y not altered) String is plotted.
!!
!!    symbol_ plots an ASCII string in a CHARACTER array. Each character
!!    (or string of characters) can be imagined as a square box with the
!!    origin at the lower left corner. The routine determines the initial
!!    position of the lower left of the first character than plots each
!!    character relative to this position. As each character is plotted the
!!    "current position" is moved to the right (along the string baseline)
!!    a fixed amount S. When the string centering option is selected,
!!    the length of the plotted string is determined and, based on the
!!    character height, the lower left corner is computed from the input
!!    (x,y) position. The special plot symbols (ASCII 0-31) are always
!!    centered about the current position.
!*==symbol_.f90 processed by SPAG 8.01RF 22:24 12 Dec 2024
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
SUBROUTINE symbol_(X,Y,S,T,A,Nn,Is)
IMPLICIT NONE
!     WRITTEN BY: D. LONG  JAN 1991,1995   BYU
!     THIS ROUTINE IS FORTRAN-77 COMPATIBLE WITH THE FOLLOWING
!     EXCEPTIONS:
!        1. INTEGER*2 ARRAYS ARE USED TO SAVE SPACE.  THEY MAY
!           BE REPLACED WITH INTEGER.
!
!     MACHINE DEPENDENT NOTES:
!        1. THE FUNCTION IBITS(I,J,K) RETURNS THE VALUE OF THE BITS
!           IN I STARTING AT J FOR K BITS.


   REAL :: X
   REAL :: Y
   CHARACTER(LEN=*) :: T
   INTEGER :: i
   INTEGER :: icc
   INTEGER :: il
   INTEGER :: ip
   INTEGER :: ipen
   INTEGER :: ipenlast
   INTEGER :: Is
   INTEGER :: iss
   INTEGER :: iw
   INTEGER :: ix
   INTEGER :: ixoff
   INTEGER :: iy
   INTEGER :: iyoff
   INTEGER :: n
   INTEGER :: Nn
   REAL :: A
   REAL :: aa
   REAL :: al
   REAL :: co
   REAL :: ox
   REAL :: oy
   REAL :: S
   REAL :: si
   REAL :: ss
   REAL :: x0
   REAL :: x1
   REAL :: xx
   REAL :: y0
   REAL :: y1
   LOGICAL :: length
   REAL , SAVE :: oldx , oldy
!INTEGER, PARAMETER :: Short   = SELECTED_INT_KIND(4)   ! Short integer
   INTEGER , PARAMETER :: SHORT = selected_int_kind(8)  ! Long integer
!
   INTEGER(KIND=SHORT) :: ifnt(968) , ipnt(176)

! ----------------------------------------------------------------------------------------------------------------------------------
INTEGER(KIND=SHORT) :: if001(88) , if002(88) , if003(88) , if004(88) , if005(88) , if006(88) , if007(88) , if008(88) , if009(88) &
& , if010(88) , if011(88)
! ----------------------------------------------------------------------------------------------------------------------------------

   INTEGER(KIND=SHORT) :: ipt001(88) , ipt002(88)
! ----------------------------------------------------------------------------------------------------------------------------------
EQUIVALENCE (ifnt(1),if001(1)) , (ifnt(89),if002(1)) , (ifnt(177),if003(1)) , (ifnt(265),if004(1)) , (ifnt(353),if005(1)) ,      &
& (ifnt(441),if006(1)) , (ifnt(529),if007(1)) , (ifnt(617),if008(1)) , (ifnt(705),if009(1)) , (ifnt(793),if010(1)) ,            &
& (ifnt(881),if011(1))
! ----------------------------------------------------------------------------------------------------------------------------------
   EQUIVALENCE (ipnt(1),ipt001(1)) , (ipnt(89),ipt002(1))
   INTEGER :: spag_nextblock_1
! ----------------------------------------------------------------------------------------------------------------------------------
DATA if001/6186 , 6826 , 6806 , 5526 , 5546 , 6186 , 2080 , 6176 , 10282 , 10538 , 10916 , 10908 , 10518 , 10006 , 9628 , 9636 , &
& 5930 , 2090 , 6176 , 6186 , 6747 , 5595 , 2090 , 6186 , 6816 , 6166 , 5536 , 6186 , 2080 , 6176 , 6688 , 5672 , 5656 ,      &
& 2592 , 6501 , 6491 , 5851 , 5861 , 14693 , 5546 , 14053 , 5526 , 14043 , 6806 , 14683 , 6176 , 2730 , 6166 , 14378 , 5536 , &
& 2720 , 5721 , 14823 , 5735 , 2521 , 5536 , 15008 , 6166 , 14378 , 5721 , 14823 , 6617 , 1639 , 5735 , 6176 , 14358 , 6176 , &
& 2535 , 6166 , 6186 , 6816 , 5536 , 2090 , 5526 , 6826 , 5546 , 2710 , 13844 , 5672 , 6696 , 5656 , 14872 , 5920 , 2336 ,    &
& 13612 , 5672 , 6680 , 6696/
! ----------------------------------------------------------------------------------------------------------------------------------
DATA if002/1560 , 6176 , 14872 , 6696 , 6176 , 1568 , 5672 , 6696 , 5656 , 6680 , 1576 , 6176 , 6680 , 6696 , 5656 , 5672 ,      &
& 2080 , 6176 , 6186 , 6747 , 5595 , 14378 , 6757 , 6166 , 5605 , 2661 , 15124 , 6696 , 5672 , 6680 , 13848 , 6432 , 1824 ,   &
& 6696 , 6680 , 5672 , 5656 , 14888 , 6180 , 2075 , 5656 , 6696 , 5672 , 6680 , 13848 , 6432 , 1824 , 5536 , 6186 , 6816 ,    &
& 6166 , 5536 , 15008 , 6186 , 2070 , 5656 , 5672 , 6696 , 6680 , 5656 , 14888 , 5672 , 2584 , 6176 , 5928 , 6440 , 6176 ,    &
& 6688 , 6424 , 6176 , 5912 , 5664 , 2080 , 10204 , 10077 , 10015 , 10017 , 10083 , 10212 , 10340 , 10467 , 10529 , 10527 ,   &
& 10461 , 10332 , 14300 , 5983 , 5985/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if003/14177 , 6046 , 14242 , 6109 , 14307 , 6173 , 14371 , 6237 , 14435 , 6302 , 14498 , 6367 , 2273 , 5916 , 5924 , 6436 , &
& 6428 , 14108 , 5981 , 14179 , 6045 , 14243 , 6109 , 14307 , 6173 , 14371 , 6237 , 14435 , 6301 , 14499 , 6365 , 2275 ,      &
& 6170 , 5859 , 6499 , 14362 , 5986 , 6173 , 14562 , 6176 , 6114 , 6176 , 2146 , 10273 , 10208 , 10271 , 10336 , 2081 ,       &
& 10204 , 10077 , 10015 , 10017 , 10083 , 10212 , 10340 , 10467 , 10529 , 10527 , 10461 , 10332 , 2012 , 10133 , 9942 , 9752 ,&
& 9627 , 9566 , 9570 , 9637 , 9768 , 9962 , 10155 , 10411 , 10602 , 10792 , 10917 , 10978 , 10974 , 10907 , 10776 , 10582 ,   &
& 10389 , 1941 , 10122 , 9803 , 9549 , 9359 , 9170 , 9045/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if004/8921 , 8862 , 8866 , 8935 , 9067 , 9198 , 9393 , 9587 , 9845 , 10166 , 10422 , 10741 , 10995 , 11185 , 11374 , 11499 ,&
& 11623 , 11682 , 11678 , 11609 , 11477 , 11346 , 11151 , 10957 , 10699 , 10378 , 1930 , 6186 , 5527 , 6743 , 2090 , 1931 ,   &
& 9163 , 9355 , 9359 , 9167 , 13259 , 5138 , 5088 , 5280 , 1042 , 5081 , 13280 , 5472 , 1369 , 5067 , 13280 , 5472 , 13643 ,  &
& 4825 , 13913 , 5714 , 722 , 4815 , 9551 , 9810 , 9557 , 9173 , 4825 , 5084 , 13916 , 5280 , 1163 , 4815 , 13916 , 9180 ,    &
& 8924 , 8928 , 9184 , 13276 , 9803 , 9547 , 9551 , 9807 , 1611 , 9810 , 9355 , 9163 , 8911 , 8914 , 9561 , 9564 , 9376 ,     &
& 9180 , 9173 , 1611 , 9177 , 9372/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if005/1184 , 9568 , 9177 , 9170 , 1355 , 9184 , 9561 , 9554 , 971 , 5263 , 13468 , 5721 , 13010 , 4825 , 1618 , 5263 ,      &
& 13468 , 4821 , 1621 , 8905 , 9165 , 975 , 4821 , 1621 , 5068 , 971 , 4811 , 1632 , 8911 , 8924 , 9184 , 9568 , 5724 , 9807 ,&
& 9547 , 5067 , 4815 , 1628 , 5067 , 13643 , 5259 , 5280 , 988 , 8924 , 9184 , 9568 , 9820 , 9817 , 4815 , 4811 , 1611 ,      &
& 4832 , 9824 , 5724 , 9365 , 9557 , 9810 , 9807 , 9547 , 9163 , 719 , 5451 , 5472 , 4821 , 4818 , 1618 , 8911 , 9163 , 9547 ,&
& 9807 , 9813 , 5465 , 4825 , 4832 , 1632 , 8917 , 9557 , 9810 , 9807 , 9547 , 9163 , 8911 , 8921 , 9376 , 1632 , 4832 ,      &
& 5728 , 9820/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if006/9170 , 971 , 9163 , 9547 , 9807 , 9810 , 9557 , 9173 , 8921 , 8924 , 9184 , 9568 , 9820 , 9817 , 13653 , 9173 , 8914 ,&
& 8911 , 971 , 8907 , 9355 , 9810 , 9820 , 9568 , 9184 , 8924 , 8921 , 9173 , 1621 , 5068 , 13259 , 5073 , 978 , 13003 ,      &
& 9163 , 9359 , 13458 , 5272 , 1177 , 5451 , 4821 , 1376 , 4818 , 13906 , 5721 , 729 , 13003 , 5067 , 5717 , 992 , 5260 ,     &
& 13451 , 9362 , 9817 , 9820 , 9568 , 9184 , 732 , 9547 , 9163 , 8911 , 8924 , 9184 , 9568 , 9820 , 9810 , 9362 , 9365 ,      &
& 1628 , 4811 , 8921 , 5280 , 5721 , 13899 , 4818 , 1618 , 4811 , 4832 , 9568 , 9820 , 5721 , 5461 , 13013 , 9557 , 9810 ,    &
& 5711 , 5451 , 715/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if007/9820 , 9568 , 9184 , 8924 , 8911 , 9163 , 9547 , 1615 , 4811 , 9547 , 9807 , 5724 , 5472 , 13024 , 5088 , 971 , 4811 ,&
& 4832 , 13920 , 5461 , 13013 , 4811 , 1611 , 4811 , 4832 , 13920 , 5269 , 725 , 9820 , 9568 , 9184 , 8924 , 8911 , 9163 ,    &
& 5707 , 5714 , 1362 , 4811 , 13024 , 4821 , 13909 , 5728 , 1611 , 5067 , 13643 , 5259 , 13472 , 5088 , 1376 , 8911 , 9163 ,  &
& 5451 , 5711 , 1632 , 4811 , 13024 , 5728 , 4821 , 1611 , 4832 , 4811 , 1611 , 4811 , 4832 , 5269 , 5728 , 1611 , 4811 ,     &
& 13024 , 4828 , 13903 , 5728 , 1611 , 8911 , 9163 , 9547 , 9807 , 9820 , 9568 , 9184 , 8924 , 719 , 4811 , 4832 , 9568 ,     &
& 9820 , 5721 , 5461/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if008/725 , 8911 , 9163 , 9547 , 9807 , 9820 , 9568 , 9184 , 8924 , 13007 , 5266 , 1611 , 4811 , 4832 , 9568 , 9820 , 5721 ,&
& 5461 , 13013 , 5269 , 1611 , 8911 , 9163 , 9547 , 9807 , 9810 , 9557 , 9173 , 8921 , 8924 , 9184 , 9568 , 1628 , 4832 ,     &
& 13920 , 5280 , 1163 , 4832 , 8911 , 9163 , 5451 , 5711 , 1632 , 4832 , 5259 , 1632 , 4832 , 5067 , 5269 , 5451 , 1632 ,     &
& 4811 , 4815 , 5724 , 13920 , 4832 , 4828 , 5711 , 1611 , 4832 , 4828 , 5269 , 13451 , 5728 , 5724 , 1173 , 4832 , 5728 ,    &
& 5724 , 4815 , 4811 , 1611 , 5280 , 4832 , 4811 , 1163 , 5707 , 736 , 4811 , 5259 , 5280 , 736 , 4821 , 5276 , 13909 , 5276 ,&
& 1167 , 5263/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if009/4821 , 13468 , 4821 , 1621 , 4832 , 1365 , 8911 , 8917 , 9177 , 9369 , 9810 , 9355 , 9163 , 13007 , 5721 , 1611 ,     &
& 4811 , 13024 , 8914 , 9369 , 9561 , 9813 , 9807 , 9547 , 9355 , 722 , 5721 , 9177 , 8917 , 4815 , 5067 , 1611 , 5728 ,      &
& 13899 , 9810 , 9369 , 9177 , 8917 , 8911 , 9163 , 9355 , 1618 , 4818 , 9810 , 9813 , 9561 , 9177 , 8917 , 8911 , 9163 ,     &
& 1355 , 5259 , 5276 , 13664 , 5077 , 1365 , 9156 , 5444 , 5704 , 13913 , 9810 , 9369 , 9177 , 8917 , 8911 , 9163 , 9355 ,    &
& 1618 , 4811 , 13024 , 8921 , 5465 , 5717 , 1611 , 5067 , 13643 , 5259 , 5269 , 13269 , 5275 , 1180 , 5061 , 5253 , 5449 ,   &
& 13657 , 5471 , 1376 , 4811/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if010/13024 , 5724 , 13007 , 5269 , 1611 , 5067 , 13643 , 5259 , 5280 , 992 , 4811 , 13017 , 8917 , 5081 , 5269 , 13451 ,   &
& 9365 , 5465 , 5717 , 1611 , 4811 , 13017 , 8917 , 9177 , 5465 , 5717 , 1611 , 8911 , 8917 , 9177 , 9561 , 9813 , 9807 ,     &
& 9547 , 9163 , 719 , 4804 , 13017 , 8914 , 9369 , 9561 , 9813 , 9807 , 9547 , 9355 , 722 , 5700 , 13913 , 9810 , 9369 ,      &
& 9177 , 8917 , 8911 , 9163 , 9355 , 1618 , 4811 , 13017 , 8914 , 5273 , 1625 , 8907 , 9547 , 9807 , 9554 , 9170 , 8917 ,     &
& 9177 , 1625 , 5081 , 13657 , 5280 , 5263 , 1355 , 4825 , 8911 , 9163 , 9547 , 13903 , 5721 , 1611 , 4825 , 5259 , 1625 ,    &
& 4825 , 5067 , 5266 , 5451/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA if011/1625 , 4811 , 13913 , 4825 , 1611 , 9156 , 5444 , 5704 , 13913 , 4825 , 8911 , 9163 , 1611 , 4825 , 5721 , 4811 ,     &
& 1611 , 5259 , 5007 , 5075 , 4885 , 5080 , 5020 , 1184 , 5259 , 13458 , 5273 , 1184 , 5067 , 5327 , 5267 , 5461 , 5272 ,     &
& 5340 , 992 , 4819 , 5079 , 5459 , 1623 , 131 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,  &
& 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA ipt001/1 , 8 , 19 , 24 , 30 , 35 , 48 , 52 , 56 , 64 , 69 , 74 , 78 , 85 , 90 , 95 , 100 , 106 , 115 , 122 , 129 , 136 ,    &
& 144 , 152 , 162 , 190 , 209 , 220 , 225 , 238 , 259 , 292 , 296 , 297 , 306 , 310 , 318 , 328 , 340 , 351 , 354 , 358 ,     &
& 362 , 368 , 372 , 375 , 377 , 379 , 381 , 391 , 396 , 404 , 414 , 419 , 428 , 438 , 443 , 460 , 470 , 474 , 480 , 483 ,     &
& 487 , 491 , 499 , 510 , 517 , 529 , 537 , 545 , 552 , 557 , 566 , 572 , 578 , 583 , 588 , 591 , 596 , 602 , 611 , 618 ,     &
& 629 , 638 , 650 , 654 , 660 , 663/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA ipt002/668 , 676 , 683 , 689 , 693 , 695 , 699 , 704 , 709 , 711 , 721 , 731 , 737 , 747 , 756 , 761 , 773 , 779 , 786 ,    &
& 792 , 798 , 803 , 813 , 820 , 829 , 839 , 849 , 854 , 862 , 867 , 874 , 877 , 882 , 886 , 894 , 898 , 905 , 909 , 916 ,     &
& 920 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,   &
& 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!-----------------------------------------------------------------------------------------------------------------------------------
         n = Nn      ! NUMBER OF CHARACTERS
         aa = A      ! PLOTTING ANGLE
         si = sin(aa*0.0174532)
         co = cos(aa*0.0174532)
         length = .TRUE. ! PLOT (TRUE) OR LENGTH ONLY
         al = 0.0       ! PLOTTED LENGTH
         iss = Is       ! CENTERING FLAG
         IF ( iss==-3 ) length = .FALSE.
         IF ( iss>-1 ) length = .FALSE.
         ox = oldx      ! SAVE CURRENT POSITION
         oy = oldy
         spag_nextblock_1 = 2
      CASE (2)
         al = 0.0       ! LENGTH OF PLOTTED STRING ACCUMULATOR
         x1 = X      ! LOWER LEFT CORNER
         y1 = Y
         IF ( iss==0 ) THEN
                          ! CENTERED
            x1 = X - al/2.*co + S/2.*si
            y1 = Y - S/2.*co - al/2.*si
         ENDIF
         IF ( iss==1 ) THEN
                          ! LOWER RIGHT CORNER
            x1 = X - al*co
            y1 = Y - al*si
         ENDIF
         IF ( X>998.0 .OR. Y>998.0 ) THEN
            IF ( X<998.0 ) oldx = oldx + x1
            IF ( Y<998.0 ) oldy = oldy + y1
         ELSE
            oldx = x1
            oldy = y1
         ENDIF
         x0 = oldx
         y0 = oldy
         IF ( length .AND. n<0 ) CALL draw_(oldx,oldy)
                                                   ! PLOT TO START
         ss = S/21.
                  ! SCALE FACTOR
         i = 0    ! CHARACTER COUNTER
         SPAG_Loop_1_1: DO
            i = i + 1
            IF ( i>iabs(n) ) EXIT SPAG_Loop_1_1
                                   ! END OF STRING COUNT
            icc = ichar(T(i:i))
                            ! GET ITH ASCII CHARACTER
            IF ( icc<=127 ) THEN  ! CODE TO LARGE
               IF ( icc==0 .AND. i>1 ) EXIT SPAG_Loop_1_1
                                            ! END OF STRING REACHED
               ixoff = 11
                        ! OFFSET
               iyoff = 11
               IF ( icc<32 ) THEN
                              ! DIFFERENT SYMBOL OFFSET
                  ixoff = 32
                  iyoff = 32
               ENDIF
               il = ipnt(icc+1)
                          ! STARTING INDEX
               iw = 21  ! CHARACTER WIDTH
               IF ( il/=0 ) THEN
                               ! NO PLOTTING INFO
                  ipenlast = 3
                  SPAG_Loop_2_2: DO
                     iy = ibits(ifnt(il),0,6)
                     ix = ibits(ifnt(il),6,6)
                     ipen = ibits(ifnt(il),12,2)
                     ip = ipenlast
                     ipenlast = ipen
                     xx = ss*(ix-ixoff)
                     ! Y1=SS*(IY-IYOFF+ISUB)
                     y1 = ss*(iy-iyoff)
                     x1 = xx*co - y1*si + oldx
                     y1 = xx*si + y1*co + oldy
                     IF ( ip==0 ) ip = 2
                     IF ( ip==1 ) ip = 2
                     IF ( length ) CALL plot_(x1,y1,ip)
                     il = il + 1
                     IF ( ipen==0 ) EXIT SPAG_Loop_2_2
                  ENDDO SPAG_Loop_2_2
               ENDIF
               xx = ss*iw
                        ! END OF CHARACTER
               al = al + ss*iw
               oldx = xx*co + oldx
               oldy = xx*si + oldy
            ENDIF
         ENDDO SPAG_Loop_1_1
         IF ( .NOT.length ) THEN
                            ! FINISHED LENGTH-ONLY PASS
            length = .TRUE.
            IF ( iss==-3 ) THEN
                             ! RETURN END POSITION
               X = oldx
               Y = oldy
            ENDIF
            oldx = ox
                     ! RESTORE OLD POSITION
            oldy = oy
            IF ( iss==0 .OR. iss==1 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            IF ( n<=1 ) CALL move_(x0,y0)
                                       ! LEAVE PEN AT START
            IF ( iss==-2 ) THEN
                             ! RETURN END POSITION
               X = oldx
               Y = oldy
            ENDIF
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE symbol_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!SUBROUTINE gridll_
!
! gridll_ plots a  logarithmic or linear  grid using solid lines, dotted
!lines, or ticks. See also GRID.
!
!CALL gridll_ (x,y,dx,dy,nx,ny,i)
!
!x,y   (R): location coordinates for the bottom-left corner of grid
!dx,dy (R): spacing of major grid lines in x and y directions
!nx    (i): number of major grid lines in x direction
!           < 0 : log spacing of minor lines
!           > 0 : no minor lines/ticks
!ny    (i): number of major grid lines in y direction
!           < 0 : log spacing of minor lines
!           > 0 : no minor lines/ticks
!i     (i): option flag
!           = 0 : solid major and minor lines
!           = 1 : dotted major and minor lines
!           = 2 : solid major lines with minor ticks
!
!
!-------------------------------------------------------------------------------
!      program testgrid
!!     PROGRAM TO DEMONSTRATE THE GRID
!!     should add background color, dash code style,
!!
!      CALL graph_init(14.0, 14.0, 1.0, 1.0, 1.0)
!      call color_(5)
!      call symbol_(-0.15,-0.1,0.30,'Simple grids',90.0,39,-1)
!      call color_(4)
!      !call width_(2)
!      x=0.0
!      y=0.0
!      dx=0.5
!      dy=0.5
!      nx=3
!      ny=3
!      i=1
!      do 10 i10=1,3
!         call color_(i10+3)
!         !call newpen_(i10+4)
!         do 20 i20=1,3
!            i=i20-1
!            call gridll_(x+0.0,y+0.0,dx,dy, nx, ny,i)
!            call gridll_(x+0.0,y+2.0,dx,dy, nx,-ny,i)
!            call gridll_(x+2.0,y+0.0,dx,dy,-nx, ny,i)
!            call gridll_(x+2.0,y+2.0,dx,dy,-nx,-ny,i)
!            x=x+4.0
!20       continue
!         x=0.0
!         y=y+4.0
!10    continue
!      CALL vflush()
!      CALL DL_PAUSE()
!      CALL vexit()
!      STOP
!      END
!-------------------------------------------------------------------------------
!David Long
!Wed Jun 12 10:34:11 MDT 1996
!-------------------------------------------------------------------------------
SUBROUTINE gridll_(X,Y,DX,DY,NX,NY,IF)
!
!     ROUTINE TO PRODUCE LOGARITHMIC GRID
!
!     CREATED BY D. LONG    NOV, 1983  AT JPL
!
!     X,Y   COORDINATES OF BOTTOM LEFT CORNER
!     DX,DY SPACING OF GRID LINES IN X AND Y DIRECTIONS
!     NX,NY NUMBER OF GRIDS IN X AND Y DIRECTIONS
!        IF NY > 0 AND NX > 0 THEN LINEAR GRID
!        IF NY < 0 AND NX < 0 THEN LOG X AND LINEAR Y
!        IF NY < 0 AND NX < 0 THEN LOG X AND LOG Y
!        IF NY > 0 AND NX > 0 THEN LINEAR Y AND LOG Y
!     IF OPTION FLAG
!        = 0 SOLID.  MINOR LINES FOR LOG SPACING
!        = 1 DOTTED. MINOR LINES FOR LOG SPACING
!        = 2 SOLID.  MINOR TICKS FOR LOG SPACING
!
real     :: dx
real     :: dy
integer  :: if
integer  :: inx1, iny1
integer  :: ix,   iy
integer  :: ixm,  iym
integer  :: nx,   ny
real     :: x,    y
real     :: x0,   y0
real     :: x1,   y1
real     :: xm,   ym
   IF (NX.EQ.0.OR.NY.EQ.0) RETURN
   IF (IF.EQ.1) CALL newpen_(2)  ! DOTTED LINES
   XM=IABS(NX)*DX+X
   YM=IABS(NY)*DY+Y
   INX1=IABS(NX)+1
   INY1=IABS(NY)+1
   CALL move_(X,Y)
   CALL draw_(X,YM)
   DO IX=1,IABS(NX)
      IF (NX.LT.0) THEN
         X0=(IX-1)*DX+X
         DO IXM=2,9
            X1=ALOG10(FLOAT(IXM))*DX+X0
            IF (IF.GE.2) THEN
               DO IY=1,INY1
                  Y0=(IY-1)*DY+Y-.08
                  Y1=Y0+.16
                  IF (IY.EQ.1.AND.DY.GT.0.0) Y0=Y
                  IF (IY.EQ.1.AND.DY.LT.0.0) Y1=Y
                  IF (IY.EQ.INY1.AND.DY.GT.0.0) Y1=YM
                  IF (IY.EQ.INY1.AND.DY.LT.0.0) Y0=YM
                  CALL move_(X1,Y0)
                  CALL draw_(X1,Y1)
               enddo
            ELSE
               CALL move_(X1,Y)
               CALL draw_(X1,YM)
            ENDIF
         enddo
      ENDIF
      X0=IX*DX+X
      CALL move_(X0,Y)
      CALL draw_(X0,YM)
   enddo
   CALL move_(X,Y)
   CALL draw_(XM,Y)
   DO IY=1,IABS(NY)
      IF (NY.LT.0) THEN
         Y0=(IY-1)*DY+Y
         DO IYM=2,9
            Y1=ALOG10(FLOAT(IYM))*DY+Y0
            IF (IF.GE.2) THEN
               DO IX=1,INX1
                  X0=(IX-1)*DX+X-.08
                  X1=X0+.16
                  IF (IX.EQ.1.AND.DX.GT.0.0) X0=X
                  IF (IX.EQ.1.AND.DX.LT.0.0) X1=X
                  IF (IX.EQ.INX1.AND.DX.GT.0.0) X1=XM
                  IF (IX.EQ.INX1.AND.DX.LT.0.0) X0=XM
                  CALL move_(X0,Y1)
                  CALL draw_(X1,Y1)
               enddo
            ELSE
               CALL move_(X,Y1)
               CALL draw_(XM,Y1)
            ENDIF
         enddo
      ENDIF
      Y0=IY*DY+Y
      CALL move_(X,Y0)
      CALL draw_(XM,Y0)
   enddo
   IF (IF.EQ.1) CALL newpen_(1)  ! SOLID LINES
END SUBROUTINE gridll_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!SUBROUTINE axisc_
!
! axisc_ plots a single axis and its markings at any desired location and
!angle. Optionally, a possible exponent is determined and placed at the end of
!the axis title in the form of 10**n. The length of the axis can take any value.
!The (i)th (i=0 to ml-1) axis major tick is labeled with the value, xm+i*(xx-xm)
!/le. This version of axis is more flexible than other versions and permits
!specifying the axis number labeling format. For a log axis see axislg_.
!
!CALL axisc_ (x,y,s,n,al,a,xm,xx,t,c,f,ic)
!
!x,y  (R): starting location of the axis
!s    (C): character variable containing the axis title
!n    (i): number of characters in the string
!          > 0 : axis labeling on positive side (anti-clockwise)
!          < 0 : axis labeling on negative side (clockwise)
! (100's digit)    = 0 : coordinate line, ticks and labels drawn
!                  = 1 : line and ticks only--no labeling
! (1000's digit)   = 0 : numeric labels parallel to axis line
!                  = 1 : numeric labels orthogonal to axis line
! (100000's digit) = 0 : color list ignored
!                  = 1 : color list used
!al   (R): length of axis
!          > 0 : tick marks placed on same side of axis as title
!          = 0 : no action
!          < 0 : tick marks placed on opposite side of axis from title
!a    (R): angle at which the axis is to be drawn
!xm   (R): value of first marking on the axis
!xx   (R): value of last marking on the axis
!t    (R): number of tick marks
!          specification is coded in the form MMM.mmss where
!          MMM is the number of major tick marks ( MMM > 0), mm is
!          the number of minor tick marks between major tick marks
!          (100 > mm => 0), and ss is the number of sub-minor tick
!          marks between minor tick marks (100 > ss => 0).
!          (example 1.0102 produces I_._._i_._._I)
!c    (R): size of characters
!          < 0 auto exponent scaling (x10 to power) disabled
!          > 0 auto exponent scaling (x10 to power) enabled
!f    (R): axis number label format (see NUMBER)
!ic   (i): array of color indexes for axis colors
!          ic(1) : color value for axis line and ticks
!                  (color upon return if no labels)
!          ic(2) : color value for numbers on axis
!          ic(3) : color value for axis label
!                  (color upon return if no exponent plotted)
!          ic(4) : color for auto exponent scale
!                  (color upon return if exponent shown)
!
!-------------------------------------------------------------------------------
!David Long
!Wed Jun 12 10:34:11 MDT 1996
!*==axisc_.f90 processed by SPAG 8.01RF 22:29 12 Dec 2024
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
SUBROUTINE axisc_(X0,Y0,T,N0,S0,A0,B0,C0,D0,E0,F0,Icol)
   IMPLICIT NONE
!
!     WRITTEN BY DGL  17-OCT-1983
!     REVISED BY DGL  28-AUG-1987
!
!     PLOTS A SINGLE COORDINATE AXIS USING SYMBOL AND NUMBER.
!     THIS VERSION OF AXIS CAN HANDLE NON-INTEGER AXIS LENGTH
!     AND PERMITS FORMATTING OF THE NUMERIC LABELS.
!
!     X0  (R)  X COORDINATE OF START OF AXIS
!     Y0  (R)  Y COORDINATE OF START OF AXIS
!     T   (C)  CHARACTER STRING TO DESCRIBE AXIS
!     N0  (I)  NUMBER OF CHARACTERS IN STRING
!        - ON CLOCKWISE SIDE OF AXIS (NORMAL FOR X)
!        + ON COUNTER CLOCKWISE SIDE OF AXIS (NORMAL FOR Y)
!        HUNDREDS DIGIT = 1 NO LABELING OF AXIS--TICKS AND LINE ONLY
!        THOUSANDS DIGIT = 1 HORIZONTAL NUMBERS IN Y AXIS LABEL
!     S0  (R)  LENGTH OF AXIS IN INCHES
!        < 0   TICKS PLACED ON OPPOSITE SIDE OF AXIS LABEL
!        = 0   NO ACTION (NOP)
!        > 0   NORMAL
!     A0  (R)  ANGLE OF AXIS WITH RESPECT TO X AXIS OF PAPER
!           0.0 FOR X-AXIS
!           90.0 FOR Y-AXIS
!     B0  (R)  MINIMUM VALUE ON TICK AXIS
!     C0  (R)  MAXIMUM VALUE ON TICK AXIS
!     D0  (R)  INT(D0) = NUMBER OF MAJOR AXIS TICKS
!        INT((INT(D0)-D0)*100) = NUMBER OF MINOR AXIS TICKS BETWEEN MAJOR TICKS
!        INT(MOD((INT(D0)-D0)*10000,100) = NUMBER OF SUB MINOR AXIS TICKS
!           BETWEEN MINOR TICKS
!     E0  (R)  CHARACTER SIZE OF TITLE AND NUMBER (IF E0=0, DEFAULTS TO 0.15)
!           < 0 THEN DO NOT AUTO SCALE BY (x10 TO POWER)
!     F0  (R)  NUMBER SPECIFICATION (FORMAT FX.Y)
!         (SEE NUMBER)
!         -1 WILL AUTOMATICALLY GENERATE A "NICE" FORMAT
!     ICOL (I) OPTIONAL COLOR ARRAY (ACCESSED ONLY IF 100000'S DIGIT <> 0)
!         ICOL(1) = AXIS LINE AND TICK COLOR
!         ICOL(2) = NUMERIC LABEL COLOR
!         ICOL(3) = TITLE COLOR
!         ICOL(4) = EXPONENT COLOR
!        PEN COLOR ON RETURN WILL DEPEND ON OPTIONS IN THE
!        ORDER LISTED ABOVE
!
   REAL :: A0 , B0 , C0 , D0 , E0 , F0
   REAL :: ang
   REAL :: c1
   REAL :: e1
   REAL :: co
   REAL :: cs
   REAL :: dnx , dny
   REAL :: dtx , dty
   REAL :: fa
   REAL :: hor
   INTEGER :: i
   INTEGER :: j
   INTEGER :: k
   INTEGER :: N0
   INTEGER :: n1
   INTEGER :: nc
   INTEGER :: nd
   INTEGER :: ndd
   INTEGER :: ng
   INTEGER :: njt
   INTEGER :: nnt
   INTEGER :: nst
   REAL :: S0
   REAL :: si
   REAL :: space
   REAL :: tl
   REAL :: tl1
   REAL :: x , X0 , x01
   REAL :: x1 , x2 , x3
   REAL :: xj
   REAL :: xn
   REAL :: xs
   REAL :: y , Y0 , y01
   REAL :: y1 , y2
   REAL :: rotx , roty
   INTEGER :: Icol(*)
   CHARACTER(LEN=*) :: T
!
   rotx(x,y) = co*x - si*y + x01
                            ! ROTATION MATRIX
   roty(x,y) = si*x + co*y + y01
   DATA space/0.08/   ! MIN SPACING BETWEEN ITEMS
!
   tl = 0.1          ! TICK LENGTH
   x01 = X0
   y01 = Y0
   ang = A0          ! ROTATION ANGLE
   e1 = E0        ! CHARACTER SIZE
   cs = abs(e1)
   IF ( cs==0. ) cs = .15
   IF ( S0/=0.0 ) THEN      ! ZERO LENGTH AXIS
      x1 = D0*1.000002
      njt = abs(x1)  ! NUMBER OF MAJOR TICKS
      nnt = 100.0*(abs(x1)-njt)
                            ! NUMBER OF MINOR TICKS
      nst = 100.0*((abs(x1)-njt)*100.0-nnt)
                                       ! NUMBER OF SUB-MINOR TICKS
      IF ( njt<2 ) njt = 2
      xj = abs(S0)/(njt-1)
                       ! INCREMENT BETWEEN MAJOR TICKS
      xn = xj
      IF ( nnt/=0 ) xn = xn/(nnt+1)
                               ! INCREMENT BETWEEN MINOR TICKS
      xs = xn/(nst+1) ! INCREMENT BETWEEN SUB-MINOR TICKS
      n1 = mod(N0,100000)
      IF ( iabs(n1)/1000/=0 ) hor = 90.0
                                    ! ROTATION ANGLE
      co = cos(ang*0.017453294)
                            ! AXIS ANGLE ROTATION
      si = sin(ang*0.017453294)
      hor = ang      ! ANGLE OF NUMBER LABELS
      nc = mod(iabs(n1),100)
                         ! NUMBER OF CHARACTERS IN TITLE
!
!     DECODE NUMBER FORMAT
!
      fa = F0
      IF ( fa==-1.0 ) THEN
                         ! INPUT WAS -1.0
         fa = 1003.0 ! DEFAULT AUTO SCALING FORMAT
         IF ( e1<0.0 ) THEN
                          ! NO AUTO SCALING SO MAKE
            ng = 1    ! FORMAT TO FIT
            IF ( B0/=0.0 ) ng = max(ng,int(alog10(abs(B0))+0.001))
            IF ( C0/=0.0 ) ng = max(ng,int(alog10(abs(C0))+0.001))
            IF ( B0<0.0 .OR. C0<0 ) ng = ng + 1
            fa = 1000.0 + float(ng)
         ENDIF
      ENDIF
                               ! OUTPUT DESIRED IS INTEGER
!        FA=3.+FLOAT(ND)*(1.01) ! DEFAULT FORMAT FOR NOT AUTOSCALE
!        IF (E1.LT.0.0) THEN ! NO AUTO SCALING TO MAKE FORMAT
!          NG=2         ! WHICH FITS
!          IF (B0.NE.0.0) NG=MAX(NG,INT(ALOG10( ABS(B0))+0.001)+1)
!          IF (C0.NE.0.0) NG=MAX(NG,INT(ALOG10( ABS(C0))+0.001)+1)
!          IF (B0.LT.0.0.OR.C0.LT.0) NG=NG+1
!          FA=FLOAT(NG)+FLOAT(ND)*1.01
!        ENDIF
      IF ( abs(fa)>1000.0 ) nd = abs(fa) - 1000.0
                        ! INPUT INTEGER VALUE
      nd = mod(abs(fa),1000.0)
                           ! NUMBER OF DIGITS
      ng = 100.0*(mod(abs(fa),1000.0)-nd+0.0001)
                                           ! NUMBER OF DIGITS RIGHT OF D.P.
      IF ( ng>17 ) ng = ng/10
                           ! CORRECT INPUT SIMPLE ERRORS
      IF ( fa<0.0 ) nd = nd - 4
                           ! EXPONENTIAL NOTATION
      IF ( nd<=0 ) nd = ng
      ndd = nd
      IF ( fa<0.0 ) ndd = nd + 4
                            ! EXPONENTIAL NOTATION
      IF ( abs(fa)>1000 ) ng = -1
                              ! FORMATTED INTEGER
!
      tl1 = tl
      IF ( S0<0.0 ) tl1 = -tl
                           ! REVERSE SIDE OF TICKS
      IF ( S0<0.0 ) tl = 0.0
                          ! REVERSE SIDE OF TICKS
      IF ( iabs(n1)/1000/=0 ) THEN
         dnx = (-cs)/2.0
                    ! NUMBER LABEL DISTANCE FROM AXIS
         dny = (-tl) - space
                     ! NUMBER LABEL DISTANCE FROM AXIS
         dty = dny - ndd*cs - 2.*space
                           ! TITLE DISTANCE FROM AXIS
         hor = ang - 90.0
      ELSE
         dnx = (-ndd)*cs/2.0
                      ! NUMBER LABEL DISTANCE FROM AXIS
         dny = (-tl) - space - cs
                      ! NUMBER LABEL DISTANCE FROM AXIS
         dty = dny - cs - space
                    ! TITLE DISTANCE FROM AXIS
      ENDIF
      IF ( n1>=0 ) THEN  ! CLOCKWISE TITLES
         dny = -dny - cs
                  ! COUNTER-CLOCKWISE TITLES
         dty = -dty - cs
         tl1 = -tl1
               ! CHANGE SIDES OF TICKS
         IF ( iabs(n1)>=1000 ) THEN
            dny = dny + cs*ndd
            dty = dny + space
         ENDIF
      ENDIF
      x1 = 0.0       ! FIRST MAJOR TICK
      y1 = 0.0
      y2 = -tl1
      IF ( iabs(N0)>=100000 ) CALL color_(Icol(1))
      CALL move_(rotx(x1,y1),roty(x1,y1))
      CALL draw_(rotx(x1,y2),roty(x1,y2))
      DO i = 1 , njt - 1
                      ! MAJOR TICKS
         CALL move_(rotx(x1,y1),roty(x1,y1))
         x1 = xj*i
         y2 = -tl1
         CALL draw_(rotx(x1,y1),roty(x1,y1))
         CALL draw_(rotx(x1,y2),roty(x1,y2))
         DO j = 1 , nnt + 1
                       ! MINOR TICKS
            y2 = (-tl1)*0.7
            x2 = x1 + xn*j - xj
            CALL move_(rotx(x2,y1),roty(x2,y1))
            CALL draw_(rotx(x2,y2),roty(x2,y2))
            y2 = (-tl1)*0.4
            DO k = 1 , nst
                        ! SUB MINOR TICKS
               x3 = x2 + xs*k - xn
               CALL move_(rotx(x3,y1),roty(x3,y1))
               CALL draw_(rotx(x3,y2),roty(x3,y2))
            ENDDO
         ENDDO
      ENDDO
      IF ( mod(iabs(n1),1000)<=100 ) THEN   ! NO LABELING
         xs = 0.0     ! EXPONENT
         IF ( e1>=0. ) THEN ! NO AUTO SCALING
!
!     COMPUTE AUTO EXPONENT SCALING SO THAT THE FORMATTED LABEL
!     HAS THE INTEGER PORTION FILLED AS MUCH AS POSSIBLE
!
            i = nd - ng - 1
            IF ( B0<0.0 .OR. C0<0.0 ) i = i - 1
            IF ( B0/=0.0 ) THEN
               x1 = alog10(abs(B0)+1.E-30)
               IF ( x1<0.0 .AND. abs(aint(x1-0.001)-x1)>0.001 ) x1 = x1 - 1.0
               IF ( x1>=0.0 ) x1 = x1 + 1.0
               x1 = aint(x1)
            ELSE
               x1 = 0.0
            ENDIF
            IF ( C0/=0.0 ) THEN
               y1 = alog10(abs(C0)+1.E-30)
               IF ( y1<0.0 .AND. abs(aint(y1-0.001)-y1)>0.001 ) y1 = y1 - 1.0
               IF ( y1>=0.0 ) y1 = y1 + 1.0
               y1 = aint(y1)
            ELSE
               y1 = 0.0
            ENDIF
            x2 = min(x1,y1)
            x3 = max(x1,y1)
            IF ( x3<0.0 ) x3 = x3 + 1
            IF ( x2<0.0 .AND. ng<=2-x2 ) xs = nd - ng - 1 - x2
            IF ( i<x3+xs ) xs = i - x3
         ENDIF
         y1 = dny
         y2 = (C0-B0)/(njt-1)
         IF ( iabs(N0)>=100000 ) CALL color_(Icol(2))
         e1 = xs     ! EXPONENT VALUE
         DO i = 1 , njt
                       ! LABEL MAJOR TICKS
            x1 = (i-1)*xj + dnx
            c1 = (y2*(i-1)+B0)*10.**e1
            CALL number_(rotx(x1,y1),roty(x1,y1),cs,c1,hor,fa,-1)
         ENDDO
!
!     PLOT TITLE
!
         IF ( nc/=0 ) THEN
            y1 = 0.0
            x1 = 0.0
            CALL symbol_(x1,y1,cs,T,0.,nc,-3)
                                        ! GET TITLE LENGTH
            dtx = (abs(S0)-x1)/2.
                            ! CENTER TITLE
            IF ( e1/=0.0 ) dtx = dtx - cs*3.0
                                    ! ADD EXPONENT SPACE
            IF ( iabs(N0)>=100000 ) CALL color_(Icol(3))
            CALL symbol_(rotx(dtx,dty),roty(dtx,dty),cs,T,ang,nc,-1)
            dtx = dtx + x1
         ELSE
            dtx = abs(S0)/2.0
            IF ( e1/=0.0 ) dtx = dtx - cs*3.0
                                    ! ADD EXPONENT SPACE
         ENDIF
         x1 = dtx + cs/2.0
         y1 = dty
         IF ( e1/=0.0 ) THEN ! NO EXPONENT
            IF ( iabs(N0)>=100000 ) CALL color_(Icol(4))
            e1 = -e1
            CALL symbol_(rotx(x1,y1),roty(x1,y1),cs,'(X10',ang,4,-1)
            x1 = x1 + 3.75*cs
            y1 = y1 + cs*0.4
            CALL number_(rotx(x1,y1),roty(x1,y1),cs,e1,ang,0.0,-1)
            x2 = aint(alog10(abs(e1))) + 0.75
            IF ( e1<0.0 ) x2 = x2 + 1.0
            x1 = x1 + x2*cs
            y1 = y1 - cs*0.4
            CALL symbol_(rotx(x1,y1),roty(x1,y1),cs,')',ang,1,-1)
         ENDIF
      ENDIF
   ENDIF
END SUBROUTINE axisc_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!SUBROUTINE rect_
!
!rect_ plots a rectangle defined by the lower left and upper right hand
!corners. The pen moves UP to the lower left hand corner of the rectangle, plots
!the rectangle, and leaves the pen DOWN at the lower left corner.
!
!CALL rect_ (x1,y1,x2,y2)
!
!x1,y1 (R): lower left hand corner coordinates
!x2,y2 (R): upper right hand corner coordinates
!
!-------------------------------------------------------------------------------
!David Long
!Wed Jun 12 10:34:11 MDT 1996
!-------------------------------------------------------------------------------
SUBROUTINE rect_(X0,Y0,X1,Y1)
real,intent(in) :: x0,y0, x1,y1
!
!     ROUTINE TO PLOT A RECTANGLE
!     CREATED BY D. LONG JULY, 1983 AT JPL
!
!     X0,Y0 (R) LOWER LEFT HAND CORNER
!     X1,Y1 (R) UPPER RIGHT HAND CORNER
!
!     NOTE: PEN ENDS UP DOWN AT LOWER-LEFT HAND CORNER
!
   CALL move_(X0,Y0)
   CALL draw_(X1,Y0)
   CALL draw_(X1,Y1)
   CALL draw_(X0,Y1)
   CALL draw_(X0,Y0)
END SUBROUTINE rect_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!SUBROUTINE newpen_
!
!newpen_ changes the hardware line type and/or width of the plotting line for
!subsequent plotting. There are 9 standard line types. Line widths are not
!supported on all devices. Default line type is a solid line of width 1 dot.
!
!CALL newpen_ (i)
!
! (I): selects a line type for all additional plotting
!        < 0 : resets line type to solid line of unit width.
!        = 0 : no change
!        > 0 : line type and width changed according to:
! (1's digit)   : line type (1-9) (value of 0 does not change type)
!                      type  pattern
!                       0 no change
!                       1 solid
!                       2 dotted
!                       3 long-dot-long
!                       4 meddash-meddash
!                       5 longdash-longdash
!                       6 long-dot-dot-long
!                       7 long-short-long
!                       8 short-short
!                       9 long-dot-dot-dot-long
! (10's digit)  : line width (1-7) (value of 0 does not change width)
! (100's digit) : line type pattern scaling (1-7) (0 is no change)
!
!#ifdef TESTPRG90
!      PROGRAM LINETYPE
!      use M_graph
!      use M_draw
!! *** LAST REVISED ON 22-JUN-1990 08:33:36.89
!! *** SOURCE FILE: [DL.GRAPHICS.libDL]LINETYPE.FOR
!!
!!     PROGRAM TO DEMONSTRATE THE AVAILABLE LINE TYPES AND COLORS
!!
!      CALL graph_init(12.0,12.0,0.2,0.3,1.0)
!      do 200 idash=0,1000,100
!!
!!        PLOT A PAGE TITLE
!         CALL newpen_(-1)
!!
!         CALL symbol_(.15,.1,.15, '(Type,Width,Color) Available Line Types' ,90.,39,-1)
!!
!!        PLOT A SAMPLE OF EACH LINE TYPE, WIDTH, AND COLOR
!!
!         ICNT=0
!         DO IWIDTH=1,5
!            DO ITYPE=1,9
!               DO ICOLOR=1,7
!      !
!10                X=(ICNT/35)*1.7+.21
!                  Y=MOD(ICNT,35)*.2
!                  IF (X.GT.10.0) THEN             ! NEW PAGE
!                      CALL DL_PAUSE()
!                      CALL VFLUSH()              ! flush graphics buffers
!                      CALL COLOR(7)
!                      CALL CLEAR()
!                      CALL COLOR(0)
!                      CALL VFLUSH()              ! flush graphics buffers
!                      ICNT=0
!                      GOTO 10
!                  ENDIF
!                  ICNT=ICNT+1
!      !
!      !           SET TO SOLID LINE OF WIDTH 1
!      !
!                  CALL newpen_(-1)
!                  CALL color_(2)
!                  CALL number_(X,Y,.15,REAL(ITYPE),0.,0.0,-1)
!                  CALL symbol_(999.,999.,.15,',',0.,1,-1)
!                  CALL number_(999.,999.,.15,REAL(IWIDTH),0.,0.0,-1)
!                  CALL symbol_(999.,999.,.15,',',0.,1,-1)
!                  CALL number_(999.,999.,.15,REAL(ICOLOR),0.,0.0,-1)
!                  CALL color_(ICOLOR)
!                  CALL newpen_(IDASH+ITYPE+10*IWIDTH)
!                  X=X+0.8
!                  CALL move_(X,Y)
!                  CALL draw_(X+.75,Y)
!                  CALL move_(X,Y)
!               enddo
!            enddo
!         enddo
!         CALL DL_PAUSE
!         CALL VFLUSH()              ! flush graphics buffers
!         CALL COLOR(7)
!         CALL CLEAR()
!         CALL COLOR(0)
!         CALL VFLUSH()              ! flush graphics buffers
!200   CONTINUE
!      CALL DL_PAUSE()
!      CALL vflush()
!      CALL vexit()
!      STOP
!      END
!#endif
!
!-------------------------------------------------------------------------------
!David Long
!Wed Jun 12 10:34:11 MDT 1996
!-------------------------------------------------------------------------------
! *************************************************************************
SUBROUTINE newpen_(IP)
!
!     CHANGE PLOTTING LINE TYPE ON ALL DEVICES
!     FORTRAN-77 VERSION:   DGL JULY, 1987
!
!     IP=DASHSCALE,WIDTH,DASHCODE

!     First pick up pen at current location, then change line type
!     note: ip<0 resets pen width to 1 and line type to solid
!
!     THE line types should resemble these:
!
!     UNITS DIGIT=line type
!         0 = no change
!         1-9 line type
!         type  pattern
!          1 solid
!          2 dotted
!          3 long-dot-long
!          4 meddash-meddash
!          5 longdash-longdash
!          6 long-dot-dot-long
!          7 long-short-long
!          8 short-short
!          9 long-dot-dot-dot-long
!
!     TENS  DIGIT = line width in pixels or dots
!       0 = no change
!       1-7 line width
!
!     HUNDREDS DIGIT = line type scale factor
!       0 = no change
!       1-9 line type scale factor
!
integer :: ip
integer :: idashcode
integer :: idashscale
integer :: iwidth
   IF (IP.LE.0) THEN
      IWIDTH=1
      IDASHSCALE=3
      IDASHCODE=1
   ELSE
      IDASHCODE=MOD(IP,10)
      IWIDTH=MOD(IP,100)/10
      IDASHSCALE=(IP/100)
   ENDIF
!     CHANGE LINE TYPE AND SCALE
   if(debug)write(*,*)'*newpen_* ',ip,idashcode,iwidth,idashscale
   IF(IDASHSCALE.NE.0)CALL dashscale_(IDASHSCALE) ! DASH SCALE FACTOR
   IF(IWIDTH   .NE.0)CALL width_(IWIDTH)        ! LINE WIDTH
   IF(IDASHCODE.NE.0)CALL dashcode_(IDASHCODE)  ! DASH TYPE
END SUBROUTINE newpen_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!SUBROUTINE axislg_
!
!axislg_ plots a single logarithmic coordinate axis. Complete decades are
!produced. See also DL_AXISA and DL_LLINE.
!
!CALL axislg_ (x,y,s,n,al,a,nmin,dx<,ic>)
!
!x,y   (R): location starting point of the axis
!s     (C): axis label string
!n     (i): number of characters in string
!           > 0 :  label on positive side
!           < 0 :  label on negative side
! (100's digit)   = 0 : coordinate line, ticks and labels drawn
!                 = 1 : line and ticks only--no labeling
! (1000's digit)  = 0 : numeric labels parallel to axis line
!                 = 1 : numeric labels orthogonal to axis line
! (10000's digit) = 0 : color list ignored
!                 = 1 : color list used
!al    (R): length of axis
!           > 0 : axis ticks placed on same side of axis as title
!           = 0 : no action (return with no plotting)
!           < 0 : ticks placed on opposite side of axis from title
!a     (R): angle at which the axis should be plotted
!nmin  (R): number to be printed at the first axis tick (power of ten)
!dx    (R): scaling factor in the form dx=(nmax-nmin)/l where
!           nmax, nmin are the exponent powers at the start
!           and end of the axis
!ic    (i): color array (accessed if mag(n)>10000))
!           ic(1) : color for axis line and ticks
!           ic(2) : color for numbers
!           ic(3) : color for axis title
!
!-------------------------------------------------------------------------------
!David Long
!Wed Jun 12 10:34:11 MDT 1996
!-------------------------------------------------------------------------------
!*==axislg_.f90 processed by SPAG 8.01RF 22:33 12 Dec 2024
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
SUBROUTINE axislg_(X0,Y0,A0,N0,S0,T0,C0,D0,Icol)
   IMPLICIT NONE
!
!     THIS ROUTINE PLOTS A LOG AXIS WITH EXPONENT LABELING
!     MODIFIED FROM AXIS D.LONG 22-OCT-83 AT JPL
!
!     X0 X COORDINATE OF START OF AXIS
!     Y0 Y COORDINATE OF START OF AXIS
!     A0 CHARACTER STRING TO DESCRIBE AXIS
!     N0 NUMBER OF CHARACTERS IN STRING
!        - ON CLOCKWISE SIDE OF AXIS (NORMAL FOR X)
!        + ON COUNTER CLOCKWISE SIDE OF AXIS (NORMAL FOR Y)
!        HUNDREDS DIGIT    = 1 TICKS AND LINE ONLY--NO LABELING
!        THOUSANDS DIGIT    = 1 HORIZONTAL NUMBERS IN Y AXIS LABEL
!        100 THOUSANDS DIGIT = 1 USE COLOR ARRAY
!     S0 LENGTH OF AXIS
!        < 0   TICKS ON OPPOSITE SIDE OF AXIS FROM LABEL
!        = 0   NO ACTION (DUMMY CALL)
!        > 0   TICKS ON SAME SIDE AS AXIS LABEL (NORMAL)
!     T0 ANGLE OF AXIS TO X AXIS OF PAPER
!        0.0 FOR X-AXIS
!        90.0 FOR Y-AXIS
!     C0 COORDINATE OF MINIMUM TICK ON AXIS
!     D0 SCALING FACTOR EXPONENT OF THE FORM (NMAX-NMIN)/LENGTH
!     ICOL  COLOR ARRAY (NEEDED ONLY IF MAG(NO)>=10000)
!        ICOL(1) AXIS COLOR
!        ICOL(2) NUMBERS COLOR
!        ICOL(3) TITLE COLOR (RETURN)
!
   REAL :: aj
   REAL :: at
   REAL :: b1
   REAL :: b3
   REAL :: b4
   REAL :: b6
   REAL :: b7
   REAL :: b8
   REAL :: C0
   REAL :: c1
   REAL :: cs
   REAL :: D0
   REAL :: hor
   REAL :: ht3
   REAL :: ht4
   INTEGER :: i
   INTEGER :: j
   INTEGER :: N0 , n1 , n2
   REAL :: S0
   REAL :: s5
   REAL :: s6
   REAL :: T0 , t1 , t2 , t3 , t4 , t5 , t6
   REAL :: X0 , x1 , x2
   REAL :: xl
   REAL :: Y0 , y1 , y2
   CHARACTER*(*) A0
   INTEGER Icol(3)
   LOGICAL labels , color
   DATA cs/.15/      ! CHARACTER SIZE
   n1 = iabs(N0)
   labels = .TRUE.
   hor = T0
   IF ( S0==0.0 ) RETURN
   t5 = 0.1       ! TICK LENGTH
   b7 = t5 + .08  ! NUMBER DISTANCE FROM AXIS
   b6 = b7
   b8 = 0.0
   IF ( N0<0 ) THEN
      b3 = (-cs)*4.0
      b4 = 0.0 - t5 - cs - .05
      t2 = T0
      t5 = -t5
   ELSE
      b3 = cs*4.0
      b4 = cs + 0.08
      t2 = T0
   ENDIF
   color = .FALSE.
   IF ( n1>=100000 ) THEN
      n1 = mod(n1,100000)
      color = .TRUE.
   ENDIF
   IF ( n1>=10000 ) n1 = mod(n1,10000)
   IF ( n1>=1000 ) THEN
      n1 = mod(n1,1000)
      hor = 0.0
      b4 = (abs(t5)+.05)*sign(1.,float(N0))
      IF ( N0>0 ) b4 = b4 + 3.5*cs
      b6 = .5*cs
      b8 = (.5*cs+abs(t5))*sign(1.,float(N0))
      IF ( N0<0 ) b8 = b8 - cs*1.6
   ENDIF
   IF ( n1>=100 ) THEN
      n1 = mod(n1,100)
      labels = .FALSE.
   ENDIF
   n2 = abs(S0*D0) + 0.5 ! NUMBER OF TICKS
   xl = abs(S0)/float(n2)  ! DISTANCE BETWEEN TICKS
   t1 = T0*0.017453294
   t3 = cos(t1)
   t4 = sin(t1)
   ht3 = cos(hor*0.017453294)
   ht4 = sin(hor*0.017453294)
   t6 = t5*t3
   t5 = t5*t4
!
   IF ( color ) CALL color_(Icol(1))
                                   !COLOR
   s5 = sign(1.0,S0)*t5
   s6 = sign(1.0,S0)*t6
   x1 = X0
   y1 = Y0
   CALL move_(x1,y1)
   DO i = 1 , n2  ! PLOT MAJOR TICKS
      DO j = 2 , 10  ! PLOT MINOR TICKS
         at = .6
         IF ( j==2 ) at = 1.0
         x2 = x1 - s5*at
         y2 = y1 + s6*at
         CALL move_(x2,y2)
         CALL draw_(x1,y1)
         aj = alog10(float(j)) - alog10(float(j-1))
         x1 = x1 + t3*xl*aj
         y1 = y1 + t4*xl*aj
         IF ( T0==90.0 ) x1 = X0
         CALL draw_(x1,y1)
      ENDDO
   ENDDO
   x2 = x1 - s5
   y2 = y1 + s6
   CALL draw_(x2,y2)
!
   IF ( color ) CALL color_(Icol(3))
                                   !COLOR
   IF ( .NOT.labels ) RETURN
   c1 = aint(C0)
   x2 = X0 - b4*t4 - b7*t3
                      ! LOCATE CENTER NUMBER LABELS
   y2 = Y0 + b4*t3 - b6*t4
   n2 = n2 + 1
   IF ( color ) CALL color_(Icol(2))
                                   !COLOR
   DO i = 1 , n2
      CALL symbol_(x2,y2,cs,'10',hor,2,-1)
      x1 = x2 - .6*cs*ht4 + cs*2.*ht3
      y1 = y2 + .6*cs*ht3 + cs*2.*ht4
      IF ( c1==0.0 ) THEN
         CALL symbol_(x1,y1,cs*.8,'0',hor,1,-1)
      ELSE
         CALL number_(x1,y1,cs*.8,c1,hor,0.0,-1)
      ENDIF
      c1 = c1 + sign(1.,D0)
      x2 = x2 + t3*xl
      y2 = y2 + t4*xl
   ENDDO
   IF ( n1>=1 ) THEN
      x1 = 0.0
      y1 = 0.0
      CALL symbol_(x1,y1,cs,A0,t2,n1,-3)
                                      ! GET TITLE LENGTH
      b1 = 0.5*(abs(S0)-x1)
      x2 = X0 + b1*t3 - b3*t4 - b8*t4
      y2 = Y0 + b1*t4 + b3*t3
      IF ( color ) CALL color_(Icol(3))
                                   !COLOR
      CALL symbol_(x2,y2,cs,A0,t2,n1,-1)
   ENDIF
END SUBROUTINE axislg_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!INTEGER FUNCTION inbox_
!
!inbox_ tests a point to determine if it lies in a rectangle defined by
!xm,ym,xx,yx and returns an integer value indicating where the point is in
!relation to the rectangle that can easily be decoded by "anding" the return
!value with the binary values of 1, 2, 4, 8.
!
!
!                      9 | 8  | 10
!                    --------------
!                      1 | 0  | 2
!                    --------------
!                      5 | 4  | 6
!
!Be sure to declare the function as type INTEGER (the default type of the name
!inbox_ is REAL).
!
!   iflag = inbox_(x,y,xm,ym,xx,yx)
!
!   x,y   (R): point to test
!   xm,ym (R): lower left corner of rectangle
!   xx,yx (R): upper right corner of rectangle
!   iflag (i): clip flag (0-10) (see above)
!
! David Long
! Wed Jun 12 10:34:11 MDT 1996
! ==================================================================================================================================
INTEGER FUNCTION inbox_(X,Y,X_BOTTOM_LEFT,Y_BOTTOM_LEFT,X_top_right,Y_top_right)
!
!     FORTRAN-77 VERSION:   DGL JULY, 1987
!     CHECKS TO SEE IF POINT X,Y IS IN RECTANGLE
!     RETURNS ZERO IF IT IS
!     inbox_ tests a point to determine if it lies in a rectangle defined
!     by <x1,y1>,<x2,y2> and returns an integer value indicating where the point
!     is in relation to the rectangle.  The value can easily be decoded by
!     ANDing the return value with the binary values of 1, 2, 4, 8.
!
!           |       |
!     1+8=9 | 0+8=8 | 2+8=10
!           |       |
!           |       |<x2,y2>
!     - - - #-------# - - - Add 8 if above this line
!           |  BOX  |
!         1 |   0   | 2
!           |       |
!     - - - #-------# - - - Add 4 if below this line
!    <x1,y1>|       |
!           |       |
!     1+4=5 | 0+4=4 | 2+4=6
!           |       |
!
   real,intent(in)    :: x,y                          ! coordinates of point
   real,intent(in)    :: x_bottom_left,y_bottom_left  ! coordinates of bottom left box corner
   real,intent(in)    :: x_top_right,y_top_right      ! coordinates of upper right box corner

   INTEGER            :: CD

   CD=0                               ! start off assuming <x,y> is in or on the box

   ! check x range, and assign CD=1 if to left of box, 0 if in range, and 2 if to right of box
   IF (X .LT. x_bottom_left) THEN     ! point is left of box
      CD=1
   ELSEIF(X .GT. x_top_right)THEN     ! point is right of box
      CD=2
   ENDIF
   ! check y range, and add 4 to CD if below box and add 8 if above box
   IF (Y .LT. Y_bottom_left) THEN     ! point is below box
      CD=CD+4
   ELSEIF (Y .GT. y_top_right)THEN    ! point is above box
      CD=CD+8
   ENDIF

   inbox_=CD                        ! now CD=0 only if <x,y> is in or on the box

END FUNCTION inbox_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE clipit_(IVTB,XV2,YV2,AV1,AV2,XM,YM,XX,YX)
!
! ident_3="@(#) M_graph clipit(3f) clips a line segment partially visible"
!
real    :: av1
real    :: av2
integer :: ivtb
real    :: xm
real    :: xv2
real    :: xx
real    :: ym
real    :: yv2
real    :: yx
   IF (IAND(IVTB,1).NE.0) THEN ! LEFT EDGE
      if (av1.ne.xv2) YV2=YV2+(AV2-YV2)*(XM-XV2)/(AV1-XV2)
      XV2=XM
      IVTB=inbox_(XV2,YV2,XM,YM,XX,YX)
   ENDIF
   IF (IAND(IVTB,2).NE.0) THEN ! RIGHT EDGE
      if (av1.ne.xv2) YV2=YV2+(AV2-YV2)*(XX-XV2)/(AV1-XV2)
      XV2=XX
      IVTB=inbox_(XV2,YV2,XM,YM,XX,YX)
   ENDIF
   IF (IAND(IVTB,4).NE.0) THEN ! BOTTOM EDGE
      if (av2.ne.yv2) XV2=XV2+(AV1-XV2)*(YM-YV2)/(AV2-YV2)
      YV2=YM
      IVTB=inbox_(XV2,YV2,XM,YM,XX,YX)
   ENDIF
   IF (IAND(IVTB,8).NE.0) THEN ! TOP EDGE
      if (av2.ne.yv2) XV2=XV2+(AV1-XV2)*(YX-YV2)/(AV2-YV2)
      YV2=YX
      IVTB=inbox_(XV2,YV2,XM,YM,XX,YX)
   ENDIF
END SUBROUTINE clipit_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE trs_(XIN,YIN,XCON,YCON)
real :: xin
real :: yin
real :: xcon
real :: ycon
real :: tang
! convert call numbers to current plot coordinate system

   TANG=ANGLEQ*.0174532              ! convert degrees to radians

   XCON=XIN*COS(TANG)-YIN*SIN(TANG)  ! rotate coordinates
   YCON=XIN*SIN(TANG)+YIN*COS(TANG)  ! rotate coordinates

   XCON=SCALEQ*XCON+TRANSLATEXQ      ! scale and translate
   YCON=SCALEQ*YCON+TRANSLATEYQ      ! scale and translate

end SUBROUTINE trs_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!SUBROUTINE number_
!
!number_ plots a floating point number in a specified format using a Fortran
!format-like specification. It also permits free-format and exponential notation
!formats. The number is converted to an ASCII string plotted at a specified
!location and baseline angle using SYMBOL. The following table illustrates the
!dependence of the output string on the type (integer/real) and value of the
!parameter e. The table shows the output for an input f=103.356 and i=-1.
!
!         Output    integer e   real e
!        --------   ---------- --------
!         103          -1       1003.0
!         103.          0        0.0
!         103.          0        3.00
!         x103.36                7.02
!         103.36        2        0.02
!         103.356000    6       10.06
!         xx103                 1005.0
!         **                    1002.0 (format overflow)
!         x103.4                 6.01
!         *.****                 6.04  (format overflow)
!         .103E+02              -8.03
!         *.***                 -5.03  (format overflow)
!
!note: x=space, * indicates overflow
!
!CALL number_ (x,y,h,f,a,e,i)
!
!x,y   (R): location position (x,y returned if i=-2 or -3)
!           If x=999 then x is continued from lower right of
!           prior call to SYMBOL or NUMBER.  If y=999 then
!           y is continued.
!h     (R): size (height) of digits
!f     (R): floating point number to be plotted
!a     (R): baseline angle at which to plot (normally zero)
!e     (R): output format (e=n.j)
!           (similar to the FORTRAN format statement Fn.j)
!           n is the total number of characters (max 18)
!           including the decimal point and j is a two digit number
!           specifying the number of digits to the right of
!           the decimal point (e.g., to get F6.4 use e=6.04)
!             if e<0 number is plotted in exponential notation (En.j)
!             if e=-1.0 then f is plotted free format exponential
!             if e=1.0 then f is plotted free format real
!             if e=0.0 then f is plotted as free format integer
!             if n = 0 then f is plotted with j digits to
!                      the right of the decimal point
!             f will be plotted as a formatted m digit integer
!               [i.e., (Im)] when e=1000+m.
!i     (i): centering flag (see SYMBOL)
!           = -3 : same as -2 but string is not plotted and
!                  last position is not affected
!           = -2 : same as -1 but returns end point in x,y
!           = -1 : (x,y) is lower left corner of plotted array
!           =  0 : (x,y) is center of plotted array
!           =  1 : (x,y) is lower right corner of plotted array
!           =  2 : no action
!
!-------------------------------------------------------------------------------
!David Long
!Wed Jun 12 10:34:11 MDT 1996
!*==number_.f90 processed by SPAG 8.01RF 22:32 12 Dec 2024
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
SUBROUTINE number_(X,Y,Hght,Z,T,F0,Ipf)
   IMPLICIT NONE
!
!     WRITTEN BY D. LONG    AUG, 1983 AT JPL
!     REVISED: JUNE 1990
!
!     PLOTS THE FLOATING POINT NUMBER Z (CAN PLOT AS AN INTEGER)
!
!     X,Y   (R) COORDINATES OF STRING
!            (999.,999.) CONTINUE FROM LAST POINT
!     HGHT  (R) HEIGHT OF THE PLOTTED NUMBER
!     Z  (R) FLOATING POINT NUMBER TO BE PLOTTED
!     T  (R) ORIENTATION ANGLE
!     F0 (R) PLOTTING FORMAT (Fn.j)
!          n = TOTAL NUMBER OF SPACES TO USE (INCLUDING SIGN AND D.P.)
!           [MAX 18 CHARACTERS WIDE]
!          j = DIGITS TO RIGHT OF DECIMAL POINT (TWO DIGITS EXPECTED)
!           (F4.2 SHOULD BE WRITTEN F4.02)
!          IF F0 < 0 USE EXPONENTIAL NOTATION (I.E., En,j)
!          F0 = 1 PLOT IN FLOATING POINT FREE FORMAT
!          F0 = 0 PLOT INTEGER PORTION WITH NO D.P. (FREE FORMAT)
!          F0 =-1 PLOT IN EXPONENTIAL FREE FORMAT
!            NOTE: FREE FORMATS HAVE LEADING SPACES SUPPRESSED
!          F0 > 1000 PLOT INTEGER PORTION IN FIXED FORMAT WITH
!           n DIGITS AND WITHOUT D.P.
!          IF n=0 THEN PLOT INTEGER PORTION, DECIMAL POINT, AND
!            j DIGITS TO RIGHT OF DECIMAL POINT
!          WHEN Z OVERFLOWS THIS FORMAT, SPACE IS FILLED WITH ASTERISKS
!     IPF   (I) NUMBER CENTERING FLAG (SEE SYMBOL)
!        =-3 X,Y ARE LOWER LEFT CORNER, END OF STRING RETURNED IN X,Y
!           BUT NUMBER IS NOT PLOTTED
!        =-2 X,Y ARE LOWER LEFT CORNER, END OF STRING RETURNED IN X,Y
!        =-1 X,Y ARE LOWER LEFT CORNER
!        = 0 X,Y ARE STRING CENTER
!        =+1 X,Y ARE LOWER RIGHT CORNER
!        =+2 NO PLOT OUTPUT
!
   REAL :: alg
   REAL :: f
   REAL :: F0
   REAL :: fa
   REAL :: hg
   REAL :: Hght
   INTEGER :: i
   INTEGER :: iff
   INTEGER :: Ipf
   INTEGER :: nd
   INTEGER :: nn
   REAL :: T
   REAL :: t1
   REAL :: X
   REAL :: Y
   REAL :: Z
   CHARACTER(LEN=18) :: b
                         ! WORKING BUFFERS
   CHARACTER(LEN=8) :: fb
                         ! WORKING BUFFERS
   CHARACTER(LEN=7) :: fb1
                         ! WORKING BUFFERS
!
   iff = 0
   hg = Hght
   IF ( hg==0.0 ) hg = 0.15
   t1 = T
   nd = 0
   nn = 0
   fa = F0
   IF ( abs(fa)>1022.0 ) fa = 0.0
   IF ( fa/=0.0 ) THEN     ! INTEGER FORMAT
      IF ( fa>999.0 ) THEN
                          ! PLOT FORMATTED INTEGER
         nn = amod(fa,1000.)
         fa = 0.0
      ELSE        ! PLOT FLOAT OR EXPON NUMBER
         f = abs(fa)*1.000002
         nn = f
         f = (f-nn)*100.0
         nd = f
      ENDIF
   ENDIF
   IF ( nd>17 ) nd = nd/10 ! CORRECT SIMPLE INPUT ERRORS
   IF ( nn==0 ) THEN  ! DIGITS TO LEFT OF DECIMAL POINT
      nn = nd + 2
      IF ( Z==0 .AND. fa==0.0 ) nn = 1
      IF ( Z/=0.0 ) THEN
         alg = alog10(abs(Z))
         IF ( alg<0.0 ) alg = 0.0
         nn = nd + 2 + alg
         IF ( fa==0.0 ) nn = 1 + alg
      ENDIF
      IF ( Z<0.0 ) nn = nn + 1
      IF ( fa<0.0 ) nn = nn + 4
   ENDIF
   IF ( nd<=nn ) THEN     ! FORMAT ERROR
      IF ( nn>18 ) nn = 18
                        ! MAX CHARACTERS
      IF ( fa==0.0 ) THEN
                        ! INTEGER
         i = Z
         fb = char(nn-10*(nn/10)+48)//')'
         fb1 = fb(1:1)
         IF ( (nn/10)>0 ) fb = char(nn/10+48)//fb1(1:1)
         fb1 = '(I'//fb(1:2)
         WRITE (b,fb1,ERR=100) i
      ELSE        ! FLOATING POINT OR EXPONENTIAL
         IF ( nn>1 ) THEN
            fb = char(nd-10*(nd/10)+48)//')'
            fb1 = fb(1:1)
            IF ( (nd/10)>0 ) fb = char(nd/10+48)//fb1(1:1)
            fb1 = char(nn-10*(nn/10)+48)//'.'//fb(1:2)
            fb = fb1(1:4)
            IF ( nn/10>0 ) fb = char(nn/10+48)//fb1(1:4)
            IF ( fa>0.0 ) THEN
               fb1 = '(F'//fb(1:5)
            ELSE
               fb1 = '(E'//fb(1:5)
            ENDIF
         ELSE
            IF ( fa>0.0 ) THEN
               fb1 = '(F)'
            ELSE
               fb1 = '(E)'
            ENDIF
            nn = 16
            iff = 1
         ENDIF
         WRITE (b,fb1,ERR=100) Z
         IF ( iff==1 ) THEN
                          ! REMOVE LEADING SPACES
            DO i = 1 , 18
               IF ( b(1:1)==' ' ) b = b(2:18)
            ENDDO
         ENDIF
      ENDIF
      CALL symbol_(X,Y,hg,b,t1,nn,Ipf)
      RETURN
   ENDIF
 100  DO i = 1 , 18
      b(i:i) = '*'
      IF ( i==nn-nd ) b(i:i) = '.'
   ENDDO
END SUBROUTINE number_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine viewport_(xmin,xmax,ymin,ymax)
real,intent(in) :: xmin, xmax, ymin, ymax
   call plot_(xmin,ymin,-4)
   call plot_(xmax,ymax,4)
end subroutine viewport_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine width_(ic)
integer,intent(in) :: ic
   call plot_(real(ic),0.0,7)
end subroutine width_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine dashscale_(ic)
integer,intent(in) :: ic
   call plot_(real(ic),0.0,1)
end subroutine dashscale_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine dashcode_(ic)
integer,intent(in) :: ic
   ! make so if call with 0 increment using ilineq?
   call plot_(real(ic),0.0,8)
end subroutine dashcode_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end module m_graph
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
