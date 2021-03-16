!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
module m_graph
use M_verify, only : debug
implicit none
private
public   graph_init
public   graph

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
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    graph(3) - [M_graph] Draw an XY graph
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
!!##EXAMPLE
!!
!!   Sample program that creates some simple curves and then
!!   prompts for index values and values in the options array
!!   to allow for testing various options.
!!
!!    program demo_graph
!!    use m_graph, only : graph, graph_init
!!    use M_draw
!!
!!    integer,parameter            :: numlines=3
!!    integer,parameter            :: numpts=25
!!    integer,parameter            :: nf=255
!!    real                         :: x(numpts),y(numpts,numlines)
!!    character(len=80)            :: c(numlines+3)
!!    integer                      :: nc(numlines+3)
!!    real                         :: f(nf)
!!    character(len=20)            :: device
!!    integer                      :: ixsize
!!    integer                      :: iysize
!!    character(len=:),allocatable :: filename
!!    integer                      :: w
!!       device='x11'
!!       ixsize=1200*.75*0.5
!!       iysize=900*.75*0.5
!!       w=40
!!       if(device.eq.'x11')then
!!          call prefposition(0,0)
!!          call prefsize(ixsize,iysize)
!!       else
!!          call prefsize(ixsize,iysize)
!!          !!call voutput(str(filename,'_',int(a),'x',int(b),'.',device,nospace=.true.))
!!       endif
!!       call vinit(device)
!!       call vsetflush(.false.)
!!       call vflush()
!!       call linewidth(w)
!!
!!    !     fill some arrays with data we can plot
!!       DO i20=1,25
!!          X(i20)=i20
!!          Y(i20,1)=i20**2+5.0
!!          Y(i20,2)=i20*20.0
!!          Y(i20,3)=(-3.0)*(i20/4.0)**3
!!       enddo
!!
!!       f=0.0 !     zero out option array
!!    !     set up color and linetype in option array
!!       do i70=55,nf,2
!!          f(i70)=mod(i70,7)
!!          f(i70-1)=mod(i70,7)
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
!!       do i60=1,6
!!          nc(i60)=len_trim(c(i60))
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
!!          read(*,*,end=999,err=888)index,value
!!          if(index.lt.1)then
!!             exit INFINITE
!!          elseif(index.gt.nf)then
!!             goto 888
!!          else
!!             f(index)=value
!!             write(*,*)'index ',index,' now ',value
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

! ident_1="@(#)M_graph::graph(3f): routine for plotting lines"

!
!     X  (R) X INPUT ARRAY DIMENSIONED X(NDP,NDL)
!            NOTE: DEPENDING ON OPTIONS SELECTED, X
!            ACTUALLY MAY BE DIMENSIONED X(NDP)
!     Y  (R) Y INPUT ARRAY DIMENSIONED Y(NDP,NDL)
!     NDP   (I) NUMBER OF POINTS PER LINE DIMENSION
!     NDL   (I) NUMBER OF LINES DIMENSION
!     F  (R) OPTIONS ARRAY (DESCRIBED BELOW)
!     C  (C) TITLES ARRAY
!             C(1) X AXIS TITLE
!             C(2) Y AXIS TITLE
!             C(3) CHART TITLE
!             C(4) LEGEND TEXT FOR LINE 1 (IF LEGEND ENABLED)
!             C(5) LEGEND TEXT FOR LINE 2 (IF LEGEND ENABLED)
!             ...   ...
!     NC (I) NUMBER OF CHARACTERS IN TITLE ARRAY
!            NC(1) NUMBER OF CHARACTERS IN C(1)
!            NC(2) NUMBER OF CHARACTERS IN C(2)
!            ...    ...
!            ETC.  NOTE: IF NC(I) IS ZERO C(I) IS NOT PLOTTED
!
!     THE ELEMENTS OF F ARE INTERPRETED ACCORDING TO THE FOLLOWING:
!     DEFAULT VALUES ARE SHOWN IN SQUARE BRACKETS
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
!     32 0/1      Y AXIS NUMBERS ORIENTATION: 0=VERTICAL; 0=HORIZONTAL
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
   NP=NDP            ! NUMBER OF POINTS/LINE
   NL=NDL            ! NUMBER OF LINES
   XM=X(1,1)         ! MIN X VALUE
   XX=XM             ! MAX X VALUE
   YM=Y(1,1)         ! MIN Y VALUE
   YX=YM             ! MAX Y VALUE
   FX=5.01           ! X AXIS NUMBER FORMAT
   FY=5.01           ! Y AXIS NUMBER FORMAT
   XLEN=7.0          ! X AXIS LENGTH
   YLEN=5.0          ! Y AXIS LENGTH
   XTICK=7.0         ! X AXIS TICK PATTERN
   YTICK=6.0         ! Y AXIS TICK PATTERN
   XCS=0.15          ! X AXIS TITLE SIZE
   YCS=0.15          ! Y AXIS TITLE SIZE
   TCS=0.18          ! OVERALL TITLE SIZE
   EBAR=0.1          ! ERROR BAR SIZE
   SYMSIZ=0.1        ! SYMBOL SIZE
   CSLEG=0.12        ! LEGEND CHARACTER SIZE
   GLEN=0.5          ! LEGEND LINE LENGTH
   XLEG=XLEN+0.3     ! DEFAULT LEGEND LOCATION
   YLEG=0.5
!
!     READ OPTIONS ARRAY
!
   IF (F(1).GT.0.0) NP=F(1) ! NUMBER OF POINT/LINE
   IF (NP.GT.NDP) NP=NDP
   IF (F(2).GT.0.0) NL=F(2) ! NUMBER OF LINES
   IF (NL.GT.NDL) NL=NDL
   JXAUTO=F(3)         ! AUTO SCALE X, 0=YES/SMOOTH,-1=NO/USER
!                   1=YES/NO SMOOTH
   IF (JXAUTO.LT.0) THEN  ! INPUT X AXIS SCALE FACTORS
      XM=F(4)          ! MIN X (OTHERWISE RETURNED)
      XX=F(5)          ! MAX X
   ENDIF
   JYAUTO=F(6)         ! AUTO SCALE Y, 0=YES/SMOOTH,-1=NO/USER
!                   1=YES/NO SMOOTH
   IF (JYAUTO.LT.0) THEN  ! INPUT Y AXIS SCALE FACTORS
      YM=F(7)          ! MIN Y (OTHERWISE RETURNED)
      YX=F(8)          ! MAX Y
   ENDIF
   JXUSE=ABS(F(9))     ! USE X LINES WITH Y LINES,0=NO,1=YES
   JCONN=ABS(F(10))    ! CONNECT POINTS WITH LINES,0=YES,1=NO
   JSYM=ABS(F(11))     ! PLOT SYMBOL EVERY JSYM PT,0=NO SYMS
   IF (F(12).GT.0.0) SYMSIZ=F(12) ! SYMBOL SIZE
   JSYMST=ABS(F(13))   ! STARTING PLOT SYMBOL NUMBER
   JEBAR=F(14)         ! TYPE OF ERROR BAR,0=NOT USED
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
   KEBAR=IABS(JEBAR)
   NBAR=1
   IF (JEBAR.NE.0) NBAR=3
   IF (F(16).GT.0.0) EBAR=F(16) ! ERROR BAR SIZE
!                   1ST LINE DEFINES CENTER
!                   2ND IS LOWER REL. ERROR
!                   3RD IS UPPER REL. ERROR
   JVLINE=F(17)       ! VERTICAL LINE TO ZVAL,0=NO,1=YES
   ZVAL=F(18)        ! ZERO REFERENCE VALUE
   JXLGAX=F(19)       ! X AXIS, LINEAR=0/LOG=1/NONE=-1
   JYLGAX=F(20)       ! Y AXIS, LINEAR=0/LOG=1/NONE=-1
   IF (F(21).GT.0.0) XLEN=F(21) ! X AXIS LENGTH
   IF (F(22).GT.0.0) YLEN=F(22) ! Y AXIS LENGTH
   IF (F(23).GT.0.0) XTICK=F(23) ! X AXIS TICK PATTERN
   IF (F(24).GT.0.0) YTICK=F(24) ! Y AXIS TICK PATTERN
   JXTITLE=ABS(F(25))  ! X AXIS TITLE SIDE,0=CW,1=CCW
   IF (JXTITLE.EQ.0) THEN
      JXTITLE=-1
   ELSE
      JXTITLE=1
   ENDIF
   JYTITLE=ABS(F(26))  ! Y AXIS TITLE SIDE,0=CCW,1=CW
   IF (JYTITLE.EQ.0) THEN
      JYTITLE=1
   ELSE
      JYTITLE=-1
   ENDIF
   JXEXPSC=ABS(F(27))  ! X AXIS ENABLE AUTO EXPONENT,0=YES,1=NO
   JYEXPSC=ABS(F(28))  ! Y AXIS ENABLE AUTO EXPONENT,0=YES,1=NO
   JXTICKS=ABS(F(29))  ! X AXIS TICKS TITLE SIDE,0=YES,1=NO
   JYTICKS=ABS(F(30))  ! Y AXIS TICKS TITLE SIDE,0=YES,1=NO
   JXNUMS=ABS(F(31))  ! X AXIS NUMS PARA AXIS, 0=YES,1=NO
   JYNUMS=ABS(F(32))  ! Y AXIS NUMS PARA AXIS, 0=YES,1=NO
   JXNUM=ABS(F(33))   ! X AXIS NUMS SHOWN, 0=YES,1=NO
   JYNUM=ABS(F(34))   ! Y AXIS NUMS SHOWN, 0=YES,1=NO
   JXLOG=ABS(F(35))   ! TAKE LOG OF X VALUES, 0=NO,1=YES
   JYLOG=ABS(F(36))   ! TAKE LOG OF Y VALUES, 0=NO,1=YES
   JXRAX=F(37)    ! ADD AXIS OPPOSITE, 0=YES,-1=W/L,1=NO LABELS
   JYRAX=F(38)    ! ADD AXIS OPPOSITE, 0=YES,-1=W/L,1=NO LABELS
   IF (F(39).GT.0.0) XCS=F(39) ! X AXIS TITLE SIZE
   IF (F(40).GT.0.0) YCS=F(40) ! Y AXIS TITLE SIZE
   IF (F(41).GT.0.0) TCS=F(41) ! OVERALL TITLE SIZE
   JGRID=F(42)       ! GRID OPTION, 0=NONE,1=SOLID,2=DOTTED
   JLEGND=F(43)       ! ADD LEGEND, 0=NONE,1=AUTO,-1=USER
   XLEG=XLEN+0.3      ! DEFAULT LEGEND LOCATION
   YLEG=0.5
   IF (JLEGND.LT.0) XLEG=F(44) ! X LEGEND LOCATION
   IF (JLEGND.LT.0) YLEG=F(45) ! Y LEGEND LOCATION
   JLEGSY=F(46)       ! USE SYMBOL ON LEGEND,0=NO,1=YES
   JLEGLI=F(47)       ! USE LINETYPE ON LEGEND,0=NO,1=YES
   IF (F(48).GT.0.0) CSLEG=F(48) ! LEGEND CHARACTER SIZE
   IF (F(49).GT.0.0) GLEN=F(49) ! LEGEND LINE LENGTH
   JTCNTR=F(50)       ! CENTER TITLE,0=YES,-1=LEFT,1=RIGHT
   JSZREF=F(51)       ! SHOW ZERO REFERENCE,0=NO,1=YES
!
   JLINE=F(52)       ! USE LINE TYPES,0=NO,1=YES
   JCOL=F(53)        ! USE COLORS,0=NO,1=YES
!
!     F(54)=FIRST COLOR
!     F(55)=FIRST LINETYPE
!     F(56)=SECOND COLOR
!     F(57)=SECOND LINETYPE
!     ...
!
   ICOFF=54
   ILOFF=55
!
!     COMPUTE SCALING
!
   IF (JXAUTO.GE.0) THEN  ! AUTO SCALE X
      X1=ABS(XM)
      X2=X1
      DO 5 I=1,NP
         K=1
         IF (JXUSE.NE.0) K=NL
         DO 5 J=1,K,NBAR
            XM=MIN(XM,X(I,J))
            X1=MIN(X1,ABS(X(I,J)))
            XX=MAX(XX,X(I,J))
            X2=MAX(XX,ABS(X(I,J)))
            IF (JEBAR.NE.0.AND.JXUSE.NE.0) THEN
               XM=MIN(XM,X(I,J)+X(I+1,J))
               X1=MIN(X1,ABS(X(I,J)+X(I+1,J)))
               XX=MAX(XX,X(I,J)+X(I+1,J))
               X2=MAX(X2,ABS(X(I,J)+X(I+1,J)))
               XM=MIN(XM,X(I,J)+X(I+2,J))
               X1=MIN(X1,ABS(X(I,J)+X(I+1,J)))
               XX=MAX(XX,X(I,J)+X(I+2,J))
               X2=MAX(X2,ABS(X(I,J)+X(I+1,J)))
            ENDIF
5     CONTINUE
      IF (JXLOG.NE.0) THEN
         XM=ALOG10(X1+1.E-34)
         XX=ALOG10(X2+1.E-34)
      ENDIF
      IF (JXAUTO.GT.0.AND.JXLGAX.EQ.0) THEN ! SMOOTH SCALING
         SMO(1)=XM
         SMO(2)=XX
         CALL range_(SMO,XLEN,2,1,1,XM,DX)
         XX=XM+XLEN*DX
      ENDIF
      IF (JXLGAX.NE.0) THEN   ! LOG AXIS SCALING
         IF (XM.LE.0.AND.XM.NE.INT(XM)) XM=XM-1.0
         XM=INT(XM)
         IF (XX.GT.0.AND.XX.NE.INT(XX)) XX=XX+1.0
         XX=INT(XX)
      ENDIF
      IF (XX.EQ.XM) XX=XM+1.0
!     RETURN SCALE FACTORS USED
      F(4)=XM
      F(5)=XX
   ENDIF
   IF (JYAUTO.GE.0) THEN  ! AUTO SCALE Y
      Y1=ABS(YM)
      Y2=Y1
      DO 6 I=1,NP
         DO 6 J=1,NL,NBAR
            YM=MIN(YM,Y(I,J))
            Y1=MIN(Y1,ABS(Y(I,J)))
            YX=MAX(YX,Y(I,J))
            Y2=MAX(YX,ABS(Y(I,J)))
            IF (JEBAR.NE.0) THEN
               YM=MIN(YM,Y(I,J)+Y(I+1,J))
               Y1=MIN(Y1,ABS(Y(I,J)+Y(I+1,J)))
               YX=MAX(YX,Y(I,J)+Y(I+1,J))
               Y2=MAX(Y2,ABS(Y(I,J)+Y(I+1,J)))
               YM=MIN(YM,Y(I,J)+Y(I+2,J))
               Y1=MIN(Y1,ABS(Y(I,J)+Y(I+1,J)))
               YX=MAX(YX,Y(I,J)+Y(I+2,J))
               Y2=MAX(Y2,ABS(Y(I,J)+Y(I+1,J)))
            ENDIF
6     CONTINUE
      IF (JYLOG.NE.0) THEN
         YM=ALOG10(Y1+1.E-34)
         YX=ALOG10(Y2+1.E-34)
      ENDIF
      IF (JYAUTO.GT.0.AND.JYLGAX.EQ.0) THEN ! SMOOTH SCALING
         SMO(1)=YM
         SMO(2)=YX
         CALL range_(SMO,YLEN,2,1,1,YM,DY)
         YX=YM+YLEN*DY
      ENDIF
      IF (JYLGAX.NE.0) THEN   ! LOG AXIS SCALING
         IF (YM.LE.0.AND.YM.NE.INT(YM)) YM=YM-1.0
         YM=INT(YM)
         IF (YX.GT.0.AND.YX.NE.INT(YX)) YX=YX+1.0
         YX=INT(YX)
      ENDIF
      IF (YX.EQ.YM) YX=YM+1.0
!     RETURN SCALE FACTORS USED
      F(7)=YM
      F(8)=YX
   ENDIF
   DX=(XX-XM)/XLEN
   DY=(YX-YM)/YLEN
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   if(debug)write(*,*)'PRODUCE PLOT'
!
!     PRODUCE PLOT X AXIS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   IF (JCOL.NE.0) THEN  ! INITIALIZE COLOR AXIS ARRAY
      DO 10 I=1,4
         IC(I)=ABS(F(2*I+ICOFF-2))
         IF (IC(I).EQ.0) IC(I)=1
10    CONTINUE
   ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   IF (JLINE.NE.0) CALL newpen_(INT(F(ILOFF)))
!
   IF (JXEXPSC.NE.0) XCS=-XCS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   IF (JXLGAX.EQ.0) THEN  ! LINEAR X AXIS
      X1=XLEN
      IF (JXTICKS.NE.0) X1=-XLEN
      NA=1000
      IF (JXNUM.NE.0) NA=NA+100
      IF (JCOL.NE.0) NA=NA+100000
      IF (JXNUMS.EQ.0) NA=NA-1000
      IF (NC(1).GT.0) THEN ! USE INPUT X AXIS TITLE
         NA=(NA+NC(1))*JXTITLE
         if(debug)write(*,*)'1) axisc_'
         CALL axisc_(0.,0.,C(1),NA,X1, 0.0,XM,XX,XTICK,XCS,FX,IC)
      ELSE        ! NO X AXIS TITLE
         NA=(NA+1)*JXTITLE
         if(debug)write(*,*)'2) axisc_'
         CALL axisc_(0.,0.,' ',NA,X1, 0.0,XM,XX,XTICK,XCS,FX,IC)
      ENDIF
      IF (JXRAX.NE.0) THEN
         NA=1000
         IF (JXRAX.GT.0) NA=NA+100
         IF (JCOL.NE.0) NA=NA+100000
         IF (JXNUMS.EQ.0) NA=NA-1000
         IF (NC(1).GT.0) THEN! USE INPUT X AXIS TITLE
            NA=(-(NA+NC(1)))*JXTITLE
            if(debug)write(*,*)'3) axisc_'
            CALL axisc_(0.,YLEN,C(1),NA,X1, 0.0,XM,XX,XTICK,XCS,FX,IC)
         ELSE        ! NO X AXIS TITLE
            NA=(NA+1)*JXTITLE
            if(debug)write(*,*)'4) axisc_'
            CALL axisc_(0.,YLEN,' ',NA,X1, 0.0,XM,XX,XTICK,XCS,FX,IC)
         ENDIF
      ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   ELSE IF (JXLGAX.EQ.1) THEN ! LOG X AXIS
      X1=XLEN
      IF (JXTICKS.NE.0) X1=-XLEN
      NA=1000
      IF (JXNUM.NE.0) NA=NA+100
      IF (JCOL.NE.0) NA=NA+10000
      IF (JXNUMS.EQ.0) NA=NA-1000
      IF (NC(1).GT.0) THEN ! USE INPUT X AXIS TITLE
         NA=(NA+NC(1))*JXTITLE
         if(debug)write(*,*)'5) axislg_'
         CALL axislg_(0.,0.,C(1),NA,X1, 0.0,XM,DX,IC)
      ELSE        ! NO X AXIS TITLE
         NA=(NA+1)*JXTITLE
         if(debug)write(*,*)'6) axislg_'
         CALL axislg_(0.,0.,' ',NA,X1, 0.0,XM,DX,IC)
      ENDIF
      IF (JXRAX.NE.0) THEN
         NA=1000
         IF (JXRAX.GT.0) NA=NA+100
         IF (JCOL.NE.0) NA=NA+10000
         IF (JXNUMS.EQ.0) NA=NA-1000
         IF (NC(1).GT.0) THEN! USE INPUT X AXIS TITLE
            NA=(-(NA+NC(1)))*JXTITLE
            if(debug)write(*,*)'7) axislg_'
            CALL axislg_(0.,YLEN,C(1),NA,X1, 0.0,XM,DX,IC)
         ELSE    ! NO X AXIS TITLE
            NA=(-(NA+1))*JXTITLE
            if(debug)write(*,*)'8) axislg_'
            CALL axislg_(0.,YLEN,' ',NA,X1, 0.0,XM,DX,IC)
         ENDIF
      ENDIF
   ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PRODUCE PLOT Y AXIS
!
   IF (JCOL.NE.0) THEN  ! INITIALIZE COLOR AXIS ARRAY
      DO 20 I=1,4
         IC(I)=ABS(F(2*I+ICOFF+6))
         IF (IC(I).EQ.0) IC(I)=1
20    CONTINUE
   ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PRODUCE PLOT Y AXIS
   IF (JLINE.NE.0) CALL newpen_(INT(F(ILOFF+2)))
!
   IF (JYEXPSC.NE.0) YCS=-YCS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PRODUCE PLOT Y AXIS
   IF (JYLGAX.EQ.0) THEN  ! LINEAR Y AXIS
      X1=YLEN
      IF (JYTICKS.NE.0) X1=-YLEN
      NA=1000
      IF (JYNUM.NE.0) NA=NA+100
      IF (JCOL.NE.0) NA=NA+100000
      IF (JYNUMS.EQ.0) NA=NA-1000
      IF (NC(2).GT.0) THEN ! USE INPUT Y AXIS TITLE
         NA=(NA+NC(2))*JYTITLE
         if(debug)write(*,*)'9) axisc_'
         CALL axisc_(0.,0.,C(2),NA,X1, 90.0,YM,YX,YTICK,YCS,FY,IC)
      ELSE        ! NO Y AXIS TITLE
         NA=(NA+1)*JYTITLE
         if(debug)write(*,*)'10) axisc_'
         CALL axisc_(0.,0.,' ',NA,X1, 90.0,YM,YX,YTICK,YCS,FY,IC)
      ENDIF
      IF (JYRAX.NE.0) THEN
         NA=1000
         IF (JYRAX.GT.0) NA=NA+100
         IF (JCOL.NE.0) NA=NA+100000
         IF (JYNUMS.EQ.0) NA=NA-1000
         IF (NC(2).GT.0) THEN! USE INPUT Y AXIS TITLE
            NA=(-(NA+NC(2)))*JYTITLE
            if(debug)write(*,*)'11) axisc_'
            CALL axisc_(XLEN,0.,C(2),NA,X1, 90.0,YM,YX,YTICK,YCS,FY,IC)
         ELSE        ! NO Y AXIS TITLE
            NA=(NA+1)*JYTITLE
            if(debug)write(*,*)'12) axisc_'
            CALL axisc_(XLEN,0.,' ',NA,X1, 90.0,YM,YX,YTICK,YCS,FY,IC)
         ENDIF
      ENDIF
   ELSE IF (JYLGAX.EQ.1) THEN ! LOG Y AXIS
      X1=YLEN
      IF (JYTICKS.NE.0) X1=-YLEN
      NA=1000
      IF (JYNUM.NE.0) NA=NA+100
      IF (JCOL.NE.0) NA=NA+10000
      IF (JYNUMS.EQ.0) NA=NA-1000
      IF (NC(2).GT.0) THEN ! USE INPUT Y AXIS TITLE
         NA=(NA+NC(2))*JYTITLE
         if(debug)write(*,*)'13) axislg_'
         CALL axislg_(0.,0.,C(2),NA,X1, 90.0,XY,DY,IC)
      ELSE        ! NO X AXIS TITLE
         NA=(NA+1)*JYTITLE
         if(debug)write(*,*)'14) axislg_'
         CALL axislg_(0.,0.,' ',NA,X1, 90.0,YM,DY,IC)
      ENDIF
      IF (JYRAX.NE.0) THEN
         NA=1000
         IF (JYRAX.GT.0) NA=NA+100
         IF (JCOL.NE.0) NA=NA+10000
         IF (JYNUMS.EQ.0) NA=NA-1000
         IF (NC(2).GT.0) THEN! USE INPUT Y AXIS TITLE
            NA=(-(NA+NC(2)))*JYTITLE
            if(debug)write(*,*)'15) axislg_'
            CALL axislg_(XLEN,0.,C(2),NA,X1, 90.0,YM,DY,IC)
         ELSE    ! NO Y AXIS TITLE
            NA=(-(NA+1))*JYTITLE
            if(debug)write(*,*)'16) axislg_'
            CALL axislg_(XLEN,0.,' ',NA,X1, 90.0,YM,DY,IC)
         ENDIF
      ENDIF
   ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PLOT GRID
!
   IF (JGRID.NE.0) THEN
      IF (JCOL.NE.0) THEN
         ICC=INT(ABS(F(ICOFF+22)))
         IF (ICC.LT.1) ICC=1
         CALL color_(ICC)
      ENDIF
      NG=JGRID-1
      I=INT(XTICK)-1
      IF (I.LT.1) I=1
      X1=XLEN/FLOAT(I)
      IF (JXLGAX.GT.0) THEN
         I=1-INT(XLEN*DX-XM+0.001)
         X1=(-XLEN)/FLOAT(I)
      ENDIF
      J=INT(YTICK)-1
      IF (J.LT.1) J=1
      Y1=YLEN/FLOAT(J)
      IF (JYLGAX.GT.0) THEN
         J=1-INT(YLEN*DY-YM+0.001)
         Y1=(-YLEN)/FLOAT(J)
      ENDIF
      if(debug)write(*,*)'17) gridll_'
      CALL gridll_(0.,0.,X1,Y1,I,J,NG)
   ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     ADD TITLE
!
   IF (NC(3).GT.0) THEN
      IF (JLINE.NE.0) CALL newpen_(INT(F(ILOFF+4)))
      IF (JCOL.NE.0) THEN
         ICC=INT(ABS(F(ICOFF+16)))
         IF (ICC.LT.1) ICC=1
         CALL color_(ICC)
      ENDIF
      ICC=0
      X1=0.5*XLEN
      Y1=YLEN+0.1+TCS*0.2
      IF (JTCNTR.LT.0) THEN
         ICC=-1
         X1=0.0
         Y1=Y1-TCS*0.2
      ENDIF
      IF (JTCNTR.GT.0) THEN
         ICC=1
         X1=XLEN
         Y1=Y1-TCS*0.2
      ENDIF
      IF (JXRAX.LT.0) Y1=Y1+2.5*XCS
      if(debug)write(*,*)'18) TITLE'
      call symbol_(X1,Y1,TCS,C(3), 0.0,NC(3),ICC)
   ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PLOT DATA IN DESIRED FORM
!
   DX=1.0
   DY=1.0
   IF (XX-XM.NE.0.0) DX=XLEN/(XX-XM)
   IF (YX-YM.NE.0.0) DY=YLEN/(YX-YM)
   ZREF=(ZREF-YM)*DY
   IF (JSZREF.NE.0) THEN ! SHOW ZERO REFERENCE LINE
      IF (JLINE.NE.0) CALL newpen_(INT(F(ILOFF+8)))
      IF (JCOL.NE.0) THEN
         ICC=INT(ABS(F(ICOFF+20)))
         IF (ICC.LT.1) ICC=1
         CALL color_(ICC)
      ENDIF
      if(debug)write(*,*)'19) ZERO REFERENCE LINE'
      CALL move_(0.0,ZREF)
      CALL draw_(XLEN,ZREF)
   ENDIF
!
   DO 100 I=1,NL,NBAR
      IF (JLINE.NE.0) CALL newpen_(INT(F(2*(I-1)+ILOFF+12)))
      IF (JCOL.NE.0) THEN
         ICC=INT(ABS(F(ICOFF+24+2*(I-1))))
         IF (ICC.LT.1) ICC=1
         CALL color_(ICC)
      ENDIF
      KSYM=MOD(I+JSYMST-1,17)
!
!     IF WE NEED CONNECTING LINES MAKE A CONNECT PASS
!
      IPEN=3
      IF (JCONN.EQ.0) THEN
         DO 50 J=1,NP
            IF (JXUSE.NE.0) THEN
               X1=X(J,I)
            ELSE
               X1=X(J,1)
            ENDIF
            IF (JXLOG.NE.0) X1=ALOG10(ABS(X1)+1.E-34)
            X1=(X1-XM)*DX
            Y1=Y(J,I)
            IF (JYLOG.NE.0) Y1=ALOG10(ABS(Y1)+1.E-34)
            Y1=(Y1-YM)*DY
            CALL plot_(X1,Y1,IPEN)
            IPEN=2
50       CONTINUE
      ENDIF
!
!     ANOTHER PASS FOR SYMBOLS, ETC.
!
      IF (JCONN.NE.0.AND.JEBAR.EQ.0.AND.JVLINE.EQ.0 .AND.JSYM.EQ.0) GOTO 100
      ISYM=0
      DO 60 J=1,NP
         IF (JXUSE.NE.0) THEN
            X1=X(J,I)
         ELSE
            X1=X(J,1)
         ENDIF
         IF (JXLOG.NE.0) X1=ALOG10(ABS(X1)+1.E-34)
         X1=(X1-XM)*DX
         Y1=Y(J,I)
         IF (JYLOG.NE.0) Y1=ALOG10(ABS(Y1)+1.E-34)
         Y1=(Y1-YM)*DY
         IF (JSYM.NE.0) THEN  ! SYMBOLS
            IF (MOD(ISYM,JSYM).EQ.0) CALL symbol_(X1,Y1,SYMSIZ,CHAR(KSYM),0.,-1,-1)
         ENDIF
         IF (JVLINE.NE.0) THEN  ! VERTICAL LINE
            CALL move_(X1,Y1)
            CALL draw_(X1,ZREF)
         ENDIF
         IF (JEBAR.NE.0) THEN  ! ERROR BAR
            IF (JLINE.NE.0) CALL newpen_(INT(F(ILOFF+10)))
            IF (JXUSE.NE.0) THEN
               X2=X(J,I+1)+X(J,I)
               X3=X(J,I+2)+X(J,I)
               IF (JXLOG.NE.0) THEN
                  X2=ALOG10(ABS(X2)+1.E-34)
                  X3=ALOG10(ABS(X3)+1.E-34)
               ENDIF
               X2=(X2-XM)*DX
               X3=(X3-XM)*DX
            ELSE
               X2=X1
               X3=X1
            ENDIF
            Y2=Y(J,I+1)+Y(J,I)
            Y3=Y(J,I+2)+Y(J,I)
            IF (JYLOG.NE.0) THEN
               Y2=ALOG10(ABS(Y2)+1.E-34)
               Y3=ALOG10(ABS(Y3)+1.E-34)
            ENDIF
            Y2=(Y2-YM)*DY
            Y3=(Y3-YM)*DY
            IF (KEBAR.EQ.2.OR.KEBAR.EQ.5) THEN ! ERROR BAR TOP
               CALL move_(X2,Y2-EBAR)
               CALL draw_(X2,Y2+EBAR)
            ELSE IF (KEBAR.EQ.3.OR.KEBAR.EQ.4) THEN
               CALL move_(X2-EBAR,Y2)
               CALL draw_(X2+EBAR,Y2)
            ENDIF
            IF (KEBAR.LT.4) THEN ! CONNECT LINE
               CALL move_(X2,Y2)
               CALL draw_(X3,Y3)
            ELSE IF (KEBAR.EQ.4) THEN ! DOUBLE CONN V LINE
               CALL move_(X2-EBAR*0.5,Y2)
               CALL draw_(X3-EBAR*0.5,Y3)
               CALL move_(X3+EBAR*0.5,Y3)
               CALL draw_(X2+EBAR*0.5,Y2)
            ELSE IF (KEBAR.EQ.5) THEN ! DOUBLE CONN HLINE
               CALL move_(X2,Y2-EBAR*0.5)
               CALL draw_(X3,Y3-EBAR*0.5)
               CALL move_(X3,Y3+EBAR*0.5)
               CALL draw_(X2,Y2+EBAR*0.5)
            ELSE IF (KEBAR.EQ.6) THEN ! VERTICAL RECT
               CALL rect_(X1-EBAR,Y2,X1+EBAR,Y3)
            ELSE IF (KEBAR.EQ.7) THEN ! RECT
               CALL rect_(X2,Y2,X3,Y3)
            ENDIF
            IF (JEBAR.LT.0) THEN ! X MARK
               CALL move_(X1-EBAR,Y1-EBAR)
               CALL draw_(X1+EBAR,Y1+EBAR)
               CALL move_(X1-EBAR,Y1+EBAR)
               CALL draw_(X1+EBAR,Y1-EBAR)
            ENDIF
            IF (KEBAR.EQ.2.OR.KEBAR.EQ.5) THEN ! BAR BOTTOM
               CALL move_(X3,Y3-EBAR)
               CALL draw_(X3,Y3+EBAR)
            ELSE IF (KEBAR.EQ.3.OR.KEBAR.EQ.4) THEN
               CALL move_(X3-EBAR,Y3)
               CALL draw_(X3+EBAR,Y3)
            ENDIF
         ENDIF
         IF (JSYM.EQ.0.AND.JCONN.EQ.0.AND.  JEBAR.EQ.0.AND.JVLINE.EQ.0) THEN
            CALL move_(X1,Y1) ! DOTS ONLY
            CALL draw_(X1,Y1)
         ENDIF
         ISYM=ISYM+1
60    CONTINUE
100 CONTINUE
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     ADD LEGEND
!
   IF (JLEGND.NE.0) THEN
      if(debug)WRITE(*,*)'20) ADD LEGEND'
      DO 200 I=1,NL
         X1=XLEG+CSLEG*1.5
         Y1=YLEG+(I-1)*CSLEG*1.5+CSLEG*0.5
         IF (JCOL.NE.0) THEN
            ICC=INT(ABS(F(ICOFF+24+2*(I-1))))
            IF (ICC.LT.1) ICC=1
            CALL color_(ICC)
         ENDIF
         IF (JLEGLI.NE.0) THEN
            IF (JLINE.NE.0) CALL newpen_(INT(F(2*(I-1)+ILOFF+12)))
            CALL move_(X1,Y1)
            CALL draw_(X1+GLEN,Y1)
            X1=X1+GLEN*0.5
         ENDIF
         IF (JLEGSY.NE.0) THEN
            ICC=I-1+JSYMST
            call symbol_(X1,Y1,SYMSIZ, CHAR(ICC),0.,-1,-1)
         ENDIF
         IF (JLEGLI.NE.0) X1=X1+GLEN*0.5
         X1=X1+CSLEG*0.7
         Y1=Y1-CSLEG*0.5
         IF (NC(I+3).GT.0) THEN
            IF (JLINE.NE.0) CALL newpen_(INT(F(ILOFF+6)))
            IF (JCOL.NE.0) THEN
               ICC=INT(ABS(F(ICOFF+18)))
               IF (ICC.LT.1) ICC=1
               CALL color_(ICC)
            ENDIF
            call symbol_(X1,Y1,CSLEG, C(I+3),0.,NC(I+3),-1)
         ENDIF
200   CONTINUE
   ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     FINISH UP
   CALL move_(0.,0.)   !PEN UP
   RETURN
END SUBROUTINE graph
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

! ident_2="@(#)M_graph::graph_init(3f): initialize the longlib graphics plot package"

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
      ISELECT=ISELECT0             ! make mutatble copy of select option
!#######################################################################
!     DECODE COMMAND
!
      DO 30 I30=1,NSELECT
         IF (ISELECT.EQ.IDECODE(I30))THEN
!                  1,2,3, 0,-2,-3,5,10,11,999,4,-4,6,7,8,12,13
      if(debug)write(*,*)'*_plot_* ',xplot0,yplot0,iselect,i30,idecode(i30)
             GOTO (1,2,3,50,22,33,5,10,11,999,4, 4,6,7,8,12,13),I30
         ENDIF
30    CONTINUE
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
      do 110 I110=1,1000 ! flush key buffer
         IF(CHECKKEY().EQ.0)GOTO 111
110   CONTINUE
111   CONTINUE
      IVALUE=GETKEY()     ! wait till keypress is read in graphic window
      CALL VFLUSH()
      RETURN
!#######################################################################
13    CONTINUE  ! dl_pause2

      WRITE(*,*)'#PAUSING -- click "n" on plot'
      CALL VFLUSH()              ! flush graphics buffers
      WRITE(*,*)CHAR(7)          ! send bell character
      do 130 I130=1,1000 ! flush key buffer
         IF(CHECKKEY().EQ.0)GOTO 131
130   CONTINUE
131   CONTINUE
      IVALUE=GETKEY()     ! wait till keypress is read in graphic window
      CALL VFLUSH()
      if(debug)write(*,*)'vflush'
      RETURN
!#######################################################################
END SUBROUTINE plot_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE range_(X,S,N,K,IX,XMIN,DX)
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
integer :: i
integer :: idx
integer :: ix
integer :: k
integer :: n
integer :: np
real    :: dx
real    :: s
real    :: si
real    :: sj
real    :: xi
real    :: xmax
real    :: xmin
real    :: xmm
REAL X(*),Q(6)
DATA Q/1.0,2.0,4.0,5.0,8.0,10.0/
   NP=N*K
   XMAX=X(1)
   XMIN=XMAX
   DO I=IX,NP,K
      XI=X(I)
      XMAX=AMAX1(XMAX,XI)
      XMIN=AMIN1(XMIN,XI)
   enddo
   XMM=XMIN
   IF (S.LE.0.0) GOTO 160
   DX=(XMAX-XMIN)/S
   IF (DX.LE.0.0) GOTO 160
   SJ=0.0
   IF (DX.LT.1.0) SJ=-1.0
   IDX=ALOG10(DX)+SJ
   DX=DX/(10.0**IDX)
   DO I=1,6
      XI=Q(I)
      IF (XI.GE.DX) GOTO 120
   enddo
120 continue
   DX=XI*(10.0**IDX)
   SI=1.0
   SJ=0.0
   IF (XMIN) 130,170,140
130 continue
   SI=-1.0
   SJ=-0.99999
   XMIN=-XMIN
140 continue
   IDX=ALOG10(XMIN)+SJ
   XMIN=XMIN/(10.0**IDX)
   XMIN=XMIN-SJ
   XMIN=IFIX(XMIN)*SI*(10.0**IDX)
   GOTO 170
160 continue
   DX=1.0
   XMIN=XMIN-0.5
170 CONTINUE
!
!     BEFORE EXIT, CHECK TO BE SURE THAT DATA IS CONTAINED WITHIN
!     THE LIMITS XMIN AND XMIN+DX*S.  IF NOT, RESET DX
!
   IF (XMM.LT.XMIN) XMIN=XMM
   IF (XMAX.GT.XMIN+DX*S) THEN
      IF (S.GT.0.0) DX=(XMAX-XMIN)/S
      IF (DX.LE.0.0) DX=1.0
   ENDIF
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
SUBROUTINE symbol_(X,Y,S,T,A,NN,IS)
!     WRITTEN BY: D. LONG  JAN 1991,1995   BYU
!     THIS ROUTINE IS FORTRAN-77 COMPATIBLE WITH THE FOLLOWING
!     EXCEPTIONS:
!        1. INTEGER*2 ARRAYS ARE USED TO SAVE SPACE.  THEY MAY
!           BE REPLACED WITH INTEGER.
!
!     MACHINE DEPENDENT NOTES:
!        1. THE FUNCTION IBITS(I,J,K) RETURNS THE VALUE OF THE BITS
!           IN I STARTING AT J FOR K BITS.

! ident_3="@(#)M_graph::symbol_(3f): routine to plot characters and symbols"

real                :: X
real                :: Y
character(len=*)    :: T
integer             :: i
integer             :: icc
integer             :: il
integer             :: ip
integer             :: ipen
integer             :: ipenlast
integer             :: is
integer             :: iss
integer             :: iw
integer             :: ix
integer             :: ixoff
integer             :: iy
integer             :: iyoff
integer             :: n
integer             :: nn
real                :: a
real                :: aa
real                :: al
real                :: co
real                :: ox
real                :: oy
real                :: s
real                :: si
real                :: ss
real                :: x0
real                :: x1
real                :: xx
real                :: y0
real                :: y1
logical             :: LENGTH
real,save           :: OLDX,OLDY
!INTEGER, PARAMETER :: Short   = SELECTED_INT_KIND(4)   ! Short integer
INTEGER, PARAMETER  :: Short   = SELECTED_INT_KIND(8)   ! Long integer
!
INTEGER(kind=short) :: IFNT(  968),IPNT( 176)

! ----------------------------------------------------------------------------------------------------------------------------------
INTEGER(kind=short) :: IF001( 88),IF002( 88),IF003( 88),          &
     & IF004( 88),IF005( 88),IF006( 88),IF007( 88),               &
     & IF008( 88),IF009( 88),IF010( 88),IF011( 88)
! ----------------------------------------------------------------------------------------------------------------------------------

INTEGER(kind=short) :: IPT001( 88),IPT002( 88)
! ----------------------------------------------------------------------------------------------------------------------------------
EQUIVALENCE (IFNT(    1),IF001(1)),(IFNT(   89),IF002(1)),        &
     & (IFNT(  177),IF003(1)),(IFNT(  265),IF004(1)),             &
     & (IFNT(  353),IF005(1)),(IFNT(  441),IF006(1)),             &
     & (IFNT(  529),IF007(1)),(IFNT(  617),IF008(1)),             &
     & (IFNT(  705),IF009(1)),(IFNT(  793),IF010(1)),             &
     & (IFNT(  881),IF011(1))
! ----------------------------------------------------------------------------------------------------------------------------------
EQUIVALENCE (IPNT(    1),IPT001(1)),(IPNT(   89),IPT002(1))
! ----------------------------------------------------------------------------------------------------------------------------------
DATA IF001/  6186,  6826,  6806,  5526,  5546,  6186,  2080,                    &
     &   6176, 10282, 10538, 10916, 10908, 10518, 10006,  9628,  9636,          &
     &   5930,  2090,  6176,  6186,  6747,  5595,  2090,  6186,  6816,          &
     &   6166,  5536,  6186,  2080,  6176,  6688,  5672,  5656,  2592,          &
     &   6501,  6491,  5851,  5861, 14693,  5546, 14053,  5526, 14043,          &
     &   6806, 14683,  6176,  2730,  6166, 14378,  5536,  2720,  5721,          &
     &  14823,  5735,  2521,  5536, 15008,  6166, 14378,  5721, 14823,          &
     &   6617,  1639,  5735,  6176, 14358,  6176,  2535,  6166,  6186,          &
     &   6816,  5536,  2090,  5526,  6826,  5546,  2710, 13844,  5672,          &
     &   6696,  5656, 14872,  5920,  2336, 13612,  5672,  6680,  6696/
! ----------------------------------------------------------------------------------------------------------------------------------
DATA IF002/  1560,  6176, 14872,  6696,  6176,  1568,  5672,            &
     &   6696,  5656,  6680,  1576,  6176,  6680,  6696,  5656,  5672,            &
     &   2080,  6176,  6186,  6747,  5595, 14378,  6757,  6166,  5605,            &
     &   2661, 15124,  6696,  5672,  6680, 13848,  6432,  1824,  6696,            &
     &   6680,  5672,  5656, 14888,  6180,  2075,  5656,  6696,  5672,            &
     &   6680, 13848,  6432,  1824,  5536,  6186,  6816,  6166,  5536,            &
     &  15008,  6186,  2070,  5656,  5672,  6696,  6680,  5656, 14888,            &
     &   5672,  2584,  6176,  5928,  6440,  6176,  6688,  6424,  6176,            &
     &   5912,  5664,  2080, 10204, 10077, 10015, 10017, 10083, 10212,            &
     &  10340, 10467, 10529, 10527, 10461, 10332, 14300,  5983,  5985/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF003/ 14177,  6046, 14242,  6109, 14307,  6173, 14371,            &
     &   6237, 14435,  6302, 14498,  6367,  2273,  5916,  5924,  6436,            &
     &   6428, 14108,  5981, 14179,  6045, 14243,  6109, 14307,  6173,            &
     &  14371,  6237, 14435,  6301, 14499,  6365,  2275,  6170,  5859,            &
     &   6499, 14362,  5986,  6173, 14562,  6176,  6114,  6176,  2146,            &
     &  10273, 10208, 10271, 10336,  2081, 10204, 10077, 10015, 10017,            &
     &  10083, 10212, 10340, 10467, 10529, 10527, 10461, 10332,  2012,            &
     &  10133,  9942,  9752,  9627,  9566,  9570,  9637,  9768,  9962,            &
     &  10155, 10411, 10602, 10792, 10917, 10978, 10974, 10907, 10776,            &
     &  10582, 10389,  1941, 10122,  9803,  9549,  9359,  9170,  9045/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF004/  8921,  8862,  8866,  8935,  9067,  9198,  9393,            &
     &   9587,  9845, 10166, 10422, 10741, 10995, 11185, 11374, 11499,            &
     &  11623, 11682, 11678, 11609, 11477, 11346, 11151, 10957, 10699,            &
     &  10378,  1930,  6186,  5527,  6743,  2090,  1931,  9163,  9355,            &
     &   9359,  9167, 13259,  5138,  5088,  5280,  1042,  5081, 13280,            &
     &   5472,  1369,  5067, 13280,  5472, 13643,  4825, 13913,  5714,            &
     &    722,  4815,  9551,  9810,  9557,  9173,  4825,  5084, 13916,            &
     &   5280,  1163,  4815, 13916,  9180,  8924,  8928,  9184, 13276,            &
     &   9803,  9547,  9551,  9807,  1611,  9810,  9355,  9163,  8911,            &
     &   8914,  9561,  9564,  9376,  9180,  9173,  1611,  9177,  9372/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF005/  1184,  9568,  9177,  9170,  1355,  9184,  9561,            &
     &   9554,   971,  5263, 13468,  5721, 13010,  4825,  1618,  5263,            &
     &  13468,  4821,  1621,  8905,  9165,   975,  4821,  1621,  5068,            &
     &    971,  4811,  1632,  8911,  8924,  9184,  9568,  5724,  9807,            &
     &   9547,  5067,  4815,  1628,  5067, 13643,  5259,  5280,   988,            &
     &   8924,  9184,  9568,  9820,  9817,  4815,  4811,  1611,  4832,            &
     &   9824,  5724,  9365,  9557,  9810,  9807,  9547,  9163,   719,            &
     &   5451,  5472,  4821,  4818,  1618,  8911,  9163,  9547,  9807,            &
     &   9813,  5465,  4825,  4832,  1632,  8917,  9557,  9810,  9807,            &
     &   9547,  9163,  8911,  8921,  9376,  1632,  4832,  5728,  9820/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF006/  9170,   971,  9163,  9547,  9807,  9810,  9557,            &
     &   9173,  8921,  8924,  9184,  9568,  9820,  9817, 13653,  9173,            &
     &   8914,  8911,   971,  8907,  9355,  9810,  9820,  9568,  9184,            &
     &   8924,  8921,  9173,  1621,  5068, 13259,  5073,   978, 13003,            &
     &   9163,  9359, 13458,  5272,  1177,  5451,  4821,  1376,  4818,            &
     &  13906,  5721,   729, 13003,  5067,  5717,   992,  5260, 13451,            &
     &   9362,  9817,  9820,  9568,  9184,   732,  9547,  9163,  8911,            &
     &   8924,  9184,  9568,  9820,  9810,  9362,  9365,  1628,  4811,            &
     &   8921,  5280,  5721, 13899,  4818,  1618,  4811,  4832,  9568,            &
     &   9820,  5721,  5461, 13013,  9557,  9810,  5711,  5451,   715/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF007/  9820,  9568,  9184,  8924,  8911,  9163,  9547,            &
     &   1615,  4811,  9547,  9807,  5724,  5472, 13024,  5088,   971,            &
     &   4811,  4832, 13920,  5461, 13013,  4811,  1611,  4811,  4832,            &
     &  13920,  5269,   725,  9820,  9568,  9184,  8924,  8911,  9163,            &
     &   5707,  5714,  1362,  4811, 13024,  4821, 13909,  5728,  1611,            &
     &   5067, 13643,  5259, 13472,  5088,  1376,  8911,  9163,  5451,            &
     &   5711,  1632,  4811, 13024,  5728,  4821,  1611,  4832,  4811,            &
     &   1611,  4811,  4832,  5269,  5728,  1611,  4811, 13024,  4828,            &
     &  13903,  5728,  1611,  8911,  9163,  9547,  9807,  9820,  9568,            &
     &   9184,  8924,   719,  4811,  4832,  9568,  9820,  5721,  5461/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF008/   725,  8911,  9163,  9547,  9807,  9820,  9568,            &
     &   9184,  8924, 13007,  5266,  1611,  4811,  4832,  9568,  9820,            &
     &   5721,  5461, 13013,  5269,  1611,  8911,  9163,  9547,  9807,            &
     &   9810,  9557,  9173,  8921,  8924,  9184,  9568,  1628,  4832,            &
     &  13920,  5280,  1163,  4832,  8911,  9163,  5451,  5711,  1632,            &
     &   4832,  5259,  1632,  4832,  5067,  5269,  5451,  1632,  4811,            &
     &   4815,  5724, 13920,  4832,  4828,  5711,  1611,  4832,  4828,            &
     &   5269, 13451,  5728,  5724,  1173,  4832,  5728,  5724,  4815,            &
     &   4811,  1611,  5280,  4832,  4811,  1163,  5707,   736,  4811,            &
     &   5259,  5280,   736,  4821,  5276, 13909,  5276,  1167,  5263/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF009/  4821, 13468,  4821,  1621,  4832,  1365,  8911,            &
     &   8917,  9177,  9369,  9810,  9355,  9163, 13007,  5721,  1611,            &
     &   4811, 13024,  8914,  9369,  9561,  9813,  9807,  9547,  9355,            &
     &    722,  5721,  9177,  8917,  4815,  5067,  1611,  5728, 13899,            &
     &   9810,  9369,  9177,  8917,  8911,  9163,  9355,  1618,  4818,            &
     &   9810,  9813,  9561,  9177,  8917,  8911,  9163,  1355,  5259,            &
     &   5276, 13664,  5077,  1365,  9156,  5444,  5704, 13913,  9810,            &
     &   9369,  9177,  8917,  8911,  9163,  9355,  1618,  4811, 13024,            &
     &   8921,  5465,  5717,  1611,  5067, 13643,  5259,  5269, 13269,            &
     &   5275,  1180,  5061,  5253,  5449, 13657,  5471,  1376,  4811/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF010/ 13024,  5724, 13007,  5269,  1611,  5067, 13643,            &
     &   5259,  5280,   992,  4811, 13017,  8917,  5081,  5269, 13451,            &
     &   9365,  5465,  5717,  1611,  4811, 13017,  8917,  9177,  5465,            &
     &   5717,  1611,  8911,  8917,  9177,  9561,  9813,  9807,  9547,            &
     &   9163,   719,  4804, 13017,  8914,  9369,  9561,  9813,  9807,            &
     &   9547,  9355,   722,  5700, 13913,  9810,  9369,  9177,  8917,            &
     &   8911,  9163,  9355,  1618,  4811, 13017,  8914,  5273,  1625,            &
     &   8907,  9547,  9807,  9554,  9170,  8917,  9177,  1625,  5081,            &
     &  13657,  5280,  5263,  1355,  4825,  8911,  9163,  9547, 13903,            &
     &   5721,  1611,  4825,  5259,  1625,  4825,  5067,  5266,  5451/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IF011/  1625,  4811, 13913,  4825,  1611,  9156,  5444,            &
     &   5704, 13913,  4825,  8911,  9163,  1611,  4825,  5721,  4811,            &
     &   1611,  5259,  5007,  5075,  4885,  5080,  5020,  1184,  5259,            &
     &  13458,  5273,  1184,  5067,  5327,  5267,  5461,  5272,  5340,            &
     &    992,  4819,  5079,  5459,  1623,   131,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IPT001/     1,     8,    19,    24,    30,    35,    48,            &
     &     52,    56,    64,    69,    74,    78,    85,    90,    95,            &
     &    100,   106,   115,   122,   129,   136,   144,   152,   162,            &
     &    190,   209,   220,   225,   238,   259,   292,   296,   297,            &
     &    306,   310,   318,   328,   340,   351,   354,   358,   362,            &
     &    368,   372,   375,   377,   379,   381,   391,   396,   404,            &
     &    414,   419,   428,   438,   443,   460,   470,   474,   480,            &
     &    483,   487,   491,   499,   510,   517,   529,   537,   545,            &
     &    552,   557,   566,   572,   578,   583,   588,   591,   596,            &
     &    602,   611,   618,   629,   638,   650,   654,   660,   663/
!-----------------------------------------------------------------------------------------------------------------------------------
DATA IPT002/   668,   676,   683,   689,   693,   695,   699,            &
     &    704,   709,   711,   721,   731,   737,   747,   756,   761,            &
     &    773,   779,   786,   792,   798,   803,   813,   820,   829,            &
     &    839,   849,   854,   862,   867,   874,   877,   882,   886,            &
     &    894,   898,   905,   909,   916,   920,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0,            &
     &      0,     0,     0,     0,     0,     0,     0,     0,     0/
!-----------------------------------------------------------------------------------------------------------------------------------
      N=NN           ! NUMBER OF CHARACTERS
      AA=A           ! PLOTTING ANGLE
      SI=SIN(AA*0.0174532)
      CO=COS(AA*0.0174532)
      LENGTH=.TRUE.      ! PLOT (TRUE) OR LENGTH ONLY
      AL=0.0            ! PLOTTED LENGTH
      ISS=IS            ! CENTERING FLAG
      IF (ISS.EQ.-3) LENGTH=.FALSE.
      IF (ISS.GT.-1) LENGTH=.FALSE.
      OX=OLDX           ! SAVE CURRENT POSITION
      OY=OLDY
1100  CONTINUE       ! TOP OF LENGTH COMPUTATION
      AL=0.0            ! LENGTH OF PLOTTED STRING ACCUMULATOR
      X1=X           ! LOWER LEFT CORNER
      Y1=Y
      IF (ISS.EQ.0) THEN  ! CENTERED
         X1=X-AL/2.*CO+S/2.*SI
         Y1=Y-S/2.*CO-AL/2.*SI
      ENDIF
      IF (ISS.EQ.1) THEN  ! LOWER RIGHT CORNER
         X1=X-AL*CO
         Y1=Y-AL*SI
      ENDIF
      IF (X.GT.998.0.OR.Y.GT.998.0) THEN
         IF (X.LT.998.0) OLDX=OLDX+X1
         IF (Y.LT.998.0) OLDY=OLDY+Y1
      ELSE
         OLDX=X1
         OLDY=Y1
      ENDIF
      X0=OLDX
      Y0=OLDY
      IF (LENGTH.AND.N.LT.0) CALL draw_(OLDX,OLDY) ! PLOT TO START
      SS=S/21.    ! SCALE FACTOR
      I=0         ! CHARACTER COUNTER
50    continue
      I=I+1
      IF (I.GT.IABS(N)) GOTO 1000  ! END OF STRING COUNT
         ICC=ICHAR(T(I:I))  ! GET ITH ASCII CHARACTER
         IF (ICC.GT.127) GOTO 50  ! CODE TO LARGE
         IF (ICC.EQ.0.AND.I.GT.1) GOTO 1000 ! END OF STRING REACHED
         IXOFF=11       ! OFFSET
         IYOFF=11
         IF (ICC.LT.32) THEN  ! DIFFERENT SYMBOL OFFSET
            IXOFF=32
            IYOFF=32
         ENDIF
         IL=IPNT(ICC+1)   ! STARTING INDEX
         IW=21          ! CHARACTER WIDTH
         IF (IL.EQ.0) GOTO 90  ! NO PLOTTING INFO
         IPENLAST=3
70       CONTINUE
         IY=IBITS(IFNT(IL),0,6)
         IX=IBITS(IFNT(IL),6,6)
         IPEN=IBITS(IFNT(IL),12,2)
         IP=IPENLAST
         IPENLAST=IPEN
         XX=SS*(IX-IXOFF)
!c       Y1=SS*(IY-IYOFF+ISUB)
         Y1=SS*(IY-IYOFF)
         X1=XX*CO-Y1*SI+OLDX
         Y1=XX*SI+Y1*CO+OLDY
         IF (IP.EQ.0) IP=2
         IF (IP.EQ.1) IP=2
         IF (LENGTH) CALL plot_(X1,Y1,IP)
         IL=IL+1
         IF (IPEN.NE.0) GOTO 70
90       continue
         XX=SS*IW       ! END OF CHARACTER
         AL=AL+SS*IW
         OLDX=XX*CO+OLDX
         OLDY=XX*SI+OLDY
         GOTO 50
1000  continue
      IF (.NOT.LENGTH) THEN ! FINISHED LENGTH-ONLY PASS
         LENGTH=.TRUE.
         IF (ISS.EQ.-3) THEN ! RETURN END POSITION
            X=OLDX
            Y=OLDY
         ENDIF
         OLDX=OX     ! RESTORE OLD POSITION
         OLDY=OY
         IF (ISS.EQ.0.OR.ISS.EQ.1) GOTO 1100
      ELSE
         IF (N.LE.1) CALL move_(X0,Y0) ! LEAVE PEN AT START
         IF (ISS.EQ.-2) THEN ! RETURN END POSITION
            X=OLDX
            Y=OLDY
         ENDIF
      ENDIF
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
SUBROUTINE axisc_(X0,Y0,T,N0,S0,A0,B0,C0,D0,E0,F0,ICOL)
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
real    :: a0, b0, c0, d0, e0, f0
real    :: ang
real    :: c1
real    :: e1
real    :: co
real    :: cs
real    :: dnx, dny
real    :: dtx, dty
real    :: fa
real    :: hor
integer :: i
integer :: j
integer :: k
integer :: n0
integer :: n1
integer :: nc
integer :: nd
integer :: ndd
integer :: ng
integer :: njt
integer :: nnt
integer :: nst
real    :: s0
real    :: si
real    :: space
real    :: tl
real    :: tl1
real    :: x, x0, x01
real    :: x1, x2, x3
real    :: xj
real    :: xn
real    :: xs
real    :: y, y0, y01
real    :: y1, y2
real    :: rotx, roty
INTEGER :: ICOL(*)
CHARACTER(len=*) :: T
!
   ROTX(X,Y)=CO*X-SI*Y+X01  ! ROTATION MATRIX
   ROTY(X,Y)=SI*X+CO*Y+Y01
   DATA SPACE/0.08/   ! MIN SPACING BETWEEN ITEMS
!
   TL=0.1            ! TICK LENGTH
   X01=X0
   Y01=Y0
   ANG=A0            ! ROTATION ANGLE
   E1=E0          ! CHARACTER SIZE
   CS=ABS(E1)
   IF (CS.EQ.0.) CS=.15
   IF (S0.EQ.0.0) GOTO 1000 ! ZERO LENGTH AXIS
   X1=D0*1.000002
   NJT=ABS(X1)       ! NUMBER OF MAJOR TICKS
   NNT=100.0*(ABS(X1)-NJT)  ! NUMBER OF MINOR TICKS
   NST=100.0*((ABS(X1)-NJT)*100.0-NNT) ! NUMBER OF SUB-MINOR TICKS
   IF (NJT.LT.2) NJT=2
   XJ=ABS(S0)/(NJT-1)  ! INCREMENT BETWEEN MAJOR TICKS
   XN=XJ
   IF (NNT.NE.0) XN=XN/(NNT+1) ! INCREMENT BETWEEN MINOR TICKS
   XS=XN/(NST+1)      ! INCREMENT BETWEEN SUB-MINOR TICKS
   N1=MOD(N0,100000)
   IF (IABS(N1)/1000.NE.0) HOR=90.0 ! ROTATION ANGLE
   CO=COS(ANG*0.017453294)  ! AXIS ANGLE ROTATION
   SI=SIN(ANG*0.017453294)
   HOR=ANG           ! ANGLE OF NUMBER LABELS
   NC=MOD(IABS(N1),100)  ! NUMBER OF CHARACTERS IN TITLE
!
!     DECODE NUMBER FORMAT
!
   FA=F0
   IF (FA.EQ.-1.0) THEN  ! INPUT WAS -1.0
      FA=1003.0      ! DEFAULT AUTO SCALING FORMAT
      IF (E1.LT.0.0) THEN ! NO AUTO SCALING SO MAKE
         NG=1         ! FORMAT TO FIT
         IF (B0.NE.0.0) NG=MAX(NG,INT(ALOG10( ABS(B0))+0.001))
         IF (C0.NE.0.0) NG=MAX(NG,INT(ALOG10( ABS(C0))+0.001))
         IF (B0.LT.0.0.OR.C0.LT.0) NG=NG+1
         FA=1000.0+FLOAT(NG)
      ENDIF
   ENDIF
   IF (ABS(FA).GT.1000.0) THEN ! OUTPUT DESIRED IS INTEGER
      ND=ABS(FA)-1000.0 ! INPUT INTEGER VALUE
!        FA=3.+FLOAT(ND)*(1.01) ! DEFAULT FORMAT FOR NOT AUTOSCALE
!        IF (E1.LT.0.0) THEN ! NO AUTO SCALING TO MAKE FORMAT
!          NG=2         ! WHICH FITS
!          IF (B0.NE.0.0) NG=MAX(NG,INT(ALOG10( ABS(B0))+0.001)+1)
!          IF (C0.NE.0.0) NG=MAX(NG,INT(ALOG10( ABS(C0))+0.001)+1)
!          IF (B0.LT.0.0.OR.C0.LT.0) NG=NG+1
!          FA=FLOAT(NG)+FLOAT(ND)*1.01
!        ENDIF
   ENDIF
   ND=MOD(ABS(FA),1000.0)  ! NUMBER OF DIGITS
   NG=100.0*(MOD(ABS(FA),1000.0)-ND+0.0001)! NUMBER OF DIGITS RIGHT OF D.P.
   IF (NG.GT.17) NG=NG/10  ! CORRECT INPUT SIMPLE ERRORS
   IF (FA.LT.0.0) ND=ND-4  ! EXPONENTIAL NOTATION
   IF (ND.LE.0) ND=NG
   NDD=ND
   IF (FA.LT.0.0) NDD=ND+4  ! EXPONENTIAL NOTATION
   IF (ABS(FA).GT.1000) NG=-1 ! FORMATTED INTEGER
!
   TL1=TL
   IF (S0.LT.0.0) TL1=-TL  ! REVERSE SIDE OF TICKS
   IF (S0.LT.0.0) TL=0.0  ! REVERSE SIDE OF TICKS
   IF (IABS(N1)/1000.NE.0) GOTO 10
   DNX=(-NDD)*CS/2.0  ! NUMBER LABEL DISTANCE FROM AXIS
   DNY=(-TL)-SPACE-CS ! NUMBER LABEL DISTANCE FROM AXIS
   DTY=DNY-CS-SPACE ! TITLE DISTANCE FROM AXIS
   GOTO 20
10 CONTINUE       ! HORIZONTAL NUMBERS ON VERTICAL AXIS
   DNX=(-CS)/2.0    ! NUMBER LABEL DISTANCE FROM AXIS
   DNY=(-TL)-SPACE   ! NUMBER LABEL DISTANCE FROM AXIS
   DTY=DNY-NDD*CS-2.*SPACE ! TITLE DISTANCE FROM AXIS
   HOR=ANG-90.0
20 CONTINUE
   IF (N1.LT.0) GOTO 30  ! CLOCKWISE TITLES
   DNY=-DNY-CS    ! COUNTER-CLOCKWISE TITLES
   DTY=-DTY-CS
   TL1=-TL1    ! CHANGE SIDES OF TICKS
   IF (IABS(N1).LT.1000) GOTO 30
   DNY=DNY+CS*NDD
   DTY=DNY+SPACE
30 continue
   X1=0.0            ! FIRST MAJOR TICK
   Y1=0.0
   Y2=-TL1
   IF (IABS(N0).GE.100000) CALL color_(ICOL(1))
   CALL move_(ROTX(X1,Y1),ROTY(X1,Y1))
   CALL draw_(ROTX(X1,Y2),ROTY(X1,Y2))
   DO 100 I=1,NJT-1   ! MAJOR TICKS
      CALL move_(ROTX(X1,Y1),ROTY(X1,Y1))
      X1=XJ*I
      Y2=-TL1
      CALL draw_(ROTX(X1,Y1),ROTY(X1,Y1))
      CALL draw_(ROTX(X1,Y2),ROTY(X1,Y2))
      DO 110 J=1,NNT+1 ! MINOR TICKS
         Y2=(-TL1)*0.7
         X2=X1+XN*J-XJ
         CALL move_(ROTX(X2,Y1),ROTY(X2,Y1))
         CALL draw_(ROTX(X2,Y2),ROTY(X2,Y2))
         Y2=(-TL1)*0.4
         DO 120 K=1,NST ! SUB MINOR TICKS
            X3=X2+XS*K-XN
            CALL move_(ROTX(X3,Y1),ROTY(X3,Y1))
            CALL draw_(ROTX(X3,Y2),ROTY(X3,Y2))
120      CONTINUE
110   CONTINUE
100 CONTINUE
   IF (MOD(IABS(N1),1000).GT.100) GOTO 1000 ! NO LABELING
   XS=0.0             ! EXPONENT
   IF (E1.LT.0.) GOTO 140   ! NO AUTO SCALING
!
!     COMPUTE AUTO EXPONENT SCALING SO THAT THE FORMATTED LABEL
!     HAS THE INTEGER PORTION FILLED AS MUCH AS POSSIBLE
!
   I=ND-NG-1
   IF (B0.LT.0.0.OR.C0.LT.0.0) I=I-1
   IF (B0.NE.0.0) THEN
      X1=ALOG10(ABS(B0)+1.E-30)
      IF (X1.LT.0.0.AND.ABS(AINT(x1-0.001)-X1).GT.0.001) X1=X1-1.0
      IF (X1.GE.0.0) X1=X1+1.0
      X1=AINT(X1)
   ELSE
      X1=0.0
   ENDIF
   IF (C0.NE.0.0) THEN
      Y1=ALOG10(ABS(C0)+1.E-30)
      IF (Y1.LT.0.0.AND.ABS(AINT(Y1-0.001)-Y1).GT.0.001) Y1=Y1-1.0
      IF (Y1.GE.0.0) Y1=Y1+1.0
      Y1=AINT(Y1)
   ELSE
      Y1=0.0
   ENDIF
   X2=MIN(X1,Y1)
   X3=MAX(X1,Y1)
   IF (X3.LT.0.0) X3=X3+1
   IF (X2.LT.0.0.AND.NG.LE.2-X2) XS=ND-NG-1-X2
   IF (I.LT.X3+XS) XS=I-X3
140 Y1=DNY
   Y2=(C0-B0)/(NJT-1)
   IF (IABS(N0).GE.100000) CALL color_(ICOL(2))
   E1=XS             ! EXPONENT VALUE
   DO 150 I=1,NJT      ! LABEL MAJOR TICKS
      X1=(I-1)*XJ+DNX
      C1=(Y2*(I-1)+B0)*10.**E1
      CALL number_(ROTX(X1,Y1),ROTY(X1,Y1),CS,C1,HOR,FA,-1)
150 CONTINUE
!
!     PLOT TITLE
!
   IF (NC.NE.0) THEN
      Y1=0.0
      X1=0.0
      call symbol_(X1,Y1,CS,T,0.,NC,-3) ! GET TITLE LENGTH
      DTX=(ABS(S0)-X1)/2.   ! CENTER TITLE
      IF (E1.NE.0.0) DTX=DTX-CS*3.0 ! ADD EXPONENT SPACE
      IF(IABS(N0).GE.100000)CALL color_(ICOL(3))
      call symbol_(ROTX(DTX,DTY),ROTY(DTX,DTY),CS,T,ANG,NC,-1)
      DTX=DTX+X1
   ELSE
      DTX=ABS(S0)/2.0
      IF (E1.NE.0.0) DTX=DTX-CS*3.0 ! ADD EXPONENT SPACE
   ENDIF
   X1=DTX+CS/2.0
   Y1=DTY
   IF (E1.EQ.0.0) GOTO 1000  ! NO EXPONENT
   IF(IABS(N0).GE.100000)CALL color_(ICOL(4))
   E1=-E1
   call symbol_(ROTX(X1,Y1),ROTY(X1,Y1),CS,'(X10',ANG,4,-1)
   X1=X1+3.75*CS
   Y1=Y1+CS*0.4
   CALL number_(ROTX(X1,Y1),ROTY(X1,Y1),CS,E1,ANG,0.0,-1)
   X2=AINT(ALOG10(ABS(E1)))+0.75
   IF (E1.LT.0.0) X2=X2+1.0
   X1=X1+X2*CS
   Y1=Y1-CS*0.4
   call symbol_(ROTX(X1,Y1),ROTY(X1,Y1),CS,')',ANG,1,-1)
1000 RETURN
end subroutine axisc_
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
SUBROUTINE axislg_(X0,Y0,A0,N0,S0,T0,C0,D0,ICOL)
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
real     :: aj
real     :: at
real     :: b1
real     :: b3
real     :: b4
real     :: b6
real     :: b7
real     :: b8
real     :: c0
real     :: c1
real     :: cs
real     :: d0
real     :: hor
real     :: ht3
real     :: ht4
integer  :: i
integer  :: j
integer  :: n0, n1, n2
real     :: s0
real     :: s5
real     :: s6
real     :: t0, t1, t2, t3, t4, t5, t6
real     :: x0, x1, x2
real     :: xl
real     :: y0, y1, y2
   CHARACTER*(*) A0
   INTEGER ICOL(3)
   LOGICAL LABELS,COLOR
   DATA CS/.15/      ! CHARACTER SIZE
   N1=IABS(N0)
   LABELS=.TRUE.
   HOR=T0
   IF (S0.EQ.0.0) RETURN
   T5=0.1         ! TICK LENGTH
   B7=T5+.08      ! NUMBER DISTANCE FROM AXIS
   B6=B7
   B8=0.0
   IF(N0.LT.0)GOTO 10
   B3=CS*4.0
   B4=CS+0.08
   T2=T0
   GOTO 20
10 B3=(-CS)*4.0
   B4=0.0-T5-CS-.05
   T2=T0
   T5=-T5
20 CONTINUE
   COLOR=.FALSE.
   IF (N1.GE.100000) THEN
      N1=MOD(N1,100000)
      COLOR=.TRUE.
   ENDIF
   IF (N1.GE.10000) N1=MOD(N1,10000)
   IF (N1.GE.1000)THEN
      N1=MOD(N1,1000)
      HOR=0.0
      B4=(ABS(T5)+.05)*SIGN(1.,FLOAT(N0))
      IF (N0.GT.0) B4=B4+3.5*CS
      B6=.5*CS
      B8=(.5*CS+ABS(T5))*SIGN(1.,FLOAT(N0))
      IF (N0.LT.0) B8=B8-CS*1.6
   ENDIF
   IF (N1.GE.100) THEN
      N1=MOD(N1,100)
      LABELS=.FALSE.
   ENDIF
   N2=ABS(S0*D0)+0.5     ! NUMBER OF TICKS
   XL=ABS(S0)/FLOAT(N2)    ! DISTANCE BETWEEN TICKS
   T1=T0*0.017453294
   T3=COS(T1)
   T4=SIN(T1)
   HT3=COS(HOR*0.017453294)
   HT4=SIN(HOR*0.017453294)
   T6=T5*T3
   T5=T5*T4
!
   IF (COLOR) CALL color_(ICOL(1)) !COLOR
   S5=SIGN(1.0,S0)*T5
   S6=SIGN(1.0,S0)*T6
   X1=X0
   Y1=Y0
   CALL move_(X1,Y1)
   DO I=1,N2      ! PLOT MAJOR TICKS
      DO J=2,10      ! PLOT MINOR TICKS
         AT=.6
         IF (J.EQ.2) AT=1.0
         X2=X1-S5*AT
         Y2=Y1+S6*AT
         CALL move_(X2,Y2)
         CALL draw_(X1,Y1)
         AJ=ALOG10(FLOAT(J))-ALOG10(FLOAT(J-1))
         X1=X1+T3*XL*AJ
         Y1=Y1+T4*XL*AJ
         IF (T0.EQ.90.0) X1=X0
         CALL draw_(X1,Y1)
      enddo
   enddo
   X2=X1-S5
   Y2=Y1+S6
   CALL draw_(X2,Y2)
!
   IF (COLOR) CALL color_(ICOL(3)) !COLOR
   IF (.NOT.LABELS) RETURN
   C1=AINT(C0)
   X2=X0-B4*T4-B7*T3  ! LOCATE CENTER NUMBER LABELS
   Y2=Y0+B4*T3-B6*T4
   N2=N2+1
   IF (COLOR) CALL color_(ICOL(2)) !COLOR
   DO I=1,N2
      call symbol_(X2,Y2,CS,'10',HOR,2,-1)
      X1=X2-.6*CS*HT4+CS*2.*HT3
      Y1=Y2+.6*CS*HT3+CS*2.*HT4
      IF (C1.EQ.0.0) THEN
         call symbol_(X1,Y1,CS*.8,'0',HOR,1,-1)
      ELSE
         CALL number_(X1,Y1,CS*.8,C1,HOR,0.0,-1)
      ENDIF
      C1=C1+SIGN(1.,D0)
      X2=X2+T3*XL
      Y2=Y2+T4*XL
   enddo
   IF (N1.LT.1) GOTO 155
   X1=0.0
   Y1=0.0
   call symbol_(X1,Y1,CS,A0,T2,N1,-3) ! GET TITLE LENGTH
   B1=0.5*(ABS(S0)-X1)
   X2=X0+B1*T3-B3*T4-B8*T4
   Y2=Y0+B1*T4+B3*T3
   IF (COLOR) CALL color_(ICOL(3)) !COLOR
   call symbol_(X2,Y2,CS,A0,T2,N1,-1)
155 continue
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
! ident_4="@(#)M_graph::clipit(3f):clips a line segment partially visible"
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
!-------------------------------------------------------------------------------
SUBROUTINE number_(X,Y,HGHT,Z,T,F0,IPF)
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
real      ::  alg
real      ::  f
real      ::  f0
real      ::  fa
real      ::  hg
real      ::  hght
integer   ::  i
integer   ::  iff
integer   ::  ipf
integer   ::  nd
integer   ::  nn
real      ::  t
real      ::  t1
real      ::  x
real      ::  y
real      ::  z
CHARACTER(len=18) :: B   ! WORKING BUFFERS
CHARACTER(len=8)  :: FB  ! WORKING BUFFERS
CHARACTER(len=7)  :: FB1 ! WORKING BUFFERS
!
   IFF=0
   HG=HGHT
   IF (HG.EQ.0.0) HG=0.15
   T1=T
   ND=0
   NN=0
   FA=F0
   IF (ABS(FA).GT.1022.0) FA=0.0
   IF (FA.EQ.0.0) GOTO 10  ! INTEGER FORMAT
   IF (FA.GT.999.0) THEN  ! PLOT FORMATTED INTEGER
      NN=AMOD(FA,1000.)
      FA=0.0
   ELSE           ! PLOT FLOAT OR EXPON NUMBER
      F=ABS(FA)*1.000002
      NN=F
      F=(F-NN)*100.0
      ND=F
   ENDIF
10 continue
   IF (ND.GT.17) ND=ND/10  ! CORRECT SIMPLE INPUT ERRORS
   IF (NN.EQ.0) THEN  ! DIGITS TO LEFT OF DECIMAL POINT
      NN=ND+2
      IF (Z.EQ.0.AND.FA.EQ.0.0) NN=1
      IF (Z.NE.0.0) THEN
         ALG=ALOG10(ABS(Z))
         IF (ALG.LT.0.0) ALG=0.0
         NN=ND+2+ALG
         IF (FA.EQ.0.0) NN=1+ALG
      ENDIF
      IF (Z.LT.0.0) NN=NN+1
      IF (FA.LT.0.0) NN=NN+4
   ENDIF
   IF (ND.GT.NN) GOTO 90  ! FORMAT ERROR
   IF (NN.GT.18) NN=18  ! MAX CHARACTERS
   IF (FA.EQ.0.0) THEN  ! INTEGER
      I=Z
      FB=CHAR(NN-10*(NN/10)+48)//')'
      FB1=FB(1:1)
      IF ((NN/10).GT.0) FB=CHAR(NN/10+48)//FB1(1:1)
      FB1='(I'//FB(1:2)
      WRITE(B,FB1,ERR=90) I
   ELSE           ! FLOATING POINT OR EXPONENTIAL
      IF (NN.GT.1) THEN
         FB=CHAR(ND-10*(ND/10)+48)//')'
         FB1=FB(1:1)
         IF ((ND/10).GT.0) FB=CHAR(ND/10+48)//FB1(1:1)
         FB1=CHAR(NN-10*(NN/10)+48)//'.'//FB(1:2)
         FB=FB1(1:4)
         IF (NN/10.GT.0) FB=CHAR(NN/10+48)//FB1(1:4)
         IF (FA.GT.0.0) THEN
            FB1='(F'//FB(1:5)
         ELSE
            FB1='(E'//FB(1:5)
         ENDIF
      ELSE
         IF(FA.GT.0.0) THEN
            FB1='(F)'
         ELSE
            FB1='(E)'
         ENDIF
         NN=16
         IFF=1
      ENDIF
      WRITE(B,FB1,ERR=90) Z
      IF (IFF.EQ.1) THEN  ! REMOVE LEADING SPACES
         DO I=1,18
            IF (B(1:1).EQ.' ') B=B(2:18)
         enddo
      ENDIF
   ENDIF
   call symbol_(X,Y,HG,B,T1,NN,IPF)
   RETURN
90 continue
   DO I=1,18
      B(I:I)='*'
      IF (I.EQ.NN-ND) B(I:I)='.'
   enddo
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
