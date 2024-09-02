










!>
!!##NAME
!!    M_pixel(3f) - [M_pixel::INTRO] module for drawing into a pixel array
!!                  with 2D vector operations
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   Module procedures
!!
!!    use M_pixel__writegif, only : writegif
!!
!!    use :: M_pixel, only : drawchar,  rect,            rdraw2,     strlength
!!    use :: M_pixel, only : color,     mapcolor,        clear,      draw2
!!    use :: M_pixel, only : circle,    circleprecision, arc,        getviewport
!!    use :: M_pixel, only : viewport,  ortho2,          rmove2
!!    use :: M_pixel, only : line,      linewidth,       polyline2
!!    use :: M_pixel, only : move2,     draw2,           prefsize,   vinit
!!    use :: M_pixel, only : textang,   textsize,        drawstr,    getgp2
!!    use :: M_pixel, only : vflush,    page,            point2,     getdisplaysize
!!    use :: M_pixel, only : poly2,     centertext,      xcentertext, ycentertext
!!    use :: M_pixel, only : makepoly,  closepoly,       font
!!
!!    use :: M_pixel, only : state,     hershey,         justfy
!!    use :: M_pixel, only : print_ascii, print_ppm, print_p3, print_p6, print_ansi
!!    use :: M_pixel, only : pixel
!!    use :: M_pixel, only : hue
!!
!!    ! Differences between M_pixel and M_draw and M_draw-related procedures:
!!    !  o  hershey(3f) and justfy(3f) do not exist in M_draw and might be
!!    !     replaced and the same font names are not available
!!    !  o  print_ansi, print_ascii(3f) and print_ppm|p3|p6(3f) do not
!!    !     exist in M_draw
!!    !  -  state(3f) does not exist in M_draw
!!    !  -  viewport is in terms of pixels, not range -1.0 to 1.0
!!
!!   Module variables
!!
!!    use M_pixel, only : P_pixel, P_ColorMap, P_debug
!!
!!##DESCRIPTION
!!    M_pixel(3fm) is intended to produce simple pixel graphics composed of
!!    line drawings and polygon fills in two dimensions. It handles circles,
!!    curves, arcs, polygons, and software text. It is designed to provide a
!!    programming interface very similar to a subset of the VOGLE graphics
!!    library (M_pixel does not support objects, interactive graphics,
!!    or 3D vectors).
!!
!!    It is primarily intended to provide a simple Fortran-based set of
!!    routines that can generate simple graphics that can be written to a
!!    GIF file using the writegif(3f) routine.
!!
!!    This is a prototype under construction starting 2017-06, but is already
!!    useful. Improvements in line width, dashed lines, polygon fill and
!!    higher level graphing routines are being worked on. If anyone is
!!    interested in collaborating on the module, contact the author.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_M_pixel
!!    use M_pixel
!!    use M_pixel__writegif, only :  writegif
!!    use M_pixel,    only : cosd, sind
!!    implicit none
!!
!!    integer  :: i
!!    integer  :: j
!!    integer  :: icolor
!!
!!       ! initialize image
!!       call prefsize(400,400)  ! set size before starting
!!       call vinit()            ! start graphics
!!       call clear(0)           ! clear to color 0
!!
!!       ! put some colored boxes into pixmap by address
!!       ! so show how the pixel map can be edited easily with
!!       ! other routines that can manipulate a pixel array.
!!       ! The P_pixel array was created when vinit(3f) was called
!!       icolor=1
!!       do i=1,4
!!          do j=1,4
!!             P_pixel((i-1)*100+1+3:i*100-3,(j-1)*100+1+3:j*100-3)=icolor
!!             icolor=icolor+1
!!          enddo
!!       enddo
!!
!!       ! map area of virtual world to pixel array
!!       ! notice Y-axis for viewport is zero at TOP
!!          ! viewport(left, right, bottom, top)
!!       call viewport(0.0,  400.0,  400.0, 0.0)
!!       ! define the virtual world area we want to work in
!!           !ortho2(left, right, bottom,   top)
!!       call ortho2(0.0,  400.0,    0.0, 400.0)
!!       ! the drawing routines use these world units
!!
!!       ! draw polar grids
!!       call linewidth(100)
!!       call color(14)
!!       call target(200.0,200.0,200.0)
!!
!!       call linewidth(75)
!!       call color(0)
!!       call target(100.0,200.0,50.0)
!!
!!       ! draw some lines
!!       call color(1)
!!       call linewidth(200)
!!       call line(1.0,1.0,400.0,400.0)
!!
!!       call color(4)
!!       call line(350.0,200.0,350.0,300.0)
!!
!!       ! print some text
!!       call color(1)
!!       !call hershey(x,y,height,itext,theta,ntext)
!!       call linewidth(125)
!!       call hershey(40.0, 40.0,35.0,'Hello World',0.0,11)
!!       call color(7)
!!       call linewidth(25)
!!       call hershey(40.0, 80.0,35.0,'Hello World',0.0,11)
!!       call linewidth(100)
!!       call hershey(40.0,120.0,35.0,'Hello World',30.0,11)
!!
!!       call hershey( 40.0,350.0,35.0,'\COMPLEX\Hello World',0.0,20)
!!       call hershey( 40.0,310.0,35.0,'\DUPLEX\Hello World',0.0,19)
!!       call hershey( 350.0,400.0,35.0,'\ITALIC\Hello World',90.0,19)
!!       call linewidth(50)
!!       call hershey(200.0,120.0,15.0,'\SIMPLEX\Hello World',20.0,20)
!!
!!       ! change background color directly
!!       where (P_pixel.eq.0) P_pixel=9
!!       ! write standard gif file
!!       call writegif('M_pixel.3M_pixel.gif',P_pixel,P_ColorMap)
!!
!!    contains
!!
!!       subroutine target(xc,yc,rc)
!!       use M_pixel,    only : cosd, sind
!!       real     :: xc,yc,rc
!!       integer  :: i
!!       real     :: x,y
!!          do i=0,360,10
!!             x=rc*cosd(i)
!!             y=rc*sind(i)
!!             call line(xc,yc,xc+x,yc+y)
!!          enddo
!!          do i=1,int(rc),10
!!             call circle(xc,yc,real(i))
!!          enddo
!!       end subroutine target
!!    end program demo_M_pixel
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
module M_pixel
!  Modify pixel data with vector drawing calls
!
!-!use M_pixel,                    only : cosd, sind
use, intrinsic :: ISO_C_binding,   only : c_short, c_int, c_float
use, intrinsic :: ISO_FORTRAN_ENV, only : INT8, INT16, INT32, INT64       !  1           2           4           8
use, intrinsic :: ISO_FORTRAN_ENV, only : REAL32, REAL64, REAL128         !  4           8          10
implicit none         !  Check all declarations
private
!-------------------------------------------------------------------------------
integer(kind=c_short),public,parameter :: D_XCENTERED=     1_C_SHORT
integer(kind=c_short),public,parameter :: D_YCENTERED=     2_C_SHORT
integer(kind=c_short),public,parameter :: D_LEFT=          4_C_SHORT  ! The default
integer(kind=c_short),public,parameter :: D_RIGHT=         8_C_SHORT
integer(kind=c_short),public,parameter :: D_TOP=          16_C_SHORT
integer(kind=c_short),public,parameter :: D_BOTTOM=       32_C_SHORT ! The default
!-------------------------------------------------------------------------------
integer(kind=c_short),public,parameter :: D_NORMAL=        0_C_SHORT ! The default
integer(kind=c_short),public,parameter :: D_BOLD=          1_C_SHORT
!-------------------------------------------------------------------------------
integer(kind=c_short),public,parameter :: D_THICK=         1_C_SHORT
integer(kind=c_short),public,parameter :: D_THIN=          0_C_SHORT ! The default
!-------------------------------------------------------------------------------
integer(kind=c_int),public,parameter   :: D_BLACK    =  0_C_INT
integer(kind=c_int),public,parameter   :: D_RED      =  1_C_INT
integer(kind=c_int),public,parameter   :: D_GREEN    =  2_C_INT
integer(kind=c_int),public,parameter   :: D_YELLOW   =  3_C_INT
integer(kind=c_int),public,parameter   :: D_BLUE     =  4_C_INT
integer(kind=c_int),public,parameter   :: D_MAGENTA  =  5_C_INT
integer(kind=c_int),public,parameter   :: D_CYAN     =  6_C_INT
integer(kind=c_int),public,parameter   :: D_WHITE    =  7_C_INT
!-------------------------------------------------------------------------------
public MATRIX
type, bind(C) :: MATRIX
   real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
end type MATRIX
!==================================================================================================================================!
real,parameter            :: PI32=3.14159265358979323844
doubleprecision,parameter :: pi                 = 3.14159265358979323846264338327950288419716939937510d0
doubleprecision,parameter :: Deg_Per_Rad        = 57.2957795130823208767981548d0
doubleprecision,parameter :: Rad_Per_Deg        = 0.01745329251994329576923691d0
doubleprecision,parameter :: degrees_to_radians = PI/180.0d0
! Global Graphics State
logical,save :: P_VINIT_CALLED=.false.
real,save    :: P_X=0.0, P_Y=0.0      ! current position
integer,save :: P_WIDTH=1             ! line width
integer,save :: P_COLOR_INDEX=1       ! pen color
integer,save :: P_NSEGS=60            ! number of line segments making up a circle
integer,save :: P_VIEWPORT_WIDTH=400, P_VIEWPORT_HEIGHT=400
real,save    :: P_TEXT_HEIGHT=10.0
real,save    :: P_TEXT_WIDTH=7.0
real,save    :: P_TEXT_ANGLE=0.0
real,save    :: P_TEXT_COSINE=1.0
real,save    :: P_TEXT_SINE  =0.0
logical,save :: P_X_CENTERTEXT=.false.
logical,save :: P_Y_CENTERTEXT=.false.
character(len=20) :: P_FONT='SIMPLEX'

integer,parameter   :: P_MAXVERTS=9999
logical,save        :: P_inpolygon=.false.
integer,save        :: P_polyvertex=1
real,save           :: P_polypoints(2,P_MAXVERTS)

real,save    :: P_viewport_left=0.0
real,save    :: P_viewport_right=0.0
real,save    :: P_viewport_bottom=0.0
real,save    :: P_viewport_top=0.0

real,save    :: P_window_left=0.0
real,save    :: P_window_right=0.0
real,save    :: P_window_bottom=0.0
real,save    :: P_window_top=0.0

real,save    :: P_a, P_b, P_c, P_d ! factors for mapping between viewport coordinates and world coordinates

integer,save,public,allocatable :: P_pixel(:,:)
integer,save,public :: P_ColorMap(3,0:255)
logical,save,public :: P_debug=.false.

!data P_ColorMap(1:3,0)     / 0,0,0 /
data P_ColorMap(1:3,0:16) / &
       255,255,255, &  !white
       255,  0,  0, &  !red
         0,255,  0, &  !green
       255,255,  0, &  !yellow
         0,  0,255, &  !blue
       255,  0,255, &  !magenta
         0,255,255, &  !cyan
         0,  0,  0, &  !black
         0,155,  0, &
       155,155,155, &
       155,255,255, &
       155,155,  0, &
         0,  0,155, &
       155,  0,155, &
         0,155,155, &
       100,100,100, &
       155,100,100/, &
     P_ColorMap(1:3,17:255) / 717*255 /
!==================================================================================================================================!
! mapping
public  :: viewport            ! define viewport into screen units
public  :: getviewport         ! query viewport in screen units
public  :: ortho2              ! define window area in virtual world to map to viewport
public  :: page                ! define window area in virtual world as big as possible with one-to-one correspondence
!public  :: getviewport         ! returns limits of current viewport in screen coordinates
public  :: getdisplaysize      ! returns the width and height of the device in pixels
! draw routines
public  :: move2               ! move current position
public  :: rmove2              ! relative move current position
public  :: draw2               ! draw from current position to specified point
public  :: rdraw2              ! relative draw from current position to specified point
public  :: line                ! draw line between two points
public  :: polyline2           ! draw polyline2
public  :: point2              ! draw a point
! polygons
public  :: rect                ! draw rectangle
public  :: circle              ! draw circle
public  :: makepoly            ! start polygon composed of a move and draws
public  :: closepoly           ! end polygon started by P_makepoly(3f)
public  :: poly2               ! fill a polygon given an array of (x,y) points
! arcs
public  :: arc                 ! arc(x, y, radius, startang, endang)| Draw an arc in world units
public  :: circleprecision     ! set circle precision
! text
public  :: hershey             ! draw a software text string
public  :: justfy
public  :: font
public  :: strlength           ! length of string in world coordinates
public  :: drawstr             ! draw the text in string at the current position
interface drawstr
   module procedure msg_scalar, msg_one
end interface drawstr
public  :: drawchar            ! draw a character at the current position
public  :: textsize            ! set text size in world units
public  :: textang             ! set text angle
public  :: centertext          ! set text centering mode
public  :: xcentertext         ! set text centering mode in X direction
public  :: ycentertext         ! set text centering mode in Y direction
! attributes
public  :: linewidth           ! set default line width
public  :: mapcolor            ! define a color in the color map
public  :: color               ! set current color
! print pixel array
public  :: print_p3            ! print pixel array as a P3 ppm file, replacing output file
public  :: print_p6            ! print pixel array as a P6 ppm file, replacing output file
public  :: print_ppm           ! print pixel array as a P6 ppm file, appending to existing files
public  :: print_ascii         ! print small pixel array as ASCII text
public  :: print_ansi          ! print small pixel array as ANSI escape sequences

public  :: pixel               ! directly set pixel value

public  :: state               ! print graphics state
public  :: vflush              ! flush graphics (NOOP)
public  :: getgp2              ! get current position
public  :: clear               ! set frame all to specified color index
public  :: prefsize            ! set size of pixel array to be created on next call to vinit
public  :: vinit               ! initialize pixel drawing module
public  :: vexit               ! close down pixel drawing module
!==================================================================================================================================!
! EXTRACTED FROM M_UNITS
public  :: cosd, sind, d2r, polar_to_cartesian
! EXTRACTED FROM M_STRINGS
public  :: i2s, lower
! EXTRACTED FROM M_COLOR
public hue                  ! converts a color's components from one color model to another
public rgbmono              ! convert RGB colors to a reasonable grayscale
public closest_color_name
public color_name2rgb
!----------------------------
private hlsrgb              ! convert HLS(hue, lightness, saturation) values to RGB (red, green, blue) components
private hvsrgb              ! given hue, saturation, value calculate red, green, & blue components
private cmyrgb              ! given cyan,magenta, and yellow calculate red,green,blue components
!----------------------------
private rgbhls              ! given red,green,blue calculate hue,lightness, and saturation components
private rgbhvs              ! given red, green, blue calculate hue, saturation and value components
private rgbcmy              ! given red,green,blue calculate cyan,magenta, and yellow components
private rgbyiq              ! given RGB calculate luma, orange-blue chrominance, and  purple-green chrominance
!----------------------------
private rgbval              ! internal routine to ensure a value is in the appropriate range and quadrant
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!    From: talcott!seismo!s3sun!sdcsvax!brian (Brian Kantor)
!    Subject: Hershey Fonts in Fortran 77 part 1 of 2
!    Newsgroups: mod.sources
!    Approved: jpn@panda.UUCP
!
!    Mod.sources:  Volume 4, Issue 25
!    Submitted by: seismo!s3sun!sdcsvax!brian (Brian Kantor)
!
!    The following is a fortran-77 subroutine called 'HERSHEY' which will use the
!    Public-Domain Hershey fonts to draw letters, numbers, and symbols. It is
!    in use here at UCSD in connection with several plotting packages for lettering
!    and for point plotting.
!
!    Part 2 of this distribution contains the BLOCKDATA statements which
!    form the actual fonts themselves, and a description of the format in
!    which they are stored.
!
!    I contacted the authors of this subroutine and obtained their permission to
!    distribute the subroutine. I'm in the process of writing a 'c' subroutine
!    to also use the Hershey data. I will submit that for posting when I'm
!    done.
!
!       Brian Kantor    UCSD Computer Graphics Lab
!                       c/o B-028, La Jolla, CA 92093 (619) 452-6865
!
!       decvax\         brian@sdcsvax.ucsd.edu
!       ihnp4  >---  sdcsvax  --- brian
!       ucbvax/         Kantor@Nosc
!==================================================================================================================================!
      ! offset
      integer :: P_ioff=0
      integer :: P_just1
      integer :: P_just2
      ! adjust
      integer :: P_nchr
      integer :: P_ichr(350)
!==================================================================================================================================!
!     From: talcott!seismo!s3sun!sdcsvax!brian (Brian Kantor)
!     Subject: Hershey Fonts in Fortran 77 part 2 of 2
!     Newsgroups: mod.sources
!     Approved: jpn@panda.UUCP
!
!     Mod.sources:  Volume 4, Issue 26
!     Submitted by: seismo!s3sun!sdcsvax!brian (Brian Kantor)
!
!
!     How it works: The subroutine and data storage assume that you are
!     using a system with 32-bit integers. The character index is used to
!     index into array 'istart'. The resulting starting index is used to
!     begin retrieval from array 'symbcd'.
!
!     Each 32 bit word in 'symbcd' contains two 16 bit fields, which in turn
!     contain three subfields:
!
!       (bit 16 - highest order bit - is zero, then)
!       3-bit-int       pencode
!       6-bit-int       delta-x
!       6-bit-int       delta-y
!
!     pencode is a drawing flag:
!       0 - end of character
!       2 - draw from current position (x,y) to (x+dx, y+dy)
!       3 - move from current position (x,y) to (x+dx, y+dy)
!       other values - undefined

integer      :: j
integer,save :: SYMBCD(4711)
integer,save :: ISTART(432)
integer,save :: SSYMBC(128)
integer,save :: ISSTAR(22)
real,save    :: WIDTH(432)

!  Data for subroutine HERSHEY providing 4 fonts, special
!  mathematical symbols, and centered symbols for data point plotting
!  Taken from Wolcott, NBS Publication
!  Modified by A.Chave, R.L.Parker, and L.Shure, IGPP/UCSD Aug 1981,Feb 1982
!
!  APPENDED FROM HERE -----
!
!  Wolcott's BLOCKDATA statement reordered for subroutine HERSHEY.
!  The new ordering is as follows:
!   The symbol numbers are:
!   1-26   UPPER CASE ROMAN SIMPLEX
!  27-52   LOWER CASE ROMAN SIMPLEX

!  53-72   SIMPLEX NUMBERS AND SYMBOLS

!  73-96   UPPER CASE GREEK SIMPLEX
!  97-120  LOWER CASE GREEK SIMPLEX

!  121-146 UPPER CASE ROMAN COMPLEX
!  147-172 LOWER CASE ROMAN COMPLEX

!  173-192 COMPLEX NUMBERS AND SYMBOLS

!  193-216 UPPER CASE GREEK COMPLEX
!  217-240 LOWER CASE GREEK COMPLEX

!  241-266 UPPER CASE ROMAN ITALIC
!  267-292 LOWER CASE ROMAN ITALIC

!  293-312 ITALIC NUMBERS AND SYMBOLS

!  313-338 UPPER CASE ROMAN DUPLEX
!  339-364 LOWER CASE ROMAN DUPLEX

!  365-384 DUPLEX NUMBERS AND SYMBOLS

!  385-432 SPECIAL MATHEMATICAL SYMBOLS
!
data (symbcd(j),  j=1,  114)/ &
     & 443556555, 443557579, 432612882,         0, 433070987, 433071584,  &
     & 323987166, 328083226, 325854871, 317404054, 317400725, 325723922,  &
     & 327657165, 323364299, 298156032, 462268125, 321889760, 309339231,  &
     & 300852123, 296493907, 298329038, 304489675, 317040204, 325527312,  &
     &         0, 433070987, 433071456, 319792797, 325953304, 327788240,  &
     & 323429900, 312845195,         0, 433070987, 433071840, 432743830,  &
     & 432383691,         0, 433070987, 433071840, 432743830,         0,  &
     & 462268125, 321889760, 309339231, 300852123, 296493907, 298329038,  &
     & 304489675, 317040204, 325527312, 327792083, 327778304, 433070987,  &
     & 462432011, 432744214,         0, 433070987,         0, 449848720,  &
     & 312911116, 306553867, 298197837, 294134546,         0, 433070987,  &
     & 462431122, 443262731,         0, 433070987, 432383627,         0,  &
     & 433070987, 433071499, 466625931, 466626443,         0, 433070987,  &
     & 433071883, 462432011,         0, 443556959, 300852123, 296493907,  &
     & 298329038, 304489675, 317040204, 325527312, 329885528, 328050397,  &
     & 321889760, 309329920, 433070987, 433071584, 323987166, 328083225,  &
     & 325822102, 317367189,         0, 443556959, 300852123, 296493907,  &
     & 298329038, 304489675, 317040204, 325527312, 329885528, 328050397,  &
     & 321889760, 309343631, 327450624, 433070987, 433071584, 323987166/

data (symbcd(j),  j =    115,   228)/ &
     & 328083226, 325854871, 317399958, 447424267,         0, 460236383,  &
     & 315630752, 300917597, 296592281, 300688471, 317367892, 323593937,  &
     & 325527116, 314942603, 300294990,         0, 441459851, 426780256,  &
     &         0, 433070993, 300360780, 310748555, 321267406, 327722784,  &
     &         0, 426779851, 460334283,         0, 428876875, 449848395,  &
     & 449849035, 470820555,         0, 430974667, 460333899,         0,  &
     & 426779862, 308655840, 309002240, 460333899, 430974688, 430286539,  &
     &         0, 443556555, 443557579, 432612882,         0, 433070987,  &
     & 433071584, 323987166, 328083226, 325854871, 317404054, 317400725,  &
     & 325723922, 327657165, 323364299, 298156032, 433070987, 433071776,  &
     &         0, 443556555, 443557579, 426092235,         0, 433070987,  &
     & 433071840, 432743830, 432383691,         0, 460333899, 430974688,  &
     & 430286539,         0, 433070987, 462432011, 432744214,         0,  &
     & 443556959, 300852123, 296493907, 298329038, 304489675, 317040204,  &
     & 325527312, 329885528, 328050397, 321889760, 309343382, 319488000,  &
     & 433070987,         0, 433070987, 462431122, 443262731,         0,  &
     & 443556555, 443557579,         0, 433070987, 433071499, 466625931,  &
     & 466626443,         0, 433070987, 433071883, 462432011,         0,  &
     & 428877472, 436938134, 428189323,         0, 443556959, 300852123/

data (symbcd(j),  j =    229,   342)/ &
     & 296493907, 298329038, 304489675, 317040204, 325527312, 329885528,  &
     & 328050397, 321889760, 309329920, 433070987, 462432011, 433071904,  &
     &         0, 433070987, 433071584, 323987166, 328083225, 325822102,  &
     & 317367189,         0, 428877014, 293974816, 324023051, 323321856,  &
     & 441459851, 426780256,         0, 428712733, 296723360, 303047775,  &
     & 307143897, 308655771, 323921503, 319825312, 313500957, 309100544,  &
     & 445654283, 441295834, 298623831, 296362898, 300459152, 315106897,  &
     & 323561172, 325822105, 321725851, 307068928, 430974667, 430286560,  &
     &         0, 447751499, 428680026, 298623957, 302621778, 310945169,  &
     & 321463955, 325756697, 330114970,         0, 430285899, 298394454,  &
     & 296559517, 303015136, 313533983, 323921626, 325789330, 317040331,  &
     &         0, 455910987, 455812568, 313304217, 302785430, 296330065,  &
     & 298263564, 306554187, 317072974,         0, 433070987, 432743448,  &
     & 307012953, 317466198, 323593873, 321332684, 312845451, 302392206,  &
     &         0, 455812568, 313304217, 302785430, 296330065, 298263564,  &
     & 306554187, 317072974,         0, 456140363, 455812568, 313304217,  &
     & 302785430, 296330065, 298263564, 306554187, 317072974,         0,  &
     & 430548563, 321562135, 317465945, 307012632, 298525523, 296264590,  &
     & 302392459, 312845772, 321323008, 445654176, 303014876, 300266265/

data (symbcd(j),  j =    343,   456)/ &
     & 309100544, 455910985, 318973381, 312616068, 302167638, 317465945,  &
     & 307012632, 298525523, 296264590, 302392459, 312845772, 321323008,  &
     & 433070987, 432710744, 309110169, 319563349, 321224704, 430973855,  &
     & 300950433, 296760217, 298156032, 435168287, 305144865, 300954649,  &
     & 302261189, 295838404,         0, 433070987, 453813135, 441034315,  &
     &         0, 433070987,         0, 432841611, 432710744, 309110169,  &
     & 319563349, 321238613, 327952281, 338471128, 344631563,         0,  &
     & 432841611, 432710744, 309110169, 319563349, 321224704, 441230360,  &
     & 298525523, 296264590, 302392459, 312845772, 321332881, 323593814,  &
     & 317465945, 307003392, 432841604, 432743448, 307012953, 317466198,  &
     & 323593873, 321332684, 312845451, 302392206,         0, 455910980,  &
     & 455812568, 313304217, 302785430, 296330065, 298263564, 306554187,  &
     & 317072974,         0, 432841611, 432645078, 304882905, 315392000,  &
     & 453715416, 311207001, 298591062, 298460179, 313075153, 319268366,  &
     & 317072651, 304456588, 296157184, 435168207, 302392459, 310752025,  &
     & 309100544, 432841615, 300295243, 310748556, 321369689, 321224704,  &
     & 428647563, 453813387,         0, 430744651, 447521867, 447522379,  &
     & 464299595,         0, 430745099, 453813067,         0, 428647563,  &
     & 453813387, 302228357, 293741252,         0, 453813067, 430745113/

data (symbcd(j),  j =    457,   570)/ &
     & 430286347,         0, 443327576, 300622740, 296264526, 298198027,  &
     & 306554124, 317171282, 325789465, 443327833, 315368918, 321332876,  &
     & 325429003,         0, 449848607, 307143705, 300622738, 296100612,  &
     & 449848864, 323954331, 321693208, 315335895, 443262294, 317335058,  &
     & 319268301, 314975499, 306553868, 300327824,         0, 426451800,  &
     & 300721177, 306980055, 311043344, 308655833, 323692116, 308651079,  &
     & 302120960, 447521945, 302785430, 296330064, 298230732, 304456907,  &
     & 312878542, 319333908, 317433177, 309175453, 307209440, 313533919,  &
     & 321814528, 451650968, 311207001, 300688342, 302654675, 443130834,  &
     & 296231758, 298198027, 308651340, 317128704, 445654175, 305079389,  &
     & 307111259, 319665691, 311206999, 298459985, 296199053, 302359753,  &
     & 310617349, 308421700, 302186496, 426418967, 298624025, 304882774,  &
     & 302588811, 436806806, 311174553, 319596183, 323626575, 314703872,  &
     & 426418967, 298624025, 304882774, 302556174, 304489611, 310748556,  &
     & 319268433, 323626713, 325985951, 319825312, 313468252, 315401750,  &
     & 323626834,         0, 437035922, 296166220, 298165259, 306619599,  &
     &         0, 437035787, 457975385, 319595928, 306848787, 300528595,  &
     & 304686225, 310781259, 314942924,         0, 426779488, 300917790,  &
     & 319141017, 293961728, 439132868, 436904912, 300328011, 308651340/

data (symbcd(j),  j =    571,   684)/ &
     & 317138514, 460105298, 319235596, 321234635, 329688975,         0,  &
     & 430744601, 300524430, 296072857, 321594900, 315139278, 302392139,  &
     &         0, 445654175, 305079389, 307111259, 319665499, 307045401,  &
     & 300655573, 304719122, 315176210, 302556048, 296166220, 300229832,  &
     & 310617349, 306324484,         0, 441230360, 298525523, 296231821,  &
     & 300295243, 308651340, 317138449, 319432151, 315368729, 307003392,  &
     & 443327435, 453813843, 323430091, 428549016, 304916377,         0,  &
     & 432645008, 300327948, 306554123, 314975758, 321431124, 319530456,  &
     & 313304281, 304882646, 298427012,         0, 462202009, 302785430,  &
     & 296330064, 298230732, 304456907, 312878542, 319333908, 317433240,  &
     & 311197696, 447521931, 428549016, 304916249,         0, 426418967,  &
     & 298624025, 304882774, 300426189, 304456907, 314975758, 323561174,  &
     & 325877760, 441197591, 298492754, 296199053, 300295243, 310748620,  &
     & 323430161, 329918295, 325887577, 317433171, 308749316,         0,  &
     & 428647321, 302753158, 318908036, 460105367, 319431561, 293806788,  &
     &         0, 458237060, 426418967, 298624025, 304882774, 302556174,  &
     & 304489675, 312845836, 323430161, 332081113,         0, 441230360,  &
     & 298492754, 296199052, 300262475, 308684111, 449422671, 314975691,  &
     & 321234636, 329754514, 332048216, 327974912, 445653835, 445654731/

data (symbcd(j),  j =    685,   798)/ &
     & 445556363, 434677265, 426091595, 451258187,         0, 435168203,  &
     & 437265419, 428877344, 326084382, 330180442, 327952087, 319501856,  &
     & 323987166, 328083226, 325854871, 319501334, 319497941, 327821138,  &
     & 329754381, 325461515, 293975574, 323659476, 327755535, 325494412,  &
     & 319127552, 460236570, 328214237, 321889696, 311436383, 300852123,  &
     & 296493907, 298329038, 304489739, 314943052, 325527312, 445654175,  &
     & 302949339, 298591123, 300426254, 306586891,         0, 435168203,  &
     & 437265419, 428877216, 321890013, 328050520, 329885456, 325527116,  &
     & 314942219, 449848863, 323921627, 327952147, 325592718, 319169931,  &
     &         0, 435168203, 437265419, 449652114, 428877600, 328017632,  &
     & 436938134, 428189451, 327722699,         0, 435168203, 437265419,  &
     & 449652114, 428877600, 328017632, 436938134, 428188875,         0,  &
     & 460236570, 328214237, 321889696, 311436383, 300852123, 296493907,  &
     & 298329038, 304489739, 314943052, 325530912, 307209245, 300786584,  &
     & 298427344, 302457996, 310752979, 325433107, 327530003, 334069760,  &
     & 435168203, 437265419, 462432011, 464529227, 428877024, 456140832,  &
     & 436938518, 428188875, 455452683,         0, 435168203, 437265419,  &
     & 428877024, 428188875,         0, 445654287, 308683851, 300262220,  &
     & 294069008, 296264592, 296203488, 308782220, 304460832, 317718528/

data (symbcd(j),  j =    799,   912)/ &
     & 435168203, 437265419, 464528403, 447457099, 445359883, 428877024,  &
     & 456140768, 428188875, 455452619,         0, 435168203, 437265419,  &
     & 428877024, 428189387, 325625483,         0, 435168203, 437265806,  &
     & 435168651, 464528779, 464529227, 466626443, 428876832, 464529504,  &
     & 428188811, 457549899,         0, 435168203, 437266189, 437200651,  &
     & 462432011, 428876832, 456140768, 428188811,         0, 445654111,  &
     & 300852123, 296461140, 298329038, 304489739, 314943052, 325527312,  &
     & 329918295, 328050397, 321889696, 311440672, 307209245, 300786583,  &
     & 298460112, 302457996, 310752651, 319170190, 325592852, 327919323,  &
     & 323921439, 315621376, 435168203, 437265419, 428877344, 326084382,  &
     & 330180441, 327919318, 319464469, 454043295, 326051612, 327984855,  &
     & 323692053, 428188875,         0, 445654111, 300852123, 296461140,  &
     & 298329038, 304489739, 314943052, 325527312, 329918295, 328050397,  &
     & 321889696, 311440672, 307209245, 300786583, 298460112, 302457996,  &
     & 310752651, 319170190, 325592852, 327919323, 323921439, 315634765,  &
     & 304555152, 310945105, 317203982, 321103494, 327362376, 329561614,  &
     & 321201800, 325297927, 329515008, 435168203, 437265419, 428877344,  &
     & 326084382, 330180442, 327952087, 319497238, 454043295, 326051612,  &
     & 328017624, 323724822, 428188875, 447423957, 319432397, 327558988/

data (symbcd(j),  j =    913,   1026)/ &
     & 331789781, 319399564, 325429067, 331786126,         0, 458139360,  &
     & 325920413, 319792480, 307241951, 296657755, 298623960, 304850389,  &
     & 321529554, 430810073, 304883158, 321562260, 325658318, 321267083,  &
     & 308651020, 298263377, 296067982,         0, 443557067, 445654283,  &
     & 430973722, 294659808, 325920416, 436577739,         0, 435168209,  &
     & 302457996, 312845771, 323364622, 329820000, 437265425, 304555212,  &
     & 312849184, 309343904, 336592896, 430974219, 433071374, 460334347,  &
     & 426779744, 451946336,         0, 433071243, 435168400, 449848459,  &
     & 449848971, 451946128, 466626187, 426779808, 460335200,         0,  &
     & 430974603, 433071819, 460333899, 426779744, 451946336, 426091595,  &
     & 451258187,         0, 430974229, 310752160, 313173323, 462431573,  &
     & 426779744, 454043552, 438674955,         0, 458236747, 460333963,  &
     & 433070938, 296756960, 430286539, 325625483,         0, 445653835,  &
     & 445654731, 445556363, 434677265, 426091595, 451258187,         0,  &
     & 435168203, 437265419, 428877344, 326084382, 330180442, 327952087,  &
     & 319501856, 323987166, 328083226, 325854871, 319501334, 319497941,  &
     & 327821138, 329754381, 325461515, 293975574, 323659476, 327755535,  &
     & 325494412, 319127552, 435168203, 437265419, 428877536, 325920416,  &
     & 428188875,         0, 445653771, 445654795, 445556427, 430319308/

data (symbcd(j),  j =    1027,   1140)/ &
     & 428189451,         0, 435168203, 437265419, 449652114, 428877600,  &
     & 328017632, 436938134, 428189451, 327722699,         0, 458236747,  &
     & 460333963, 433070938, 296756960, 430286539, 325625483,         0,  &
     & 435168203, 437265419, 462432011, 464529227, 428877024, 456140832,  &
     & 436938518, 428188875, 455452683,         0, 445654111, 300852123,  &
     & 296461140, 298329038, 304489739, 314943052, 325527312, 329918295,  &
     & 328050397, 321889696, 311440672, 307209245, 300786583, 298460112,  &
     & 302457996, 310752651, 319170190, 325592852, 327919323, 323921439,  &
     & 315634841, 306787865, 319370390, 319501461, 319455232, 435168203,  &
     & 437265419, 428877024, 428188875,         0, 435168203, 437265419,  &
     & 464528403, 447457099, 445359883, 428877024, 456140768, 428188875,  &
     & 455452619,         0, 445653835, 445654731, 445556363, 426091595,  &
     & 451258187,         0, 435168203, 437265806, 435168651, 464528779,  &
     & 464529227, 466626443, 428876832, 464529504, 428188811, 457549899,  &
     &         0, 435168203, 437266189, 437200651, 462432011, 428876832,  &
     & 456140768, 428188811,         0, 433103708, 464561948, 441197651,  &
     & 455878163, 432513866, 463972106, 433039135, 433006366, 441132566,  &
     & 441099797, 432449293, 432416524,         0, 445654111, 300852123,  &
     & 296461140, 298329038, 304489739, 314943052, 325527312, 329918295/

data (symbcd(j),  j =    1141,   1254)/ &
     & 328050397, 321889696, 311440672, 307209245, 300786583, 298460112,  &
     & 302457996, 310752651, 319170190, 325592852, 327919323, 323921439,  &
     & 315621376, 435168203, 437265419, 462432011, 464529227, 428877856,  &
     & 428188875, 455452683,         0, 435168203, 437265419, 428877344,  &
     & 326084382, 330180441, 327919318, 319464469, 454043295, 326051612,  &
     & 327984855, 323692053, 428188875,         0, 430974230, 293974816,  &
     & 309015328, 326117146, 324023116, 323367691, 325429009, 323321856,  &
     & 443557067, 445654283, 430973722, 294659808, 325920416, 436577739,  &
     &         0, 428712733, 296723360, 303047775, 307143897, 308654877,  &
     & 298820639, 307148507, 326018719, 321922528, 315598173, 311207179,  &
     & 460236383, 317695325, 436577739,         0, 445654283, 447751499,  &
     & 441295834, 298623831, 296362898, 300459152, 317204113, 325658388,  &
     & 327919321, 323823067, 307082395, 302851033, 298558356, 300491793,  &
     & 306722256, 321431186, 325723863, 323790426, 317568096, 319829067,  &
     & 319127552, 430974603, 433071819, 460333899, 426779744, 451946336,  &
     & 426091595, 451258187,         0, 447751499, 449848715, 428647258,  &
     & 300721173, 304718994, 310948698, 298623957, 302621778, 310945233,  &
     & 323561171, 327853913, 332215761, 321463955, 325756697, 332212185,  &
     & 441460320, 440772171,         0, 430384011, 306553871, 298427222/

data (symbcd(j),  j =    1255,   1368)/ &
     & 296559517, 303015136, 317728415, 328116058, 329983763, 323462667,  &
     & 327526222, 436708306, 298525594, 300852319, 309343712, 321890013,  &
     & 328017686, 325658255, 432415820, 455485196,         0, 434873302,  &
     & 298525591, 300688473, 313304536, 319530581, 321332876, 325432855,  &
     & 319235660, 325429003, 453682644, 304718738, 296231758, 298198091,  &
     & 310748556, 319239251, 300491664, 298263500, 304447488, 435168203,  &
     & 437265419, 436937880, 311207321, 321660630, 327788305, 325527116,  &
     & 314942731, 306586638, 449619480, 323692243, 325625486, 319169931,  &
     & 428876832,         0, 455812629, 321529493, 323692056, 315401433,  &
     & 302785430, 296330065, 298263564, 308651339, 319170190, 443327576,  &
     & 300622739, 298361806, 304489675,         0, 456140363, 458237579,  &
     & 455812568, 313304281, 302785430, 296330065, 298263564, 308651339,  &
     & 317072974, 443327576, 300622739, 298361806, 304489675, 449848992,  &
     & 455452491,         0, 432645779, 323659351, 319563161, 309109784,  &
     & 298525523, 296264590, 302392523, 312845836, 323434067, 321594904,  &
     & 443327576, 300622739, 298361806, 304489675,         0, 445621470,  &
     & 311338334, 313500960, 307242015, 300852171, 441459807, 302949387,  &
     & 428647705, 428188875,         0, 441230360, 300655509, 298427345,  &
     & 302523535, 310879632, 317236755, 319464919, 315368729, 307016728/

data (symbcd(j),  j =    1369,   1482)/ &
     & 300622802, 302527888, 317269462, 315373015, 319563417, 323757592,  &
     & 434676624, 296166221, 298165322, 314910281, 323236685, 298198091,  &
     & 314943050, 323233415, 321037700, 302129989, 293839624, 296035339,  &
     &         0, 435168203, 437265419, 436937880, 313304537, 323757782,  &
     & 325432793, 321660566, 323334944, 303051531, 308655563, 331710464,  &
     & 435168159, 300885023, 300954585, 300266521, 302363417, 302822155,  &
     & 308641792, 437265375, 302982239, 303051865, 304325637, 297935620,  &
     & 291676870, 293839686, 293778457, 302228421, 297939801, 304906240,  &
     & 435168203, 437265419, 458007567, 447325899, 445228683, 428876832,  &
     & 451716953, 428188875, 451258187,         0, 435168203, 437265419,  &
     & 428876832, 428188875,         0, 434938827, 437036043, 436937880,  &
     & 313304537, 323757782, 325432793, 321660566, 323335894, 330049561,  &
     & 340568408, 348858763, 474786072, 346761547, 428647449, 428188875,  &
     & 451258251, 474327627,         0, 434938827, 437036043, 436937880,  &
     & 313304537, 323757782, 325432793, 321660566, 323334937, 302822155,  &
     & 308655563, 331710464, 443327512, 298525523, 296264590, 302392523,  &
     & 312845836, 323430097, 325691030, 319563097, 309114073, 304882646,  &
     & 298427281, 300360780, 308655435, 317072974, 323528339, 321594840,  &
     & 313294848, 434938820, 437036036, 436937880, 311207321, 321660630/

data (symbcd(j),  j =    1483,   1596)/ &
     & 327788305, 325527116, 314942731, 306586638, 449619480, 323692243,  &
     & 325625486, 319169931, 428647449, 427959492,         0, 455910980,  &
     & 458008196, 455812568, 313304281, 302785430, 296330065, 298263564,  &
     & 308651339, 317072974, 443327576, 300622739, 298361806, 304489675,  &
     & 448931652,         0, 434938827, 437036043, 436839510, 309077337,  &
     & 319596120, 321627670, 317433368, 428647449, 428188875,         0,  &
     & 451651097, 319464919, 315368729, 302818200, 296461141, 298460179,  &
     & 313042384, 319271766, 298492948, 313075153, 319301133, 317072715,  &
     & 304456652, 298230607, 296067981,         0, 435168207, 302392459,  &
     & 310748556, 317142048, 302490700, 306557721, 311197696, 434938830,  &
     & 302392523, 312845836, 323433497, 302457932, 308655769, 323335897,  &
     & 325432089, 302822873, 325891723, 331710464, 430744779, 432841933,  &
     & 455910603, 426550361, 447522521,         0, 432841867, 434939022,  &
     & 449619083, 449619595, 451716750, 466396811, 426550425, 460105817,  &
     &         0, 432842315, 434939531, 458007435, 428647577, 449619737,  &
     & 428188811, 449160971,         0, 432841995, 434939149, 458007819,  &
     & 306422789, 297935684, 293774150, 297972505, 307017113, 327974912,  &
     & 453813067, 455910283, 432841557, 296527449, 430286411, 321365515,  &
     &         0, 445424728, 300622740, 296264526, 298198091, 308651340/

data (symbcd(j),  j =    1597,   1710)/ &
     & 319268498, 327886681, 445424792, 302719956, 298361742, 300295243,  &
     & 445425049, 319563350, 325527308, 329627033, 317466134, 323430092,  &
     & 329623435,         0, 451945759, 307143705, 300622738, 296100612,  &
     & 451945823, 309240921, 302719954, 298197828, 451946080, 326084382,  &
     & 328050393, 323757527, 309048928, 326051547, 323790424, 317437143,  &
     & 317400660, 323561103, 321299980, 312845515, 304489485, 300430551,  &
     & 315303444, 321463887, 319202764, 312836096, 426451800, 300721241,  &
     & 309077271, 313140560, 310780996, 428581784, 306980119, 462202582,  &
     & 323626317, 306455556, 460105366, 321529165,         0, 451683673,  &
     & 309109784, 298492754, 296199053, 300295243, 308651404, 319268434,  &
     & 321562135, 311305438, 309339425, 315663904, 323957977, 304882645,  &
     & 298394510, 300299467, 312878543, 319366678, 317465947, 311338271,  &
     & 313533920, 323944448, 455812568, 313304153, 300688342, 304751891,  &
     & 439133208, 302720148, 311014675, 300491600, 296166284, 304456971,  &
     & 314975758, 445228050, 298328974, 300295243,         0, 447751391,  &
     & 307176605, 309208475, 325953244, 319661337, 304849812, 296264527,  &
     & 298230859, 310682951, 312648964, 306324549, 449651863, 300557201,  &
     & 298296269, 304447488, 426418967, 298624089, 306979990, 304686027,  &
     & 437036120, 304817170, 298169426, 309011800, 317498969, 325854999/

data (symbcd(j),  j =    1711,   1824)/ &
     & 327821007, 318912089, 325822164, 323462596,         0, 426418967,  &
     & 298624089, 306979990, 304653390, 306586827, 437036120, 304817169,  &
     & 302457932, 308651339, 317072974, 325625620, 330082141, 328181408,  &
     & 319825310, 315499993, 321595092, 331953612, 321365649, 325723929,  &
     & 328115935, 324009984, 437035922, 296166220, 298165323, 308716815,  &
     & 439133138, 298263436, 300253184, 437035787, 439133003, 458008280,  &
     & 327952089, 321693144, 308946003, 300528723, 308880716, 314946643,  &
     & 306783500, 312845771, 321267407,         0, 430973920, 305112222,  &
     & 309208654, 323364555, 435168350, 307111438, 321267403, 327529753,  &
     & 293975321, 296058880, 439132868, 441230084, 439034896, 302425227,  &
     & 310748556, 319235729, 462202446, 321267339, 329623501, 336050009,  &
     & 323430028, 325419008, 437035915, 439133203, 300360587, 460105365,  &
     & 319338265, 325789332, 319333775, 308716620, 298169177, 304906240,  &
     & 447751391, 307176605, 309208475, 321762715, 307045401, 300655573,  &
     & 304719122, 317273499, 309142617, 302752789, 306816274, 445195281,  &
     & 298328910, 296100810, 310650183, 312648900, 304231698, 304653264,  &
     & 298263436, 302327048,         0, 443327512, 298492754, 296199053,  &
     & 300295243, 308651404, 319268434, 321562135, 317465945, 309114073,  &
     & 304882645, 298394510, 300299467, 312878543, 319366678, 317456384/

data (symbcd(j),  j =    1825,   1938)/ &
     & 443294667, 443294731, 455878219, 455878283, 428549016, 304916377,  &
     & 428549015, 304883608,         0, 432546765, 302392459, 310748620,  &
     & 321365650, 323659351, 319563161, 311207000, 300589970, 289551627,  &
     & 314975759, 321463894, 319567129, 306979861, 300491460,         0,  &
     & 464299225, 302785429, 296297295, 298230732, 304456907, 314975759,  &
     & 321463893, 319530456, 313308377, 304882645, 298394510, 300299467,  &
     & 312878543, 319366678, 317470168, 330039296, 447489163, 447489227,  &
     & 428549016, 304916249, 428549015, 304883480,         0, 426418967,  &
     & 298624089, 306979990, 302523405, 306557977, 304882774, 300426189,  &
     & 302392459, 308651404, 319235729, 325723863, 323790424, 323725012,  &
     & 457746135,         0, 441197591, 298492754, 296199053, 300295243,  &
     & 310748620, 323430161, 329918295, 325887577, 317433171, 308749316,  &
     & 430416845, 304489740, 317105807, 327726935, 325854808, 317400403,  &
     & 308716612,         0, 428647321, 302785622, 314811845, 318911385,  &
     & 300688406, 312714629, 318908036, 460105367, 319431561, 293806788,  &
     &         0, 456139972, 458237060, 426418967, 298624089, 306979990,  &
     & 304653390, 308684172, 319203024, 329888793, 304882774, 302556174,  &
     & 304489675, 314942988, 323430161, 329885657,         0, 432710679,  &
     & 309077145, 302785429, 296297295, 298197963, 304456908, 312976786/

data (symbcd(j),  j =    1939,   2052)/ &
     & 430416781, 300295244, 308716879, 447292751, 314975691, 321234636,  &
     & 329754514, 332048216, 327984856, 330016661, 447194509, 317072972,  &
     & 325494607,         0, 451945099, 451945995, 449783243, 432580049,  &
     & 419799947, 444966539,         0, 443556683, 445653899, 437266144,  &
     & 332376029, 334342040, 330016406, 460334943, 332310427, 330049303,  &
     & 323695702, 323692309, 329885521, 327624332, 314942091, 457909973,  &
     & 327788305, 325527116, 314933248, 462366558, 332408666, 330180382,  &
     & 326084192, 315630815, 305046490, 298558291, 296231821, 300295307,  &
     & 312845772, 321332880, 449848607, 307143706, 300655507, 298329037,  &
     & 302392459,         0, 443556683, 445653899, 437266016, 328181598,  &
     & 332244887, 329885391, 321299916, 308650635, 456140511, 328148827,  &
     & 330016531, 323462669, 314975435,         0, 443556683, 445653899,  &
     & 453846418, 437266400, 332212128, 439035350, 423994955, 325592587,  &
     &         0, 443556683, 445653899, 453846418, 437266400, 332212128,  &
     & 439035350, 423994443,         0, 462366558, 332408666, 330180382,  &
     & 326084192, 315630815, 305046490, 298558291, 296231821, 300295307,  &
     & 310748620, 321332946, 449848607, 307143706, 300655507, 298329037,  &
     & 302392459, 444966284, 319235730, 451487634,         0, 443556683,  &
     & 445653899, 470820491, 472917707, 437265888, 464529696, 439035734/

data (symbcd(j),  j =    2053,   2166)/ &
     & 423994443, 451258251,         0, 443556683, 445653899, 437265888,  &
     & 423994443,         0, 456140047, 308716684, 302359435, 294003406,  &
     & 292037393, 296231695, 454042831, 306619403, 447751968,         0,  &
     & 443556683, 445653899, 472917011, 451651275, 449554059, 437265888,  &
     & 464529632, 423994443, 451258187,         0, 443556683, 445653899,  &
     & 437265888, 423994955, 325625355,         0, 443556683, 443557131,  &
     & 445654349, 472917259, 472917707, 475014923, 437265696, 472918368,  &
     & 423994379, 453355467,         0, 443556683, 443557518, 443459211,  &
     & 470820491, 437265632, 464529632, 423994379,         0, 449848543,  &
     & 305046490, 298558291, 296231821, 300295243, 310748620, 321332945,  &
     & 327821144, 330147614, 326084192, 315635104, 311403677, 302851031,  &
     & 298427280, 300328011, 444966284, 319235729, 325723928, 328050398,  &
     & 321912832, 443556683, 445653899, 437266208, 334473245, 336439256,  &
     & 329983573, 304789280, 332376029, 334342040, 327886421, 423994443,  &
     &         0, 449848543, 305046490, 298558291, 296231821, 300295243,  &
     & 310748620, 321332945, 327821144, 330147614, 326084192, 315635104,  &
     & 311403677, 302851031, 298427280, 300328011, 444966284, 319235729,  &
     & 325723928, 328050398, 321926093, 300360720, 306750673, 313009550,  &
     & 314811846, 321070728, 323270030, 316941831, 321103496,         0/

data (symbcd(j),  j =    2167,   2280)/ &
     & 443556683, 445653899, 437266144, 332376029, 334342040, 330016406,  &
     & 304821984, 330278813, 332244824, 327919254, 449521173, 321529484,  &
     & 325429067, 331786126, 455747277, 327558988, 331788939, 304447488,  &
     & 464463774, 334505882, 332277598, 328181344, 313533599, 302949403,  &
     & 304915608, 321529554, 437101721, 321562260, 325658319, 323397196,  &
     & 314942603, 300295053, 296198993, 293970765, 298221568, 451945547,  &
     & 454042763, 439362458, 303048672, 332212128, 432383307,         0,  &
     & 441459669, 298361742, 300295307, 314943052, 325527313, 336606432,  &
     & 302687185, 300360716, 306557920, 315635552, 342884352, 437265483,  &
     & 439362701, 466625611, 433071392, 458237984,         0, 441459723,  &
     & 443556941, 458236939, 458237451, 460334669, 475014667, 435168672,  &
     & 468724064,         0, 439363083, 441460299, 468722379, 435168608,  &
     & 460335200, 421897163, 447063755,         0, 437265686, 304460896,  &
     & 313205899, 468723030, 433071392, 460335200, 432383307,         0,  &
     & 466625227, 468722443, 441459674, 305145824, 426092107, 325625355,  &
     &         0, 466527124, 331710464, 432973716, 298156032, 455747095,  &
     & 317465945, 309109784, 298492754, 296199053, 300295243, 308651404,  &
     & 319235665, 323692187, 321857055, 315630816, 305112094, 302949469,  &
     & 305083609, 304882645, 298394510, 300299467, 312878542, 319333974/

data (symbcd(j),  j =    2281,   2394)/ &
     & 321758750, 315621376, 428877067, 430974221, 462431499, 428877600,  &
     & 430941919,         0, 453780889, 309109784, 298525523, 296231821,  &
     & 300295307, 312845772, 443327576, 300622739, 298329037, 302392459,  &
     & 432612754,         0, 466625433, 331953040, 331887499, 331710464,  &
     & 433072025, 298398608, 331887499, 331710464, 468166479, 325592658,  &
     & 315303255, 309077080, 300655509, 298427345, 304620752, 313042322,  &
     & 321595096, 330082265,         0, 468821922, 334538786, 336701412,  &
     & 330442467, 321955359, 317597080, 310781128, 306394786, 321922588,  &
     & 315106636, 310682823, 304260036, 295838469, 293806919, 298001221,  &
     &         0, 468821922, 334538786, 336701412, 330442467, 321955359,  &
     & 317597080, 310781128, 306394786, 321922588, 315106636, 310682823,  &
     & 304260036, 295838469, 293806919, 298001221, 447587482, 302785493,  &
     & 300524560, 306652493, 317105806, 327690067, 329951000, 323823067,  &
     & 313360384, 470394833, 329787088, 321431058, 313206039, 306979864,  &
     & 298558293, 296330129, 302523536, 310945106, 319497815, 325855064,  &
     & 334211093, 336166912, 449717643, 432678804, 432383883,         0,  &
     & 449717643, 432940956, 432678804,         0, 432908045, 462267277,  &
     &         0, 451847580, 317564444, 317633428, 336213453, 314975691,  &
     & 319169997,         0, 439493700, 441590916, 479340804, 481438020/

data (symbcd(j),  j =    2395,   2508)/ &
     & 431106660, 430056836, 469903940,         0, 434807700, 300524564,  &
     & 300580864, 430744665, 317109273, 317044772, 317030400, 435299926,  &
     & 297939876, 319501156, 319468388, 345123229, 343028677, 344109956,  &
     & 344074635, 341966848, 447751327, 302916570, 298558290, 296166284,  &
     & 302359691, 312878543, 319333972, 323790493, 321889760, 313537888,  &
     & 309306460, 302851031, 298394510, 300295179, 440771852, 315074001,  &
     & 319432281, 321824287, 317731798, 319488000, 443688035, 303113184,  &
     & 300885020, 304981145, 306947093, 439460897, 303015005, 307111130,  &
     & 309077142, 298460306, 308815054, 306586699, 302294023, 304264211,  &
     & 306750607, 304522252, 300229576, 302195781, 308412416, 435299427,  &
     & 307307744, 309273756, 304981017, 302752917, 439461025, 307209309,  &
     & 302916570, 300688406, 311043090, 300426190, 302392395, 306488455,  &
     & 304264339, 302556175, 304522380, 308618440, 306390085, 300023808,  &
     & 462169818, 321758619, 311239897, 306914451, 308847952, 319301265,  &
     & 325694875, 311207126, 308913425, 313014043, 325691089, 329787344,  &
     & 338241685, 340502618, 336471966, 328181344, 315630815, 305079260,  &
     & 298656599, 296362897, 300393549, 308684171, 321234700, 331786190,  &
     & 464365331, 327722832,         0, 426321109, 325661394, 309012178,  &
     &        0, 298394766, 308651209, 306390020, 300032901, 295936842/

data (symbcd(j),  j =    2509,   2622)/ &
     & 298263570, 306881880, 317498969, 327952214, 329852686, 323364363,  &
     & 317040012, 315041231, 319235533, 455911128, 327886610, 325527180,  &
     &         0, 458008082, 317138380, 319137483, 329688975, 460105298,  &
     & 319235596, 321238546, 319464920, 313304281, 302785429, 296297295,  &
     & 298230732, 304456907, 312878543, 319370457, 304882645, 298394510,  &
     & 300285952, 441459603, 298329037, 302396640, 300528595, 302720152,  &
     & 311207321, 319563351, 323659410, 321365452, 310748299, 302392271,  &
     & 300529176, 321594962, 319268236, 310752224, 309329920, 453715477,  &
     & 321562198, 319563161, 309109784, 298492754, 296199053, 300295243,  &
     & 308651404, 319272153, 304882645, 298394510, 300285952, 462431762,  &
     & 317138380, 319137483, 329688975, 464528978, 319235596, 321238546,  &
     & 319464920, 313304281, 302785429, 296297295, 298230732, 304456907,  &
     & 312878543, 319370457, 304882645, 298394510, 300299872, 330301440,  &
     & 432546961, 313075220, 321594904, 315401433, 302785429, 296297295,  &
     & 298230732, 304456907, 314975758, 443327576, 300589970, 298263500,  &
     &         0, 456107550, 321824414, 323987040, 317728095, 311370972,  &
     & 307012555, 298033989, 451945822, 311305432, 304587787, 300163974,  &
     & 295871172, 287449605, 285418055, 289612357, 432842265,         0,  &
     & 460105163, 314844421, 304227204, 293774022, 291742472, 295936774/

data (symbcd(j),  j =    2623,   2736)/ &
     & 458007947, 312747205, 304231954, 319464920, 313304281, 302785429,  &
     & 296297295, 298230732, 304456907, 312878543, 319370457, 304882645,  &
     & 298394510, 300285952, 441459467, 443556683, 434709590, 309077337,  &
     & 317498968, 323724949, 319268364, 321238489, 321627733, 317171148,  &
     & 319137483, 329688975, 435168480,         0, 443557023, 309273887,  &
     & 309342933, 294364057, 304915608, 306881551, 302392395, 437036120,  &
     & 304784335, 300295179, 308651341, 315064320, 445654239, 311371103,  &
     & 311440149, 296461273, 307012824, 308978699, 300163974, 295871172,  &
     & 287449605, 285418055, 289612357, 439133336, 306881483, 298066758,  &
     & 291635200, 441459467, 443556683, 457975383, 323692247, 325854873,  &
     & 321693144, 308946003, 300528723, 308880716, 314946643, 306783500,  &
     & 312845771, 321267407, 435168480,         0, 441459602, 296166220,  &
     & 298165323, 308716815, 443556818, 298263436, 300266464, 309329920,  &
     & 426418967, 298624089, 306979990, 304686027, 437036120, 304817170,  &
     & 298169426, 309011800, 317498969, 325854999, 327853643, 455911127,  &
     & 325756427, 459876182, 334243929, 342665560, 348891541, 344434956,  &
     & 346405081, 346794325, 342337740, 344304075, 354855567,         0,  &
     & 426418967, 298624089, 306979990, 304686027, 437036120, 304817170,  &
     & 298169426, 309011800, 317498969, 325854999, 327853711, 323364555/

data (symbcd(j),  j =    2737,   2850)/ &
     & 455911127, 325756495, 321267339, 329623501, 336035840, 443327512,  &
     & 298492754, 296199053, 300295243, 308651404, 319268434, 321562135,  &
     & 317465945, 309114073, 304882645, 298394510, 300299467, 312878543,  &
     & 319366678, 317456384, 426418967, 298624089, 306979990, 304685892,  &
     & 437036120, 304817170, 293745746, 306881816, 315401753, 323757783,  &
     & 327853842, 325559884, 314942731, 306586703, 304690840, 325789394,  &
     & 323462668, 314946116, 302120960, 458007812, 460105028, 453584405,  &
     & 317465945, 309109784, 298492754, 296199053, 300295243, 308651340,  &
     & 317171218, 443327576, 300589970, 298263500, 438445572,         0,  &
     & 426418967, 298624089, 306979990, 304686027, 437036120, 304817170,  &
     & 298169426, 309011800, 317498969, 323757719, 321594903, 321650688,  &
     & 453748246, 321594967, 319563097, 307012568, 298558357, 300557712,  &
     & 317174678, 300590481, 317203917, 314975435, 302359372, 294036238,  &
     & 296166221,         0, 443556818, 298263436, 300262539, 310814031,  &
     & 445654034, 300360652, 302363481, 315392000, 426418967, 298624089,  &
     & 306979989, 302490637, 306557977, 304882773, 300393421, 302392459,  &
     & 310748556, 319235730, 462202514, 321332812, 323331915, 333883407,  &
     & 464299730, 323430028, 325419008, 426418967, 298624089, 306979989,  &
     & 302490637, 306557977, 304882773, 300393421, 302392459, 308651404/

data (symbcd(j),  j =    2851,   2964)/ &
     & 319235729, 325756633, 323790551,         0, 426418967, 298624089,  &
     & 306979989, 302490637, 306557977, 304882773, 300393421, 302392459,  &
     & 310748556, 319235664, 460105296, 321300108, 327526283, 335947918,  &
     & 342370580, 344762585, 344700697, 323495565, 327516160, 430613464,  &
     & 304915737, 313238868, 443327767, 311043280, 306652172, 298165067,  &
     & 294003469, 296166285, 296105168, 308716811, 317040204, 325564120,  &
     & 323725014, 327919384, 325887641, 319563158, 313140496, 310814027,  &
     &         0, 426418967, 298624089, 306979989, 302490637, 306557977,  &
     & 304882773, 300393421, 302392459, 310748556, 319235730, 464299595,  &
     & 319038853, 308421636, 297968454, 295936904, 300131206, 462202379,  &
     & 316941637, 308412416, 460105367, 319464463, 298230603, 432710615,  &
     & 304915737, 319534039, 304882968, 319530647, 432448525, 310781388,  &
     & 321303565, 310748619, 321300111,         0, 433202052, 435299268,  &
     & 433202532, 432153924,         0, 443688132, 445785348, 431105316,  &
     & 430056708,         0, 447751044, 460334340, 432711445, 430417615,  &
     &         0, 447653148, 313370012, 315532639, 309339232, 300917661,  &
     & 298689497, 304850324, 434939158, 315237842, 317203854, 310785048,  &
     & 298525524, 296297360, 302458187, 432547021, 312845705, 314811717,  &
     & 308421700, 300065671, 298066889, 302261191,         0, 441459806/

data (symbcd(j),  j =    2965,   3078)/ &
     & 307111134, 307246240, 306328725, 304686212, 308880533, 428647320,  &
     & 302818202, 294433561, 319599897, 315368985, 315434265,         0,  &
     & 434938776, 300655640, 300725197, 298197963, 302392269,         0,  &
     & 434938776, 300655640, 300725195, 298197965, 302392330, 300163975,  &
     &         0, 435168158, 300491806, 300954590, 300692429, 298197963,  &
     & 302392269,         0, 432939995, 298656603, 296625054, 300917856,  &
     & 311436767, 319759964, 321725976, 317433045, 308884768, 315598302,  &
     & 319694362, 317465942, 442934412, 308651276, 308707328, 468722507,  &
     & 441459998, 311305434, 304915417, 296592221, 298820640, 307242271,  &
     & 317662878, 330278880, 459875921, 319268365, 323331851, 331753422,  &
     & 333981522, 325648384, 468461463, 334178327, 336340953, 332179288,  &
     & 327886481, 319235468, 310748235, 298197838, 296264595, 311141785,  &
     & 317564381, 315598112, 307209309, 304981144, 311076430, 325461899,  &
     & 333817868, 335983691, 300295054, 298361811, 304788571, 307013262,  &
     & 327559051,         0, 437035992, 302752856, 302822221, 294003531,  &
     & 298188800, 437035992, 302752856, 302822219, 294003533, 298197899,  &
     & 296002247,         0, 441459807, 300528799, 300528800, 309306323,  &
     & 430351116, 296067980, 296124416, 439231643, 304948251, 302916702,  &
     & 307209568, 321922847, 330213211, 327984856, 313205973, 308913426/

data (symbcd(j),  j =    3079,   3192)/ &
     & 315176544, 326084381, 328050393, 323757591, 440837196, 306554060,  &
     & 306610176, 430482259, 298525719, 306947350, 319399570, 327755667,  &
     & 334148435, 298492950, 306914581, 319366801, 327722898, 334145495,  &
     &         0, 445784916, 310509568, 433202516, 297926656, 433202052,  &
     &         0, 435168153, 437265305, 451945881, 454043033,         0,  &
     & 323397323, 441131922, 296231758, 298197835, 430449612, 432612240,  &
     & 300360652, 296072531, 323761693, 319628888, 325854938, 321758749,  &
     & 453944922, 325844992, 437265311, 296657755, 298624024, 306980121,  &
     & 313369949, 311403680, 303038464, 464201748, 329856665, 334112399,  &
     & 432678868,         0, 454042756, 456139844, 445424664, 298525523,  &
     & 296231822, 302392523, 314943116, 327624529, 329918230, 323757529,  &
     & 311211289, 304882646, 298427280, 300360780, 308655499, 321267406,  &
     & 327722772, 325789272, 317489152, 443557017, 445654169,         0,  &
     & 306787478, 304751824, 306652240, 308946070, 441001092, 440673350,  &
     & 306324678, 306459417, 298591257, 298656537, 428647961, 445425048,  &
     & 319595930, 311210763, 298132491, 298197771, 428189195, 444966282,  &
     & 319137164, 310738944, 443556895, 298722135, 296362895, 302392523,  &
     & 312845836, 323462868, 325822108, 319792480, 309329920, 437134493,  &
     & 313533771,         0, 432907164, 300885023, 307242400, 319792734/

data (symbcd(j),  j =    3193,   3306)/ &
     & 323888794, 321660373, 296068811,         0, 435168928, 311174616,  &
     & 321627798, 325691089, 323429900, 312845451, 300295053, 296189952,  &
     & 451945298, 327759328, 317030400, 456139744, 298558424, 307012953,  &
     & 319563414, 325691089, 323429900, 312845451, 300295053, 296189952,  &
     & 458139231, 315630880, 305112028, 298558354, 300360780, 310748491,  &
     & 319170190, 325625554, 323659287, 313271576, 304849877, 298385408,  &
     & 460334155, 430974688,         0, 441459679, 298754971, 300721240,  &
     & 313239062, 323626706, 325559949, 321267083, 306553804, 298230607,  &
     & 296297364, 302720215, 317466201, 323856029, 321889696, 307232768,  &
     & 458008150, 317334803, 308913172, 298525529, 296559517, 303015136,  &
     & 311436767, 321824409, 323626575, 317072651, 306553804, 298254336,  &
     & 451847627, 432678932,         0, 432678932,         0, 466756356,  &
     &         0, 432777239, 432580625,         0, 447882466, 305112027,  &
     & 298525586, 300328009, 308487492,         0, 431104994, 305112283,  &
     & 311108882, 308716617, 300098372,         0, 441263246, 430679505,  &
     & 451650385,         0, 436609995, 298197965, 302392330, 300163975,  &
     &         0, 434545548, 300262412, 300318720, 441590919, 449979783,  &
     & 460236383, 315630752, 300917597, 296592281, 300688471, 317367892,  &
     & 323593937, 325527116, 314942603, 300294990,         0, 443556895/

data (symbcd(j),  j =    3307,   3420)/ &
     & 298722135, 296362895, 302392523, 312845836, 323462868, 325822108,  &
     & 319792480, 309343456, 305112094, 300819351, 298460111, 302425164,  &
     & 308655435, 317072909, 321365652, 323724892, 319759839, 313524224,  &
     & 437134493, 313533771, 445621515, 436577867,         0, 432939995,  &
     & 298656603, 296625054, 300917920, 315631199, 323954396, 325920408,  &
     & 317400212, 302621585, 296166219, 449848863, 321857180, 323823192,  &
     & 315303060, 430351246, 302458188, 319170189, 325530638, 312845899,  &
     & 323364558, 325582848, 432939995, 298656603, 296625054, 300917920,  &
     & 315631199, 323921562, 321660311, 309048736, 319792733, 321725976,  &
     & 315340183, 319497876, 325658319, 323397196, 314942603, 300295053,  &
     & 296198992, 298361808, 298301013, 323561103, 321299980, 314933248,  &
     & 449783179, 451945931, 451945233, 327726283, 323321856, 435168086,  &
     & 430646232, 307012953, 319563414, 325691089, 323429900, 312845451,  &
     & 300295053, 296198992, 298361808, 298300761, 317466198, 323593873,  &
     & 321332684, 312849376, 321926111, 311404128,         0, 456042012,  &
     & 321758876, 323921503, 317728032, 305112029, 298689367, 296264590,  &
     & 302392523, 312845836, 323430097, 325658261, 319530328, 311174231,  &
     & 300589970, 445654175, 302949339, 298558353, 300360780, 308655435,  &
     & 317072974, 323528338, 321562071, 313262080, 430973786, 430842782/

data (symbcd(j),  j =    3421,   3534)/ &
     & 303047840, 317630045, 323954400, 433005599, 307209693, 460334813,  &
     & 323822997, 313107728, 310752922, 313173267, 308815051,         0,  &
     & 441459679, 298754970, 300688535, 315336280, 323823261, 321889696,  &
     & 307246240, 303014877, 300753944, 306951575, 319563354, 321824287,  &
     & 315634839, 300622741, 296330063, 298230732, 306554251, 321267341,  &
     & 325560019, 323659350, 315339927, 302719957, 298427279, 300327948,  &
     & 306558347, 319170125, 323462803, 321562134, 315326464, 458008150,  &
     & 317334803, 308913172, 298525529, 296559517, 303015136, 313533983,  &
     & 323921626, 325723792, 321332684, 310748235, 300295054, 298296272,  &
     & 302490574, 443130964, 300622745, 298656733, 305112288, 447751647,  &
     & 321824410, 323626576, 319235468, 310738944, 451847627, 432678932,  &
     &         0, 432678932,         0, 466756356,         0, 432777239,  &
     & 432580625,         0, 447882466, 305112027, 298525586, 300328009,  &
     & 308487492, 443622494, 302883798, 300491789, 304424134,         0,  &
     & 431104994, 305112283, 311108882, 308716617, 300098372, 435233886,  &
     & 307078358, 308880525, 304423878,         0, 441459860, 430876119,  &
     & 451846999,         0, 434480012, 300327948, 302326728, 298024960,  &
     & 434545548, 300262412, 300318720, 441590919, 449979783, 458139228,  &
     & 323856092, 326018655, 315630752, 300917597, 296592281, 300688471/

data (symbcd(j),  j =    3535,   3648)/ &
     & 317367892, 325661531, 300721240, 317400661, 323626706, 325527116,  &
     & 314942603, 300294990, 296199056, 300393358,         0, 449848543,  &
     & 305046490, 298558291, 296231821, 300295243, 308651404, 319235729,  &
     & 325723928, 328050398, 323986976, 315635104, 311403677, 302851031,  &
     & 298427280, 300328011, 442869068, 317138513, 323626712, 325953182,  &
     & 319815680, 449717323, 454042763, 454042973, 307078170, 451847387,  &
     & 302841856, 439231643, 304948251, 302916702, 307209568, 319825631,  &
     & 328115995, 325887575, 315270291, 300458831, 291878432, 323987165,  &
     & 325953177, 319530131, 428254030, 300360972, 317072973, 323466190,  &
     & 310748619, 321267343,         0, 439231643, 304948251, 302916702,  &
     & 307209568, 319825631, 328115995, 325887511, 313210400, 323987165,  &
     & 325953177, 319534294, 313206293, 321529490, 323462733, 319169867,  &
     & 304456588, 296133391, 294134609, 298328911, 447423957, 319432274,  &
     & 321365517, 317072715,         0, 458204427, 460334411, 460333841,  &
     & 327712768, 443556758, 443557728, 443524639, 330314646, 300655768,  &
     & 313271831, 321595028, 323528270, 317072651, 304456588, 296133391,  &
     & 294134609, 298328911, 447489495, 319497812, 321431054, 314975499,  &
     &         0, 460236444, 325953308, 328115935, 321922464, 309306461,  &
     & 300753815, 296330063, 298230732, 304456971, 317072974, 323495571/

data (symbcd(j),  j =    3649,   3762)/ &
     & 321562134, 315335895, 304817108, 298399136, 311403677, 302851031,  &
     & 298427278, 300299531, 314975758, 321398356, 319488000, 437265306,  &
     & 464529181, 323822932, 308847759, 304461466, 311043217, 304587787,  &
     & 435070112, 311436893, 437200031, 311404125, 326018846, 330301440,  &
     & 447751327, 305079324, 302818391, 309011862, 323725016, 328017693,  &
     & 326084128, 313537888, 309306526, 305013849, 306947286, 449521239,  &
     & 323757786, 326018719, 319829206, 300589907, 294167310, 296100875,  &
     & 310748684, 321300111, 323561044, 319464854, 443229205, 298427217,  &
     & 296166284, 302363915, 317072909, 321365587, 319455232, 460105367,  &
     & 319464852, 308946005, 302719960, 300786717, 307209568, 319825567,  &
     & 326051612, 327952084, 323528206, 314975435, 302359436, 296166223,  &
     & 298329039, 298267733, 302752795, 305046751, 313538207, 326018776,  &
     & 323626577, 317138252, 308641792, 451847627, 432678932,         0,  &
     & 432678932,         0, 475144708,         0, 432777239, 432580625,  &
     &         0, 456271201, 307176475, 298558290, 296166281, 300098564,  &
     & 447784093, 302818262, 298361740, 300131332,         0, 443688226,  &
     & 313501082, 315303249, 308716618, 298033796, 443688225, 313402711,  &
     & 310977743, 304456583,         0, 445654292, 435070551, 456041431,  &
     &         0, 430285580, 296133516, 298165065, 291733504, 430351116/

data (symbcd(j),  j =    3763,   3876)/ &
     & 296067980, 296124416, 449979271, 460465351, 462300891, 328017755,  &
     & 330180382, 326084128, 311436383, 300852187, 302818392, 319432338,  &
     & 435004505, 319465044, 323561103, 321299980, 312845387, 298197837,  &
     & 294101776, 296264592, 296189952, 443556895, 298722135, 296362895,  &
     & 302392523, 312845836, 323462868, 325822108, 319792480, 309343327,  &
     & 300819351, 298460111, 304493581, 308684108, 319206860, 321365652,  &
     & 323724892, 317699614, 313500895, 302972928, 437134493, 313533771,  &
     & 437134363, 307111198, 310748491,         0, 432907164, 300885023,  &
     & 307242400, 319792734, 323888794, 321660373, 298169243, 300786652,  &
     & 302982303, 315598366, 321791578, 319563157, 296072076, 325461707,  &
     & 430286539,         0, 435168928, 309048288, 300918367, 456139927,  &
     & 443295064, 319530645, 325658321, 323429900, 312845451, 300295053,  &
     & 296199055, 441165143, 319497875, 449554005, 323561105, 321332620,  &
     & 457713165, 312878220, 300327823, 438707086,         0, 451847627,  &
     & 319141408, 319141408, 296232720, 451847056, 432580369, 327680000,  &
     & 435168151, 437232600, 435168864, 321893407, 321893336, 307012953,  &
     & 319563414, 325691089, 323429900, 312845451, 300295053, 296199055,  &
     & 432776151, 304883032, 319530644, 449586774, 323593873, 321332620,  &
     & 457713165, 312878220, 300327823, 438707086,         0, 454010461/

data (symbcd(j),  j =    3877,   3990)/ &
     & 323921503, 315630880, 305112028, 298558354, 300360780, 310748491,  &
     & 319170190, 325625554, 323659287, 313271576, 304849877, 456074655,  &
     & 311403614, 441426972, 300655570, 302458060, 434644045, 310781260,  &
     & 319202960, 449193550, 323528338, 321562007, 457811478, 313238807,  &
     & 304817107, 443261973, 300482560, 430974688, 304460640, 296724127,  &
     & 458236939, 304447488, 441459679, 298754971, 300721176, 306947478,  &
     & 319465044, 323561103, 321299852, 306586573, 298296210, 300557333,  &
     & 306914711, 319563353, 323856029, 321889696, 307246111, 300852187,  &
     & 302818456, 315336214, 323626706, 325559949, 321267083, 306553804,  &
     & 298230607, 296297364, 302720151, 315368985, 321758813, 319796830,  &
     & 315597983, 300888974, 304494028, 323420160, 455812564, 311010515,  &
     & 302654358, 296526682, 298755103, 309339424, 317695581, 323790484,  &
     & 321365452, 310748299, 300295054, 300360716, 455910934, 313144920,  &
     & 317367572, 308945941, 298595476, 300622745, 298656733, 307213211,  &
     & 302982367, 311403998, 321762655, 319727193, 321529359, 314979789,  &
     & 310781068, 300318720, 449750412, 317076893, 317629900, 432711637,  &
     & 334115733, 298461140,         0, 432711637, 334115733, 298461140,  &
     &         0, 466756356, 295843748, 334635844,         0, 432842713,  &
     & 334246809, 298592216, 432580561, 333984657, 298330064,         0/

data (symbcd(j),  j =    3991,   4104)/ &
     & 445785250, 303014811, 296428370, 298230793, 306390276, 312620324,  &
     & 313664738, 305112027, 298525586, 300328009, 308487492,         0,  &
     & 431104994, 305112283, 311108882, 308716617, 300098372, 297939812,  &
     & 298984482, 307209499, 313206098, 310813833, 302195588,         0,  &
     & 441459807, 308978836, 441459860, 441459935, 304784532, 430875549,  &
     & 315336151, 430876119, 430875484, 317466071, 451847581, 298558295,  &
     & 451846999, 451847644, 296493911,         0, 438707211, 300262284,  &
     & 298230734, 302457933, 304423944, 298038221, 300295180, 302425037,  &
     & 436577354, 438707208,         0, 434578317, 298197963, 302359628,  &
     & 304522254, 300364749, 300295180, 302425037,         0, 443688135,  &
     & 310621412, 311567623, 453944989, 319792480, 307241951, 296657755,  &
     & 298623960, 317335059, 321431119, 319202636, 306586637, 300365341,  &
     & 317662559, 307209182, 298754971, 300721621, 321496721, 323462733,  &
     & 319169867, 306553804, 296166350, 455550348,         0, 445653771,  &
     & 445555531, 293975325, 325429003, 445654795, 434677329, 432547472,  &
     &         0, 433070987, 435135436, 433071520, 321889950, 325986009,  &
     & 323724886, 315274207, 315598430, 323888793, 321627542, 434840982,  &
     & 321562260, 325658319, 323397196, 314942347, 434808213, 321529490,  &
     & 323462733, 314975180,         0, 462268125, 321889760, 309339231/

data (symbcd(j),  j =    4105,   4218)/ &
     & 300852123, 296493907, 298329038, 304489675, 317040204, 325527312,  &
     & 462268123, 323921502, 317695199, 305079259, 298591123, 300426317,  &
     & 308684236, 321300110, 325592848,         0, 433070987, 435135436,  &
     & 433071456, 319792797, 325953304, 327788240, 323429900, 312845195,  &
     & 435135839, 319759965, 323856088, 325691024, 321332749, 312878028,  &
     &         0, 433070987, 435135436, 433071776, 435136159, 324023254,  &
     & 313206101, 434808149, 434513548, 323335051, 323321856, 433070987,  &
     & 435135435, 298169248, 324023263, 323987104, 434840918, 313177045,  &
     & 313163776, 462268125, 321889760, 309339231, 300852123, 296493907,  &
     & 298329038, 304489675, 317040204, 325527312, 327820756, 462268123,  &
     & 323921502, 317695199, 305079325, 300786584, 298427344, 302457933,  &
     & 308684236, 321300110, 325592787, 317302228,         0, 433070987,  &
     & 433071072, 300262283, 462431968, 325429003, 462432011, 434841302,  &
     & 434808533,         0, 433070987, 300266400, 300950475,         0,  &
     & 449848720, 312911052, 304489421, 298328912, 449848800, 317203853,  &
     & 312878283, 304456652, 298230608,         0, 433070987, 300266400,  &
     & 300950475, 462431968, 300562208, 300528791, 325429003, 443262731,  &
     &         0, 433070987, 433071072, 300299212, 323364491, 432383627,  &
     &         0, 433070987, 435004363, 298169307, 314946464, 315045792/

data (symbcd(j),  j =    4219,   4332)/ &
     & 315045723, 314947419, 329623435, 466626443,         0, 433070987,  &
     & 435069899, 298169309, 327529376, 325531360, 325531360, 328214283,  &
     &         0, 443556959, 300852123, 296493907, 298329038, 304489675,  &
     & 317040204, 325527312, 329885528, 328050397, 321889760, 309343519,  &
     & 305079259, 298591123, 300426317, 310781324, 321300176, 327788312,  &
     & 325953118, 315598111,         0, 433070987, 435135435, 298169248,  &
     & 317728351, 323954396, 325887639, 321594837, 300594143, 317695582,  &
     & 323888793, 321627606, 300613632, 443556959, 300852123, 296493907,  &
     & 298329038, 304489675, 317040204, 325527312, 329885528, 328050397,  &
     & 321889760, 309343519, 305079259, 298591123, 300426317, 310781324,  &
     & 321300176, 327788312, 325953118, 315598111, 449259209, 327464334,  &
     & 317138697,         0, 433070987, 435135435, 298169248, 315631199,  &
     & 323954396, 325887639, 321594773, 300594143, 315598430, 323888793,  &
     & 321627542, 300627221, 323331787, 447391435,         0, 460236383,  &
     & 315630752, 300917597, 296592281, 300688471, 315270676, 321496721,  &
     & 323429965, 314975372, 302425038, 296171229, 321824286, 315597983,  &
     & 300884893, 298689497, 304883094, 319465107, 325625550, 321267083,  &
     & 306553804, 296157184, 441427083, 443524299, 306557728, 321922655,  &
     & 428876575, 321880064, 433070993, 300360780, 310748555, 321267406/

data (symbcd(j),  j =    4333,   4446)/ &
     & 327722784, 433071072, 300459022, 304522508, 314975821, 323430097,  &
     & 326117152,         0, 428877067, 428876640, 310851360, 326116622,  &
     & 462431499,         0, 428876939, 428876640, 306656736, 306656733,  &
     & 306558429, 327529952, 327628960, 338700046, 475014923,         0,  &
     & 430974603, 325432160, 298854091, 460334752, 296072928, 298165067,  &
     &         0, 428877014, 308651275, 428876640, 311113440, 324019414,  &
     & 460334358, 310738944, 458236747, 460333963, 430974688, 430973791,  &
     & 323990412, 325461707, 430286539,         0, 455910987, 323335769,  &
     & 323790475, 455812568, 313304217, 302785430, 296330065, 298263564,  &
     & 306554187, 317072974, 455812440, 306979863, 300622739, 298361806,  &
     & 302425228, 312878670,         0, 433070987, 300266400, 300950475,  &
     & 434840664, 309110169, 319563414, 325691089, 323429900, 314942667,  &
     & 304489422, 434840792, 315368983, 321595027, 323528270, 319202700,  &
     & 308683726,         0, 455812568, 313304217, 302785430, 296330065,  &
     & 298263564, 306554187, 317072974, 455812629, 317433176, 306979863,  &
     & 300622739, 298361806, 302425228, 312878541, 319268430,         0,  &
     & 456140363, 323335776, 324019851, 455812568, 313304217, 302785430,  &
     & 296330065, 298263564, 306554187, 317072974, 455812440, 306979863,  &
     & 300622739, 298361806, 302425228, 312878670,         0, 432612946/

data (symbcd(j),  j =    4447,   4560)/ &
     & 321562135, 317465945, 307012632, 298525523, 296264590, 302392459,  &
     & 312845772, 321336211, 319399445, 317433176, 306979863, 300622739,  &
     & 298361806, 302425228, 312878541, 319268430,         0, 447751392,  &
     & 305112092, 302359627, 447751519, 309306462, 441427036, 304460633,  &
     & 311207192, 430744408, 311164928, 458008153, 321201671, 316876101,  &
     & 308454470, 302228359, 458008202, 321103301, 312616068, 302162823,  &
     & 455812568, 313304217, 302785430, 296330065, 298263564, 306554187,  &
     & 317072974, 455812440, 306979863, 300622739, 298361806, 302425228,  &
     & 312878670,         0, 433070987, 300266400, 300950475, 434807960,  &
     & 311207385, 321660565, 323335125, 306947352, 315368983, 321562187,  &
     & 323321856, 433070943, 296690589, 300852254, 303014880, 298857375,  &
     & 298787806, 300917663, 432841611, 300266393, 300721099,         0,  &
     & 433070943, 296690589, 300852254, 303014880, 298857375, 298787806,  &
     & 300917663, 432841604, 300037017, 300721092,         0, 433070987,  &
     & 300266400, 300950475, 458008153, 300398233, 300364946, 319137419,  &
     & 443131531,         0, 433070987, 300266400, 300950475,         0,  &
     & 432841611, 300266393, 300721099, 434807960, 311207385, 321660565,  &
     & 323335125, 306947352, 315368983, 321562187, 323335829, 330049497,  &
     & 340568344, 346728779, 457877335, 334243928, 342599957, 344303947/

data (symbcd(j),  j =    4561,   4674)/ &
     &         0, 432841611, 300266393, 300721099, 434807960, 311207385,  &
     & 321660565, 323335125, 306947352, 315368983, 321562187, 323321856,  &
     & 441230360, 298525523, 296264590, 302392459, 312845772, 321332881,  &
     & 323593814, 317465945, 307016856, 302752726, 298427281, 300360717,  &
     & 306586956, 317105678, 321431123, 319497687, 313271448,         0,  &
     & 432841604, 300037017, 300721092, 434840664, 309110169, 319563414,  &
     & 325691089, 323429900, 314942667, 304489422, 434840792, 315368983,  &
     & 321595027, 323528270, 319202700, 308683726,         0, 455910980,  &
     & 323106393, 323790468, 455812568, 313304217, 302785430, 296330065,  &
     & 298263564, 306554187, 317072974, 455812440, 306979863, 300622739,  &
     & 298361806, 302425228, 312878670,         0, 432841611, 300266393,  &
     & 300721099, 434742294, 306980121, 317502419, 302687383, 311174616,  &
     & 317489152, 453715416, 311207001, 298591062, 298460179, 313042384,  &
     & 449357263, 317138316, 451323148, 304489357, 434512782, 296171030,  &
     & 317400472, 451650840, 304882583, 434906006, 300561301, 302654802,  &
     & 317236751, 319235532, 310748235, 298197838,         0, 435168203,  &
     & 302363616, 303047691, 428647641, 309080857, 294397144,         0,  &
     & 432841615, 300295243, 310748556, 321368985, 300721103, 302425228,  &
     & 310781325, 321369689, 321234571, 455911065, 323321856, 428647563/

data (symbcd(j),  j =    4675,   4711)/ &
     & 428647257, 306624025, 317498509, 453813387,         0, 430744715,  &
     & 430744473, 306656665, 306656662, 306558358, 323335577, 323434457,  &
     & 332179086, 468493963,         0, 430745099, 321237849, 298624587,  &
     & 455910937, 296072793, 298165067,         0, 428647563, 428647257,  &
     & 306624025, 317498509, 297940505, 306553796, 297926656, 451683147,  &
     & 455910348, 430745177, 430744408, 317469644, 321267275, 430286411,  &
     &        0/
!
data (istart(j),  j=1, 229)/ &
     &    1,    5,   16,   26,   34,   39,   43,   54,   58,   60,   66,   70,   73,  &
     &   78,   82,   93,  100,  112,  120,  131,  134,  140,  143,  148,  151,  154,  &
     &  296,  305,  314,  322,  331,  340,  344,  355,  360,  364,  370,  374,  376,  &
     &  385,  390,  399,  408,  417,  421,  430,  434,  439,  442,  447,  450,  455,  &
     & 3177, 3186, 3189, 3197, 3205, 3208, 3217, 3229, 3232, 3247, 3259, 3262, 3264,  &
     & 3266, 3269, 3275, 3281, 3285, 3290, 3293,  &
     &  158,  162,  173,  176,  180,  185,  189,  193,  205,  207,  211,  214,  219,  &
     &  223,  227,  238,  242,  249,  253,  256,  265,  275,  278,  287,  &
     &  459,  471,  486,  494,  506,  515,  526,  535,  549,  554,  563,  567,  577,  &
     &  584,  598,  607,  613,  623,  632,  636,  644,  655,  662,  672,  &
     &  683,  690,  710,  726,  740,  749,  757,  775,  785,  790,  799,  809,  815,  &
     &  826,  834,  855,  868,  898,  918,  935,  942,  952,  958,  967,  975,  983,  &
     & 1272, 1290, 1305, 1319, 1335, 1350, 1360, 1388, 1399, 1406, 1417, 1427, 1432,  &
     & 1450, 1461, 1478, 1494, 1509, 1519, 1535, 1542, 1553, 1559, 1568, 1576, 1585,  &
     & 3306, 3325, 3330, 3351, 3373, 3378, 3396, 3419, 3433, 3462, 3485, 3488, 3490,  &
     & 3492, 3495, 3505, 3515, 3519, 3523, 3526,  &
     &  990,  997, 1017, 1023, 1029, 1038, 1045, 1055, 1080, 1085, 1095, 1101, 1112,  &
     & 1120, 1133, 1154, 1162, 1175, 1183, 1190, 1205, 1226, 1234, 1252,  &
     & 1592, 1611, 1637, 1650, 1671, 1686, 1701, 1716, 1737, 1744, 1757, 1767, 1779/

data (istart(j),  j=230,  432)/ &
     & 1789, 1810, 1825, 1834, 1849, 1865, 1872, 1887, 1905, 1916, 1932,  &
     & 1953, 1960, 1978, 1995, 2009, 2018, 2026, 2046, 2056, 2061, 2071, 2081, 2087,  &
     & 2098, 2106, 2126, 2138, 2167, 2185, 2202, 2209, 2220, 2226, 2235, 2243, 2251,  &
     & 2522, 2540, 2556, 2568, 2587, 2600, 2617, 2637, 2651, 2663, 2678, 2693, 2701,  &
     & 2725, 2742, 2757, 2776, 2791, 2803, 2817, 2825, 2842, 2855, 2874, 2894, 2913,  &
     & 3546, 3566, 3572, 3592, 3616, 3620, 3638, 3660, 3673, 3702, 3724, 3727, 3729,  &
     & 3731, 3734, 3744, 3754, 3758, 3762, 3765,  &
     & 4074, 4082, 4102, 4121, 4136, 4146, 4154, 4176, 4185, 4189, 4199, 4208, 4214,  &
     & 4224, 4232, 4252, 4264, 4287, 4302, 4323, 4329, 4341, 4347, 4357, 4364, 4371,  &
     & 4379, 4396, 4413, 4429, 4446, 4464, 4474, 4497, 4508, 4519, 4530, 4539, 4543,  &
     & 4562, 4573, 4591, 4608, 4625, 4634, 4656, 4663, 4674, 4680, 4690, 4697, 4704,  &
     & 3784, 3803, 3809, 3825, 3846, 3853, 3876, 3904, 3909, 3941, 3969, 3976, 3980,  &
     & 3984, 3991, 4003, 4015, 4031, 4042, 4050,  &
     & 2258, 2260, 2262, 2283, 2289, 2301, 2305, 2309, 2320, 2336, 2360, 2373, 2377,  &
     & 2381, 2384, 2391, 2399, 2402, 2406, 2415, 2435, 2454, 2473, 2500,  &
     & 2927, 2932, 2937, 2942, 2964, 2977, 2983, 2990, 2997, 3012, 3027, 3051, 3056,  &
     & 3063, 3070, 3086, 3098, 3100, 3102, 3104, 3123, 3130, 3135, 3154/

data (width(j),  j=1, 216)/ &
     & 18., 21., 21., 21., 19., 18., 21., 22.,  8., 16., 21., 17., 24., 22., 22., 21.,  &
     & 22., 21., 20., 16., 22., 18., 24., 20., 18., 20.,  &
     & 19., 19., 18., 19., 18., 12., 19., 19.,  8., 10., 17.,  8., 30., 19., 19., 19.,  &
     & 19., 13., 17., 12., 19., 16., 22., 17., 16., 17.,  &
     & 20., 20., 20., 20., 20., 20., 20., 20., 20., 20., 26., 26., 22., 26., 14., 14.,  &
     & 16., 10., 10., 20.,  &
     & 18., 21., 17., 18., 19., 20., 22., 22.,  8., 21., 18., 24., 22., 18., 22., 22.,  &
     & 21., 18., 16., 18., 20., 20., 22., 20.,  &
     & 21., 19., 19., 18., 16., 15., 20., 21., 11., 18., 16., 21., 18., 16., 17., 22.,  &
     & 18., 20., 20., 20., 22., 18., 23., 23.,  &
     & 20., 22., 21., 22., 21., 20., 23., 24., 11., 15., 22., 18., 25., 23., 22., 22.,  &
     & 22., 22., 20., 19., 24., 20., 24., 20., 21., 20.,  &
     & 20., 21., 19., 21., 19., 13., 19., 22., 11., 11., 21., 11., 33., 22., 20., 21.,  &
     & 20., 17., 17., 15., 22., 18., 24., 20., 19., 18.,  &
     & 20., 20., 20., 20., 20., 20., 20., 20., 20., 20., 26., 26., 22., 26., 14., 14.,  &
     & 16., 10., 10., 20.,  &
     & 20., 22., 18., 20., 21., 20., 24., 22., 11., 22., 20., 25., 23., 22., 22., 24.,  &
     & 22., 21., 19., 19., 21., 20., 23., 22./

data (width(j),  j= 217,  432)/ &
     & 23., 21., 20., 19., 18., 18., 22., 23., 12., 20., 20., 23., 20., 17., 18., 22.,  &
     & 19., 21., 20., 20., 22., 18., 23., 23.,  &
     & 20., 24., 21., 23., 23., 22., 22., 26., 13., 18., 23., 20., 27., 25., 22., 23.,  &
     & 22., 24., 23., 21., 25., 20., 26., 22., 21., 22.,  &
     & 21., 19., 18., 21., 18., 15., 20., 21., 13., 13., 20., 12., 33., 23., 18., 21.,  &
     & 20., 17., 17., 14., 23., 20., 29., 20., 21., 20.,  &
     & 21., 21., 21., 21., 21., 21., 21., 21., 21., 21., 26., 26., 22., 26., 15., 15.,  &
     & 17., 11., 11., 21.,  &
     & 20., 20., 21., 21., 19., 18., 21., 22.,  9., 17., 21., 17., 24., 22., 22., 20.,  &
     & 22., 20., 20., 17., 22., 20., 26., 20., 19., 20.,  &
     & 20., 20., 18., 20., 18., 14., 20., 20.,  9.,  9., 19.,  9., 31., 20., 19., 20.,  &
     & 20., 14., 17., 11., 20., 16., 24., 18., 16., 18.,  &
     & 20., 20., 20., 20., 20., 20., 20., 20., 20., 20., 25., 25., 23., 25., 14., 14.,  &
     & 16., 11., 11., 19.,  &
     & 24., 24., 19., 20., 17., 24., 24., 25., 24., 24., 25., 24., 24., 22., 26., 34.,  &
     & 10., 22., 31., 19., 14., 14., 27., 22.,  &
     & 14., 14., 21., 16., 16., 10., 10., 10., 18., 24., 25., 11., 11., 11., 21., 24.,  &
     & 14., 14.,  8., 16., 14., 26., 22.,  8./

data (ssymbc(j),  j=1, 120)/ &
     &            471149226, 357246358, 315959338, 336592896, 470820906,  &
     & 345320100, 357443862, 327886236, 315762474, 336920576, 470820906,  &
     & 355313115, 336920576, 470493226, 449850016, 0, 455911911, 456370649, 0,  &
     & 471149216, 336274848, 336930848, 0, 470493226, 357574048, 336920576,  &
     & 449522346, 315959958, 0, 470820906, 355641947, 336274907, 317892650, 0,  &
     & 456370208, 336279584, 351502336, 481470811, 325953253, 347256234,  &
     & 326284694, 325958294, 346929184, 357892096, 449850016, 470493226,  &
     & 455911911, 485271143, 0, 450177706, 315304598, 315949056, 470493226, 0,  &
     & 470820906, 355313115, 336935525, 336274917, 355631104, 470853600,  &
     &            336570464, 336625664, 468592477, 328181537, 330409956,  &
     & 338831587, 345024799, 342796380, 334364672, 466265814, 319563163,  &
     & 313468258, 315794984, 326444971, 341158250, 353643173, 359738078,  &
     & 357411352, 346761365, 332038144, 465905227, 312910991, 300491605,  &
     & 292332190, 290530023, 297116654, 307799411, 322611126, 341518837,  &
     & 360295345, 372714731, 380874146, 382676313, 376089682, 365406925,  &
     & 350595210, 331677696, 468592477, 328181537, 330409956, 338831587,  &
     & 345024799, 342796380, 334378847, 330344289, 466560930, 468625379,  &
     & 470722595, 472819811, 474949794, 477079777, 0, 462300964, 345123100,  &
     & 328087389, 330413981, 332511197, 334608413, 336705629, 338802845/

data (ssymbc(j), j=121, 128)/ 340900061, 342982656, 470623971, 347187226, 464594973, 342964256, 334571552, 338755584/

data isstar /1, 5, 11, 14, 17, 20, 24, 27, 30, 35, 38, 45, 50, 53, 55, 60, 63, 70, 81, 98, 113, 123/
!----------------------------------------------------------------------------------------------------------------------------------!
   interface d2r
      module procedure d2r_d
      module procedure d2r_r
      module procedure d2r_i
   end interface
!----------------------------------------------------------------------------------------------------------------------------------!
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!      rect(3f) - [M_pixel:POLYGON] draw rectangle given two corners
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine rect(x1,y1, x2,y2)
!!    real,intent(in) :: x1,y1,x2,y2
!!
!!##DESCRIPTION
!!    Draw rectangle given two opposite corners.
!!
!!##OPTIONS
!!    X1,Y1  coordinates of a corner of the rectangle
!!    X2,Y2  coordinates of corner point opposite first point
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_rect
!!    use M_pixel
!!    use M_pixel__writegif, only : writegif
!!    implicit none
!!       integer :: i
!!
!!       !! set up graphics area
!!       call prefsize(400,400)
!!       call vinit()
!!       call ortho2(left=-100.0, right=100.0, bottom=-100.0, top=100.0)
!!
!!       !! draw some filled rectangles
!!       do i=95,5,-10
!!          call makepoly()
!!          call color(i/10)
!!          call rect( -1.0*i, -1.0*i, 1.0*i, 1.0*i )
!!          call closepoly()
!!       enddo
!!
!!       !! draw some rectangles
!!       call linewidth(50)
!!       call color(7)
!!       do i=5,95,5
!!          call rect( -1.0*i, -1.0*i, 1.0*i, 1.0*i )
!!       enddo
!!
!!       !! render pixel array to a file
!!       call writegif('rect.3M_pixel.gif',P_pixel,P_colormap)
!!
!!       !! display graphic assuming display(1) is available
!!       call execute_command_line('display rect.3M_pixel.gif')
!!
!!       !! wrap up graphics
!!       call vexit()
!!
!!    end program demo_rect
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine rect(x1,y1, x2,y2)

! ident_1="@(#) M_pixel rect(3f) draw line rectangle given two opposite corners"

!
!  x1,y1 ############ x2,y1
!        #          #
!        #          #
!        #          #
!  x1,y2 ############ x2,y2
!

real,intent(in)            :: x1,y1,x2,y2

   call move2(x1,y1)
   call draw2(x1,y2)
   call draw2(x2,y2)
   call draw2(x2,y1)
   call draw2(x1,y1)

end subroutine rect
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!      line(3f) - [M_pixel:DRAW] draw line between two points
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine line(x1,y1, x2,y2 )
!!    real,intent(in)            :: x1,y1,x2,y2
!!
!!##DESCRIPTION
!!    Draw line between two points using current line width and color
!!
!!##OPTIONS
!!    X1,Y1  starting point for line segment
!!    X2,Y2  end point for line segment
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine line(x1,y1, x2,y2 )

! ident_2="@(#) M_pixel line(3f) draw line between two points applying line width and color"

real,intent(in)  :: x1,y1,x2,y2

real             :: xx1,yy1,xx2,yy2
integer          :: i
integer          :: ix1,iy1,ix2,iy2

   P_x=x2                                                  ! update current position
   P_y=y2

   if(P_debug)write(*,*)'linewidth ',P_width,';move2 ',x1,y1,';draw2 ',x2,y2
!-----------------------------------------------------------------------------------------------------------------------------------
! allow collecting points in a continuous polyline into a polygon as a first cut using makepoly(3f) and closepoly(3f)
! assuming all line drawing goes thru this routine, and using fixed size array
   if(P_inpolygon)then
      if(P_polyvertex.gt.P_MAXVERTS)then
         write(*,*)'*P_line* exceeded limit on number of points in a polygon (',P_MAXVERTS,')'
      else
         if(P_polyvertex.eq.1)then
            P_polypoints(1,1)=x1
            P_polypoints(2,1)=y1
            P_polyvertex=P_polyvertex+1
         endif
         P_polypoints(1,P_polyvertex)=x2
         P_polypoints(2,P_polyvertex)=y2
         P_polyvertex=P_polyvertex+1
      endif
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------

   call world2viewport(x1,y1,xx1,yy1)                      ! convert from world coordinates to pixel array addresses
   call world2viewport(x2,y2,xx2,yy2)

   ix1=nint(xx1)                                        ! change values to integers
   iy1=nint(yy1)
   ix2=nint(xx2)
   iy2=nint(yy2)

   select case(P_width)
   case(:1)
      call draw_line_single(ix1, iy1 , ix2, iy2 )             ! draw line
   case(2:5)
      do i=1,P_width/2                                        ! thicken line NEEDS BETTER METHOD
         call draw_line_single(ix1+i, iy1  , ix2+i, iy2  )
         call draw_line_single(ix1  , iy1+i, ix2  , iy2+i)
      enddo

      do i=1,(P_width-1)/2
         call draw_line_single(ix1-i, iy1  , ix2-i, iy2  )
         call draw_line_single(ix1  , iy1-i, ix2  , iy2-i)
      enddo
   case(6:)
      call PPM_draw_thick_line(ix1, iy1, ix2, iy2)
   end select

end subroutine line
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine swapcoord(p1, p2)

! ident_3="@(#) M_pixel swapcoor(3fp) swap two coordinates (integers)"

    integer, intent(inout) :: p1, p2
    integer :: t
    t = p2
    p2 = p1
    p1 = t
end subroutine swapcoord
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!      draw_line_single(3fp) - [M_pixel:LINE] Bresenham's line algorithm
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine draw_line_single(x1,y1, x2,y2)
!!    integer,intent(in)            :: x1,y1,x2,y2
!!
!!##DESCRIPTION
!! From Wikipedia, the free encyclopedia
!!
!! The Bresenham line algorithm is an algorithm that determines which
!! points in an n-dimensional raster should be plotted in order to form
!! a close approximation to a straight line between two given points. It
!! is commonly used to draw lines on a computer screen, as it uses only
!! integer addition, subtraction and bit shifting all of which are very
!! cheap operations in standard computer architectures. It is one of the
!! earliest algorithms developed in the field of computer graphics.
!!
!! Through a minor expansion, the original algorithm for lines can also
!! be used to draw circles. Also this can be done with simple arithmetic
!! operations; quadratic or trigonometric expressions can be avoided or
!! recursively dissolved into simpler steps.
!!
!! The mentioned properties make it still an important algorithm, and it
!! is used among others in plotters, in graphics chips of modern graphics
!! cards, and in many graphics libraries. As it is so simple, it is not
!! only implemented in the firmware of such devices, but is also cast into
!! hardware of those graphics chips.
!!
!! To be precise, the label "Bresenham" is today often used for a whole
!! family of algorithms, which have actually been developed by others, later,
!! yet in succession of Bresenham and with a similar basic approach. See
!! deeper references below.
!!
!!##CONTENTS
!!
!!   * 1 The algorithm
!!   * 2 Generalization
!!   * 3 Optimization
!!   * 4 Different approach to the algorithm
!!       + 4.1 Generalized version for this approach
!!   * 5 Circle Variant
!!       + 5.1 Drawing incomplete octants
!!       + 5.2 Ellipses
!!   * 6 History
!!   * 7 Similar Algorithms
!!   * 8 References
!!   * 9 See also
!!   * 10 External links
!!
!! The common conventions that pixel coordinates increase in the down and
!! right directions and that pixel centers have integer coordinates will
!! be used. The endpoints of the line are the pixels at (x[0], y[0]) and
!! (x[1], y[1]), where the first coordinate of the pair is the column and
!! the second is the row.
!!
!! The algorithm will be initially presented only for the octant in which
!! the segment goes down and to the right (x[0]?x[1] and y[0]?y [1] ) ,
!! and its horizontal projection x[1] ? x[0] is longer than the vertical
!! projection y[1] ? y[0] (in other words, the line has a slope less
!! than 1 and greater than 0.) In this octant, for each column x between
!! x[0] and x[1], there is exactly one row y (computed by the algorithm)
!! containing a pixel of the line, while each row between y[0] and y[1]
!! contains multiple rasterized pixels.
!!
!! Bresenham's algorithm chooses the integer y corresponding to the pixel
!! center that is closest to the ideal (fractional) y for the same x; on
!! successive columns y can remain the same or increase by 1. The general
!! equation of the line through the endpoints is given by:
!!
!!     y - y_0 = \frac{y_1-y_0}{x_1-x_0} (x-x_0).
!!
!! Since we know the column, x, the pixel's row, y, is given by rounding
!! this quantity to the nearest integer:
!!
!!     \frac{y_1-y_0}{x_1-x_0} (x-x_0) + y_0.
!!
!! The slope (y[1] ? y[0]) / (x[1] ? x[0]) depends on the endpoint
!! coordinates only and can be precomputed, and the ideal y for successive
!! integer values of x can be computed starting from y[0] and repeatedly
!! adding the slope.
!!
!! In practice, the algorithm can track, instead of possibly large y values,
!! a small error value between ?0.5 and 0.5: the vertical distance between
!! the rounded and the exact y values for the current x. Each time x is
!! increased, the error is increased by the slope; if it exceeds 0.5, the
!! rasterization y is increased by 1 (the line continues on the next lower
!! row of the raster) and the error is decremented by 1.0.
!!
!! In the following pseudocode sample plot(x,y) plots a point and abs
!! returns absolute value:
!!
!!
!!  function line(x0, x1, y0, y1)
!!      int deltax := x1 - x0
!!      int deltay := y1 - y0
!!      real error := 0
!!      real deltaerr := deltay / deltax    // Assume deltax != 0 (line is not vertical)
!!      int y := y0
!!      for x from x0 to x1
!!          plot(x,y)
!!          error := error + deltaerr
!!          if abs(error) ? 0.5 then
!!              y := y + 1
!!              error := error - 1.0
!!
!!##GENERALIZATION
!!
!! This first version only handles lines that descend to the right. We
!! would of course like to be able to draw all lines. The first case is
!! allowing us to draw lines that still slope downwards but head in the
!! opposite direction. This is a simple matter of swapping the initial
!! points if x0 > x1. Trickier is determining how to draw lines that go
!! up. To do this, we check if y[0] ? y[1]; if so, we step y by -1 instead
!! of 1. Lastly, We still need to generalize the algorithm to drawing lines
!! in all directions. Up until now we have only been able to draw lines with
!! a slope less than one. To be able to draw lines with a steeper slope,
!! we take advantage of the fact that a steep line can be reflected across
!! the line y=x to obtain a line with a small slope. The effect is to switch
!! the x and y variables throughout, including switching the parameters to
!! plot. The code looks like this:
!!
!!  function line(x0, x1, y0, y1)
!!      boolean steep := abs(y1 - y0) > abs(x1 - x0)
!!      if steep then
!!          swap(x0, y0)
!!          swap(x1, y1)
!!      if x0 > x1 then
!!          swap(x0, x1)
!!          swap(y0, y1)
!!      int deltax := x1 - x0
!!      int deltay := abs(y1 - y0)
!!      real error := 0
!!      real deltaerr := deltay / deltax
!!      int ystep
!!      int y := y0
!!      if y0 < y1 then ystep := 1 else ystep := -1
!!      for x from x0 to x1
!!          if steep then plot(y,x) else plot(x,y)
!!          error := error + deltaerr
!!          if error ? 0.5 then
!!              y := y + ystep
!!              error := error - 1.0
!!
!! The function now handles all lines and implements the complete Bresenham's
!! algorithm. A more standard C code for the algorithm is shown here:
!!
!!    void Bresenham(int x1, int y1, int x2, int y2) {
!!             int slope;
!!             int dx, dy, incE, incNE, d, x, y;
!!             // Reverse lines where x1 > x2
!!             if (x1 > x2)
!!             {
!!                 Bresenham(x2, y2, x1, y1);
!!                 return;
!!             }
!!             dx = x2 - x1;
!!             dy = y2 - y1;
!!             // Adjust y-increment for negatively sloped lines
!!             if (dy < 0)
!!             {
!!                 slope = -1;
!!                 dy = -dy;
!!             }
!!             else
!!             {
!!                 slope = 1;
!!             }
!!             // Bresenham constants
!!             incE = 2 * dy;
!!             incNE = 2 * dy - 2 * dx;
!!             d = 2 * dy - dx;
!!             y = y1;
!!             // Blit
!!             for (x = x1; x <= x2; x++)
!!             {
!!                 putpixel(x, y);
!!                 if (d <= 0)
!!                 {
!!                     d += incE;
!!                 }
!!                 else
!!                 {
!!                     d += incNE;
!!                     y += slope;
!!                 }
!!             }
!!         }
!!
!!##OPTIMIZATION
!!
!! The problem with this approach is that computers operate relatively
!! slowly on fractional numbers like error and deltaerr; moreover, errors
!! can accumulate over many floating-point additions. Working with integers
!! will be both faster and more accurate. The trick we use is to multiply
!! all the fractional numbers above by deltax, which enables us to express
!! them as integers. The only problem remaining is the constant 0.5?to deal
!! with this, we change the initialization of the variable error. The new
!! program looks like this:
!!
!!  function line(x0, x1, y0, y1)
!!      boolean steep := abs(y1 - y0) > abs(x1 - x0)
!!      if steep then
!!          swap(x0, y0)
!!          swap(x1, y1)
!!      if x0 > x1 then
!!          swap(x0, x1)
!!          swap(y0, y1)
!!      int deltax := x1 - x0
!!      int deltay := abs(y1 - y0)
!!      int error := -deltax / 2
!!      int ystep
!!      int y := y0
!!      if y0 < y1 then ystep := 1 else ystep := -1
!!      for x from x0 to x1
!!          if steep then plot(y,x) else plot(x,y)
!!          error := error + deltay
!!          if error > 0 then
!!              y := y + ystep
!!              error := error - deltax
!!
!!##DIFFERENT APPROACH TO THE ALGORITHM
!!
!! A different approach to the Bresenham algorithm works more from the
!! practical side. It was published by Pitteway ^[1] and confirmed by van
!! Aken ^[2]. Again we first consider a line in the first octant, which
!! means a slope between 0 and 1. Mathematically spoken, we want to draw
!! a line from point (x[1],y[1]) to (x[2],y[2]). The intervals in the
!! two directions are dx=x[2]-x[1] and dy=y[2]-y [1], and the slope is
!! dy/dx. The line equation can be written as y=y[1]+(x-x[1])*dy/dx. In
!! this first octant, we have 0<dy<=dx.
!!
!! So, when working pixel-wise along this line, we have one "fast"
!! direction, the positive x direction, and a "slow" direction, the positive
!! y direction, where fewer steps have to be done than in the fast one. So
!! the algorithm simply goes like this: a) Always do a single pixel step
!! in the fast direction. b) Every now and then also do a step in the
!! slow direction.
!!
!! Bresenham's trick is the introduction of an error term, which deals with
!! the decision, when to do also this extra step in the slow direction. The
!! line equation is transformed into 0=dx*(y-y[1])-dy*(x-x[1]), and then
!! the null on the left side is replaced by the error term. A step by 1 in
!! the x direction (variable x) causes a decrement of the error term by
!! one times dy. If the error term gets below zero due to this, it will
!! be increased by one times dx through a step by 1 in the y direction
!! (variable y). Because of dx>=dy, this will render the error term positive
!! again in any case, at least brought back to zero.
!!
!! You realize a cross-wise subtraction of dy from the error term for any x
!! step and an addition of dx for any y step. This way, the division dy/dx
!! for the slope is dissolved into a number of more elementary operations.
!!
!! A critical issue is the initialisation of the error term. In this approach
!! here, we simply consider a line with dy=1, so with only one single step
!! in the y direction along the whole line. Of course for the best look
!! of the line, we want this step to happen right in the middle of the
!! line. This leads to initialising the error term to dx/2. (Rounding this
!! term to integers in case of odd dx is no problem.)
!!
!! This approach comes out a little different from the original, as it
!! avoids the additional factor of 2 on both sides, which has to do with
!! the initialisation.
!!
!! To generalize this algorithm for all octants, you will again have to do
!! role changes of x and y and consider the different signs of dx and dy.
!!
!! A simple implementation of this approach is not very elegant, but
!! demonstrates the principle of the algorithm fairly well.
!!
!!    REM Bresenham algorithm for a line in the first octant in Pseudo Basic
!!    dx = xend-xstart
!!    dy = yend-ystart
!!    REM in first octant, we have 0 < dy <= dx
!!
!!    REM Initialisations
!!    x = xstart
!!    y = ystart
!!    SETPIXEL x,y
!!    error = dx/2
!!
!!    REM Pixel loop: always do a step in fast direction, every now and then also one in the slow direction
!!    WHILE x < xend
!!       REM Step in fast direction
!!       x = x + 1
!!       error = error-dy
!!       IF error < 0 THEN
!!          REM Step in slow direction
!!          y = y + 1
!!          error = error + dx
!!          ENDIF
!!       SETPIXEL x,y
!!       WEND
!!
!!##GENERALIZED VERSION FOR THIS APPROACH
!!
!! This generalized version in BASIC shall be valid for all octants. For
!! this, all signs of the coordinate distances have to be considered, as
!! well as the possible role change of x and y. If these if clauses would
!! all be put into the innermost loop, which would mean a high number of
!! executions, it would considerably increase the time consumption. A more
!! efficient solution tries to put all these case differentiations into the
!! initialisation phase of the procedure before the start of the inner main
!! loop. Then the inner loop will still contain a single if clause for the
!! Bresenham error term.
!!
!! This version in BASIC introduces a number of abstractions: First the step
!! in the "fast" direction is now considered a parallel step (parallel to
!! one of the coordinate axis), and if additionally a step in the "slow"
!! direction becomes necessary, it becomes a diagonal step. For these cases
!! we can compute variable values during initialisation, in advance, which
!! contain the step widths (including signs) in the coordinate directions
!! and thus achieve the generalization for the eight octants. For example
!! the step width in perpendicular direction to a parallel step is just
!! zero. Secondly the error term is still computed like in the first octant
!! by using the absolute values of the distances. In the innermost loop,
!! no more the step in the fast direction is executed first, but the error
!! term is updated, and only after that the step widths are added to the
!! current coordinate values, depending on whether a parallel or a diagonal
!! step has to be done:
!!
!!    REM Bresenham algorithm for a line in an arbitrary octant in pseudo Basic
!!    dx = xend-xstart
!!    dy = yend-ystart
!!
!!    REM Initialisations
!!    adx = ABS(dx): ady = ABS(dy) ' Absolute values of distances
!!    sdx = SGN(dx): sdy = SGN(dy) ' Signum of distances
!!
!!    IF adx > ady THEN
!!      ' x is fast direction
!!      pdx = sdx: pdy = 0   ' pd. is parallel step
!!      ddx = sdx: ddy = sdy ' dd. is diagonal step
!!      ef  = ady: es  = adx ' error steps fast, slow
!!                 ELSE
!!      ' y is fast direction
!!      pdx = 0  : pdy = sdy ' pd. is parallel step
!!      ddx = sdx: ddy = sdy ' dd. is diagonal step
!!      ef  = adx: es  = ady ' error steps fast, slow
!!      ENDIF
!!
!!    x = xstart
!!    y = ystart
!!    SETPIXEL x,y
!!    error = es/2
!!
!!    REM Pixel loop: always a step in fast direction, every now and then also one in slow direction
!!    FOR i=1 TO es          ' es also is the count of pixels zo be drawn
!!       REM update error term
!!       error = error - ef
!!       IF error < 0 THEN
!!          error = error + es ' make error term positive (>=0) again
!!          REM step in both slow and fast direction
!!          x = x + ddx: y = y + ddy ' Diagonal step
!!                    ELSE
!!          REM step in fast direction
!!          x = x + pdx: y = y + pdy ' Parallel step
!!          ENDIF
!!       SETPIXEL x,y
!!       NEXT
!!
!!##RASTERIZATION OF A CIRCLE BY THE BRESENHAM ALGORITHM
!!
!! The approach for the Circle Variant shown here is also not originally
!! from Bresenham, see again references to Pitteway and van Aken below. The
!! algorithm starts accordingly with the circle equation x?+y?=r?. Again
!! we consider first only the first octant. Here you want to draw a curve
!! which starts at point (r,0) and then proceeds to the top left, up to
!! reaching the angle of 45?.
!!
!! The "fast" direction here is the y direction. You always do a step in
!! the positive y direction (upwards), and every now and then you also have
!! to do a step in the "slow" direction, the negative x direction.
!!
!! The frequent computations of squares in the circle equation, trigonometric
!! expressions or square roots can again be avoided by dissolving everything
!! into single steps and recursive computation of the quadratic terms from
!! the preceding ones.
!!
!! From the circle equation you get to the transformed equation
!! 0=x?+y?-r? with r? to be computed only a single time during
!! initialisation, x?=(xpreceding-1)?=xpreceding?-2*xpreceding+1 (according
!! for y), where x? (or xpreceding?) is kept as an own variable. Additionally
!! you need to add the mid point coordinates when setting a pixel. These
!! frequent integer additions do not limit the performance much, as you
!! spare those square (root) computations in the inner loop in turn. Again
!! the zero in the transformed circle equation is replaced by the error term.
!!
!! The initialization of the error term is derived from an offset of ? pixel
!! at the start. Until the intersection with the perpendicular line, this
!! leads to an accumulated value of r in the error term, so that this value
!! is used for initialisation.
!!
!! The following implementation is shown here only for the first octant,
!! and again the other octants need sign changes for x and/or y and the
!! swapping of x and y. An easy expansion for full circles, as it is possible
!! for graphics displays, but not for plotters, is added in the comments.
!!
!!
!!    REM Bresenham Algorithm for one eighth of a circle in Pseudo-Basic
!!    REM given: r, xmid, ymid
!!    REM initialisations for the first octant
!!    r2 = r*r : REM single multiplication
!!    x = r
!!    y = 0
!!    error = r
!!    SETPIXEL xmid + x, ymid + y
!!
!!    REM Pixel loop: always a step in fast direction, every now and then also in slow one
!!    WHILE y <= x
!!       REM step in fast direction (positive y direction)
!!       dy = y*2+1 : REM in Assembler implementation *2 per Shift
!!       y = y+1
!!       error = error-dy
!!       IF error<0 THEN
!!          REM step in slow direction (here the negative x direction)
!!          dx = 1-x*2 : REM in Assembler implementation *2 per Shift
!!          x = x-1
!!          error = error-dx
!!          ENDIF
!!       SETPIXEL  xmid+x, ymid+y
!!       REM If this deals with a screen and not a mechanical plotter,
!!       REM you can cover simultaneously also the other octants:
!!       REM SETPIXEL xmid-x, ymid+y
!!       REM SETPIXEL xmid-x, ymid-y
!!       REM SETPIXEL xmid+x, ymid-y
!!       REM SETPIXEL xmid+y, ymid+x
!!       REM SETPIXEL xmid-y, ymid+x
!!       REM SETPIXEL xmid-y, ymid-x
!!       REM SETPIXEL xmid+y, ymid-x
!!       WEND
!!
!! A possible implementation of the Bresenham Algorithm for a full circle
!! in C. Here another variable for recursive computation of the quadratic
!! terms is used, which corresponds with the term 2*n+1 above. It just has
!! to be increased by 2 from one step to the next:
!!
!!  void rasterCircle(int x0, int y0, int radius)
!!  {
!!    int f = 1 - radius;
!!    int ddF_x = 0;
!!    int ddF_y = -2 * radius;
!!    int x = 0;
!!    int y = radius;
!!
!!    setPixel(x0, y0 + radius);
!!    setPixel(x0, y0 - radius);
!!    setPixel(x0 + radius, y0);
!!    setPixel(x0 - radius, y0);
!!
!!    while(x < y)
!!    {
!!      if(f >= 0)
!!      {
!!        y--;
!!        ddF_y += 2;
!!        f += ddF_y;
!!      }
!!      x++;
!!      ddF_x += 2;
!!      f += ddF_x + 1;
!!      setPixel(x0 + x, y0 + y);
!!      setPixel(x0 - x, y0 + y);
!!      setPixel(x0 + x, y0 - y);
!!      setPixel(x0 - x, y0 - y);
!!      setPixel(x0 + y, y0 + x);
!!      setPixel(x0 - y, y0 + x);
!!      setPixel(x0 + y, y0 - x);
!!      setPixel(x0 - y, y0 - x);
!!    }
!!  }
!!
!! Note: There is correlation between this algorithm and the sum of first
!! N odd numbers. Which this one basically does. Sum of N odd numbers, from
!! 1 inclusive, is equal to the square of N ( N squared). See Square number.
!!
!!  So.
!!  When we compare sum of N odd numbers to this algorithm we have.
!!  ddF_y = -2 * radius       is connected to last member of of sum of N odd numbers.
!!                            This member has index equal to value of radius (integral).
!!                            Since odd number is 2*n + 1 there is 1 handled elsewhere
!!                            or it should be -2*radius - 1
!!  ddF_x = 0                 should be 1. Because difference between two consecutive odd numbers is 2.
!!                            If so f += ddF_y + 1 is f+= ddF_y. Saving one operation.
!!  f = - radius + 1          Initial error equal to half of "bigger" step.
!!                            In case of saving one addition it should be either -radius or -radius + 2.
!!  In any case there should be addition of 1 driven out of outer loop.
!!  So.
!!  f += ddF_y                Adding odd numbers from Nth to 1st.
!!  f += ddF_x                Adding odd numbers from 1st to Nth. 1 is missing because it can be moved outside of loop.
!!
!!##DRAWING INCOMPLETE OCTANTS
!!
!! The implementations above always only draw complete octants or circles. If
!! you want to draw only a certain arc from an angle ? to an angle ?, you
!! have to implement it in a way to first calculate the x and y coordinates
!! of these end points, where you inevitably have to resort to trigonometric
!! or square root computations (see Methods of computing square roots). Then
!! you run the Bresenham algorithm over the complete octant or circle
!! and set the pixels only if they fall into the wanted interval. After
!! finishing this arc, you can abort the algorithm prematurely.
!!
!!##ELLIPSES
!!
!! By scaling the drawn x and y values (and horizontal or vertical line
!! expansion, respectively) you can produce even ellipses parallel to the
!! x or y axis. For this, you use the circle algorithm with the smaller
!! ellipse axis as radius and add a value in the other direction, which
!! again is computed through another Bresenham line algorithm increasing
!! from the pole to the equator. As the ellipse has to be elongated into
!! the longer axis direction, you don't set single pixels anymore, but
!! have to draw lines (though simple ones, only horizontal or vertical)
!! from the previous to the next point.
!!
!! A general ellipse can be derived from such an axis-parallel one by
!! application of a shearing operation on it. Again you use an additional
!! Bresenham line algorithm to compute the offset increasing in one of the
!! axis directions and to let it contribute to every drawn coordinate.
!!
!!##HISTORY
!!
!! The algorithm was developed by Jack E. Bresenham in 1962 at IBM. In 2001
!! Bresenham wrote:
!!
!!     "I was working in the computation lab at IBM's San Jose development
!!     lab. A Calcomp plotter had been attached to an IBM 1401 via the
!!     1407 typewriter console. [The algorithm] was in production use by
!!     summer 1962, possibly a month or so earlier. Programs in those days
!!     were freely exchanged among corporations so Calcomp (Jim Newland and
!!     Calvin Hefte) had copies. When I returned to Stanford in Fall 1962,
!!     I put a copy in the Stanford comp center library.
!!
!!     A description of the line drawing routine was accepted for
!!     presentation at the 1963 ACM national convention in Denver,
!!     Colorado. It was a year in which no proceedings were published, only
!!     the agenda of speakers and topics in an issue of Communications of
!!     the ACM. A person from the IBM Systems Journal asked me after I made
!!     my presentation if they could publish the paper. I happily agreed,
!!     and they printed it in 1965."
!!
!! Bresenham later modified his algorithm to produce circles.
!!
!!##SIMILAR ALGORITHMS
!!
!! The principle of using an incremental error in place of division
!! operations has other applications in graphics. It is possible to use
!! this technique to calculate the U,V co-ordinates during raster scan of
!! texture mapped polygons. The voxel heightmap software-rendering engines
!! seen in some PC games also used this principle.
!!
!!##REFERENCES
!!
!!   * "The Bresenham Line-Drawing Algorithm", by Colin Flanagan
!!
!! Bresenham also published a Run-Slice (as opposed to the Run-Length) computational algorithm.
!!
!!  1. ^ Pitteway, M.L.V., "Algorithm for Drawing Ellipses or Hyperbolae with a Digital Plotter", Computer J., 10(3) November 1967, pp
!!     282-289
!!  2. ^ Van Aken, J.R., "An Efficient Ellipse Drawing Algorithm", CG&A, 4(9), September 1984, pp 24-35
!!
!!##SEE ALSO
!!
!!   * Patrick-Gilles Maillot's Thesis an extension of the Bresenham line drawing algorithm to perform 3D hidden lines removal; also
!!     published in MICAD '87 proceedings on CAD/CAM and Computer Graphics, page 591 - ISBN 2-86601-084-1.
!!
!!   * Xiaolin Wu's line algorithm, a similarly fast method of drawing lines with antialiasing.
!!
!!##EXTERNAL LINKS
!!
!!   * Analyze Bresenham's line algorithm in an online Javascript IDE
!!   * Basic Graphics Programs
!!   * The Bresenham Line-Drawing Algorithm by Colin Flanagan
!!   * National Institute of Standards and Technology page on Bresenham's algorithm
!!   * Calcomp 563 Incremental Plotter Information
!!   * Bresenham's Original Paper
!!   * Implementations in Java, C, and O Caml at the Code Codex
!!
!! Retrieved from "http://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm"
subroutine draw_line_single(x1,y1, x2,y2 )

! ident_4="@(#) M_pixel draw_line_single(3fp) draw line between two points in pixel array"

integer,intent(in)            :: x1,y1,x2,y2

   integer                    :: xx1,yy1,xx2,yy2
   integer                    :: dx, dy, error, ystep, x, y
   logical                    :: steep
   integer                    :: mostx, mosty

   xx1 = x1
   yy1 = y1
   xx2 = x2
   yy2 = y2
   call if_init()
   mostx=size(P_pixel,dim=1)-1
   mosty=size(P_pixel,dim=2)-1

   steep = (abs(yy2 - yy1) > abs(xx2 - xx1))
   if ( steep ) then
      call swapcoord(xx1, yy1)
      call swapcoord(xx2, yy2)
   endif
   if ( xx1 > xx2 ) then
      call swapcoord(xx1, xx2)
      call swapcoord(yy1, yy2)
   endif

   dx = xx2 - xx1
   dy = abs(yy2 - yy1)
   error = dx / 2
   y = yy1

   if ( yy1 < yy2 ) then
      ystep = 1
   else
      ystep = -1
   endif

   do x = xx1, xx2
      if ( steep ) then
         if(y.le.mostx.and.x.le.mosty.and.x.gt.0.and.y.gt.0) P_pixel(y,x)=P_COLOR_INDEX
         if(P_debug)write(*,*)'! ',P_COLOR_INDEX,y,x
      else
         if(x.le.mostx.and.y.le.mosty.and.x.gt.0.and.y.gt.0) P_pixel(x,y)=P_COLOR_INDEX
         if(P_debug)write(*,*)'! ',P_COLOR_INDEX,x,y
      endif
      error = error - dy
      if ( error < 0 ) then
         y = y + ystep
         error = error + dx
      endif
   enddo

end subroutine draw_line_single
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    hershey(3f) - [M_pixel:TEXT] draw text string as Hershey software
!!                  vector fonts
!!    (LICENSE:PD
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine hershey(x,y,height,itext,theta,ntext)
!!    character(len=*),intent(in)   :: itext
!!    real,intent(in)               :: x,y
!!    real,intent(in)               :: height
!!    real,intent(in)               :: theta
!!    integer,intent(in)            :: ntext
!!
!!##OPTIONS
!!    X,Y    are the coordinates in inches from the current origin to the
!!           lower left corner of the 1st character to be plotted. If either
!!           is set to 999.0 then saved next character position is used.
!!    HEIGHT is the character height in inches
!!    ITEXT  contains the text to be plotted
!!    THETA  is the positive CCW angle W.R.T. the X-axis
!!    NTEXT  is the number of characters in itext to plot
!!           o If NTEXT.lt.-1 the pen is down to (X,Y) and a single special
!!             centered symbol is plotted. ITEXT must be from CHAR(0) to
!!             CHAR(21).
!!           o If NTEXT.eq.-1 the pen is up to (X,Y) and a single special
!!             centered symbol is plotted. ITEXT must be from CHAR(0) to
!!             CHAR(21).
!!           o if NTEXT=0 a single Simplex Roman character from ITEXT,
!!             left-justified, is plotted.
!!           o if NTEXT.gt.0 NTEXT characters from ITEXT are decoded and
!!             NCHR characters are plotted where NCHR.le.NTEXT to remove
!!             backslashes, command codes, etc.
!!
!!##DESCRIPTION
!!    FEATURES:
!!      1) Four HERSHEY letter fonts--SIMPLEX,COMPLEX,ITALIC, and DUPLEX--
!!         are provided in upper and lower case ROMAN
!!      2) Two hershey letter fonts--SIMPLEX and COMPLEX--are provided in
!!         upper and lower case GREEK
!!      3) 47 special mathematical symbols, e.g. integral sign, del... are
!!         provided
!!      4) SUPER- and SUB-scripting is possible within a character string
!!         without separate calls to HERSHEY
!!
!!    Change of font is made by enclosing the name of the font in upper
!!    case in backslashes, e.g \SIMPLEX\. Three letters suffice to
!!    specify the font. SIMPLEX is the default font on the initial call
!!    to HERSHEY. A font remains in effect until explicitly changed.
!!    SUPER- or SUB-scripting is accomplished by enclosing the expression
!!    to be SUPER- or SUB-scripted in curly brackets and preceding it by
!!    SUP or SUB. the closing curly bracket terminates the
!!    SUPER- or SUB-scripting and returns to normal character plotting.
!!    Note that SUPER- and SUB-script letters are plotted with a
!!    different character size.
!!
!!    GREEK letters are drawn by enclosing the ENGLISH name of the
!!    letter in backslashes, e.g. \ALPHA\. The case of the first letter
!!    determines the case of the GREEK letter. The closing backslash must
!!    be included.
!!
!!    Any symbol may be called by enclosing the symbol number+1000 in
!!    backslashes. This is the only way to call some symbols, especially
!!    special mathematical symbols.
!!
!!   The symbol numbers are
!!
!!     1-26    upper case ROMAN SIMPLEX
!!    27-52    lower case ROMAN SIMPLEX
!!    53-72    SIMPLEX numbers and symbols
!!    73-96    upper case GREEK SIMPLEX
!!    97-120   lower case GREEK SIMPLEX
!!    121-146  upper case ROMAN COMPLEX
!!    147-172  lower case ROMAN COMPLEX
!!    173-192  COMPLEX numbers and symbols
!!    193-216  upper case GREEK COMPLEX
!!    217-240  lower case GREEK COMPLEX
!!    241-266  upper case ROMAN ITALIC
!!    267-292  lower case ROMAN ITALIC
!!    293-312  ITALIC numbers and symbols
!!    313-338  upper case ROMAN DUPLEX
!!    339-364  lower case ROMAN DUPLEX
!!    365-384  DUPLEX numbers and symbols
!!    385-432  special mathematical symbols
!!
!!    Additional features added Feb 1982:
!!
!!    The pen may be moved back to the start point for the previous character
!!    by \BS\. This is useful, for example, in writing integral signs with
!!    limits above and below them.
!!
!!    Symbol parameters taken from N.M.Wolcott, FORTRAN IV Enhanced Character
!!    Graphics, NBS
!!
!!    A. CHAVE IGPP/UCSD Aug 1981, Modified Feb 1982 by A. Chave,
!!    R.L. Parker, and L. Shure
!!
!!    programmed in FORTRAN-77
!!
!!##EXAMPLE
!!
!!   Show all Hershey characters
!!
!!    program demo_hershey
!!    use M_pixel
!!    use M_pixel__writegif_animated, only : write_animated_gif
!!    implicit none
!!    integer,parameter :: isize=600
!!    integer,parameter :: topsym=432
!!    integer           :: movie(1:topsym,0:isize-1,0:isize-1)
!!    integer           :: i
!!    !! set up environment
!!       call prefsize(isize,isize)
!!       call vinit()
!!       call ortho2(-150.0,150.0,-150.0,150.0)
!!
!!       !! draw all characters using hershey numeric strings
!!       do i=1,topsym
!!          !! draw reference circle and crosshairs
!!          call color(0)
!!          call clear()
!!
!!          call color(4)
!!          call linewidth(100)
!!          call circle(0.0,0.0,75.0)
!!          call move2(-75.0,0.0)
!!          call draw2(75.0,0.0)
!!          call move2(0.0,-75.0)
!!          call draw2(0.0,75.0)
!!
!!          call centertext(.true.)
!!          call color(7)
!!          call linewidth(500)
!!          call textang(3.0*i)
!!          call textang(0.0)
!!          call move2(0.0,0.0)
!!          call textsize(150.0,150.0)
!!          call drawstr('\',i+1000,'\',sep='')
!!
!!          call centertext(.false.)
!!          call color(1)
!!          call move2(-120.0,120.0)
!!          call textsize(10.0,10.0)
!!          call linewidth(40)
!!          call drawstr(i+1000,' ')
!!          movie(i,:,:)=P_pixel
!!       enddo
!!       call vexit()
!!       !! write to file and display with display(1)
!!       call write_animated_gif('hershey.3M_pixel.gif',&
!!       & movie,P_colormap,delay=40)
!!       !call execute_command_line('display hershey.3M_pixel.gif')
!!    end program demo_hershey
!!
!!##AUTHOR
!!    Derived from the Longlib93 library.
!!
!!##LICENSE
!!    Public Domain
!!
!!    Longlib was written by an employee of a US government contractor and
!!    is in the public domain.
!!
!!    Changes to modernize and make more portable by John S. Urban are also
!!    placed in the public domain.
subroutine hershey(x,y,height,itext,theta,ntext)

! ident_5="@(#) M_pixel hershey(3f) draw text string as Hershey software vector fonts"

      character(len=*),intent(in)   :: itext
      real,intent(in)               :: x,y
      real,intent(in)               :: height
      real,intent(in)               :: theta
      integer,intent(in)            :: ntext

      real                          :: oldwid
      real                          :: scale
      character(len=4096) :: text
      real                :: raise(20)
      real,save           :: xo,yo
      real,parameter      :: supsub(*)=[0.50,-0.50]
      real,parameter      :: factor=0.75
      integer,parameter   :: iup=3
      integer,parameter   :: idown=2
      real                :: yy, xx
      real                :: yoff
      real                :: yi, xi
      real                :: si
      real                :: rscale
      real                :: co
      integer :: ipen
      integer :: isav
      integer :: ia
      integer :: ib
      integer :: ic
      integer :: is
      integer :: ix
      integer :: iy
      integer :: i,k,l,n

!  P_ICHR(J) contains the symbol number of the Jth symbol or a
!  code to indicate SPACE (1000),BEGIN SUPER-SCRIPTING (1001),
!  BEGIN SUB-SCRIPTING (1002), OR END SUPER/SUB-SCRIPTING (1003),
!  OR BACK-SPACE (1004).
!  ISTART(P_ICHR(J)) contains the address in SYMBOL of the Jth
!  character. SYMBCD contains the pen instructions stored in a
!  special form. ISSTAR and SSYMBC contain addresses and pen
!  instructions for the special centered symbols. WIDTH contains
!  the widths of the characters.
!
!-----------------------------------------------------------------------------------------------------------------------------------
   integer :: ixtrct
   integer :: nstart
   integer :: nbits
   integer :: iword
!  IXTRCT gets NBITS from IWORD starting at the NSTART bit from the right
      IXTRCT(NSTART,NBITS,IWORD)=MOD(IWORD/(2**(NSTART-NBITS)), &
     &                           2**NBITS)+((1-ISIGN(1,IWORD))/2)* &
     &                           (2**NBITS-MIN0(1,MOD(-IWORD, &
     &                           2**(NSTART-NBITS))))
!-----------------------------------------------------------------------------------------------------------------------------------
      !!write(*,*)'GOT HERE A','X=',x,'Y=',y,'HEIGHT=',height,'ITEXT=',itext,'THETA=',theta,'NTEXT=',ntext
      yoff=0.0
      si=sind(theta)
      co=cosd(theta)
      scale=height/21.0
      if(scale.eq.0.0)return
      if(x.ge.999.0)then
         xi=xo
      else
         xi=x
      endif
      if(y.ge.999.0)then
         yi=yo
      else
         yi=y
      endif
      if(ntext.lt.0)then                                   !  plot a single special centered symbol
       if(ntext.lt.-1)call hstylus(xi,yi,idown)
       ia=ichar(itext(1:1))+1
       if(ia.gt.size(isstar))then
          write(*,*)'*hershey* error: character out of range for centered characters=',ia,itext(1:1)
          ia=size(isstar)
       endif
       is=isstar(ia)
       ib=30
          INFINITE: do
             ipen=ixtrct(ib,3,ssymbc(is))
             if(ipen.eq.0)then
               call hstylus(xi,yi,iup)
               xi=xi+20.0*co
               yi=yi+20.0*si
               xo=xi
               yo=yi
               return
             endif
             ix=ixtrct(ib-3,6,ssymbc(is))
             iy=ixtrct(ib-9,6,ssymbc(is))
             xx=scale*(ix-32)
             yy=scale*(iy-32)
             call hstylus(xi+xx*co-yy*si,yi+xx*si+yy*co,ipen)
             ib=45-ib
             if(ib.eq.30)is=is+1
          enddo INFINITE
      elseif (ntext.eq.0)then                               ! plot a single simplex roman character
        isav=P_ioff
        P_ioff=0
        text(1:1)=itext(1:1)
        call chrcod(text,1)
        P_ioff=isav
        is=istart(P_ichr(1))
        ib=30
        do
           ipen=ixtrct(ib,3,symbcd(is))
           if(ipen.eq.0)then
             xi=xi+co*scale*width(P_ichr(1))
             yi=yi+si*scale*width(P_ichr(1))
             xo=xi
             yo=yi
             return
           endif
           ix=ixtrct(ib-3,6,symbcd(is))
           iy=ixtrct(ib-9,6,symbcd(is))
           xx=(ix-10)*scale
           yy=(iy-11)*scale
           call hstylus(xi+co*xx-si*yy,yi+co*yy+si*xx,ipen)
           ib=45-ib
           if(ib.eq.30)is=is+1
        enddo
      else
         !  plot a character string.
         !  first find pointer array P_ichr containing the starts of characters-
         !  but only if P_just1 and P_just2  are not 1, when P_ichr is assumed
         !  correctly transmitted through common /ajust/.
        if(P_just1.ne.1.or.P_just2.ne.1)then
          n=ntext
          k=1
          do i=1,n
             text(i:i)=itext(i:i)
             k=k+1
          enddo
          call chrcod(text,n)
        endif
        P_just2=2
        oldwid=0.0
        l=1
        rscale=scale
        EACH_CHAR: do i=1,P_nchr                                 !  plot each character
           ic=P_ichr(i)
           if(ic.eq.1000)then
             !  plot a space
             xi=xi+20.*rscale*co
             yi=yi+20.*rscale*si
             xo=xi
             yo=yi
             call hstylus(xi,yi,iup)
           elseif ((ic.eq.1001).or.(ic.eq.1002))then
             !  begin super-scripting or sub-scripting
             raise(l)=supsub(ic-1000)*height*rscale/scale
             rscale=factor*rscale
             yoff=raise(l)+yoff
             l=l+1
           elseif (ic.eq.1003)then
             !  end super/sub-scripting
             rscale=rscale/factor
             l=l-1
             yoff=yoff-raise(l)
           elseif (ic.eq.1004)then
             !  backspace -use the width of the previous letter in oldwid.
             xi=xi - co*oldwid
             yi=yi - si*oldwid
             xo=xi
             yo=yi
           else
             ! plot a single symbol
             is=istart(ic)
             ib=30
             do
                ipen=ixtrct(ib,3,symbcd(is))
                if(ipen.eq.0)then
                  xi=xi+co*rscale*width(ic)
                  yi=yi+si*rscale*width(ic)
                  xo=xi
                  yo=yi
                  oldwid=width(ic)*rscale
               cycle EACH_CHAR
                endif
                ix=ixtrct(ib-3,6,symbcd(is))
                iy=ixtrct(ib-9,6,symbcd(is))
                xx=(ix-10)*rscale
                yy=(iy-11)*rscale+yoff
                call hstylus(xi+co*xx-si*yy,yi+co*yy+si*xx,ipen)
                ib=45-ib
                if(ib.eq.30)is=is+1
             enddo
           endif
        enddo EACH_CHAR
      endif
end subroutine hershey
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine hstylus(xi,yi,ipen)

! ident_6="@(#) M_pixel hstylus(3fp) move to new current position(CP) or draw from CP to new position and update CP"

real,intent(in)    :: xi,yi
integer,intent(in) :: ipen
real               :: P_x_tmp,P_y_tmp

   integer,parameter  :: idown=2 !, iup=3

   if(ipen.eq.idown)then
      P_X_tmp=P_X
      P_Y_tmp=P_Y
      call line(P_x_tmp, P_y_tmp, xi, yi )
   else
      P_x=xi
      P_y=yi
   endif


end subroutine hstylus
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine chrcod(text,ntext)

! ident_7="@(#) M_pixel chrcod(3fp) return symbol numbers or formatting codes for a text string"

!  Given text string in text, NTEXT characters
!  returns P_ICHR containing P_NCHR symbol numbers or codes for
!   o SPACE (1000)
!   o BEGIN SUPERSCRIPTING (1001)
!   o BEGIN SUBSCRIPTING (1002)
!   o END SUPER/SUB-SCRIPTING (1003)
!   o BACKSPACE (1004)
!   o VECTOR (1005)
!   o HAT (1006)
!  Change of font commands are decoded and executed internally
!
   CHARACTER(len=*),intent(in) :: TEXT
   integer,intent(in)          :: ntext
   INTEGER,save :: IRLU(95),IILU(95),IGLU(26)
   integer :: number
   integer :: nt
   integer :: igoff
   integer :: igr
   integer :: ib
   integer :: ic
   integer :: ig
   integer :: ico
   integer :: k,l,n
!  IRLU IS A LOOK-UP TABLE FOR ROMAN CHARACTERS ARRANGED BY
!  INTEGER VALUE FOR THE ASCII CHARACTER SET WITH AN
!  OFFSET TO REMOVE THE 31 NONPRINTING CONTROL CHARACTERS.
!  IRLU RETURNS WITH THE SYMBOL NUMBER OR, IF NO SYMBOL
!  EXISTS, THE CODE FOR SPACE.
   data irlu/1000,416,428,411,72,418,419,432,67,68,69,63,70, &
     &          64,71,65,53,54,55,56,57,58,59,60,61,62,414,415, &
     &          385,66,386,417,407,1,2,3,4,5,6,7,8,9,10,11,12,13, &
     &          14,15,16,17,18,19,20,21,22,23,24,25,26,409,1000, &
     &          410,408,1000,1000,27,28,29,30,31,32,33,34,35,36, &
     &          37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52, &
     &          405,427,406,424/
!  IILU IS A LOOK-UP TABLE FOR ITALIC CHARACTERS ONLY. IT IS
!  IDENTICAL TO IRLU WITH FOUR ITALIC SPECIAL SYMBOLS SUBSTITUTED
!  FOR REGULAR ONES.
   data iilu/1000,422,1000,411,72,418,419,1000,67,68,69,63,70, &
     &          64,71,65,53,54,55,56,57,58,59,60,61,62,420,421, &
     &          385,66,386,423,407,1,2,3,4,5,6,7,8,9,10,11,12,13, &
     &          14,15,16,17,18,19,20,21,22,23,24,25,26,409,1000, &
     &          410,1000,1000,1000,27,28,29,30,31,32,33,34,35,36, &
     &          37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52, &
     &          405,427,406,424/
!  IGLU IS A LOOK-UP TABLE FOR GREEK CHARACTERS ARRANGED BY THE
!  INTEGER VALUE OF THEIR ROMAN EXPRESSION WITH A=1, B=2, ETC.
!  AMBIGUOUS CASES GIVE 25 FOR EPSILON OR ETA, 26 FOR OMEGA OR
!  OMICRON, 27 FOR PHI,PI,OR PSI, AND 28 FOR TAU OR THETA. ADDITIONAL
!  LETTERS MUST BE CHECKED FOR THESE CASE. A VALUE OF 50 IS RETURNED
!  FOR THOSE ROMAN LETTERS WHICH HAVE NO CORRESPONDING GREEK LETTER.
   data iglu/1,2,22,4,25,50,3,50,9,50,10,11,12,13,26,27,50,17,18,28,20,50,50,14,50,6/
! FINDS LENGTH OF STRING WITH BLANKS TRIMMED FROM RIGHT END.
   DO N=NTEXT,1,-1
      IF(TEXT(N:N).NE.' ')GOTO 15
   enddo
   P_NCHR=0
   RETURN
15 continue
   NT=N
!  SCAN TEXT CHARACTER BY CHARACTER
   K=1
   J=1
!  K IS CURRENT ADDRESS OF CHARACTER IN TEXT
!  J IS INDEX OF NEXT SYMBOL CODE IN P_ICHR
   INFINITE: do
20 continue
      IF(K.GT.N)THEN
        P_NCHR=J-1
        RETURN
      ENDIF
      IF(TEXT(K:K).NE.'\')THEN
        !  ROMAN CHARACTER OR KEYBOARD SYMBOL
        IF(TEXT(K:K).EQ.'}')THEN
          !  CHECK FOR CLOSING CURLY BRACKET-IF FOUND, RETURN 1003
          P_ICHR(J)=1003
          J=J+1
          K=K+1
          CYCLE INFINITE
          GOTO 20
        ENDIF
        !  ICHAR RETURNS INTEGER ASCII VALUE OF CHARACTER
        !  OFFSET BY NONPRINTING CHARACTERS TO GET ENTRY IN LOOK-UP TABLE
        IC=ICHAR(TEXT(K:K))-ICHAR(' ')+1
        IF(IC.LE.0)THEN                           !  NONPRINTING CONTROL CHARACTER-ERROR RETURN
          P_ICHR(J)=1000
        ELSEIF (P_IOFF.NE.240)THEN                !  NOT ITALIC FONT
          P_ICHR(J)=IRLU(IC)
        ELSE                                      !  ITALIC FONT
          P_ICHR(J)=IILU(IC)
        ENDIF
        IF(P_ICHR(J).LT.385)P_ICHR(J)=P_ICHR(J)+P_IOFF    !  ADD OFFSET FOR FONT IF NOT A SPECIAL SYMBOL
          J=J+1
          K=K+1
          CYCLE INFINITE
          GOTO 20
        ELSE                                      !  BACKSLASH FOUND
          !  CHECK NEXT FOUR CHARACTERS FOR FOUR DIGIT NUMBER
          K=K+1
          READ(TEXT(K:K+3),'(i4)',ERR=50)NUMBER
          !  NUMBER FOUND-CHECK ITS VALIDITY
          IC=NUMBER-1000
          IF((IC.GT.0).AND.(IC.LT.433))THEN
            !  VALID SYMBOL CODE
            P_ICHR(J)=IC
          ELSEIF ((IC.GT.999).AND.(IC.LT.1004))THEN
            !  VALID COMMAND CODE
            P_ICHR(J)=IC
          ELSE
            ! NOT RECOGNIZED-ERROR RETURN
            P_ICHR(J)=1000
          ENDIF
          J=J+1
           !  MOVE BEYOND CLOSING BACKSLASH-IGNORE EXTRA CHARACTERS
           !  FUNCTION INDEX RETURNS OFFSET OF SECOND SUBSTRING IN FIRST
           !  RETURNS 0 IF SUBSTRING NOT FOUND
           L=INDEX(TEXT(K:NT),'\')
           IF(L.EQ.0)THEN
             K=NT+1
           ELSE
             K=K+L
           ENDIF
          CYCLE INFINITE
           GOTO 20
   50      CONTINUE
           !  NOT A NUMBER
           !  CHECK FOR FONT CHANGE COMMAND
         IF(TEXT(K:K+2).EQ.'SIM'.OR.TEXT(K:K+2).EQ.'sim')THEN
           !  SIMPLEX FONT
           P_IOFF=0
         ELSEIF(TEXT(K:K+1).EQ.'CO'.OR.TEXT(K:K+1).EQ.'co')THEN
           !  COMPLEX FONT
           P_IOFF=120
         ELSEIF(TEXT(K:K+1).EQ.'IT'.OR.TEXT(K:K+1).EQ.'it')THEN
           !  ITALIC FONT
           P_IOFF=240
         ELSEIF (TEXT(K:K+1).EQ.'DU'.OR.TEXT(K:K+1).EQ.'du')THEN
           !  DUPLEX FONT
           P_IOFF=312
           !  FOUND THE BACK-SPACE CODE
         ELSEIF(TEXT(K:K+1).EQ.'BS'.OR.TEXT(K:K+1).EQ.'bs') THEN
           P_ICHR(J)=1004
           J=J+1
           K=K+3
           GO TO 20
          CYCLE INFINITE
           !  CHECK FOR SUPER/SUB-SCRIPT COMMAND
         ELSEIF(TEXT(K:K+3).EQ.'SUP{'.OR.TEXT(K:K+3).EQ.'sup{')THEN
           !  BEGIN SUPERSCRIPTING
           P_ICHR(J)=1001
           J=J+1
           K=K+4
           GOTO 20
          CYCLE INFINITE
         ELSEIF (TEXT(K:K+3).EQ.'SUB{'.OR.TEXT(K:K+3).EQ.'sub{')THEN
           !  BEGIN SUBSCRIPTING
           P_ICHR(J)=1002
           J=J+1
           K=K+4
           GOTO 20
          CYCLE INFINITE
         ELSE
           !  GREEK CHARACTER OR INVALID CHARACTER
           IC=ICHAR(TEXT(K:K))
           IGOFF=MIN0(P_IOFF, 120)
           IF(P_IOFF.EQ.312)IGOFF=0
           IF((IC.GE.ICHAR('A')).AND.(IC.LE.ICHAR('Z')))THEN
             !  UPPER CASE
             IGR=72
             ICO=ICHAR('A')-1
           ELSEIF((IC.GE.ICHAR('a')).AND.(IC.LE.ICHAR('z')))THEN
             !  LOWER CASE
             IGR=96
             ICO=ICHAR('a')-1
           ELSE
             !  NOT A LETTER-ERROR RETURN
             P_ICHR(J)=1000
             J=J+1
             L=INDEX(TEXT(K:NT),'\')
             IF(L.EQ.0)THEN
               K=NT+1
             ELSE
               K=K+L
             ENDIF
             GOTO 20
          CYCLE INFINITE
           ENDIF
           !  LOOK UP THE CHARACTER
           IG=IGLU(IC-ICO)
           IF(IG.LT.25)THEN                !  UNAMBIGUOUS GREEK LETTER
             P_ICHR(J)=IG+IGR+IGOFF
           ELSEIF (IG.EQ.25)THEN           !  EPSILON OR ETA
             IB=ICHAR(TEXT(K+1:K+1))-ICO
             IF(IB.EQ.16)THEN              !  EPSILON
               P_ICHR(J)=5+IGR+IGOFF
             ELSEIF (IB.EQ.20)THEN         !  ETA
               P_ICHR(J)=7+IGR+IGOFF
             ELSE                          !  NOT A GREEK CHARACTER--ERROR RETURN
               P_ICHR(J)=1000
             ENDIF
         ELSEIF (IG.EQ.26)THEN             !  OMEGA OR OMICRON
           IB=ICHAR(TEXT(K+1:K+1))-ICO
           IF(IB.NE.13)THEN                ! NOT A GREEK CHARACTER-ERROR RETURN
             P_ICHR(J)=1000
           ELSE
             IC=ICHAR(TEXT(K+2:K+2))-ICO
             IF(IC.EQ.5)THEN               !  OMEGA
               P_ICHR(J)=24+IGR+IGOFF
             ELSEIF (IC.EQ.9)THEN          !  OMICRON
               P_ICHR(J)=15+IGR+IGOFF
             ELSE                          !  NOT A GREEK CHARACTER-ERROR RETURN
               P_ICHR(J)=1000
             ENDIF
           ENDIF
         ELSEIF (IG.EQ.27)THEN             !  PHI,PI, OR PSI
           IB=ICHAR(TEXT(K+1:K+1))-ICO
           IF(IB.EQ.8)THEN                 !  PHI
             P_ICHR(J)=21+IGR+IGOFF
           ELSEIF (IB.EQ.9)THEN            !  PI
             P_ICHR(J)=16+IGR+IGOFF
           ELSEIF (IB.EQ.19)THEN           !  PSI
             P_ICHR(J)=23+IGR+IGOFF
           ELSE                            !  NOT A GREEK CHARACTER-ERROR RETURN
             P_ICHR(J)=1000
           ENDIF
           ELSEIF (IG.EQ.28)THEN           ! TAU OR THETA
             IB=ICHAR(TEXT(K+1:K+1))-ICO
             IF(IB.EQ.1)THEN               !  TAU
               P_ICHR(J)=19+IGR+IGOFF
             ELSEIF(IB.EQ.8)THEN           !  THETA
               P_ICHR(J)=8+IGR+IGOFF
             ELSE                          !  NOT A GREEK CHARACTER-ERROR RETURN
               P_ICHR(J)=1000
             ENDIF
           ELSE                            !  NOT A GREEK CHARACTER-ERROR RETURN
             P_ICHR(J)=1000
           ENDIF
          J=J+1
        ENDIF
        L=INDEX(TEXT(K:NT),'\')
        IF(L.EQ.0)THEN
          K=NT+1
        ELSE
          K=K+L
        ENDIF
        GOTO 20
        CYCLE INFINITE
      ENDIF
      exit INFINITE
      enddo INFINITE
END SUBROUTINE CHRCOD
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    strlength(3f) - [M_pixel:TEXT] return length of string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    function strlength(string)
!!    character(len=*),intent(in)    :: string
!!
!!##DESCRIPTION
!!    Return the length of the string "STRING" in world units.
!!
!!##RETURNS
!!    STRLENGTH  length of string using current font size
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_strlength
!!    use :: M_pixel
!!    use :: M_pixel__writegif, only : writegif
!!    implicit none
!!    real    :: left
!!    real    :: baseline
!!    integer :: icolor=0
!!    real    :: texth=10.0
!!       !! set up drawing surface
!!       call prefsize(800, 400)
!!       call vinit()
!!       call viewport(0.0, 800.0, 400.0, 0.0)
!!       call ortho2(-100.0, 300.0, -100.0, 100.0)
!!       call color(7)
!!       call clear()
!!       call linewidth(30)
!!       call textsize(texth, texth)
!!       call xcentertext()
!!       call color(1)
!!
!!       baseline=85.0
!!       call move2(0.0,baseline)
!!       call drawstr('If I Can Stop One Heart')
!!       baseline= baseline-texth*1.20
!!       call move2(0.0,baseline)
!!       call drawstr('by Emily Dickinson')
!!       call centertext(.false.)
!!
!!       texth=8.5
!!       baseline=baseline-texth*1.50
!!       call textsize(texth, texth)
!!       left=-90.0
!!
!!       call nextline('If I can stop one heart from breaking,')
!!       call nextline('I shall not live in vain;')
!!       call nextline('If I can ease one life the aching,')
!!       call nextline('Or cool one pain,')
!!       call nextline('Or help one fainting robin')
!!       call nextline('Unto his nest again,')
!!       call nextline('I shall not live in vain.')
!!
!!       call writegif('strlength.3M_pixel.gif',P_pixel,P_colormap)
!!       call execute_command_line('display strlength.3M_pixel.gif')
!!       call vexit()
!!    contains
!!    subroutine nextline(string)
!!    character(len=*) :: string
!!    real :: xx
!!    !! reduce some duplicate code; very specific to this example
!!       call color(icolor)
!!       baseline=baseline-texth*1.5    ! move down before drawing line
!!       call makepoly()
!!       xx=strlength(string)
!!       call rect(left,baseline-texth*0.3,left+xx,baseline+texth)
!!       call closepoly()
!!       call color(7)
!!       call move2(left, baseline)
!!       call drawstr(string)    ! draw string
!!       icolor=icolor+1         ! set pen color
!!    end subroutine nextline
!!
!!    end program demo_strlength
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function strlength(string)

! ident_8="@(#) M_pixel strlength length of string using current font size"

character(len=*),intent(in)    :: string
real                           :: strlength

   real                        :: s(4)
   !!character(len=:),allocatable :: fontstring
   !!fontstring='\'//trim(P_FONT)//'\'//trim(string)

   call justfy(s, P_TEXT_HEIGHT, trim(string), len_trim(string))

!  S(1)  to the left edge of the 1st nonblank character
!  s(2)  to the center of the string, blanks removed from the ends
!  s(3)  to the right edge of the last nonblank character
!  s(4)  to the right edge of the last character of the string.

   strlength=s(4)

end function strlength
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    justfy(3f) - [M_pixel:TEXT] return lengths used to justify a string
!!                 when calling hershey
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine justfy(s, height, text, ntext)
!!    real,intent(out)               :: s(4)
!!    real,intent(in)                :: height
!!    character(len=*),intent(in)    :: text
!!    integer,intent(in)             :: ntext
!!
!!##DESCRIPTION
!!    Given the text string TEXT with NTEXT characters, height HEIGHT,
!!    this routine gives 4 distances in inches, all from the left end of
!!    the string -
!!
!!    o S(1)  to the left edge of the 1st nonblank character
!!    o S(2)  to the center of the string, blanks removed from the ends
!!    o S(3)  to the right edge of the last nonblank character
!!    o S(4)  to the right edge of the last character of the string.
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine justfy(s, height, text, ntext)

! ident_9="@(#) M_pixel justfy(3f) calculate values for justifying Hershey fonts called by hershey(3f)"

!  Given the text string TEXT with NTEXT characters, height HEIGHT, this routine
!  gives 4 distances in inches, all from the left end of the string -
!  S(1)  to the left edge of the 1st nonblank character
!  s(2)  to the center of the string, blanks removed from the ends
!  s(3)  to the right edge of the last nonblank character
!  s(4)  to the right edge of the last character of the string.

      real,intent(out)               :: s(4)
      real,intent(in)                :: height
      character(len=*),intent(in)    :: text
      character(len=4096)            :: text_local
      integer,intent(in)             :: ntext

      real,parameter                 :: factor=0.75
      integer,parameter              :: ipower(*)=[1,1,-1]
      real                           :: scale
      real                           :: oldwid
      integer                        :: jquart
      integer                        :: lead
      integer                        :: i
      integer                        :: l
      integer                        :: ntxt
!
      text_local=text
      ntxt=ntext
      scale=height/21.0
      jquart=(ntext+3)/4
!  translate integer string into character variable, then get pointers
!  into the array P_ichr.
!
      call chrcod(text_local,ntxt)
!
!  count leading blanks.
      do lead=1,P_nchr
         if(P_ichr(lead).ne.1000)goto 1110
      enddo
      lead=ntxt
 1110 continue
      s(1)=20.0*scale*(lead-1)
      s(3)=s(1)
!
!  sum the widths of the remaining text, recalling that trailing blanks
!  were lopped off by chrcod.
      oldwid=0.0
      if(lead.ne.0)then
         do i=lead,P_nchr
            l=P_ichr(i)
            if (l.lt.1000) then
              oldwid=width(l)*scale
              s(3)=s(3) + oldwid
            endif
            if(l.eq.1000)s(3)=s(3)+20.0*scale
            if(l.ge.1001.and.l.le.1003)scale=scale*factor**ipower(l-1000)
            if(l.eq.1004)s(3)=s(3)-oldwid
         enddo
      endif
!
!  add on width of surplus trailing blanks.
      s(4)=s(3)+20.0*scale*(ntxt-P_nchr)
!
!  find center of nonblank text.
      s(2)=(s(1)+s(3))/2.0
      P_just2=1
end subroutine justfy
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    polyline2(3f) - [M_pixel:DRAW] - draw an unclosed polyline in the XY plane
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine polyline2(arrx,arry)
!!
!!           integer,intent(in)          :: arrx(:)
!!           integer,intent(in),optional :: arry(:)
!!
!!##DESCRIPTION
!!        Given either a single array composed of pairs <x(i),y(i)> of
!!        values defining points or an X and Y array move to first point
!!        and draw to remaining points using current line style.
!!
!!##OPTIONS
!!        ARRX   If ARRY is present, an array of X values
!!
!!        ARRY   An optional array of Y values
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_polyline2
!!    use M_pixel
!!    use M_pixel__writegif, only : writegif
!!    implicit none
!!    integer :: transparent=0
!!    integer :: ipaws
!!       call prefsize(300,300)
!!       call vinit(' ')
!!       call ortho2(-2.0,2.0,-2.0,2.0)
!!       call color(2)
!!       call linewidth(100)
!!       call polyline2([-0.5,-0.5, -0.5,+0.5, +0.5,+0.5, +0.5,-0.5])
!!       call color(4)
!!       call polyline2( [-1,-1,+1,+1,-1] , &  ! X values
!!       & [-1,+1,+1,-1,-1] )    ! Y values
!!        ! write gif with a transparent background
!!       call writegif('polyline2.3M_pixel.gif',P_pixel,P_ColorMap,transparent)
!!       call vexit()
!!    end program demo_polyline2
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine polyline2(x,y)
!-!use :: M_pixel, only : anyscalar_to_real
class(*),intent(in)          :: x(:)
class(*),intent(in),optional :: y(:)
real,allocatable             :: arrx(:)
real,allocatable             :: arry(:)
integer                      :: i
integer                      :: isizex
integer                      :: isizey
integer                      :: ipairs
! assuming nice data in x,y pairs
arrx=anyscalar_to_real(x)
if(present(y))then    ! two arrays means X array and Y array
   arry=anyscalar_to_real(y)
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

end subroutine polyline2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    clear(3f) - [M_pixel] clear background to current color or specified
!!                color index
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine clear(indx)
!!    integer,intent(in),optional :: indx
!!
!!##DESCRIPTION
!!    Clears the screen to the current color or to color specified
!!
!!##OPTIONS
!!    INDX   color index to set pixel array to. Optional
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_clear
!!    use :: M_pixel
!!    use :: M_pixel__writegif, only : writegif
!!    implicit none
!!    real,parameter :: x=400.0, y=400.0
!!       call prefsize(int(x), int(y)) ! set up drawing surface
!!       call vinit()
!!       call color(1)
!!       call linewidth(300)
!!       ! clear a circle and rectangle in default window and viewport
!!       call rect(0.0,0.0,x,y)
!!       call circle(x/2.0,y/2.0,x/2.0)
!!       ! now clear screen to current color
!!       call color(3)
!!       call clear()
!!       ! gif should be blank
!!       call writegif('clear.3M_pixel.gif',P_pixel,P_colormap)
!!       call execute_command_line('display clear.3M_pixel.gif')
!!       call vexit()
!!    end program demo_clear
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine clear(indx)

! ident_10="@(#) M_pixel clear(3f) set background color all to specified color index"

integer,intent(in),optional :: indx
call if_init()
if(present(indx))then
   P_pixel=indx
else
   P_pixel=P_COLOR_INDEX
endif
end subroutine clear
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    pixel(3f) - [M_pixel] set pixel to current color
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    elemental impure subroutine pixel(row,column,indx)
!!    integer,intent(in)          :: row
!!    integer,intent(in)          :: column
!!    integer,intent(in),optional :: indx
!!
!!##DESCRIPTION
!!    Directly set a pixel to the current or specified color index.
!!    The ROW and COLUMN start at 1.
!!
!!##OPTIONS
!!    ROW      row number in P_pixel to set
!!
!!              0 < ROW < size(P_pixel,dim=1)-1.
!!    COLUMN   column number in P_pixel to set
!!
!!              0 < COLUMN < size(P_pixel,dim=2)-1.
!!
!!    INDX     color index to set pixel array to. Optional
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_pixel
!!    use :: M_pixel
!!    implicit none
!!       call prefsize(10,10) ! set up drawing surface
!!       call mapcolor(0,255,255,255)
!!       call mapcolor(1,255,000,000)
!!       call mapcolor(2,255,255,000)
!!       call mapcolor(3,255,000,255)
!!       call mapcolor(4,000,255,255)
!!       call mapcolor(5,000,255,000)
!!       call mapcolor(6,000,000,255)
!!       call mapcolor(7,000,000,000)
!!       call vinit()
!!       call color(0)
!!       call clear()
!!       call color(1)
!!       call pixel(1,1)
!!       call color(3)
!!       call pixel(3,3)
!!       call pixel(5,5,5)
!!       call print_ascii()
!!       call vexit()
!!    end program demo_pixel
!!
!!   Results:
!!
!!    1000000000
!!    0000000000
!!    0030000000
!!    0000000000
!!    0000500000
!!    0000000000
!!    0000000000
!!    0000000000
!!    0000000000
!!    0000000000
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental impure subroutine pixel(row,column,indx)

! ident_11="@(#) M_pixel pixel(3f) set background color all to specified color index"

integer,intent(in)          :: row
integer,intent(in)          :: column
integer,intent(in),optional :: indx
   call if_init()
   CHECK: block
      if(row.lt.1.or.row.gt.P_VIEWPORT_HEIGHT) exit CHECK
      if(column.lt.1.or.column.gt.P_VIEWPORT_WIDTH) exit CHECK
      if(present(indx))then
         P_pixel(row-1,column-1)=indx
      else
         P_pixel(row-1,column-1)=P_COLOR_INDEX
      endif
   end block CHECK
end subroutine pixel
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine if_init()

! ident_12="@(#) M_pixel if_init(3f) check if pixel graphics library has been initialized"

   if(.not.P_VINIT_CALLED)then
      write(*,*)'*draw_line_single* WARNING: P_vinit(3f) was not called'
      call vinit()
   endif
end subroutine if_init
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    arc(3f) - [M_pixel:ARCS] draw an arc using current line width and color
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine arc(x, y, radius, startang, endang)
!!    real,intent(in) :: x
!!    real,intent(in) :: y
!!    real,intent(in) :: radius
!!    real,intent(in) :: startang
!!    real,intent(in) :: endang
!!
!!##DESCRIPTION
!!    Draw an arc. x, y, and radius are values in world units.
!!
!!    Angles are in degrees, positive measured counterclockwise from the
!!    +X axis. The current position after the arc is drawn is at the end
!!    of the arc.
!!
!!##OPTIONS
!!    X,Y        Coordinates for the center of the circle
!!    RADIUS     Radius of the circle
!!    STARTANG   Start angle
!!    ENDANG     End angle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_arc
!!    use M_pixel
!!    use M_pixel__writegif, only : writegif
!!    implicit none
!!    integer  :: transparent=0
!!       call prefsize(600,240)
!!       call vinit()
!!       call ortho2(0.0,60.0,0.0,24.0)
!!       call linewidth(400)
!!       call color(1)
!!       call arc(16.0,12.0,12.0,90.0,270.0)
!!       call color(2)
!!       call arc(44.0,12.0,12.0,-90.0,90.0)
!!       ! write gif with a transparent background
!!       call writegif('arc.3M_pixel.gif',P_pixel,P_ColorMap,transparent)
!!       call vexit()
!!    end program demo_arc
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine arc(x,y,radius,startang,endang)

! ident_13="@(#) M_pixel arc(3f) draw a arc using current line width and color"

real,intent(in) :: x,y
real,intent(in) :: radius
real,intent(in) :: startang,endang

   real               :: deltang
   integer            :: i
   real               :: dx,dy,cx,cy,cosine,sine
   integer            :: numsegs

   numsegs = nint( abs(endang - startang) / 360.0) * P_nsegs
   deltang = (endang - startang) / numsegs
   cosine = cosd(deltang)
   sine = sind(deltang)

   ! calculates initial point on arc

   cx = x + radius * cosd(startang)
   cy = y + radius * sind(startang)
   call move2(cx, cy)

   do i=0,numsegs-1
      dx = cx - x
      dy = cy - y
      cx = x + dx * cosine - dy * sine
      cy = y + dx * sine + dy * cosine
      call draw2(cx, cy)
   enddo

end subroutine arc
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    circle(3f) - [M_pixel:ARCS] draw a circle using current line width and color
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine circle(x,y,radius)
!!    real,intent(in) :: x
!!    real,intent(in) :: y
!!    real,intent(in) :: radius
!!
!!##DESCRIPTION
!!    Draw a circle using the current line width and color into the pixel
!!    array. Units are in world coordinates.
!!
!!##OPTIONS
!!    X,Y        Coordinates for the center of the circle
!!    RADIUS     Radius of the circle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_circle
!!    use M_pixel
!!    use M_pixel__writegif, only : writegif
!!    implicit none
!!       !! set up drawing surface
!!       call prefsize(400,400)
!!       call vinit()
!!       call ortho2(left=-100.0, right=100.0, bottom=-100.0, top=100.0)
!!       call color(3)
!!       call clear()
!!       call color(4)
!!       call linewidth(200)
!!       !! draw some circles
!!       call circle(0.0, 0.0, 90.0)
!!       call color(1)
!!       call circle(0.0, 0.0, 40.0)
!!       call color(2)
!!       call circle(-25.0, 25.0, 20.0)
!!       call circle(-25.0,-25.0, 20.0)
!!       call circle( 25.0, 25.0, 20.0)
!!       call circle( 25.0,-25.0, 20.0)
!!       !! render the pixel map
!!       call writegif('circle.3M_pixel.gif',P_pixel,P_colormap)
!!       !! display the graphic assuming display(1) is available
!!       call execute_command_line('display circle.3M_pixel.gif')
!!       !! exit graphics mode
!!       call vexit()
!!    end program demo_circle
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine circle(x,y,radius)

! ident_14="@(#) M_pixel circle(3f) draw a circle using current line width and color"

real,intent(in) :: x
real,intent(in) :: y
real,intent(in) :: radius

real               :: degrees
real               :: increment
integer            :: i
real               :: xx1,yy1, xx2,yy2

increment=360.0/P_NSEGS

do i=1,P_NSEGS

   degrees=(i-1)*increment
   xx1=x+radius*cosd(degrees)
   yy1=y+radius*sind(degrees)

   degrees=i*increment
   xx2=x+radius*cosd(degrees)
   yy2=y+radius*sind(degrees)

   call line(xx1,yy1,xx2,yy2)
enddo

end subroutine circle
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    linewidth(3f) - [M_pixel] set linewidth
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine linewidth(iwidth)
!!    integer iwidth
!!
!!##DESCRIPTION
!!    Set the current line width in units of 1/10,000 of the X size of the
!!    display surface
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_linewidth
!!    use M_pixel,    only : prefsize, vinit, ortho2, clear, P_pixel, P_colormap
!!    use M_pixel,    only : move2, draw2, vexit, color, linewidth
!!    use M_pixel__writegif, only : writegif
!!    use M_pixel,    only : d2r, polar_to_cartesian
!!    implicit none
!!    integer :: i
!!    real    :: x,y,r,a,b,theta
!!    ! The Archimedean spiral is the locus of points corresponding
!!    ! to the locations over time of a point moving away from a
!!    ! fixed point with a constant speed along a line which rotates
!!    ! with constant angular velocity.
!!    !    r=a+b*theta
!!    ! Changing the parameter a will turn the spiral,
!!    ! while b controls the distance between successive turnings.
!!       call prefsize(401,401)
!!       call vinit('')
!!       call ortho2(-150.0,150.0,-150.0,150.0)
!!       call clear()
!!       call move2(0.0,0.0)
!!       call color(2)
!!       a=0.0
!!       b=2.0
!!       do i=0,360*10,5
!!          theta=d2r(i)
!!          r=a+b*theta
!!          call polar_to_cartesian(r,theta,x,y)
!!          call linewidth(i/5/3)
!!          call draw2(x,y)
!!       enddo
!!       call writegif('linewidth.3M_pixel.gif',P_pixel,P_colormap)
!!       call vexit()
!!    end program demo_linewidth
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine linewidth(iwidth)

! ident_15="@(#) M_pixel linewidth(3f) set line width for lines drawn in pixel image"

integer,intent(in) :: iwidth
   real            :: xwidth
   xwidth= iwidth*P_VIEWPORT_WIDTH /10000
   P_width=max(nint(xwidth),1)
end subroutine linewidth
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    color(3f) - [M_pixel:COLOR] set current color index
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine color(col)
!!    integer,intent(in) :: col
!!
!!##DESCRIPTION
!!    Set the current color. The standard colors are as follows:
!!
!!       black  =  0  red      =  1  green  =  2  yellow  =  3
!!       blue   =  4  magenta  =  5  cyan   =  6  white   =  7
!!
!!##OPTION
!!     COL  A color number from 0 to 255. To define additional
!!          colors see mapcolor(3f).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_color
!!     use M_pixel
!!     use M_pixel__writegif, only : writegif
!!     implicit none
!!     real    :: b=0.5
!!     real    :: y1,y2,ym,x1,x2
!!     real    :: width=50.0/8.0,width2
!!     integer :: i
!!        !! set up long bar as plotting area
!!        call prefsize(1000,200)
!!        call vinit()
!!        call ortho2(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!        call textsize( 3.5, 4.0)
!!        call font('DUPLEX')
!!        call centertext(.true.)
!!        call linewidth(90)
!!        y1=-5
!!        y2=5
!!        ym=0
!!        x1=-25+.05*width
!!        ! draw colored rectangle and a circle and label center of circle
!!        ! and repeat from colors 0 to 7.
!!        width2=width*0.95
!!        do i=0,7
!!           call color(i)
!!           x2=x1+width2
!!           call makepoly()
!!           call rect(x1,y1,x2,y2)
!!           call closepoly()
!!           call color(i+1)
!!           call move2((x1+x2)/2.0,ym)
!!           call drawstr(i)     ! convert number to string and draw it
!!           call circle((x1+x2)/2.0, ym, (x2-x1)/2.10)
!!           x1=x1+width
!!        enddo
!!        ! write plot as GIF file
!!        call writegif('color.3M_pixel.gif',P_pixel,P_colormap)
!!        call vexit()
!!        ! use system to display GIF file
!!        call execute_command_line('display color.3M_pixel.gif')
!!     end program demo_color
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine color(icolor)

! ident_16="@(#) M_pixel color(3f) set current color for lines drawn in pixel image"

integer,intent(in) :: icolor
   P_COLOR_INDEX=icolor
end subroutine color
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     mapcolor(3f) - [M_pixel:COLOR] set a color index using RGB values
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    elemental impure subroutine mapcolor(indx, red, green, blue)
!!    integer indx, red, green, blue
!!
!!##DESCRIPTION
!!    Set the color map index indx to the color represented by (red,
!!    green, blue). rgb values are in the range of 0 to 255.
!!
!!##OPTIONS
!!    INDX    color index number, in range 0 to 255
!!    RED     red component of color being defined, in range 0 to 255
!!    GREEN   green component of color being defined, in range 0 to 255
!!    BLUE    blue component of color being defined, in range 0 to 255
!!
!!##EXAMPLE
!!
!!  Color wheel example:
!!
!!    !     good program to exercise color tables, and look at differences
!!    !     when actual output device has a color table that is dynamic,
!!    !     or only has a small color table (a frame in this program takes
!!    !     at least SLICES*RINGS colors to produce accurately).
!!    !
!!    program demo_mapcolor
!!    use M_pixel
!!    use M_pixel, only: hue
!!    use M_pixel__writegif, only : writegif
!!    use M_pixel,    only : cosd, sind
!!    use M_pixel__writegif_animated, only : write_animated_gif
!!    implicit none
!!    character(len=4096)  :: filename
!!    real                 :: lightstep
!!    integer              :: ii,iframe
!!    integer,parameter    :: SLICES=30
!!    integer,parameter    :: RINGS=  8
!!    real                 :: LIGHTNESS
!!    integer,parameter    :: BOX=1200
!!    integer              :: movie(1:19,0:box-1,0:box-1)
!!       call prefsize(BOX,BOX)
!!       call vinit(' ')
!!       call color(0)
!!       call clear()
!!       call color(7)
!!       call page(-110./2.,85./2.,-110./2.,110./2.)
!!       LIGHTNESS=100.0
!!       lightstep=-5
!!       do ii=1,19
!!          iframe=ii
!!          call color(0)
!!          call clear()
!!          call color(7)
!!          call wheel()
!!          write(filename,'("mapcolor.3_",i3.3,".gif")')int(LIGHTNESS)
!!          call writegif(filename,P_pixel,P_colormap)
!!          movie(ii,:,:)=P_pixel
!!          LIGHTNESS=LIGHTNESS+LIGHTSTEP
!!       enddo
!!       call write_animated_gif('mapcolor.3M_pixel.gif',movie,P_colormap,delay=40)
!!       call vexit()
!!    contains
!!    subroutine wheel() ! draw an entire wheel
!!       character(len=40) :: inline
!!       real              :: hue_val
!!       integer           :: ii
!!       call textang(0.0)
!!       call color(7)
!!       call textsize(5.0,6.0)
!!       call font('times.r')
!!       call move2(0.0,103.0/2.0)
!!       call centertext(.true.)
!!       call linewidth(30)
!!       call drawstr('COLOR WHEEL')
!!       call linewidth(0)
!!       call textsize( 2.5,2.5)
!!       call font('futura.l')
!!       call move2(0.0,90.0/2.0)
!!       write(inline,'("lightness=",f6.2)')LIGHTNESS
!!       call linewidth(30)
!!       call drawstr(inline)
!!       call linewidth(0)
!!       call textsize(1.5,1.5)
!!       hue_val=0
!!       do ii=SLICES, 1,-1
!!          call slice(hue_val)
!!       enddo
!!       call centertext(.false.)
!!    end subroutine wheel
!!    subroutine slice(hue_val) ! draw a slice
!!    integer           :: buffer
!!    real              :: hue_val, ang_inc
!!    character(len=40) :: inline
!!    real              :: step
!!    real              :: X1, X2, X3, X4
!!    real              :: Y1, Y2, Y3, Y4
!!    !
!!    integer           :: maxcolors, current_color
!!    integer           :: ir, ig, ib
!!    real              :: r,g,b
!!    real              :: saturation
!!    !
!!    integer           :: status
!!    integer           :: icount
!!    real              :: angle1, angle2
!!    real              :: radius1, radius2, radius3, radius4
!!    !
!!    integer,save      :: color_count=0
!!    !
!!       buffer=8
!!       ANG_INC=360.0/SLICES
!!       angle1=hue_val-ANG_INC/2
!!       angle2=angle1+ANG_INC
!!       saturation=100
!!       radius1=32
!!       radius3=radius1+4
!!       radius4=radius1+7
!!       ! draw tic from wheel to start of angle label
!!       call color(7)
!!       call linewidth(40)
!!       call move2( radius1*cosd(hue_val), radius1*sind(hue_val) )
!!       call draw2( radius3*cosd(hue_val), radius3*sind(hue_val) )
!!       ! draw degree label at tic
!!       call textang(hue_val)
!!       call move2( radius4*cosd(hue_val), radius4*sind(hue_val) )
!!       write(inline,'(i0)')nint(hue_val)
!!       call linewidth(20)
!!       call drawstr(inline)
!!       call linewidth(0)
!!       step=radius1/(RINGS)
!!       radius2=radius1-step
!!       ! draw a chunk in a slice
!!       MAXCOLORS=(256)-buffer
!!       do icount=RINGS+1,2,-1
!!          ! add buffer to leave base colors alone
!!          CURRENT_COLOR=MOD(color_count,MAXCOLORS)+buffer
!!          color_count=color_count+1
!!          ! fancy mapcolor
!!          call hue("hls",hue_val,LIGHTNESS,saturation,"rgb",r,g,b,status)
!!          ir=int(r*255.0/100.0+0.50)
!!          ig=int(g*255.0/100.0+0.50)
!!          ib=int(b*255.0/100.0+0.50)
!!          call mapcolor(CURRENT_COLOR,ir,ig,ib)
!!          call color(CURRENT_COLOR)
!!          !
!!          X1=cosd(angle1)*radius2
!!          Y1=sind(angle1)*radius2
!!          X2=cosd(angle1)*radius1
!!          Y2=sind(angle1)*radius1
!!          !
!!          X3=cosd(angle2)*radius2
!!          Y3=sind(angle2)*radius2
!!          X4=cosd(angle2)*radius1
!!          Y4=sind(angle2)*radius1
!!          !
!!          call makepoly()
!!          call move2(X1,Y1)
!!          call draw2(X2,Y2)
!!          call draw2(X4,Y4)
!!          call draw2(X3,Y3)
!!          call closepoly()
!!          !
!!          saturation=saturation-100.0/RINGS
!!          radius1=radius2
!!          radius2=radius1-step
!!       enddo
!!       hue_val=hue_val+ANG_INC
!!    end subroutine slice
!!    end program demo_mapcolor
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental impure subroutine mapcolor(indx,red,green,blue)

! ident_17="@(#) M_pixel mapcolor(3f) set a color index using RGB values"

integer,intent(in) :: indx
integer,intent(in) :: red
integer,intent(in) :: green
integer,intent(in) :: blue
   CHECKRANGE: block

      if(  indx .lt. 0 .or. indx  .gt. 255) exit CHECKRANGE
      if(   red .lt. 0 .or. red   .gt. 255) exit CHECKRANGE
      if( green .lt. 0 .or. green .gt. 255) exit CHECKRANGE
      if(  blue .lt. 0 .or. blue  .gt. 255) exit CHECKRANGE

      P_ColorMap(:,indx)= [red,green,blue]
      return

   endblock CHECKRANGE
   write(*,*)'*mapcolor* value out of range. input=',indx,red,green,blue
end subroutine mapcolor
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     circleprecision(3f) - [M_pixel:ARCS] set number of line segments
!!                           used to approximate a circle
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine circleprecision(nsegs)
!!    integer   :: nsegs
!!
!!##DESCRIPTION
!!    Set the number of line segments making up a circle. Default is
!!    currently 60. The number of segments in an arc or sector is calculated
!!    from the variable "nsegs" according to the span of the arc or sector.
!!
!!##OPTIONS
!!    NSEGS   number of line segments making up a circle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_circleprecision
!!    use M_pixel
!!    use M_pixel__writegif, only : writegif
!!    implicit none
!!    real              :: b=0.5
!!    real              :: y1,y2,ym,x1,x2
!!    real              :: width=50.0/8.0,width2
!!    integer,parameter :: ivals(*)=[3,5,7,10,20,30,60,100]
!!    integer           :: i
!!       !! set up long bar as plotting area
!!       call prefsize(1000,200)
!!       call vinit()
!!       call ortho2(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!       call textsize( 2.5/2.0, 3.0/2.0)
!!       call font('DUPLEX')
!!       call centertext(.true.)
!!       call linewidth(30)
!!       call color(2)
!!       y1=-5
!!       y2=5
!!       ym=0
!!       x1=-25+.05*width
!!       ! draw colored rectangle and a circle and label center of circle repeat
!!       width2=width*0.95
!!       do i=1,size(ivals)
!!          x2=x1+width2
!!          call move2((x1+x2)/2.0,ym)
!!          call circleprecision(ivals(i))
!!          call drawstr(ivals(i))     ! convert number to string and draw it
!!          call circle((x1+x2)/2.0, ym, (x2-x1)/2.10)
!!          x1=x1+width
!!       enddo
!!       ! write plot as GIF file
!!       call writegif('circleprecision.3M_pixel.gif',P_pixel,P_colormap)
!!       call vexit()
!!       ! use system to display GIF file
!!       call execute_command_line('display circleprecision.3M_pixel.gif')
!!    end program demo_circleprecision
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine circleprecision(nsegs)

! ident_18="@(#) M_pixel circleprecision(3f) set number of line segments making up a circle"

integer,intent(in) :: nsegs
   P_nsegs=nsegs
end subroutine circleprecision
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    getviewport(3f) - [M_pixel] return viewport in screen pixel coordinates
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine getviewport(left, right, bottom, top)
!!    real,intent(out)    :: left
!!    real,intent(out)    :: right
!!    real,intent(out)    :: bottom
!!    real,intent(out)    :: top
!!
!!##DESCRIPTION
!! Returns the left, right, bottom and top limits of the current viewport
!! in screen coordinates (-1.0 to 1.0).
!!
!!     Fortran:
!!          subroutine getviewport(left, right, bottom, top)
!!          real left, right, bottom, top
!!    If a pixel array has been declared to be real :: array(600,400)
!!
!!         o-----> X                         (right=600,top=0)
!!         | #------------------------------------#
!!         | |                                    |
!!         | |                                    |
!!         V |                                    |
!!         Y |                                    |
!!           #------------------------------------#
!!      (left=0,bottom=400)
!!
!!##OPTIONS
!!    LEFT     value for left side
!!    RIGHT    value for right side
!!    BOTTOM   value for bottom side
!!    TOP      value for top side
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine getviewport(left,right,bottom,top)

! ident_19="@(#) M_pixel getviewport(3f) return viewport in screen pixel coordinates"

real,intent(out)    :: left
real,intent(out)    :: right
real,intent(out)    :: bottom
real,intent(out)    :: top

   left    =  P_viewport_left
   right   =  P_viewport_right
   bottom  =  P_viewport_bottom
   top     =  P_viewport_top

end subroutine getviewport
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    viewport(3f) - [M_pixel] Specify which part of the screen to draw in.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine viewport(left, right, bottom, top)
!!    real,intent(in) :: left, right, bottom, top
!!
!!##DESCRIPTION
!!    Specify which part of the screen to draw in. Left, right, bottom,
!!    and top are real values in screen coordinates (0:n,0:m).
!!
!!    If a pixel array has been declared to be real :: array(600,400)
!!
!!         o-----> X                         (right=600,top=0)
!!         | #------------------------------------#
!!         | |                                    |
!!         | |                                    |
!!         V |                                    |
!!         Y |                                    |
!!           #------------------------------------#
!!      (left=0,bottom=400)
!!
!!##EXAMPLE
!!
!!  Sample program
!!
!!    program demo_viewport
!!    use :: M_pixel
!!    use :: M_pixel__writegif, only : writegif
!!    implicit none
!!       call prefsize(400, 400) ! set up drawing surface
!!       call vinit()
!!       call color(7)
!!       call linewidth(40)
!!       call clear()
!!       call ortho2(-88.0, 88.0, -88.0, 88.0)
!!       ! draw the same circle, just changing viewport
!!
!!       call viewport(   0.0, 200.0,   0.0, 200.0 ); call draw_circle(1)
!!       call viewport( 200.0, 400.0,   0.0, 200.0 ); call draw_circle(2)
!!       call viewport(   0.0, 200.0, 200.0, 400.0 ); call draw_circle(3)
!!       call viewport( 200.0, 400.0, 200.0, 400.0 ); call draw_circle(4)
!!       call viewport( 250.0, 350.0, 150.0, 300.0 ); call draw_circle(5)
!!
!!       call writegif('viewport.3M_pixel.gif',P_pixel,P_colormap)
!!       !call execute_command_line('display viewport.3M_pixel.gif')
!!       call vexit()
!!    contains
!!    subroutine draw_circle(icolor)
!!    integer,intent(in) :: icolor
!!       call color(0)
!!       call rect(-88.0,-88.0,88.0,88.0)
!!       call color(icolor)
!!       call makepoly()
!!       call circle(0.0,0.0,88.0)
!!       call closepoly()
!!    end subroutine draw_circle
!!    end program demo_viewport
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine viewport(left,right,bottom,top)

! ident_20="@(#) M_pixel viewport(3f) Specify which part of the screen to draw in."

real,intent(in) :: left, right, bottom, top

   P_viewport_left=left
   P_viewport_right=right
   P_viewport_bottom=bottom   ! pixel row,column has (0,0) in upper left so switch top and bottom
   P_viewport_top=top
   call mapping()

end subroutine viewport
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mapping(3fp) - [M_pixel] calculate conversion factors between viewport
!!                   and world window
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine mapping()
!!
!!##DESCRIPTION
!!    calculate conversion factors between viewport and world window
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine mapping()
!-!use M_math,only : invert_4x4

! ident_21="@(#) M_pixel mapping(3fp) calculate conversion factors between viewport and world window"

   real, dimension(4,4) :: viewport,viewport_inv
   real, dimension(4)   :: window, factors

   viewport(1,:)=[ P_viewport_left,   P_viewport_bottom, 1.0, 0.0 ]
   viewport(2,:)=[-P_viewport_bottom, P_viewport_left,   0.0, 1.0 ]
   viewport(3,:)=[ P_viewport_right,  P_viewport_top,    1.0, 0.0 ]
   viewport(4,:)=[-P_viewport_top,    P_viewport_right,  0.0, 1.0 ]

   window=[P_window_left,P_window_bottom,P_window_right,P_window_top]

   viewport_inv=invert_4x4(viewport)

   factors=matmul(viewport_inv,window)
   P_a=factors(1)
   P_b=factors(2)
   P_c=factors(3)
   P_d=factors(4)
end subroutine mapping
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine world2viewport(xw,yw,xv,yv)

! ident_22="@(#) M_pixel world2viewport(3fp) convert world coordinates to viewports"

real,intent(in)  :: xw,yw
real,intent(out) :: xv,yv

   xv = (P_a*xw + P_b*yw - P_b*P_d - P_a*P_c)/(P_a**2 + P_b**2)
   yv = (P_b*xw - P_a*yw - P_b*P_c + P_a*P_d)/(P_a**2 + P_b**2)

end subroutine world2viewport
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine viewport2world(xv,yv,xw,yw)

! ident_23="@(#) M_pixel viewport2world(3fp) convert viewport to world coordinates"

real,intent(in)  :: xv,yv
real,intent(out) :: xw,yw

   xw = P_a*xv + P_b*yv + P_c
   yw = P_b*xv - P_a*yv + P_d

end subroutine viewport2world
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    ortho2(3f) - [M_pixel] define the area of the virtual world coordinates
!!                 to map to the viewport
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine ortho2(left, right, bottom, top)
!!    real,intent(in) :: left, right, bottom, top
!!
!!##DESCRIPTION
!!    Defines the section of the virtual world coordinates to map to the
!!    viewport. All the projection routines define a new transformation
!!    matrix, and consequently the world units. Parallel projections are
!!    defined by ortho2.
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine ortho2(left, right, bottom, top)

! ident_24="@(#) M_pixel ortho2(3f) define the area of the virtual world coordinates to map to the viewport"

real,intent(in) :: left, right, bottom, top ! Define x (left, right), and y (bottom, top) clipping planes.

   P_window_left=left
   P_window_right=right
   P_window_bottom=bottom
   P_window_top=top
   call mapping()

end subroutine ortho2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    page(3f) - [M_pixel] define the area of the virtual world coordinates
!!               to map to the viewport
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine page(left, right, bottom, top)
!!    real,intent(in) :: left, right, bottom, top
!!
!!##DESCRIPTION
!!    Defines the section of the virtual world coordinates to map to
!!    the viewport. Automatically use the largest viewport that provides
!!    one-to-one correspondence between the window and the viewport.
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine page(xsmall,xlarge,ysmall,ylarge)
!use M_journal, only : journal

! ident_25="@(#) M_pixel page(3f) given a window size find and set to largest accommodating viewport"

real,intent(in) :: xsmall
real,intent(in) :: xlarge
real,intent(in) :: ysmall
real,intent(in) :: ylarge

real :: rps
real :: spr
real :: tryx
real :: tryy
real :: vhigh
real :: vwide
real :: xdelta
real :: xmax
real :: xmin
real :: xsplit
real :: ydelta
real :: ymax
real :: ymin
real :: ysplit

!
!     given a window size, and assuming a one-to-one correspondence of window
!     units (ie. an "x-unit" is as long as a "y-unit"), find the largest area
!     on the display surface that has the same aspect ratio, and set the
!     viewport to it.
!     assumes that the screen rasters are square.
!
      call getdisplaysize(vwide,vhigh) !get screen size in terms of raster units
!
!     the default viewport is in "screen units", and goes from top-left of 0,0
!     to bottom-right of vwide,vhigh
!     all new viewports are defined in terms of this original viewport.
!
      rps=1.0                          ! number of rasters per screen unit
      spr=1.0                          ! number of screen units per raster
      tryx=vwide                       ! make as wide as display as a trial fit
      if(xlarge-xsmall.ne.0.0)then
         tryy=vwide*(ylarge-ysmall)/(xlarge-xsmall) ! calculate required height
      else                             ! ERROR: do something desperate
         call journal('*P_page* window has a zero X dimension')
         tryy=vhigh
      endif
      if(tryy.gt.vhigh)then ! if required height too great, fit with y maximized
         tryy=vhigh
         if(ylarge-ysmall.ne.0.0)then
            tryx=vhigh*(xlarge-xsmall)/(ylarge-ysmall)
         else                          ! ERROR: do something desperate
            call journal('*P_page* window has a zero Y dimension')
            tryx=vwide
         endif
      endif
!
!   tryx and tryy are now the required viewport in raster units. The raster
!   units now need converted to screen units to be used in viewport procedure
!
!   some explanation of physical viewport units is required:
!   assuming maximizing the required aspect ratio in the available drawing area,
!   and that the original viewport "origin" 0,0 stays in its original position,
!   and that the original -1,1,-1,1 viewport is the largest square that can fit
!   on the display, bottom left justified.
!   the screen coordinate system is a right-handed Cartesian coordinate system
!   with positive x to the viewer's right, positive y up.
!
!   at this point,
!    vwide=width in rasters of entire display
!    vhigh=height in rasters of entire display
!   assuming a square raster
!     tryx is desired width in rasters
!     tryy is desired height in rasters
!
      xdelta=tryx-2.0*rps  ! need this many more rasters in x direction from 1,1
      ydelta=tryy-2.0*rps  ! need this many more rasters in y direction from 1,1
      ! to center (to left bottom justify, make xsplit and ysplit 0)
      xsplit=(vwide-tryx)/2.0
      ysplit=(vhigh-tryy)/2.0
      xmax=1+xdelta*spr+xsplit*spr
      ymax=1+ydelta*spr+ysplit*spr
      xmin=-1+xsplit*spr
      ymin=-1+ysplit*spr
      if(P_debug)then
         write(*,*)'max. display area is', vwide, ' by ',vhigh,' rasters'
         write(*,*)'shape is ',xsmall,xlarge,ysmall,ylarge
         write(*,*)'attempting to get a viewport of ',tryx,' by ',tryy
         write(*,*)'needed more rasters, ',xdelta,' by ',ydelta
         write(*,*)'resulted in viewport ',xmin,xmax,ymin,ymax
      endif
      if(xmin.ne.xmax.and.ymin.ne.ymax)then
         call viewport(xmin,xmax,ymax,ymin)
      else
       call journal('*P_page* window has zero dimension,no viewport set')
      endif
      if(xsmall.ne.xlarge.and.ysmall.ne.ylarge)then
         call ortho2(xsmall,xlarge,ysmall,ylarge)
      else    ! ERROR: do something desperate
        call journal('*P_page* window has zero dimension, no window set')
      endif
end subroutine page
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    rmove2(3f) - [M_pixel:DRAW] relative move
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine rmove2(deltax, deltay)
!!    real,intent(in) :: deltax, deltay
!!
!!##DESCRIPTION
!!    Update current position.
!!    Relative move2. deltax and deltay are offsets in world units.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_rmove2
!!      use M_pixel, only: prefsize, vinit, ortho2, clear
!!      use M_pixel, only: move2, rmove2, rdraw2, vexit
!!      use M_pixel, only: linewidth
!!      use M_pixel, only: P_pixel, P_colormap
!!      use M_pixel__writegif, only : writegif
!!      implicit none
!!      integer :: i
!!         call prefsize(500,500)
!!         call vinit()
!!         call ortho2(-110.0,110.0,-110.0,110.0)
!!         call move2(-100.0,-100.0)
!!         call linewidth(70)
!!         do i=1,20
!!            call rmove2(10.0, 0.0)
!!            call rdraw2( 0.0,10.0)
!!         enddo
!!         call writegif('rmove2.3M_pixel.gif',P_pixel,P_colormap)
!!         call  execute_command_line('display rmove2.3M_pixel.gif')
!!         call vexit()
!!      end program demo_rmove2
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine rmove2(Xdelta,Ydelta)

! ident_26="@(#) M_pixel rmove2(3f) relative move"

real,intent(in) :: Xdelta
real,intent(in) :: Ydelta

   P_X=P_X+Xdelta
   P_Y=P_Y+Ydelta

end subroutine rmove2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    move2(3f) - [M_pixel:DRAW] change current position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine move2(x, y)
!!    real x, y
!!
!!##DESCRIPTION
!!    Update current position.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_move2
!!      use M_pixel, only : prefsize, vinit, ortho2, clear
!!      use M_pixel, only : move2, draw2, vexit
!!      use M_pixel, only : P_pixel,P_colormap
!!      use M_pixel__writegif, only : writegif
!!      implicit none
!!         call prefsize(60,40)
!!         call vinit()
!!         call ortho2(-300.0,300.0,-200.0,200.0)
!!         call clear(0)
!!         call move2(-300.0,-200.0)
!!         call draw2(300.0,200.0)
!!         call move2(300.0,-200.0)
!!         call draw2(-300.0,200.0)
!!         call writegif('move2.3M_pixel.gif',P_pixel,P_colormap)
!!         call vexit()
!!      end program demo_move2
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine move2(x,y)

! ident_27="@(#) M_pixel move2(3f) move current position"

real,intent(in) :: x,y

   P_X=X
   P_Y=Y

end subroutine move2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    rdraw2(3f) - [M_pixel:DRAW] draw from current position to given point
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    elemental impure subroutine rdraw2(x, y)
!!    real,intent(in) :: x, y
!!
!!##DESCRIPTION
!!    Relative draw from current position to specified point using current
!!    color and line width. Updates current position to new point.
!!    (x, y) is a point in world coordinates.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_rdraw2
!!      use M_pixel, only: vinit, prefsize, ortho2,linewidth
!!      use M_pixel, only: clear, move2, rdraw2, vexit,color
!!      use M_pixel, only: P_pixel, P_colormap
!!      use M_pixel__writegif, only : writegif
!!      implicit none
!!
!!         call prefsize(200,200)
!!         call vinit()
!!         call ortho2(-55.0, 55.0, -55.0,  55.0)
!!         call linewidth(400)
!!         call color(7)
!!         call clear()
!!
!!         call color(1)
!!         call move2(-50.0,0.0)
!!         call square(50.0)
!!
!!         call linewidth(200)
!!         call color(2)
!!         call move2(  0.0,-50.0)
!!         call square(50.0)
!!
!!         call writegif('rdraw2.3M_pixel.gif',P_pixel,P_colormap)
!!         call execute_command_line('display rdraw2.3M_pixel.gif')
!!         call vexit()
!!
!!         contains
!!
!!         subroutine square(side)
!!         real,intent(in) :: side
!!         call rdraw2( side,   0.0)
!!         call rdraw2(  0.0,  side)
!!         call rdraw2(-side,   0.0)
!!         call rdraw2(  0.0, -side)
!!         end subroutine square
!!
!!      end program demo_rdraw2
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental impure subroutine rdraw2(xdelta,ydelta)

! ident_28="@(#) M_pixel rdraw2(3f) relative draw"

real,intent(in) :: xdelta
real,intent(in) :: ydelta
real            :: P_x_tmp
real            :: P_y_tmp

   P_x_tmp=P_x
   P_y_tmp=P_y
   call line( P_x_tmp, P_y_tmp, P_x_tmp+xdelta, P_y_tmp+ydelta )

end subroutine rdraw2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    draw2(3f) - [M_pixel:DRAW] draw from current position to given point
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    elemental impure subroutine draw2(x, y)
!!    real,intent(in) :: x, y
!!
!!##DESCRIPTION
!!    Draw from current position to specified point using current
!!    color and line width. Updates current position to new point.
!!    (x, y) is a point in world coordinates.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_draw2
!!    use M_pixel,    only : prefsize, vinit, ortho2, clear
!!    use M_pixel,    only : move2, draw2, vexit, color,linewidth
!!    use M_pixel,    only : P_pixel, P_colormap
!!    use M_pixel__writegif, only : writegif
!!    use M_pixel,    only : d2r, polar_to_cartesian
!!    !
!!    ! The Archimedean spiral is the locus of points corresponding
!!    ! to the locations over time of a point moving away from a
!!    ! fixed point with a constant speed along a line which rotates
!!    ! with constant angular velocity.
!!    !    r=A+B*theta
!!    ! Changing the parameter A will turn the spiral,
!!    ! while B controls the distance between successive turnings.
!!    !
!!    implicit none
!!    integer        :: i
!!    real           :: x,y,radius,theta
!!    real,parameter :: rotate=0.0, gap=2.0
!!       call prefsize(400,400)
!!       call vinit('')
!!       call ortho2(-150.0,150.0,-150.0,150.0)
!!       call color(5)
!!       call clear()
!!       call move2(0.0,0.0)
!!       call color(0)
!!       call linewidth(40)
!!       do i=0,360*10,5
!!          theta=d2r(i)
!!          ! equation in polar coordinates
!!          radius=rotate+gap*theta
!!          ! convert polar coordinates to cartesian
!!          call polar_to_cartesian(radius,theta,x,y)
!!          ! draw from current position to end of next segment
!!          call draw2(x,y)
!!       enddo
!!       ! write the pixel map array as a GIF image file
!!       call writegif('draw2.3M_pixel.gif',P_pixel,P_colormap)
!!       ! exit graphics mode
!!       call vexit()
!!    end program demo_draw2
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental impure subroutine draw2(x,y)

! ident_29="@(#) M_pixel draw2(3f) draw a line from current position to specified point"

real,intent(in) :: x
real,intent(in) :: y
real            :: P_x_tmp
real            :: P_y_tmp

   P_x_tmp=P_x
   P_y_tmp=P_y
   call line( P_x_tmp, P_y_tmp, x, y )

end subroutine draw2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    prefsize(3f) - [M_pixel] specify size of pixel array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine prefsize(width, height)
!!    integer width, height
!!
!!##DESCRIPTION
!!    Specify the preferred width and height of the pixel array opened
!!    by the *next* vinit(3f). The pixel array is then available via
!!    the M_pixel(3fm) module as variable P_pixel. Note that the width
!!    corresponds to the number of rows in the array, and height to the
!!    number of columns.
!!
!!##OPTIONS
!!    WIDTH   width of pixel array to create when vinit(3f) is called
!!    HEIGHT  height of pixel array to create when vinit(3f) is called
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_prefsize
!!      use M_pixel, only: prefsize, vinit, ortho2, clear
!!      use M_pixel, only: move2, draw2, vexit, color
!!      use M_pixel, only : P_pixel,P_colormap
!!      use M_pixel__writegif, only : writegif
!!      implicit none
!!         ! make first file with one size
!!         call prefsize(60*2,40*2)
!!         call vinit()
!!         call picture()
!!         call writegif('prefsize.3M_pixel.gif',P_pixel,P_colormap)
!!         call vexit()
!!
!!         ! make second file with another size
!!         call prefsize(60*3,40*3)
!!         call vinit()
!!         call picture()
!!         call writegif('prefsize_B.3M_pixel.gif',P_pixel,P_colormap)
!!         call vexit()
!!      contains
!!      subroutine picture
!!         call ortho2(-300.0,300.0,-200.0,200.0)
!!         call clear(0)
!!         call color(1)
!!         call move2(-300.0,-200.0)
!!         call draw2(300.0,200.0)
!!         call move2(300.0,-200.0)
!!         call draw2(-300.0,200.0)
!!      end subroutine picture
!!      end program demo_prefsize
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine prefsize(x,y)

! ident_30="@(#) M_pixel prefsize(3f) specify size of pixel array"

integer,intent(in) :: x
integer,intent(in) :: y

   P_VIEWPORT_WIDTH=X
   P_VIEWPORT_HEIGHT=Y

end subroutine prefsize
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    vexit(3f) - [M_pixel] exit pixel graphics mode
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine vexit()
!!
!!##DESCRIPTION
!!    Used to terminate pixel graphics mode. Does any actions required to
!!    terminate graphics mode including unallocating the module pixel array
!!    P_pixel. Required before calling vinit(3f) more than once.
!!
!!    Resets the window/terminal (must be the last M_PIXEL routine called).
!!
!!##OPTIONS
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_vexit
!!      use M_pixel, only: prefsize, vexit, ortho2, clear
!!      use M_pixel, only: move2, draw2, color, vinit
!!      use M_pixel, only : P_pixel,P_colormap
!!      use M_pixel__writegif, only : writegif
!!      implicit none
!!         call prefsize(60,40)
!!         call vinit()
!!         call ortho2(-300.0,300.0,-200.0,200.0)
!!         call clear(0)
!!         call color(1)
!!         call move2(-300.0,-200.0)
!!         call draw2(300.0,200.0)
!!         call move2(300.0,-200.0)
!!         call draw2(-300.0,200.0)
!!         call writegif('vexit.3M_pixel.gif',P_pixel,P_colormap)
!!         call vexit()
!!      end program demo_vexit
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine vexit()

! ident_31="@(#) M_pixel vexit(3f) exit pixel array drawing module"

   if(allocated(P_pixel))then
      deallocate(P_Pixel)
   endif
   P_VINIT_CALLED=.false.

end subroutine vexit
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    vinit(3f) - [M_pixel] initialize pixel graphics module
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!   subroutine vinit()
!!
!!##DESCRIPTION
!!    Initialize the pixel graphics module. The pixel array P_pixel and the
!!    colormap P_ColorMap are directly accessible after the call to allow
!!    display or printing
!!
!!##OPTIONS
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_vinit
!!      use M_pixel, only    : prefsize, vinit, ortho2, clear
!!      use M_pixel, only    : move2, draw2, vexit, color
!!      use M_pixel, only    : P_pixel, P_colormap
!!      use M_pixel__writegif, only : writegif
!!      implicit none
!!         call prefsize(60,40)
!!         call vinit()
!!         call ortho2(-300.0,300.0,-200.0,200.0)
!!         call clear(0)
!!         call color(1)
!!         call move2(-300.0,-200.0)
!!         call draw2(300.0,200.0)
!!         call move2(300.0,-200.0)
!!         call draw2(-300.0,200.0)
!!         call writegif('vinit.3M_pixel.gif',P_pixel,P_colormap)
!!         call vexit()
!!      end program demo_vinit
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine vinit(string)

! ident_32="@(#) M_pixel vinit(3f) initialize pixel array drawing module"

character(len=*),optional :: string

   P_X=0                                ! initialize current position
   P_Y=0

   if(allocated(P_pixel))then
      deallocate(P_Pixel)
   endif
   allocate(P_pixel(0:P_VIEWPORT_WIDTH-1,0:P_VIEWPORT_HEIGHT-1))
   P_VINIT_CALLED=.true.
   P_pixel=0

   P_WIDTH=1             ! line width
   P_COLOR_INDEX=1       ! pen color
   P_NSEGS=60            ! number of line segments making up a circle

!  If a pixel array has been declared to be real :: array(600,400)
!
!       o-----> X                         (right=600,top=0)
!       | #------------------------------------#
!       | |                                    |
!       | |                                    |
!       V |                                    |
!       Y |                                    |
!         #------------------------------------#
!    (left=0,bottom=400)
   P_viewport_left=0.0
   P_viewport_right=real(P_VIEWPORT_WIDTH-1)
   P_viewport_bottom=real(P_VIEWPORT_HEIGHT-1)
   P_viewport_top=0.0

   P_window_left=0.0
   P_window_right=real(P_VIEWPORT_WIDTH)
   P_window_bottom=0.0
   P_window_top=real(P_VIEWPORT_HEIGHT)
   call mapping()

   P_TEXT_HEIGHT=10.0
   P_TEXT_WIDTH=7.0
   P_TEXT_ANGLE=0.0
   P_TEXT_COSINE=1.0
   P_TEXT_SINE  =0.0
   P_X_CENTERTEXT=.false.
   P_Y_CENTERTEXT=.false.
   P_FONT='SIMPLEX'

   P_inpolygon=.false.
   P_polyvertex=1

end subroutine vinit
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    makepoly(3f) - [M_pixel:POLYGONS] opens polygon constructed by a
!!                   series of move-draws and closed by closepoly
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine makepoly()
!!
!!##DESCRIPTION
!!    MAKEPOLY(3f) opens up a polygon which will then be constructed by a
!!    series of move-draws and closed by a CLOSEPOLY(3f).
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!    program demo_makepoly
!!    use :: M_pixel
!!    use :: M_pixel__writegif, only : writegif
!!    use :: M_pixel__writegif_animated, only : write_animated_gif
!!    implicit none
!!    integer,parameter :: wide=640, tall=640
!!    integer :: rows, xoff, yoff, box_sz
!!    integer :: i20, i30, ncols, nrows, ilines
!!    real    :: bottom, left, sun_radius, planet_radius, planet_offset
!!    character(len=40) :: filename
!!    integer :: movie(300,0:wide-1,0:tall-1)
!!       call prefsize(wide,tall)
!!       call vinit()
!!       call ortho2(0.0, real(wide), 0.0, real(tall) )
!!       ! call linewidth(3) Note:
!!       ! really slows down pbm driver because all lines are polygons
!!       call color(7)
!!       call clear()
!!       call color(0)
!!       rows=1
!!       box_sz=MIN(wide,tall)/rows       ! size of biggest box to use
!!                                        ! and get specified number of rows
!!       nrows = tall/box_sz              ! number of rows of objects to draw
!!       ncols = wide/box_sz              ! number of columns of objects to draw
!!       xoff = (wide - ncols * box_sz)/2 ! initial x offset to begin row at
!!                                        ! to center drawings
!!       yoff = (tall - nrows * box_sz)/2 ! initial x offset to begin column
!!                                        ! at to center drawings
!!       sun_radius = 148
!!       planet_radius = 1
!!       do ilines = 1, 300
!!          do i20 = 1, ncols
!!             left = (i20-1)*box_sz+xoff
!!             do i30 = 1, nrows
!!                bottom = (i30-1)*box_sz+yoff
!!                call color(0)
!!             call makepoly()
!!                call rect(left,bottom,left+box_sz,bottom+box_sz)
!!             call closepoly()
!!                planet_offset= sun_radius
!!                   call color(mod(ilines,15)+1)
!!                   call hypoc(left + box_sz/2.0, bottom + box_sz/2.0, &
!!                & sun_radius, planet_radius, planet_offset, &
!!                & box_sz/2.0, ilines,  &
!!                & 0.0, 0.0, 1)
!!             enddo
!!          enddo
!!          movie(ilines,:,:)=P_pixel
!!          write(filename,'("hypoc.",i0,".gif")')ilines
!!          !!call writegif(filename,P_pixel,P_colormap)
!!       enddo
!!       call write_animated_gif('makepoly.3M_pixel.gif',&
!!               movie,P_colormap,delay=70)
!!       call vexit()
!!    contains
!!    !
!!    !  Make shapes using hypocycloidal curves.
!!    !
!!    subroutine hypoc(xcenter,ycenter,sunr0,planet0,offset0,&
!!                    radius,ilines,ang,angs,ifill)
!!    use M_pixel
!!    implicit none
!!    real,parameter     :: PI=3.14159265358979323846264338327950288419716939937510
!!    real,intent(in)    :: xcenter, ycenter      ! center of curve
!!    real,intent(in)    :: sunr0,planet0,offset0 ! radii of sun, planet,
!!                                                ! and planet offset
!!    real,intent(in)    :: radius                ! radius to fit the shape to
!!                                                ! (no fit if radius is 0)
!!    integer,intent(in) :: ilines                ! number of points to sample
!!                                                ! along curve
!!    real,intent(in)    :: ang                   ! angle to rotate the shape by,
!!                                                ! to orientate it.
!!    real,intent(in)    :: angs                  ! angle to start sampling points
!!                                                ! at; ccw is +; 0 is East
!!    integer,intent(in) :: ifill                 ! 1 make a filled polygon,
!!                                                ! 2 make a hatched polygon
!!    integer            :: i10
!!    real               :: ang1, con1, con2, factor
!!    real               :: offset, planet, r, sunr, u
!!    real               :: xpoin, xpoin1, ypoin, ypoin1
!!       sunr=sunr0
!!       offset=offset0
!!       planet=planet0
!!       if(ilines.eq.0.0) return
!!       if(planet.eq.0.0) return
!!       if(sunr.eq.0.0)   return
!!       if(radius.ne.0.and.sunr-planet+offset.ne.0)then
!!          factor=radius/(sunr-planet+offset)
!!          sunr=factor*sunr
!!          planet=factor*planet
!!          offset=factor*offset
!!       endif
!!       u=0.0+ang
!!       con1=PI*2.0*(sunr/planet)/real(ilines)
!!       con2=(1.0-planet/sunr)*u
!!       xpoin1=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
!!       ypoin1=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)
!!       ang1=atan2(ypoin1,xpoin1)+angs
!!       r=sqrt(xpoin1**2+ypoin1**2)
!!       xpoin1=r*cos(ang1)+xcenter
!!       ypoin1=r*sin(ang1)+ycenter
!!       select case(ifill)
!!       case(:0)
!!       case(1:)
!!          call makepoly()
!!       end select
!!       call move2(xpoin1,ypoin1)
!!       do i10=1,ilines
!!          u=con1*i10+ang
!!          con2=(1.0-planet/sunr)*u
!!          if(con2.ge.2**24) con2=amod(con2,PI)
!!          xpoin=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
!!          ypoin=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)
!!          ang1=atan2(ypoin,xpoin)+angs
!!          r=sqrt(xpoin**2+ypoin**2)
!!          xpoin=r*cos(ang1)+xcenter
!!          ypoin=r*sin(ang1)+ycenter
!!          call draw2(xpoin,ypoin)
!!       enddo
!!       call draw2(xpoin1,ypoin1)
!!       if(ifill.gt.0)then
!!         call closepoly()
!!       endif
!!    end subroutine hypoc
!!    end program demo_makepoly
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine makepoly()

! ident_33="@(#) M_pixel makepoly(3f) opens polygon constructed by a series of move-draws and closed by closepoly"

   P_inpolygon=.true.
   P_polyvertex=1
end subroutine makepoly
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    closepoly(3f) - [M_pixel:POLYGONS] Terminates a polygon opened by
!!                    makepoly(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!       subroutine closepoly()
!!
!!##DESCRIPTION
!!    Terminates a polygon opened by MAKEPOLY(3f).
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine closepoly()

! ident_34="@(#) M_pixel makepoly(3f) terminate a polygon opened by makepoly(3f)"

   P_inpolygon=.false.
   call poly2(P_polyvertex-1,P_polypoints)
end subroutine closepoly
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    print_ppm(3f) - [M_pixel:PRINT] print pixel array as a ppm file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine print_ppm(filename)
!!    character(len=*),intent(in) :: filename
!!
!!##DESCRIPTION
!!   This driver makes an P6 PPM(portable pixmap) file. Any
!!   existing file will be appended to.
!!
!!##OPTIONS
!!   FILENAME  name of output file to create or append to.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_print_ppm
!!      use M_pixel, only : prefsize,vinit,ortho2,vexit
!!      use M_pixel, only : linewidth,circle,color
!!      use M_pixel, only : print_ppm
!!      implicit none
!!         call prefsize(40,40)
!!         call vinit()
!!         call ortho2(-100.0,100.0,-100.0,100.0)
!!         call linewidth(400)
!!         call circle(0.0,0.0,45.0)
!!         call color(3)
!!         call circle(0.0,0.0,25.0)
!!         call print_ppm('demo_print.ppm')
!!         call vexit()
!!      end program demo_print_ppm
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine print_ppm(filename)

! ident_35="@(#) M_pixel print_ppm(3f) print pixel array as a P6 PPM file appending to any existing file"

character(len=*),intent(in) :: filename
integer                     :: lun,ios
character(len=4096)         :: message

   open(newunit=lun,file=filename, &
      & status='unknown',          & !  STATUS    =  NEW        | REPLACE     | OLD    | SCRATCH | UNKNOWN
      & access='stream',           & !  ACCESS    =  SEQUENTIAL | DIRECT      | STREAM
      & action='write',            & !  ACTION    =  READ|WRITE | READWRITE
      & position='append',         & !  POSITION  =  ASIS       | REWIND      | APPEND
      & form='unformatted',        & !  FORM      =  FORMATTED  | UNFORMATTED
      & iostat=ios,                &
      & iomsg=message)
   if(ios.ne.0)then
       write(*,'(a)')'<ERROR>*p6out*: writing '//trim(filename)//':'//trim(message)
   else
      call output_ppm(lun)
   endif
end subroutine print_ppm
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    print_p6(3f) - [M_pixel:PRINT] print pixel array as a ppm file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine print_p6(filename)
!!    character(len=*),intent(in) :: filename
!!
!!##DESCRIPTION
!!   This driver makes an P6 PPM(portable pixmap) file. Any
!!   existing file will be replaced.
!!
!!##OPTIONS
!!   FILENAME  name of output file to create.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_print_p6
!!      use M_pixel, only : prefsize,vinit,ortho2,vexit
!!      use M_pixel, only : linewidth,circle,color
!!      use M_pixel, only : print_p6
!!      implicit none
!!         call prefsize(40,40)
!!         call vinit()
!!         call ortho2(-100.0,100.0,-100.0,100.0)
!!         call linewidth(400)
!!         call circle(0.0,0.0,45.0)
!!         call color(3)
!!         call circle(0.0,0.0,25.0)
!!         call print_p6('demo_print.p6')
!!         call vexit()
!!      end program demo_print_p6
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine print_p6(filename)
! ident_36="@(#) M_pixel print_p6(3f) print pixel array as a P6 PPM file replacing any existing file"

character(len=*),intent(in) :: filename
integer                     :: lun,ios
character(len=4096)         :: message

   open(newunit=lun,file=filename, &
      & status='replace',          & !  STATUS    =  NEW        | REPLACE     | OLD    | SCRATCH | UNKNOWN
      & access='stream',           & !  ACCESS    =  SEQUENTIAL | DIRECT      | STREAM
      & action='write',            & !  ACTION    =  READ|WRITE | READWRITE
      & position='rewind',         & !  POSITION  =  ASIS       | REWIND      | APPEND
      & form='unformatted',        & !  FORM      =  FORMATTED  | UNFORMATTED
      & iostat=ios,                &
      & iomsg=message)
   if(ios.ne.0)then
       write(*,'(a)')'<ERROR>*p6out*: writing '//trim(filename)//':'//trim(message)
   else
      call output_ppm(lun)
   endif
end subroutine print_p6
!==================================================================================================================================!
subroutine output_ppm(lun)
! ident_37="@(#) M_pixel output_ppm(3f) print pixel array as a PPM file"
integer,intent(in)  :: lun
integer             :: ios
integer             :: i, j
character(len=100)  :: message
   call if_init()
   associate( xs=>size(P_pixel,dim=1), ys=>size(P_pixel,dim=2), cs=>size(P_ColorMap,dim=2) )
    write(message,'(''P6'', 3(1x,i0))')  xs, ys , cs-1 ! header
    write(lun)trim(message)//' '
    if(cs-1.gt.255)then
       write(lun) ((num2bytes2(P_ColorMap(1:3,P_pixel(i,j))),i=0,xs-1),j=0,ys-1)
    else
       write(lun) ((char(P_ColorMap(1:3,P_pixel(i,j))),i=0,xs-1),j=0,ys-1)
    endif
   end associate
   flush(unit=lun,iostat=ios)
   close(unit=lun,iostat=ios)
end subroutine output_ppm
!==================================================================================================================================!
elemental pure function num2bytes2(inum) result (byt2)
integer,intent(in) :: inum
character(len=2)   :: byt2
integer            :: itmp1, itmp2
   itmp2 = inum / 256
   itmp1 = inum -(itmp2 * 256)
   byt2(1:1) = char(itmp1)
   byt2(2:2) = char(itmp2)
end function num2bytes2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    print_p3(3f) - [M_pixel:PRINT] print pixel array as a ppm p3 file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine print_p3(filename)
!!    character(len=*),intent(in) :: filename
!!
!!##DESCRIPTION
!!   This driver makes an ASCII P3 portable pixmap file. Any existing
!!   file is replaced.
!!
!!##OPTIONS
!!   FILENAME  name of output file to create or replace
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_print_p3
!!      use M_pixel, only : prefsize,vinit,ortho2,vexit
!!      use M_pixel, only : linewidth,circle,color
!!      use M_pixel, only : print_p3
!!      implicit none
!!         call prefsize(40,40)
!!         call vinit()
!!         call ortho2(-100.0,100.0,-100.0,100.0)
!!         call linewidth(400)
!!         call circle(0.0,0.0,45.0)
!!         call color(3)
!!         call circle(0.0,0.0,25.0)
!!         call print_p3('demo_print.p3')
!!         call vexit()
!!      end program demo_print_p3
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine print_p3(filename)

! ident_38="@(#) M_pixel print_p3(3f) print pixel array as a P3 PPM file"

character(len=*),intent(in) :: filename

   integer             :: iu,ios,i,xs,ys,cs
   character(len=4096) :: message

   open(file=trim(filename),newunit=iu,iostat=ios,iomsg=message,action='write')
   if(ios.eq.0)then
      call if_init()
      xs=size(P_pixel,dim=1)
      ys=size(P_pixel,dim=2)
      cs=size(P_ColorMap,dim=2)
      write(iu,'("P3",/,i0,1x,i0,/,i0,/,(20(i0,1x)))') xs,ys,cs,((P_ColorMap(1:3,P_pixel(i,j)),i=0,xs-1),j=0,ys-1)
   else
      write(*,*)'*P_print_p3* ERROR: ',trim(message)
   endif

   flush(unit=iu,iostat=ios)
   close(unit=iu,iostat=ios)

end subroutine print_p3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!   print_ansi(3f) - [M_pixel:PRINT] print small pixel array as colored
!!   text on terminals and terminal emulators that obey ANSI escape sequences
!!   (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine print_ansi(filename)
!!    character(len=*),intent(in) :: filename
!!
!!##DESCRIPTION
!!   This driver prints the pixmap as a simple array of ANSI terminal
!!   escape sequences. It assumes only single-digit colors are used. It is
!!   appropriate for inspecting small pixmaps.
!!
!!##OPTIONS
!!   FILENAME  name of output file. If blank write to stdout.
!!
!!##EXAMPLE
!!
!!
!!   Sample Program:
!!
!!    program demo_print_ansi
!!    use M_pixel
!!    implicit none
!!    call prefsize(80,24)
!!       call vinit()
!!       call ortho2(0.0,80.0,0.0,24.0)
!!       call linewidth(400)
!!       call color(1)
!!       call circle(12.0,12.0,6.0)
!!       call color(2)
!!       call circle(72.0,12.0,6.0)
!!       call print_ansi()
!!       call vexit()
!!    end program demo_print_ansi
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine print_ansi(filename)
use,intrinsic :: iso_fortran_env, only : ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT

! ident_39="@(#) M_pixel print_ansi(3f) print pixel array as an ASCII block of text"

character(len=*),intent(in),optional  :: filename
character(len=1024)                   :: message
   integer                            :: iu,ios,i,j

   if(present(filename))then  ! if filename is present and not blank open specified filename else use stdout
      if(filename.eq.'')then
         iu=OUTPUT_UNIT
         ios=0
      else
         open(file=trim(filename),newunit=iu,iostat=ios,iomsg=message,action='write')
         if(ios.ne.0)then
            write(ERROR_UNIT,'(*(a))',iostat=ios)'*P_print_ansi* OPEN ERROR:',trim(message)
         endif
      endif
   else
      iu=OUTPUT_UNIT
      ios=0
   endif

   if(ios.eq.0)then
      call if_init()
      do i=0,size(P_pixel,dim=2)-1
         do j=1,size(P_pixel,dim=1)
            write(iu,'(*(g0))',iostat=ios,iomsg=message,advance='no')char(27),'[4',P_pixel(j,i),'m '
            if(ios.ne.0)then
               write(ERROR_UNIT,'(*(a))',iostat=ios)'*P_print_ansi* WRITE ERROR:',trim(message)
               exit
            endif
         enddo
         write(iu,'(*(g0))',iostat=ios,iomsg=message,advance='yes')char(27),'[0m'
      enddo
   endif

   flush(unit=iu,iostat=ios)
   if(iu.ne.OUTPUT_UNIT)then
      close(unit=iu,iostat=ios)
   endif

end subroutine print_ansi
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!   print_ascii(3f) - [M_pixel:PRINT] print small pixel array as ASCII text
!!   (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine print_ascii(filename)
!!    character(len=*),intent(in) :: filename
!!
!!##DESCRIPTION
!!   This driver prints the pixmap as a simple ASCII array. It assumes
!!   only single-digit colors are used. It is appropriate for inspecting
!!   small pixmaps.
!!
!!##OPTIONS
!!   FILENAME  name of output file. If blank write to stdout.
!!
!!##EXAMPLE
!!
!!
!!   Sample Program:
!!
!!    program demo_print_ascii
!!    use M_pixel
!!    implicit none
!!    call prefsize(65,24)
!!       call vinit()
!!       call ortho2(0.0,65.0,0.0,24.0)
!!       call linewidth(400)
!!       call color(1)
!!       call circle(12.0,12.0,6.0)
!!       call color(2)
!!       call circle(55.0,12.0,6.0)
!!       call print_ascii()
!!       call vexit()
!!    end program demo_print_ascii
!!
!!   Results:
!!
!!    00000000000000000000000000000000000000000000000000000000000000000
!!    00000000000000000000000000000000000000000000000000000000000000000
!!    00000000000000000000000000000000000000000000000000000000000000000
!!    00000000000000000000000000000000000000000000000000000000000000000
!!    00000000000000000000000000000000000000000000000000000000000000000
!!    00000000000011100000000000000000000000000000000000000000000000000
!!    00000000001111111000000000000000000000000000000000000222222000000
!!    00000000011101111100000000000000000000000000000000022222222200000
!!    00000000100000011010000000000000000000000000000000220000022220000
!!    00000001100000000110000000000000000000000000000002200000002220000
!!    00000011000000000111000000000000000000000000000002000000000202000
!!    00000011000000000111000000000000000000000000000022000000000022000
!!    00000011000000000011000000000000000000000000000022000000000022000
!!    00000011000000000011000000000000000000000000000022000000000022000
!!    00000011100000000110000000000000000000000000000020200000000022000
!!    00000011100000000110000000000000000000000000000002200000000220000
!!    00000001011000001100000000000000000000000000000002022000000200000
!!    00000000111111111000000000000000000000000000000000222220222000000
!!    00000000011111100000000000000000000000000000000000022222220000000
!!    00000000000000000000000000000000000000000000000000000222000000000
!!    00000000000000000000000000000000000000000000000000000000000000000
!!    00000000000000000000000000000000000000000000000000000000000000000
!!    00000000000000000000000000000000000000000000000000000000000000000
!!    00000000000000000000000000000000000000000000000000000000000000000
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine print_ascii(filename)
use,intrinsic :: iso_fortran_env, only : ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT

! ident_40="@(#) M_pixel print_ascii(3f) print pixel array as an ASCII block of text"

character(len=*),intent(in),optional  :: filename
character(len=1024)                   :: message
   integer                            :: iu,ios,i

   if(present(filename))then  ! if filename is present and not blank open specified filename else use stdout
      if(filename.eq.'')then
         iu=OUTPUT_UNIT
         ios=0
      else
         open(file=trim(filename),newunit=iu,iostat=ios,iomsg=message,action='write')
         if(ios.ne.0)then
            write(ERROR_UNIT,'(*(a))',iostat=ios)'*P_print_ascii* OPEN ERROR:',trim(message)
         endif
      endif
   else
      iu=OUTPUT_UNIT
      ios=0
   endif

   if(ios.eq.0)then
      call if_init()
      do i=0,size(P_pixel,dim=2)-1
         write(iu,'(*(i1))',iostat=ios,iomsg=message)P_pixel(:,i)
         if(ios.ne.0)then
            write(ERROR_UNIT,'(*(a))',iostat=ios)'*P_print_ascii* WRITE ERROR:',trim(message)
            exit
         endif
      enddo
   endif

   flush(unit=iu,iostat=ios)
   if(iu.ne.OUTPUT_UNIT)then
      close(unit=iu,iostat=ios)
   endif

end subroutine print_ascii
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!      ppm - portable pixmap file format
!!
!!##DESCRIPTION
!!      The portable pixmap format is a lowest common denominator
!!      color image file format. The definition is as follows:
!!
!!      - A "magic number" for identifying the file type. A ppm
!!        file's magic number is the two characters "P3".
!!
!!      - Whitespace (blanks, TABs, CRs, LFs).
!!
!!      - A width, formatted as ASCII characters in decimal.
!!
!!      - Whitespace.
!!
!!      - A height, again in ASCII decimal.
!!
!!      - Whitespace.
!!
!!      - The maximum color-component value, again in ASCII decimal.
!!
!!      - Whitespace.
!!
!!      - Width * height pixels, each three ASCII decimal values
!!        between 0 and the specified maximum value, starting at the
!!        top-left corner of the pixmap,  proceeding in normal
!!        English reading order. The three values for each pixel
!!        represent red, green, and blue, respectively; a value of 0
!!        means that color is off, and the maximum value means that
!!        color is maxxed out.
!!
!!      - Characters from a "#" to the next end-of-line are ignored
!!        (comments).
!!
!!      - No line should be longer than 70 characters.
!!
!!      Here is an example of a small pixmap in this format:
!!      P3
!!      # feep.ppm
!!      4 4
!!      15
!!       0  0  0    0  0  0    0  0  0   15  0 15
!!       0  0  0    0 15  7    0  0  0    0  0  0
!!       0  0  0    0  0  0    0 15  7    0  0  0
!!      15  0 15    0  0  0    0  0  0    0  0  0
!!
!!      Programs that read this format should be as lenient as possible,
!!      accepting anything that looks remotely like a pixmap.
!!
!!      There is also a variant on the format, available by setting
!!      the RAWBITS option at compile time. This variant is
!!      different in the following ways:
!!
!!      - The "magic number" is "P6" instead of "P3".
!!
!!      - The pixel values are stored as plain bytes,  instead of
!!        ASCII decimal.
!!
!!      - Whitespace is not allowed in the pixels area, and only a
!!        single character of whitespace (typically a newline) is
!!        allowed after the maxval.
!!
!!      - The files are smaller and many times faster to read and
!!        write.
!!
!!      Note that this raw format can only be used for maxvals less
!!      than or equal to 255. If you use the ppm library and try to
!!      write a file with a larger maxval,  it will automatically
!!      fall back on the slower but more general plain format.
!!
!!##AUTHOR
!!      Copyright (C) 1989, 1991 by Jef Poskanzer.
!!
!!                  Last change: 27 September 1991
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    textsize(3f) - [M_pixel:TEXT] set text size in world units
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine textsize(width, height)
!!    real,intent(in) :: width
!!    real,intent(in) :: height
!!
!!##DESCRIPTION
!!    Set the maximum size of a character in the current font. Width
!!    and height are values in world units. This only applies to software
!!    text. This must be done after the font being scaled is loaded. To keep
!!    text of different sizes aligned along the same baseline note that you
!!    typically need to subtrace the descender height from the Y position.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textsize
!!    use M_pixel
!!    use M_pixel__writegif, only : writegif
!!    implicit none
!!    integer :: i,ii
!!       !! set up long bar as plotting area
!!       call prefsize(900,150)
!!       call vinit()
!!       call ortho2(-30.0, 30.0, -5.0, 5.0)
!!       call font('DUPLEX')
!!       call move2(-23.0,-4.5)
!!       call color(7)
!!       call textsize(2.0,2.0)
!!       call move2(-27.5,-3.0)
!!       call draw2( 27.5,-3.0)
!!       call move2(-27.5,-3.0)
!!       do i=1,7
!!          ii=nint((i*20)*0.30)
!!          call linewidth(nint(ii*2.35))
!!          call textsize(real(i),real(i))
!!          call color(5)
!!          call drawstr('aA')
!!       enddo
!!       ! write plot as GIF file
!!       call writegif('textsize.3M_pixel.gif',P_pixel,P_colormap)
!!       call vexit()
!!       ! use system to display GIF file
!!       call execute_command_line('display textsize.3M_pixel.gif')
!!    end program demo_textsize
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine textsize(width,height)

! ident_41="@(#) M_pixel textsize(3f) set text size in world units"

real,intent(in) :: width
real,intent(in) :: height

   P_TEXT_HEIGHT=height
   P_TEXT_WIDTH=width

end subroutine textsize
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    ycentertext(3f) - [M_pixel:TEXT] set text centering mode on for
!!                      drawstr(3f) and drawc(3f) in Y direction
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine ycentertext()
!!
!!##DESCRIPTION
!!    Centers text in the Y direction. The text string will be draw so
!!    that its center line is aligned with the current y position. Top
!!    justification and Bottom justification are turned off.
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine ycentertext()

! ident_42="@(#) M_pixel ycentertext(3f) set text centering mode on for drawstr(3f) and drawc(3f) in Y direction"

   P_X_centertext=.false.
   P_Y_centertext=.true.

end subroutine ycentertext
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    xcentertext(3f) - [M_pixel:TEXT] set text centering mode on for
!!                      drawstr(3f) and drawc(3f) in X direction
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine xcentertext()
!!
!!##DESCRIPTION
!!    Set text centering mode on in X direction. Y justification is
!!    turned off.
!!
!!    Centers text in the X direction. The text string will begin at a
!!    point to the notional left of the current position and finish at a
!!    point to the right of the current position. Left justification and
!!    Right justification are turned off.
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine xcentertext()

! ident_43="@(#) M_pixel xcentertext(3f) set text centering mode for drawstr(3f) and drawc(3f) in X direction"

   P_X_CENTERTEXT=.true.
   P_Y_centertext=.false.

end subroutine xcentertext
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    centertext(3f) - [M_pixel:TEXT] set text centering mode for drawstr(3f)
!!                     and drawc(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine centertext(onoff)
!!    logical,intent(in) :: onoff
!!
!!##DESCRIPTION
!!    Set text centering mode on or off. Only approximate in vertical
!!    direction.
!!
!!##OPTIONS
!!    ONOFF  set centering mode on or off
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_centertext
!!    use :: M_pixel
!!    use :: M_pixel, only : cosd, sind
!!    use :: M_pixel__writegif, only : writegif
!!    implicit none
!!    real    :: x1, y1, xx, yy, ang, r
!!    integer :: i, j
!!    !! set up drawing environment
!!    call prefsize(600,600)
!!    call vinit()
!!    call ortho2(-300.0,300.0,-300.0,300.0)
!!    call textsize(8.0,8.0)
!!    call linewidth(30)
!!    x1=-150
!!    y1=-150
!!    do j=1,4
!!       select case(j)
!!       case(1);  call  xcentertext();        x1=-150;  y1=-150;  r=100
!!       case(2);  call  ycentertext();        x1=+150;  y1=-150;  r= 30
!!       case(3);  call  centertext(.true.);   x1=-150;  y1=+150;  r=100
!!       case(4);  call  centertext(.false.);  x1=+150;  y1=+150;  r= 30
!!       end select
!!       !! draw radial lines
!!       call color(1)
!!       do i=1,80
!!          call move2(x1,y1)
!!          call draw2(x1+150.0*cosd(i*12), y1+150.0*sind(i*12))
!!       enddo
!!
!!       !! draw rotated text
!!       call color(2)
!!       do i=1,30
!!          ang=i*12.0
!!          xx=x1+r*cosd(ang)
!!          yy=y1+r*sind(ang)
!!          call move2(xx,yy)
!!          call textang(ang)
!!          call color(7)
!!          call drawstr('This is angled text')
!!          call color(1)
!!       enddo
!!    enddo
!!
!!    call  writegif('centertext.3M_pixel.gif',P_pixel,P_colormap)
!!    call  execute_command_line('display centertext.3M_pixel.gif')
!!
!!    call vexit()
!!
!!    end program demo_centertext
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine centertext(onoff)

! ident_44="@(#) M_pixel centertext(3f) set text centering mode for drawstr(3f) and drawc(3f)"

logical,intent(in) :: onoff

   P_X_CENTERTEXT=onoff
   P_Y_CENTERTEXT=onoff

end subroutine centertext
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    textang(3f) - [M_pixel:TEXT] set text angle
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine textang(ang)
!!    real,intent(in) :: ang
!!
!!##DESCRIPTION
!!    Set the text angle. This angles strings and chars. This routine only
!!    affects software text.
!!
!!##OPTIONS
!!    ANG   The angle in degrees to draw text with when using drawstr(3f).
!!          Angles are measured counterclockwise with zero degrees at the
!!          horizontal line to the right of the original.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textang
!!    use :: M_pixel
!!    use :: M_pixel, only : cosd, sind
!!    use :: M_pixel__writegif, only : writegif
!!    implicit none
!!    integer :: i
!!    !! set up drawing environment
!!    call prefsize(600,600)
!!    call vinit()
!!    call ortho2(-100.0,100.0,-100.0,100.0)
!!    call textsize(7.0,7.0)
!!    call linewidth(20)
!!    do i=1,30
!!       !! draw radial lines
!!       call color(1)
!!       call move2(0.0,0.0)
!!       call draw2(100.0*cosd(i*12),100.0*sind(i*12))
!!       !! draw rotated text
!!       call color(7)
!!       call move2(30.0*cosd(i*12),30.0*sind(i*12))
!!       call textang(i*12.0)
!!       call drawstr('angled text')
!!    enddo
!!
!!    call writegif('textang.3M_pixel.gif',P_pixel,P_colormap)
!!    call execute_command_line('display textang.3M_pixel.gif')
!!
!!    call vexit()
!!
!!    end program demo_textang
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine textang(ang)

! ident_45="@(#) M_pixel textang(3f) set angle in degrees to draw text at using drawstr(3f)"

real,intent(in) :: ang

   P_TEXT_ANGLE=ang
   P_TEXT_COSINE=cosd(P_TEXT_ANGLE)
   P_TEXT_SINE  =sind(P_TEXT_ANGLE)

end subroutine textang
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    font(3f) - [M_pixel:TEXT] select font style by name
!!    (LICENSE:PD)
!!
!!##SYNOPSIS:
!!  definition:
!!
!!         subroutine font(fontname)
!!         character(len=*),intent(in) :: fontname
!!
!!##DESCRIPTION
!!    Set the current font. Allowed names are
!!
!!       o futura.l  SIMPLEX
!!       o futura.m  DUPLEX
!!       o times.r   COMPLEX
!!       o times.i   ITALIC
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_font
!!    use :: M_pixel
!!    use :: M_pixel__writegif, only : writegif
!!    implicit none
!!    real    :: left
!!    real    :: baseline=80.0
!!    integer :: icolor=1
!!       !! set up drawing surface
!!       call prefsize(400, 400)
!!       call vinit()
!!       call viewport(0.0, 400.0, 400.0, 0.0)
!!       call ortho2(-100.0, 100.0, -100.0, 100.0)
!!       call color(7)
!!       call clear()
!!       call textsize(10.0, 10.0)
!!       !! place a vertical line along the edge
!!       call color(1)
!!       call move2(-90.0, -90.0)
!!       call draw2(-90.0, 90.0)
!!       !! make a centered title at top a bit bolder and bigger
!!       call xcentertext()
!!       call textsize(13.0, 13.0)
!!       call linewidth(90)
!!       left=0
!!       call nextline('Font Samples')
!!       !! print the font samples
!!       left=-90
!!       call linewidth(0)
!!       call textsize(10.0, 10.0)
!!       call centertext(.false.)
!!       icolor=icolor-1
!!       call nextline('DEFAULT (ie. futura.l)')
!!       icolor=icolor-1
!!       call nextline('now call font(3f) ...')
!!       call nextline('SIMPLEX, or futura.l')
!!       call nextline('COMPLEX, or times.r')
!!       call nextline('ITALIC, or times.i')
!!       call nextline('DUPLEX, or futura.m')
!!       call writegif('font.3M_pixel.gif',P_pixel,P_colormap)
!!       !call execute_command_line('display font.3M_pixel.gif')
!!       call vexit()
!!    contains
!!    subroutine nextline(string)
!!    character(len=*) :: string
!!    !! reduce some duplicate code; very specific to this example
!!    integer :: iend
!!       iend=index(string,',')  ! if comma, assume font name found
!!       if(iend.ne.0)call font(string(:iend-1)) ! change font
!!       icolor=icolor+1         ! set pen color
!!       call color(icolor)
!!       baseline=baseline-20    ! move down before drawing line
!!       call move2(left, baseline)
!!       call drawstr(string)    ! draw string
!!    end subroutine nextline
!!
!!    end program demo_font
subroutine font(fontname)

! ident_46="@(#) M_pixel font(3f) select font style by name"

character(len=*),intent(in) :: fontname
      select case(fontname)
      case ('futura.l','SIMPLEX','simplex')
        P_FONT='SIMPLEX'
      case ('futura.m','DUPLEX','duplex')
        P_FONT='DUPLEX'
      case ('times.r' ,'COMPLEX','complex')
        P_FONT='COMPLEX'
      case ('times.i' ,'ITALIC','italic')
        P_FONT='ITALIC'
      case default
        P_FONT='SIMPLEX'
      end select
end subroutine font
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    drawchar(3f) - [M_pixel:TEXT]  Draw a character at the current position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine drawchar(ch)
!!    character(len=1),intent(in) :: ch
!!
!!##DESCRIPTION
!!    Draw a character at the current position. Uses current line color
!!    and thickness and text justification mode.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_drawchar
!!    use M_pixel
!!    use M_pixel__writegif_animated, only : write_animated_gif
!!    implicit none
!!    integer,parameter :: isize=600
!!    integer           :: movie(32:124,0:isize-1,0:isize-1)
!!    integer           :: i
!!    !! set up environment
!!    call prefsize(isize,isize)
!!    call vinit()
!!    call ortho2(-100.0,100.0,-100.0,100.0)
!!    call textsize(150.0,150.0)
!!    call centertext(.true.)
!!
!!    do i=33,124
!!       !! draw reference circle and crosshairs
!!       call linewidth(100)
!!       call color(0)
!!       call clear()
!!       call color(4)
!!       call circle(0.0,0.0,75.0)
!!       call move2(-75.0,0.0)
!!       call draw2(75.0,0.0)
!!       call move2(0.0,-75.0)
!!       call draw2(0.0,75.0)
!!       call color(7)
!!       call linewidth(200)
!!       call textang(3.0*i)
!!       call move2(0.0,0.0)
!!       call drawchar(char(i))
!!       movie(i,:,:)=P_pixel
!!    enddo
!!    call vexit()
!!    !! write to file and display with display(1)
!!    call write_animated_gif('drawchar.3M_pixel.gif',movie,P_colormap)
!!    call execute_command_line('display drawchar.3M_pixel.gif')
!!    end program demo_drawchar
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine drawchar(ch)

! ident_47="@(#) M_pixel drawchar(3f) draw text at the current position"

character(len=1),intent(in) :: ch

   call drawstr(ch)

end subroutine drawchar
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    drawstr(3f) - [M_pixel:TEXT]  Draw the text string at the current position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine drawstr(string)
!!    character(len=*),intent(in) :: string
!!
!!##DESCRIPTION
!!    Draw a text string at the current position. Uses current line color
!!    and thickness and text centering mode.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!       program demo_drawstr
!!       use M_pixel
!!       use :: M_pixel__writegif, only : writegif
!!       implicit none
!!       call prefsize(400,400)
!!       call vinit()
!!       call ortho2(-1.0,1.0,-1.0,1.0)
!!       ! by default the drawing surface is
!!       ! a square ranging from -1 to 1 in both
!!       ! the X and Y axis
!!       write(*,*)D_BLACK, D_GREEN, D_RED
!!
!!       call color(D_BLACK)    ! set current color to black
!!       call clear()           ! clear to current color
!!
!!       ! SET COMMON TEXT ATTRIBUTES
!!       call color(D_GREEN)    ! we want to draw in green
!!       call circle(0.0,0.0,1.0)
!!       call font('futura.m')  ! set font
!!       call textsize(0.1,0.1) ! font size
!!
!!       ! DRAW A STRING
!!       call move2(-1.0, 0.0)
!!       call drawstr('Hello')  ! draw string at current position
!!       ! note that current position is now at end of this string
!!
!!       ! CHANGE SOME TEXT ATTRIBUTES AGAIN
!!       call linewidth(20)     ! set line width
!!       call color(D_RED)      ! change color
!!       call textang(45.0)     ! change text angle
!!
!!       call drawstr(' World!')! draw string at current position
!!       !! render pixel array to a file
!!       call writegif('drawstr.3M_pixel.gif',P_pixel,P_colormap)
!!       !! display graphic assuming display(1) is available
!!       call execute_command_line('display drawstr.3M_pixel.gif')
!!
!!       call vexit()           !  wrap up and exit graphics mode
!!
!!       end program demo_drawstr
!!   Results:
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine drawstr_(string)
!-!use :: M_pixel, only : cosd, sind

! ident_48="@(#) M_pixel drawstr(3f) draw text at the current position"

character(len=*),intent(in)  :: string
character(len=:),allocatable :: fontstring
   real                     :: s(4)
   real                     :: xt, yt
   real                     :: xx, yy, ll
   !
   !   gives 4 distances in world coordinates, all from the left end of the string -
   !
   !   o S(1)  to the left edge of the 1st nonblank character
   !   o S(2)  to the center of the string, blanks removed from the ends
   !   o S(3)  to the right edge of the last nonblank character
   !   o S(4)  to the right edge of the last character of the string.
   !  XCENTER            *    !                   *        !                 *         *
   !               *         *      !             *\        *          !           *    \    *          *
   !         *        \*          *
   !       *         *  \       *        X2=X1-S(2)*COSD(P_TEXT_ANGLE)
   !       \       *      \   *          Y2=Y1-S(2)*SIND(P_TEXT_ANGLE)
   !         \   *          * X1,Y1
   !           *          *
   !            \       *
   !              \   *    P_TEXT_ANGLE
   !                *  X2,Y2==================
   !
   !
   !  YCENTER            *    !                   *        !                 *         *
   !               *         *      !             *\        *          !           *    \    *          *
   !         *        \* X1,Y1    *
   !       *         *  \       *        X2=X1+P_TEXT_HEIGHT/2.0*COSD(P_TEXT_ANGLE+90)
   !       \       *      \   *          Y2=Y1-P_TEXT_HEIGHT/2.0*SIND(P_TEXT_ANGLE+90)
   !         \   *          * X2,Y2      X3=
   !           *          *
   !            \       *
   !              \   *    P_TEXT_ANGLE
   !                *  =======================
   !
   !  CENTER             *    !                   *        !                 *         *
   !               *         *      !             *\        *          !           *    \    *          *
   !         *        \* X1,Y1    *
   !       *         *  \       *        X2=X1+S(2)*COSD(P_TEXT_ANGLE+90)
   !       \       *      \   *          Y2=Y1-S(2)*SIND(P_TEXT_ANGLE+90)
   !         \   *          *X2,Y2
   !           *          *
   !            \       *
   !              \   *    P_TEXT_ANGLE
   !                * X3,Y3 ==================
   !
   !

   xt=P_X
   yt=P_Y
   fontstring='\'//trim(P_FONT)//'\'//trim(string)

   if (P_X_CENTERTEXT.or.P_Y_CENTERTEXT)then
      call justfy(s, P_TEXT_HEIGHT, trim(string), len_trim(fontstring))

      if (P_Y_CENTERTEXT)then
         XT=XT-P_TEXT_HEIGHT/2.0*COSD(P_TEXT_ANGLE+90)
         YT=YT-P_TEXT_HEIGHT/2.0*SIND(P_TEXT_ANGLE+90)
      endif
      if (P_X_CENTERTEXT)then
         xt=xt-s(2)*P_TEXT_COSINE
         yt=yt-s(2)*P_TEXT_SINE
      endif

   endif
   call hershey(xt,yt,P_TEXT_HEIGHT,fontstring,P_TEXT_ANGLE,len_trim(fontstring))
   ! hershey(3f) appears to leave off at last vector written in the last character, so a
   ! series of calls creates a very strange string. This makes more sense.
   ll=strlength(trim(string))
   xx=xt+cosd(P_TEXT_ANGLE)*ll
   yy=yt+sind(P_TEXT_ANGLE)*ll
   call move2(xx,yy)

end subroutine drawstr_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    getgp2(3f) - [M_pixel] Gets the current graphics position in world coords.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine getgp2(x, y)
!!    real,intent(out) :: x,y
!!
!!##DESCRIPTION
!!    Gets the current graphics position in world coords.
!!
!!##RETURNS
!!    X  X coordinate of current position
!!    Y  Y coordinate of current position
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!      program demo_getgp2
!!      use M_pixel
!!      implicit none
!!      real :: X,Y
!!      call prefsize(20,20)
!!      call vinit()
!!      call ortho2(-100.0,100.0,-100.0,100.0)
!!      call move2(0.0,0.0)
!!      call draw2(96.5,98.333)
!!
!!      call getgp2(X,Y)
!!      write(*,*)'CURRENT POSITION (X,Y)=',X,Y
!!
!!      call vexit()
!!      end program demo_getgp2
!!
!!   Results
!!
!!    CURRENT POSITION (X,Y)=   96.5000000       98.3330002
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine getgp2(x, y)

! ident_49="@(#) M_pixel getgp2(3f) get current graphics position"

real,intent(out) :: x, y

   x=P_X
   y=P_Y

end subroutine getgp2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    getdisplaysize(3f) - [M_pixel] Returns the width and height of the
!!                         device in pixels
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine getdisplaysize(w, h)
!!    real,intent(in) :: w, h
!!
!!##DESCRIPTION
!!    Returns the width and height of the device in pixels in w and h
!!    respectively.
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine getdisplaysize(w, h)

! ident_50="@(#) M_pixel getdisplaysize(3f) Returns the width and height of the device in pixels"

real,intent(out) :: w, h

   w=P_VIEWPORT_WIDTH
   h=P_VIEWPORT_HEIGHT

end subroutine getdisplaysize
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    point2(3f) - [M_pixel:DRAW] Draw a point at x, y
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    elemental impure subroutine point2(x, y)
!!    real,intent(in) :: x, y
!!
!!##DESCRIPTION
!!    Draw a point at x, y. Points are drawn with the current color as
!!    a circle with a diameter equal to the current linewidth.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_point2
!!    use :: M_pixel
!!    use :: M_pixel__writegif, only : writegif
!!    implicit none
!!    integer :: i
!!    call vinit()
!!    call color(5)
!!    do i=1,20
!!       call linewidth(50*i)
!!       call point2(real(i*25),real(i*25))
!!    enddo
!!    call writegif('point2.3M_pixel.gif',P_pixel,P_colormap)
!!    call vexit()
!!    end program demo_point2
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental impure subroutine point2(x, y)

! ident_51="@(#) M_pixel point2(3f) Draw a point at x y"

real,intent(in) :: x, y

   call line(x,y,x,y)

end subroutine point2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    state(3f) - [M_pixel] print graphics state of M_pixel graphics module
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    recursive subroutine state(string)
!!    character(len=*),intent(in),optional :: string
!!
!!##DESCRIPTION
!!    Print the state of the M_pixel graphics module. This is primarily
!!    used in debugging during program development and is not currently in
!!    the M_draw library.
!!
!!##OPTIONS
!!    STRING  can have the following values
!!            o all
!!            o default
!!            o colormap
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_state
!!    use M_pixel
!!    implicit none
!!       call prefsize(640,400)
!!       call vinit()
!!       call state()
!!       call vexit()
!!    end program demo_state
!!
!!   Results:
!!
!!    VINIT CALLED:        T
!!    PREFSIZE: WIDTH=         640  HEIGHT=         400
!!    CURRENT POSITION: X=   0.00000000      Y=   0.00000000
!!    LINE WIDTH:                    1
!!    FONT:               SIMPLEX
!!    COLOR NUMBER:                  1
!!    CIRCLE PRECISION:             60
!!    TEXT:               HEIGHT=   10.000  WIDTH= 7.0000 ANGLE= 0.0000
!!    TEXT JUSTIFICATION: X_CENTER= F Y_CENTER= F
!!    VIEWPORT:           LEFT=   0.0000  RIGHT= 639.00 BOTTOM= 399.00 TOP= 0.0000
!!    WINDOW:             LEFT=   0.0000  RIGHT= 640.00 BOTTOM= 0.0000 TOP= 400.00
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
recursive subroutine state(string)

! ident_52="@(#) M_pixel state(3f) print graphics state of M_pixel graphics module"

character(len=*),intent(in),optional :: string
character(len=40)         :: string_local
character(len=*),parameter :: g='(*(g0))'
integer :: i

if(present(string))then
   string_local=string
else
   string_local='all'
endif

!-----------------------------------------------------------------------------------------------------------------------------------
select case(trim(string))
!-----------------------------------------------------------------------------------------------------------------------------------
case ('all')
   call state('colormap')
   call state('default')
!-----------------------------------------------------------------------------------------------------------------------------------
case ('colormap','color')
write(*,g)'COLOR MAP:          ',new_line('n'),(i,P_COLORMAP(:,i),new_line('n'),i=0,255)
!-----------------------------------------------------------------------------------------------------------------------------------
case default
write(*,g)'VINIT CALLED:       ',P_VINIT_CALLED
write(*,g)'PREFSIZE: WIDTH=    ',P_VIEWPORT_WIDTH,' HEIGHT=',P_VIEWPORT_HEIGHT
write(*,g)'CURRENT POSITION: X=',P_X,' Y=',P_Y
write(*,g)'LINE WIDTH:         ',P_WIDTH
write(*,g)'FONT:               ',P_FONT
write(*,g)'COLOR NUMBER:       ',P_COLOR_INDEX
write(*,g)'CIRCLE PRECISION:   ',P_NSEGS
write(*,g)'TEXT:               ','HEIGHT=',P_TEXT_HEIGHT,'WIDTH=',P_TEXT_WIDTH,'ANGLE=',P_TEXT_ANGLE
write(*,g)'TEXT JUSTIFICATION: ','X_CENTER=',P_X_CENTERTEXT,'Y_CENTER=',P_Y_CENTERTEXT
write(*,g)'VIEWPORT:           ','LEFT=',P_VIEWPORT_LEFT,'RIGHT=',P_VIEWPORT_RIGHT,'BOTTOM=',P_VIEWPORT_BOTTOM,'TOP=',P_VIEWPORT_TOP
write(*,g)'WINDOW:             ','LEFT=',P_WINDOW_LEFT,'RIGHT=',P_WINDOW_RIGHT,'BOTTOM=',P_WINDOW_BOTTOM,'TOP=',P_WINDOW_TOP
!-----------------------------------------------------------------------------------------------------------------------------------
end select
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine state
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    poly2(3f) - [M_pixel:POLYGONS] construct a polygon from an array of points
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  definition:
!!
!!    subroutine poly2(n, points)
!!    integer,intent(in) :: n
!!    real,intent(in)    :: points(2, n)
!!
!!##DESCRIPTION
!!    Construct a polygon from an array of points
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_poly2
!!    use M_pixel
!!    use M_pixel__writegif, only : writegif
!!    implicit none
!!    integer :: i, j, icolor
!!    real    :: xx, yy
!!       call prefsize(512,512)
!!       call vinit()
!!       call ortho2(0.0,256.0,0.0,256.0)
!!       call linewidth(1)
!!       ! step thru a series of rectangular cells
!!       icolor=0
!!       xx=0.0
!!       do i=1,16
!!          yy=0.0
!!          do j=1,16
!!             yy=yy+16.0
!!             icolor=icolor+1
!!             call setcolor(icolor,xx,yy)
!!          enddo
!!          xx=xx+16.0
!!       enddo
!!       call writegif('poly2.3M_pixel.gif',P_pixel,P_colormap)
!!       call vexit()
!!    contains
!!
!!    subroutine setcolor(iset,xx,yy)
!!    use M_pixel,  only : i2s
!!    use M_pixel,  only : color_name2rgb
!!    integer,intent(in) :: iset
!!    real,intent(in)    :: xx,yy
!!    character(len=80)  :: echoname
!!    real               :: points(2,100)
!!    real               :: red, green, blue
!!       if(iset.gt.255)return
!!       ! determine coordinates of next square
!!       points(1:2,1)=[xx,      yy      ]
!!       points(1:2,2)=[xx,      yy+16.0 ]
!!       points(1:2,3)=[xx+16.0, yy+16.0 ]
!!       points(1:2,4)=[xx+16.0, yy      ]
!!       points(1:2,5)=[xx,      yy      ]
!!       ! get some nice RGB values to try from named colors known by M_pixel
!!       call color_name2rgb(i2s(icolor),red,green,blue,echoname)
!!       if(echoname.eq.'Unknown') return
!!       ! set a color number to the new RGB values
!!       write(*,*)icolor, nint(red*2.55), nint(green*2.55), nint(blue*2.55),&
!!              & trim(echoname)
!!       call mapcolor(icolor, nint(red*2.55), nint(green*2.55), nint(blue*2.55))
!!       ! set to the new color
!!       call color(icolor)
!!       ! fill the rectangle in that color
!!       call poly2(5,points)
!!    end subroutine setcolor
!!
!!    end program demo_poly2
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine poly2(n,points)

! ident_53="@(#) M_pixel poly2(3f) construct a polygon from an array of points"

integer,intent(in) :: n
real,intent(in)    :: points(2, n)
   real            :: xx, yy
   integer         :: ix(n), iy(n)
   integer         :: i

   do i=1,n                                                   ! convert array from world coordinates to pixel coordinates
      call world2viewport(points(1,i), points(2,i), xx, yy)
      ix(i)=nint(xx)
      iy(i)=nint(yy)
   enddo

   call PPM_SOLID_FILL(ix, iy, n)

end subroutine poly2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine vflush()

! ident_54="@(#) M_pixel vflush(3f) flush current page"

end subroutine vflush
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine PPM_DRAW_FILL_LINE(xstart,ystart,x,y)

! ident_55="@(#) M_pixel PPM_DRAW_FILL_LINE(3fp) draws a line across a graphics array"

integer,intent(in) :: xstart,ystart
integer,intent(in) :: x,y
   integer         :: ix,iy
   integer         :: runcount
   integer         :: dx,dy
   integer         :: xinc,yinc
   integer         :: xplot,yplot
   integer         :: mostx, mosty

   mostx=size(P_pixel,dim=1)-1
   mosty=size(P_pixel,dim=2)-1
   if(x.le.mostx.and.y.le.mosty.and.x.gt.0.and.y.gt.0) P_PIXEL(xstart,ystart)=P_COLOR_INDEX ! move to initial spot
   ix=xstart
   iy=ystart

   runcount=0

   dx = abs(ix-x)

   xinc=0
   if (x > ix)  xinc=  1
   if (x == ix) xinc=  0
   if (x < ix)  xinc= -1

   dy = abs(iy-y)

   yinc=0
   if (y > iy)  yinc=  1
   if (y == iy) yinc=  0
   if (y < iy)  yinc= -1

   xplot = ix
   yplot = iy

   if (dx>dy) then
      ! iterate x
      do while (xplot /= x)
         xplot = xplot + xinc
         runcount = runcount + dy
         if (runcount >= (dx-runcount)) then
            yplot = yplot + yinc
            runcount = runcount - dx
         endif
         if(xplot.le.mostx.and.yplot.le.mosty.and.xplot.gt.0.and.yplot.gt.0) P_PIXEL(xplot,yplot)=P_COLOR_INDEX
      enddo
   else
      ! iterate y
      do while (yplot /= y)
         yplot = yplot + yinc
         runcount = runcount + dx
         if (runcount >= (dy-runcount)) then
            xplot = xplot + xinc
            runcount = runcount - dy
         endif
         if(xplot.le.mostx.and.yplot.le.mosty.and.xplot.gt.0.and.yplot.gt.0) P_pixel(xplot,yplot)=P_COLOR_INDEX
      enddo
   endif

end subroutine PPM_DRAW_FILL_LINE
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine PPM_draw_thick_line(inx1,iny1,inx2, iny2)

! ident_56="@(#) M_pixel PPM_DRAW_THICK_LINE(3fp) draw line from current pixel graphics position to (x y) using polygons for line thickness"

integer,intent(in) :: inx1,iny1,inx2,iny2
   integer         :: cosine, sine
   real            :: angle
   !
   !               *  P2
   !             *      !           *          !         *         * inx2,iny2
   !   P1  *         *       !       \       *        \ P3
   !         \   *          *
   ! inx1,iny1 *          *
   !            \       *
   !              \   *
   !                * P4
   !

   ! thick lines are made from filled polygon(s). Add a circle to ends of really thick lines
   call PPM_ENDCAP_CIRCLE(inx1,iny1)
   call PPM_ENDCAP_CIRCLE(inx2,iny2)

   angle=atan2(real(iny2-iny1),real(inx2-inx1)) + PI32/2.0
   cosine=nint((P_WIDTH/2.0)*cos(angle))
   sine=nint((P_WIDTH/2.0)*sin(angle))

   call PPM_SOLID_FILL( [inx1+cosine, inx2+cosine, inx2-cosine, inx1-cosine], [iny1+sine, iny2+sine, iny2-sine, iny1-sine], 4)

end subroutine PPM_draw_thick_line
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function PPM_YINTERCEPT(yscan, x1, y1, x2, y2, xintercept, yprev)
logical :: PPM_YINTERCEPT
integer :: yscan
integer :: x1
integer :: y1
integer :: x2
integer :: y2
integer :: xintercept
integer :: yprev
! Determine if scan line intercepts the line segment. If it does, return the x intercept.
   integer :: deltay, yprevious
   real    :: t
   yprevious = yprev   ! the value we need to use in this pass
   yprev = y1          ! store the value for the next call to (probably) use
   deltay = y2 - y1
   if ( deltay == 0 )then
      ! horizontal lines do not contribute to scan line intercepts
      yprev=yprevious
      PPM_YINTERCEPT=.false.
      return
   endif
   t = real(yscan - y1) / deltay
   if (t > 0.0 .and. t <= 1.0) then
      ! scan line and line segment intersect but not at leading vertex
      xintercept = x1 + nint(t*(x2 - x1))
      PPM_YINTERCEPT=.true.
      return
   elseif ( t == 0.0 )then
      ! scan line and line segment intersect at leading vertex
      xintercept = x1 + nint(t*(x2 - x1))
      if(yprevious <= y1 .and. y2 <= y1 )then
         ! local maximum
         PPM_YINTERCEPT=.true.
         return
      elseif(yprevious >= y1 .and. y2 >= y1 )then
         ! local minimum
         PPM_YINTERCEPT=.true.
         return
      else
         ! ignore duplicate at vertex that is not a local maximum or minimum
         PPM_YINTERCEPT=.false.
         return
      endif
   endif
   PPM_YINTERCEPT=.false.
   ! scan line and line segment did not intersect
end function PPM_YINTERCEPT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine PPM_SOLID_FILL(x,y,n)
!-!use M_sort, only : sort_shell

! ident_57="@(#) M_pixel PPM_SOLID_FILL(3fp) fill polygon of n points that are in viewport coordinates"

integer,intent(in) :: n
integer,intent(in) :: x(0:n-1)
integer,intent(in) :: y(0:n-1)
integer,parameter  :: MAX_VERTICES=9999
integer            :: x1,y1

   integer :: i, j, yhorizontal, xint, xmin, xmax, ymax, ymin, xi(0:MAX_VERTICES), yprev

   xi=-999999
   if ( n >= MAX_VERTICES) then
      write(*,*)"*PPM_SOLID_FILL* more than ",MAX_VERTICES," vertices in a polygon"
      return
   endif

   ! find clip range
   ymin=minval(y)
   ymax=maxval(y)
   xmin=minval(x)
   xmax=maxval(x)

   ! ensure scan lines are generated that do not cause out-of-bound problems in the y direction
   ymin=MAX(ymin,0)
   ymax=MIN(ymax,P_VIEWPORT_HEIGHT-1)

   ! For each y value, get a list of X intersections...
   yhorizontal = ymax
   do while (yhorizontal >= ymin)
      j = 0
      yprev = y(n-1)
      do i = 0,n-2
         if (PPM_YINTERCEPT(yhorizontal, x(i), y(i), x(i+1), y(i+1), xint, yprev))then
            xi(j) = xint
            j=j+1
         endif
      enddo
      ! Last one.
      if (PPM_YINTERCEPT(yhorizontal, x(n-1), y(n-1), x(0), y(0), xint, yprev))then
         xi(j) = xint
         j=j+1
      endif

      ! odd pairs means something went wrong in figuring out whether to count vertices or not
      if( 2 * (j/2) /= j)then
         if(P_DEBUG) then
            write(*,*)"*PPM_SOLID_FILL* Internal error: odd number of intersection points ",j
         endif
      endif

      call sort_shell_integers_hl(xi(0:j-1)) ! Sort the X intersections

      ! Draw the horizontal lines
      ! should make sure within X clipping range
      do i = 0, j-2, 2
         x1=MAX(0,MIN(xi(i),P_VIEWPORT_WIDTH-1))
         y1=yhorizontal
         call PPM_DRAW_FILL_LINE(x1,y1,MAX(0, MIN(xi(i+1), P_VIEWPORT_WIDTH-1)),  yhorizontal)
      enddo
      yhorizontal = yhorizontal - 1
   enddo
end subroutine PPM_SOLID_FILL
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine PPM_ENDCAP_CIRCLE(x, y)

! ident_58="@(#) M_pixel PPM_ENDCAP_CIRCLE(3fp) Draw a circle on thick line segment end point"

integer,intent(in) :: x
integer,intent(in) :: y

   integer,parameter :: nsegs=15                       ! circle precision
   real              :: angle_step
   integer           :: cxras(nsegs), cyras(nsegs)     ! array to place circle points on
   integer           :: i

   angle_step = 360.0 / nsegs
   do i=0,nsegs-1
      cxras(i+1) = nint(x+(P_WIDTH-1)/2.0*cosd(angle_step*i))
      cyras(i+1) = nint(y+(P_WIDTH-1)/2.0*sind(angle_step*i))
   enddo
   call PPM_SOLID_FILL(cxras,cyras,nsegs)

end subroutine PPM_ENDCAP_CIRCLE
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    drawstr(3f) - [M_pixel] converts any standard scalar type to a string and prints it
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine drawstr(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,sep)
!!
!!     class(*),intent(in),optional  :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9
!!     class(*),intent(in),optional  :: ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!     character(len=*),intent(in),optional :: sep
!!     character(len=:),allocatable  :: sep_local
!!
!!##DESCRIPTION
!!    drawstr(3f) builds a space-separated string from up to twenty scalar values.
!!
!!##OPTIONS
!!    g[0-9a-j]   optional value to print the value of after the message. May
!!                be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION,
!!                COMPLEX, or CHARACTER.
!!
!!                Optionally, all the generic values can be
!!                single-dimensioned arrays. Currently, mixing scalar
!!                arguments and array arguments is not supported.
!!
!!    sep         separator between values. Defaults to a space.
!!
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_msg
!!    use M_pixel, only : str
!!    implicit none
!!    character(len=:),allocatable :: pr
!!    character(len=:),allocatable :: frmt
!!    integer                      :: biggest
!!
!!       pr=str('HUGE(3f) integers',huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
!!       write(*,'(a)')pr
!!       pr=str('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!       write(*,'(a)')pr
!!       pr=str('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!       write(*,'(a)')pr
!!       pr=str('complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!       write(*,'(a)')pr
!!
!!       ! create a format on the fly
!!       biggest=huge(0)
!!       frmt=str('(*(i',int(log10(real(biggest))),':,1x))',sep='')
!!       write(*,*)'format=',frmt
!!
!!       ! although it will often work, using str(3f) in an I/O statement is not recommended
!!       ! because if an error occurs str(3f) will try to write while part of an I/O statement
!!       ! which not all compilers can handle and is currently non-standard
!!       write(*,*)str('program will now stop')
!!
!!    end program demo_msg
!!
!!  Output
!!
!!    HUGE(3f) integers 2147483647 and real 3.40282347E+38 and double 1.7976931348623157E+308
!!    real            : 3.40282347E+38 0.00000000 12345.6787 1.17549435E-38
!!    doubleprecision : 1.7976931348623157E+308 0.0000000000000000 12345.678900000001 2.2250738585072014E-308
!!    complex         : (3.40282347E+38,1.17549435E-38)
!!     format=(*(i9:,1x))
!!     program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine msg_scalar(generic0, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9, &
                  & generica, genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj, &
                  & sep)
implicit none

! ident_2="@(#)M_pixel::msg_scalar(3fp): writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic0, generic1, generic2, generic3, generic4
class(*),intent(in),optional  :: generic5, generic6, generic7, generic8, generic9
class(*),intent(in),optional  :: generica, genericb, genericc, genericd, generice
class(*),intent(in),optional  :: genericf, genericg, generich, generici, genericj
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      increment=1+len(sep)
      sep_local=sep
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=''
   if(present(generic0))call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   if(present(generica))call print_generic(generica)
   if(present(genericb))call print_generic(genericb)
   if(present(genericc))call print_generic(genericc)
   if(present(genericd))call print_generic(genericd)
   if(present(generice))call print_generic(generice)
   if(present(genericf))call print_generic(genericf)
   if(present(genericg))call print_generic(genericg)
   if(present(generich))call print_generic(generich)
   if(present(generici))call print_generic(generici)
   if(present(genericj))call print_generic(genericj)
   msg=trim(line)
   call drawstr_(msg)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(l1)') generic
      type is (character(len=*));       write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   line=line(:istart-1)//sep_local
   istart=len_trim(line)+increment
end subroutine print_generic
!===================================================================================================================================
end subroutine msg_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine msg_one(generic0,generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
implicit none

! ident_3="@(#)M_pixel::msg_one(3fp): writes a message to a string composed of any standard one dimensional types"

class(*),intent(in)           :: generic0(:)
class(*),intent(in),optional  :: generic1(:), generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      increment=1+len(sep)
      sep_local=sep
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   msg=trim(line)
   call drawstr_(msg)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in),optional :: generic(:)
integer :: i
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int16));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int32));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int64));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (real(kind=real32));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real64));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real128));     write(line(istart:),'("[",*(1pg0,1x))') generic
      !type is (real(kind=real256));     write(error_unit,'(1pg0)',advance='no') generic
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*));       write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
      class default
         stop 'unknown type in *print_generic*'
   end select
   istart=len_trim(line)+increment
   line=trim(line)//"]"//sep_local
end subroutine print_generic
!===================================================================================================================================
end subroutine msg_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
! EXTRACTED FROM OTHER MODULES
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
elemental real function cosd(angle_in_degrees)

! ident_59="@(#) M_pixel cosd(3f) cos(3f) with degrees as input instead of radians"

class(*),intent(in) :: angle_in_degrees
real                :: angle_in_degrees_local
   angle_in_degrees_local=anyscalar_to_double(angle_in_degrees)
   cosd=cos(angle_in_degrees_local*degrees_to_radians)
end function cosd
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function sind(angle_in_degrees)

! ident_60="@(#) M_pixel sind(3f) sin(3f) with degrees as input instead of radians"

class(*),intent(in)  :: angle_in_degrees
real                 :: angle_in_degrees_local
   angle_in_degrees_local=anyscalar_to_double(angle_in_degrees)
   sind=sin(angle_in_degrees_local*degrees_to_radians)
end function sind
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure elemental function anyscalar_to_real(valuein) result(r_out)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

! ident_5="@(#)M_pixel::anyscalar_to_real(3f): convert integer or real parameter of any kind to real"

class(*),intent(in) :: valuein
real                :: r_out
real,parameter      :: big=huge(0.0)
   select type(valuein)
   type is (integer(kind=int8));   r_out=real(valuein)
   type is (integer(kind=int16));  r_out=real(valuein)
   type is (integer(kind=int32));  r_out=real(valuein)
   type is (integer(kind=int64));  r_out=real(valuein)
   type is (real(kind=real32));    r_out=real(valuein)
   type is (real(kind=real64))
      !!if(valuein.gt.big)then
      !!   write(error_unit,*)'*anyscalar_to_real* value too large ',valuein
      !!endif
      r_out=real(valuein)
   type is (real(kind=real128))
      !!if(valuein.gt.big)then
      !!   write(error_unit,*)'*anyscalar_to_real* value too large ',valuein
      !!endif
      r_out=real(valuein)
   type is (logical);              r_out=merge(0.0d0,1.0d0,valuein)
   type is (character(len=*));     read(valuein,*) r_out
   !type is (real(kind=real128));  r_out=real(valuein)
   end select
end function anyscalar_to_real
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function invert_4x4(A) result(B)
   !! Performs a direct calculation of the inverse of a 4 x 4 matrix.
   integer,parameter            :: wp=kind(0.0)
   real(kind=wp), intent(in) :: A(4,4)   !! Matrix
   real(kind=wp)             :: B(4,4)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = &
     1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
      - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
      + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
      - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

   ! Calculate the inverse of the matrix
   B(1,1) = detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
   B(2,1) = detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
   B(3,1) = detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(4,1) = detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(1,2) = detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
   B(2,2) = detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
   B(3,2) = detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(4,2) = detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(1,3) = detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
   B(2,3) = detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
   B(3,3) = detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
   B(4,3) = detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
   B(1,4) = detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
   B(2,4) = detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
   B(3,4) = detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
   B(4,4) = detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
end function invert_4x4
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine journal(string)
character(len=*),intent(in) :: string
write(*,'(g0)')trim(string)
end subroutine journal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function i2s(ivalue) result(outstr)

! ident_61="@(#) M_pixel i2s(3fp) private function returns string given integer value"

integer,intent(in)           :: ivalue                         ! input value to convert to a string
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   write(string,'(g0)')ivalue
   outstr=trim(string)
end function i2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sort_shell_integers_hl(iarray)
! Copyright (C) 1989,1996 John S. Urban;  all rights reserved

! ident_62="@(#) M_sort sort_shell_integers_hl(3fp) sort integer array using Shell sort (high to low)"

integer,intent(inout)      :: iarray(:)  ! input/output array
integer                    :: n          ! number of elements in input array (iarray)
integer                    :: igap, i, j, k, jg
   n=size(iarray)
   igap=n
   INFINITE: do
      igap=igap/2
      if(igap.eq.0) exit INFINITE
      k=n-igap
      i=1
      INNER: do
         j=i
         INSIDE: do
            jg=j+igap
            if(iarray(j).ge.iarray(jg)) exit INSIDE
            call swapcoord(iarray(j),iarray(jg))
            j=j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i=i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_shell_integers_hl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure elemental function anyscalar_to_double(valuein) result(d_out)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

! ident_4="@(#)M_pixel::anyscalar_to_double(3f): convert integer or real parameter of any kind to doubleprecision"

class(*),intent(in)       :: valuein
doubleprecision           :: d_out
doubleprecision,parameter :: big=huge(0.0d0)
   select type(valuein)
   type is (integer(kind=int8));   d_out=dble(valuein)
   type is (integer(kind=int16));  d_out=dble(valuein)
   type is (integer(kind=int32));  d_out=dble(valuein)
   type is (integer(kind=int64));  d_out=dble(valuein)
   type is (real(kind=real32));    d_out=dble(valuein)
   type is (real(kind=real64));    d_out=dble(valuein)
   Type is (real(kind=real128))
      !!if(valuein.gt.big)then
      !!   write(error_unit,*)'*anyscalar_to_double* value too large ',valuein
      !!endif
      d_out=dble(valuein)
   type is (logical);              d_out=merge(0.0d0,1.0d0,valuein)
   type is (character(len=*));      read(valuein,*) d_out
   !type is (real(kind=real128))
   !   if(valuein.gt.big)then
   !      write(error_unit,*)'*anyscalar_to_double* value too large ',valuein
   !   endif
   !   d_out=dble(valuein)
   class default
     d_out=0.0d0
     !!stop '*M_pixel::anyscalar_to_double: unknown type'
   end select
end function anyscalar_to_double
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================

!>
!!##NAME
!!    HUE(3f) - [M_pixel:COLOR] converts color components from one color
!!              model to another
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine hue(modei,clr1i,clr2i,clr3i,modeo,clr1o,clr2o,clr3o,status)
!!
!!     character(len=*),intent(in) :: modei
!!     character(len=*),intent(in) :: modeo
!!     real,intent(in)             :: clr1i,clr2i,clr3i
!!     real,intent(out)            :: clr1o,clr2o,clr3o
!!     integer,intent(out)         :: status
!!
!!##DESCRIPTION
!!    Basic color models:
!!
!!     +----------------------------------------------------------+
!!     | valid values for modei and modeo as well as the          |
!!     | corresponding meanings for clr1*, clr2*, and clr3* are:  |
!!     +----------------------------------------------------------+
!!     |model| clr1         |         clr2      |         clr3    |
!!     |-----+--------------+-------------------+-----------------|
!!     |hls  |hue           |lightness          |saturation       |
!!     |-----+--------------+-------------------+-----------------|
!!     |hsl  |hue           |saturation         |lightness        |
!!     |-----+--------------+-------------------+-----------------|
!!     |hvs  |hue           |value              |saturation       |
!!     |-----+--------------+-------------------+-----------------|
!!     |hsv  |hue           |saturation         |value            |
!!     |-----+--------------+-------------------+-----------------|
!!     |rgb  |red           |green              |blue             |
!!     |-----+--------------+-------------------+-----------------|
!!     |cmy  |cyan          |magenta            |yellow           |
!!     |-----+--------------+-------------------+-----------------|
!!     |yiq  |gray scale)   |orange-blue        |purple-green     |
!!     |     |              |chrominance        |chrominance      |
!!     +----------------------------------------------------------+
!!
!!    *  lightness, value, saturation, red, green, blue, cyan, magenta, and
!!       yellow range from 0 to 100,
!!
!!       * hue ranges from 0 to 360 degrees,
!!       * y ranges from 0 to 100,
!!       * i ranges from -60 to 60,
!!       * q ranges from -52 to 52
!!
!!    The STATUS variable can signal the following conditions:
!!
!!      -1   modei = modeo, so no substantial conversion was done,
!!       1   one of the input color values was outside the allowable range,
!!       2   modei was invalid
!!       3   modeo was invalid
!!
!!##EXAMPLE
!!
!! Sample program
!!
!!     program demo_hue
!!     use M_pixel, only : hue
!!     implicit none
!!        !                      NAME       RGB(0-255)            HLS(0-100)
!!        call check_name('hls','red',      [ 100, 0,   0   ],[ 0,   50,  100 ])
!!        call check_name('hls','orange',   [ 100, 65,  0   ],[ 39,  50,  100 ])
!!        call check_name('hls','yellow',   [ 100, 100, 0   ],[ 60,  50,  100 ])
!!        call check_name('hls','green',    [ 0,   100, 0   ],[ 120, 50,  100 ])
!!        call check_name('hls','cyan',     [ 0,   100, 100 ],[ 180, 50,  100 ])
!!        call check_name('hls','blue',     [ 0,   0,   100 ],[ 240, 50,  100 ])
!!        call check_name('hls','magenta',  [ 100, 0,   100 ],[ 300, 50,  100 ])
!!        call check_name('hls','black',    [ 0,   0,   0   ],[ 0,   0,   0   ])
!!        call check_name('hls','white',    [ 100, 100, 100 ],[ 0,   100, 0   ])
!!        call check_name('hsv','black',    [ 0,   0,   0   ],[ 0,   0,   0   ])
!!        !                      NAME        RGB(0-255)            HSV(0-100)
!!        call check_name('hsv','gray50',   [ 50,  50,  50  ],[ 0,   0,   50  ])
!!        call check_name('hsv','silver',   [ 75,  75,  75  ],[ 0,   0,   75  ])
!!        call check_name('hsv','white',    [ 100, 100, 100 ],[ 0,   0,   100 ])
!!        call check_name('hsv','red4',     [ 55,  0,   0   ],[ 0,   100, 55  ])
!!        call check_name('hsv','red',      [ 100, 0,   0   ],[ 0,   100, 100 ])
!!        call check_name('hsv','olive',    [ 50,  50,  0   ],[ 60,  100, 50  ])
!!        call check_name('hsv','yellow',   [ 100, 100, 0   ],[ 60,  100, 100 ])
!!        call check_name('hsv','green',    [ 0,   100, 0   ],[ 120, 100, 100 ])
!!        call check_name('hsv','lime',     [ 0,   100, 0   ],[ 120, 100, 100 ])
!!        call check_name('hsv','teal',     [ 0,   50,  50  ],[ 180, 100, 50  ])
!!        call check_name('hsv','cyan',     [ 0,   100, 100 ],[ 180, 100, 100 ])
!!        call check_name('hsv','navy',     [ 0,   0,   50  ],[ 240, 100, 50  ])
!!        call check_name('hsv','blue',     [ 0,   0,   100 ],[ 240, 100, 100 ])
!!        call check_name('hsv','purple',   [ 63,  13,  94  ],[ 277, 87,  94  ])
!!        call check_name('hsv','magenta4', [ 55,  0,   55  ],[ 300, 100, 55  ])
!!        call check_name('hsv','magenta',  [ 100, 0,   100 ],[ 300, 100, 100 ])
!!        call check_name('hsv','maroon',   [ 69,  19,  38  ],[ 338, 73,  69  ])
!!     contains
!!     subroutine check_name(modelout,name,rgb,other)
!!     ! given a color convert to MODELOUT and compare to expected values
!!     character(len=*),intent(in)   :: name
!!     integer,intent(in)            :: rgb(3), other(3)
!!     character(len=*),intent(in)   :: modelout
!!        real                       :: val1,val2,val3
!!        integer                    :: status
!!        ! convert RGB values to MODELOUT values
!!        call hue('rgb',REAL(rgb(1)),REAL(rgb(2)),REAL(rgb(3)), &
!!        & modelout,val1,val2,val3,status)
!!           write(*,*)'COLOR '//trim(name)
!!           write(*,*)'EXPECTED '//modelout//' ====>',other
!!           write(*,*)'RETURNED '//modelout//' ====>', &
!!           & int([val1+0.5,val2+0.5,val3+0.5])
!!           write(*,*)'STATUS ==========>',status
!!     end subroutine check_name
!!     end program demo_hue
!!
!!    Results:
!!
!!     COLOR red
!!     EXPECTED hls ====>           0          50         100
!!     RETURNED hls ====>           0          50         100
!!     STATUS ==========>           0
!!     COLOR orange
!!     EXPECTED hls ====>          39          50         100
!!     RETURNED hls ====>          39          50         100
!!     STATUS ==========>           0
!!     COLOR yellow
!!     EXPECTED hls ====>          60          50         100
!!     RETURNED hls ====>          60          50         100
!!     STATUS ==========>           0
!!     COLOR green
!!     EXPECTED hls ====>         120          50         100
!!     RETURNED hls ====>         120          50         100
!!     STATUS ==========>           0
!!     COLOR cyan
!!     EXPECTED hls ====>         180          50         100
!!     RETURNED hls ====>         180          50         100
!!     STATUS ==========>           0
!!     COLOR blue
!!     EXPECTED hls ====>         240          50         100
!!     RETURNED hls ====>         240          50         100
!!     STATUS ==========>           0
!!     COLOR magenta
!!     EXPECTED hls ====>         300          50         100
!!     RETURNED hls ====>         300          50         100
!!     STATUS ==========>           0
!!     COLOR black
!!     EXPECTED hls ====>           0           0           0
!!     RETURNED hls ====>           0           0           0
!!     STATUS ==========>           0
!!     COLOR white
!!     EXPECTED hls ====>           0         100           0
!!     RETURNED hls ====>           0         100           0
!!     STATUS ==========>           0
!!     COLOR black
!!     EXPECTED hsv ====>           0           0           0
!!     RETURNED hsv ====>           0           0           0
!!     STATUS ==========>           0
!!     COLOR gray50
!!     EXPECTED hsv ====>           0           0          50
!!     RETURNED hsv ====>           0           0          50
!!     STATUS ==========>           0
!!     COLOR silver
!!     EXPECTED hsv ====>           0           0          75
!!     RETURNED hsv ====>           0           0          75
!!     STATUS ==========>           0
!!     COLOR white
!!     EXPECTED hsv ====>           0           0         100
!!     RETURNED hsv ====>           0           0         100
!!     STATUS ==========>           0
!!     COLOR red4
!!     EXPECTED hsv ====>           0         100          55
!!     RETURNED hsv ====>           0         100          55
!!     STATUS ==========>           0
!!     COLOR red
!!     EXPECTED hsv ====>           0         100         100
!!     RETURNED hsv ====>           0         100         100
!!     STATUS ==========>           0
!!     COLOR olive
!!     EXPECTED hsv ====>          60         100          50
!!     RETURNED hsv ====>          60         100          50
!!     STATUS ==========>           0
!!     COLOR yellow
!!     EXPECTED hsv ====>          60         100         100
!!     RETURNED hsv ====>          60         100         100
!!     STATUS ==========>           0
!!     COLOR green
!!     EXPECTED hsv ====>         120         100         100
!!     RETURNED hsv ====>         120         100         100
!!     STATUS ==========>           0
!!     COLOR lime
!!     EXPECTED hsv ====>         120         100         100
!!     RETURNED hsv ====>         120         100         100
!!     STATUS ==========>           0
!!     COLOR teal
!!     EXPECTED hsv ====>         180         100          50
!!     RETURNED hsv ====>         180         100          50
!!     STATUS ==========>           0
!!     COLOR cyan
!!     EXPECTED hsv ====>         180         100         100
!!     RETURNED hsv ====>         180         100         100
!!     STATUS ==========>           0
!!     COLOR navy
!!     EXPECTED hsv ====>         240         100          50
!!     RETURNED hsv ====>         240         100          50
!!     STATUS ==========>           0
!!     COLOR blue
!!     EXPECTED hsv ====>         240         100         100
!!     RETURNED hsv ====>         240         100         100
!!     STATUS ==========>           0
!!     COLOR purple
!!     EXPECTED hsv ====>         277          87          94
!!     RETURNED hsv ====>         277          86          94
!!     STATUS ==========>           0
!!     COLOR magenta4
!!     EXPECTED hsv ====>         300         100          55
!!     RETURNED hsv ====>         300         100          55
!!     STATUS ==========>           0
!!     COLOR magenta
!!     EXPECTED hsv ====>         300         100         100
!!     RETURNED hsv ====>         300         100         100
!!     STATUS ==========>           0
!!     COLOR maroon
!!     EXPECTED hsv ====>         338          73          69
!!     RETURNED hsv ====>         337          72          69
!!     STATUS ==========>           0
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine hue(modei,clr1i,clr2i,clr3i,modeo,clr1o,clr2o,clr3o,status)
character(len=*),intent(in) :: modei
real,intent(in)             :: clr1i,clr2i,clr3i
character(len=*),intent(in) :: modeo
real,intent(out)            :: clr1o,clr2o,clr3o
integer,intent(out)         :: status

character(len=3)            :: input_color_model,output_color_model
real                        :: c1, c2, c3, r, g, b
!-----------------------------------------------------------------------------------------------------------------------------------
!-- initialize the status flag.
   status=0
!-- set the output colors equal to invalid values
   clr1o=-99999.0
   clr2o=-99999.0
   clr3o=-99999.0
!-- ensure that the input character strings are lowercase
   input_color_model=lower(modei)
   output_color_model=lower(modeo)
!-----------------------------------------------------------------------------------------------------------------------------------
!-- check for a trivial instance where the input and output model names are the same
   if(input_color_model .eq. output_color_model) then
      clr1o=clr1i
      clr2o=clr2i
      clr3o=clr3i
      status=-1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!-- check for a transpose of terms, another trivial instance.
   SELECT CASE (input_color_model)
   CASE ('hls','hsl','hvs','hsv')
      if( input_color_model.eq.'hls' .and. output_color_model.eq.'hsl'   &
    & .or.input_color_model.eq.'hsl' .and. output_color_model.eq.'hls'   &
    & .or.input_color_model.eq.'hvs' .and. output_color_model.eq.'hsv'   &
    & .or.input_color_model.eq.'hsv' .and. output_color_model.eq.'hvs') then
         clr1o=clr1i
         clr2o=clr3i
         clr3o=clr2i
         status=-1
         return
      endif
   END SELECT
!-----------------------------------------------------------------------------------------------------------------------------------
!-- assign new variables so that the input arguments can't possibly be changed by subsequent procedures.
   c1=clr1i
   c2=clr2i
   c3=clr3i
!-----------------------------------------------------------------------------------------------------------------------------------
!-- first, convert input values to rgb values.
   SELECT CASE (input_color_model)
   CASE ('hls'); call hlsrgb(c1,c2,c3,r,g,b,status)
   CASE ('hvs'); call hvsrgb(c1,c2,c3,r,g,b,status)
   CASE ('hsl'); call hlsrgb(c1,c3,c2,r,g,b,status)
   CASE ('hsv'); call hvsrgb(c1,c3,c2,r,g,b,status)
   CASE ('cmy'); call cmyrgb(c1,c2,c3,r,g,b,status)
   CASE ('yiq'); call yiqrgb(c1,c2,c3,r,g,b,status)
   CASE ('rgb'); r=c1;g=c2;b=c3
   CASE DEFAULT ! unknown input model name
      status=2
      return
   END SELECT
!-----------------------------------------------------------------------------------------------------------------------------------
   if(status .ne. 0 )then
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!-- then convert from RGB to the desired output values
!
   SELECT CASE (output_color_model)
   CASE ('hls'); call rgbhls(r,g,b,clr1o,clr2o,clr3o,status)
   CASE ('hsl'); call rgbhls(r,g,b,clr1o,clr3o,clr2o,status)
   CASE ('hvs'); call rgbhvs(r,g,b,clr1o,clr2o,clr3o,status)
   CASE ('hsv'); call rgbhvs(r,g,b,clr1o,clr3o,clr2o,status)
   CASE ('cmy'); call rgbcmy(r,g,b,clr1o,clr2o,clr3o,status)
   CASE ('rgb'); clr1o=r; clr2o=g; clr3o=b
   CASE ('yiq'); call rgbyiq(r,g,b,clr1o,clr2o,clr3o,status)
   CASE DEFAULT ! unknown output model name
      status=3
      return
   END SELECT
!-----------------------------------------------------------------------------------------------------------------------------------
   if(status .ne. 0 )then
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine hue
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine rgbhls(r0,g0,b0,h,l,s,status)

!     given  : r, g, b each as a value of 0 to 100
!     desired: h as a value of 0 to 360 degrees.
!     .        l and s each as a value of 0 to 100
!
real    :: r0,g0,b0
real    :: r,g,b,h,l,s
real    :: clrmax,clrmin,clrdel,clrsum,rr,gg,bb
integer :: status
   if(r0 .lt. 0.0 .or. r0 .gt. 100.0 ) status = 1 !---- passive check for valid range of values.
   if(g0 .lt. 0.0 .or. g0 .gt. 100.0 ) status = 1 !---- passive check for valid range of values.
   if(b0 .lt. 0.0 .or. b0 .gt. 100.0 ) status = 1 !---- passive check for valid range of values.
   r=r0/100.0
   g=g0/100.0
   b=b0/100.0
   clrmax=amax1(r,g,b)
   clrmin=amin1(r,g,b)
   clrdel=clrmax-clrmin
   clrsum=clrmax+clrmin
   l=clrsum/2.0
   if(clrdel.ne.0.0 ) then
      rr=(clrmax-r)/clrdel
      gg=(clrmax-g)/clrdel
      bb=(clrmax-b)/clrdel
      if(l.le.0.5) then
         s=clrdel/clrsum
      else
         s=clrdel/(2.0-clrsum)
      endif
      if(r.eq.clrmax) then
         h=bb-gg
      else if(g.eq.clrmax) then
         h=2.0 +rr-bb
      else if(b.eq.clrmax) then
         h=4.0 +gg-rr
      endif
      h=h*60.0
      if(h.lt.0.0 ) then
         h=h+360.0
      endif
   else
      s=0.0
      h=0.0
   endif
   l=l*100.0
   s=s*100.0
   if(h .lt. 0.0 ) h = 0.0   !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   if(h .gt. 360.0 ) h = 360.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   if(l .lt. 0.0 ) l=0.0
   if(l .gt. 100.0 ) l = 100.0
   if(s .lt. 0.0 ) s=0.0
   if(s .gt. 100.0 ) s = 100.0
end subroutine rgbhls
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine rgbhvs(r0,g0,b0,h,v,s,status)

! ident_63="@(#) M_pixel rgbhvs(3fp) given red green blue calculate hue saturation value components"

!---- this procedure calculates a hue, saturation, value equivalent for a
!     color given in red, green, & blue components.
!     given  : r, g, b each as a value of 0 to 100.
!     desired: h as a value of 0 to 360 degrees.
!     .        s and v each as a value of 0 to 100.
!
real,intent(in)  :: r0,g0,b0
real,intent(out) :: h,v,s
integer          :: status
real             :: r,g,b
real             :: clrmax,clrmin,clrdel,rr,gg,bb
   if(r0 .lt. 0.0 .or. r0 .gt. 100.0 ) status = 1 !---- check for valid range of values.
   if(g0 .lt. 0.0 .or. g0 .gt. 100.0 ) status = 1 !---- check for valid range of values.
   if(b0 .lt. 0.0 .or. b0 .gt. 100.0 ) status = 1 !---- check for valid range of values.
   r=r0
   g=g0
   b=b0
   r=r/100.0
   g=g/100.0
   b=b/100.0
   clrmax=amax1(r,g,b)
   clrmin=amin1(r,g,b)
   clrdel=clrmax-clrmin
   v=clrmax
   if(clrmax.ne.0.0 )then
         s=clrdel/clrmax
   else
         s=0.0
   endif
   if(s.ne.0.0 )then
         rr=(clrmax-r)/clrdel
         gg=(clrmax-g)/clrdel
         bb=(clrmax-b)/clrdel
         if(r.eq.clrmax)then
            h=bb-gg
         else if(g.eq.clrmax) then
            h=2.0 +rr-bb
         else if(b.eq.clrmax) then
            h=4.0 +gg-rr
         endif
         h=h*60.0
         if(h.lt.0.0 ) then
            h=h+360.0
         endif
   endif
   v=v*100.0
   s=s*100.0
   if(h .gt. 360.0 ) h = 360.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   if(h .lt. 0.0 ) h =   0.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   if(v .gt. 100.0 ) v = 100.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   if(v .lt. 0.0 ) v =   0.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   if(s .gt. 100.0 ) s = 100.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   if(s .lt. 0.0 ) s =   0.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
end subroutine rgbhvs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmyrgb(c,m,y,r,g,b,status)

! ident_64="@(#) M_pixel cmyrgb(3fp) given cyan magenta yellow calculate red green blue components"

! given  : r, g, b each as a value of 0 to 100
! desired: c, m, y each as a value of 0 to 100
real,intent(in)   :: c,m,y
real,intent(out)  :: r,g,b
integer           :: status
   if(c .lt. 0.0 .or. c .gt. 100.0 ) status = 1 !---- passively check for valid range of values.
   if(m .lt. 0.0 .or. m .gt. 100.0 ) status = 1 !---- passively check for valid range of values.
   if(y .lt. 0.0 .or. y .gt. 100.0 ) status = 1 !---- passively check for valid range of values.
   r= 100.0 - c
   g= 100.0 - m
   b= 100.0 - y
end subroutine cmyrgb
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine rgbcmy(r,g,b,c,m,y,status)

! ident_65="@(#) M_pixel rgbcmy(3fp) given red green blue calculate cyan magenta yellow components"

!     given  : r, g, b each as a value of 0 to 100
!     desired: c, m, y each as a value of 0 to 100
real,intent(in)  :: r,g,b
real,intent(out) :: c,m,y
integer          :: status
   if(r .lt. 0.0 .or. r .gt. 100.0 ) status = 1 !---- check for valid range of values.
   if(g .lt. 0.0 .or. g .gt. 100.0 ) status = 1 !---- check for valid range of values.
   if(b .lt. 0.0 .or. b .gt. 100.0 ) status = 1 !---- check for valid range of values.
   c = 100.0 - r
   m = 100.0 - g
   y = 100.0 - b

end subroutine rgbcmy
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine rgbmono(rr,rg,rb,ri,status)

! ident_66="@(#) M_pixel rgbmono(3f) convert RGB colors to a reasonable grayscale"

! monochrome devices that support intensity can have intensity calculated from the specified Red, Green, Blue
! intensities as 0.30*R + 0.59*G + 0.11*B, as in US color television systems, NTSC encoding.
! Note that most devices do not have an infinite range of monochrome intensities available.

real,intent(in)      :: rr,rg,rb                ! red, green, blue, & intensity range from 0 to 100
real,intent(out)     :: ri
integer,intent(out)  :: status
   status=0
   if(rr .lt. 0.0 .or. rr .gt. 100.0 ) status = 1 !---- passive check for valid range of values.
   if(rg .lt. 0.0 .or. rg .gt. 100.0 ) status = 1 !---- passive check for valid range of values.
   if(rb .lt. 0.0 .or. rb .gt. 100.0 ) status = 1 !---- passive check for valid range of values.
   ri = 0.30*rr + 0.59*rg + 0.11*rb
end subroutine rgbmono
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
real function rgbval(clr1,clr2,h)

! ident_67="@(#) M_pixel rgbval(3fp) ensure a value is in the appropriate range and quadrant"

real    :: clr1,clr2
real    :: h
real    :: h2
   h2=h
   do
      if(h2.gt.360.0 ) then
         h2=h2-360.0
         cycle
      endif
      exit
   enddo

   do
      if( h2 .lt. 0.0 ) then
         h2=h2+360.0
         cycle
      endif
      exit
   enddo

   if(h2.lt.60.0 ) then
      rgbval=clr1+(clr2-clr1)*h2/60.0
   else if(h2.lt.180.0) then
      rgbval=clr2
   else if(h2.lt.240.0) then
      rgbval=clr1+(clr2-clr1)*(240.0-h2)/60.0
   else
      rgbval=clr1
   endif

end function rgbval
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine hlsrgb(H,L,S,R,G,B,status)

! ident_68="@(#) M_pixel hlsrgb(3fp) convert HLS(hue lightness saturation) values to RGB components"

!     given  : hue as a value of 0 to 360 degrees.
!     .        lightness and saturation each as a value of 0 to 100.
!     desired: r, g, and b each as a value of 0 to 100.
!
real,intent(in)   :: H,L,S
real,intent(out)  :: R,G,B
integer           :: status
real              :: hue,lightness,saturation
real              :: clr1,clr2
   if(h .lt. 0.0 .or. h .gt.360.0 ) status = 1 ! passively report on bad input values
   if(l .lt. 0.0 .or. l .gt.100.0 ) status = 1 ! passively report on bad input values
   if(s .lt. 0.0 .or. s .gt.100.0 ) status = 1 ! passively report on bad input values
   hue =           H
   lightness =     L/100.0
   saturation =    S/100.0
   if( saturation .eq. 0.0 ) then
      R = lightness
      G = lightness
      B = lightness
   endif
   if(lightness .le. 0.50) then
      clr2= lightness*( 1.0 + saturation )
   else
      clr2= lightness + saturation - lightness * saturation
   endif
   clr1= 2.0 * lightness - clr2
   R = rgbval(clr1,clr2,hue+120.0)  *100.0
   G = rgbval(clr1,clr2,hue)        *100.0
   B = rgbval(clr1,clr2,hue-120.0)  *100.0
end subroutine hlsrgb
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine hvsrgb(h,v,s,r,g,b,status)

! ident_69="@(#) M_pixel hvsrgb(3fp) given hue saturation value calculate red green blue components"

!     given  : hue as value of 0 to 360 degrees.
!     .        saturation and value each as a value of 0 to 100.
!     desired: r, g, and b as a value of 0 to 100.
real,intent(in)    :: h,v,s
real,intent(out)   :: r,g,b
integer            :: status
real               :: hue,value,saturation
integer            :: ifloor
real               :: f,p,q,t
   if(h .lt. 0.0 .or. h .gt.360.0 ) status = 1 ! passively report on bad input values
   if(v .lt. 0.0 .or. v .gt.100.0 ) status = 1 ! passively report on bad input values
   if(s .lt. 0.0 .or. s .gt.100.0 ) status = 1 ! passively report on bad input values
   hue=h
   value=v/100.0
   saturation=s/100.0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(saturation.eq.0.0) then
      r=value
      g=value
      b=value
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(hue.eq.360.0) then
      hue=0.0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   hue=hue/60.0
   ifloor=int(hue)
   f=hue-ifloor
   p=value*(1.0-saturation)
   q=value*(1.0-(saturation*f))
   t=value*(1.0-(saturation*(1-f)))
   SELECT CASE (ifloor)
   CASE (0) ;r=value; g=t; b=p
   CASE (1) ;r=q; g=value; b=p
   CASE (2) ;r=p; g=value; b=t
   CASE (3) ;r=p; g=q; b=value
   CASE (4) ;r=t; g=p; b=value
   CASE (5) ;r=value; g=p; b=q
   CASE DEFAULT
   END SELECT
   r=r*100.0
   g=g*100.0
   b=b*100.0
end subroutine hvsrgb
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine yiqrgb(y,i,q,r,g,b,status)

! ident_70="@(#) M_pixel yiqrgb(3fp) convert luma orange-blue chrominance purple-green chrominance to RGB"

real,intent(in)  :: y,i,q
real,intent(out) :: r,g,b
integer          :: status
!
!----    i don't believe that this is an exhaustive test of value ranges
!        for yiq. for example yiq=(100.0,60.0,52.0) when converted to
!        rgb produces values greater than 100!?
!
      if(i .lt. -60.0 .or. i .gt.  60.0) status = 1
      if(q .lt. -53.0 .or. q .gt.  53.0) status = 1

      r = 1.0 * y + 0.956 * i + 0.621 * q
      g = 1.0 * y - 0.272 * i - 0.647 * q
      b = 1.0 * y - 1.106 * i + 1.703 * q
      !r= 1.0 *y + 0.94826224*i + 0.62401264*q
      !g= 1.0 *y - 0.27606635*i - 0.63981043*q
      !b= 1.0 *y - 1.1054502 *i + 1.7298578 *q
!
!-- If outside the valid range of values, truncate to allow for reasonable roundoff and then retest.
!   This should pass values essentially 0 or 100, but fail others.
!   The above formula for rgb from yiq can give answers slightly less than 0 and slightly greater than 100.0
!   The truncation should fix this.
!   The retest should then catch the instances such as yiq=(100.0,60.0,52.0) as mentioned earlier.

   r=min(100.0,max(0.0,r))
   g=min(100.0,max(0.0,g))
   b=min(100.0,max(0.0,b))

end subroutine yiqrgb
!=============================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine rgbyiq(r,g,b,y,i,q,status)

! ident_71="@(#) M_pixel rgbyiq(3fp) convert RGB to luma orange-blue chrominance purple-green chrominance"

real,intent(in)  :: r,g,b
real,intent(out) :: y,i,q
integer          :: status
   if(r.lt.0.0 .or. r.gt.100.0) status=1
   if(g.lt.0.0 .or. g.gt.100.0) status=1
   if(b.lt.0.0 .or. b.gt.100.0) status=1

   y= 0.299 * r + 0.587 * g + 0.114 * b
   i= 0.596 * r - 0.274 * g - 0.322 * b
   q= 0.211 * r - 0.523 * g + 0.312 * b

!-- Eliminate any roundoff that exceeds the limits.
   if(i .lt. -59.57 ) i = -59.57
   if(i .gt.  59.57 ) i =  59.57
   if(q .lt. -52.26 ) q = -52.26
   if(q .gt.  52.26 ) q =  52.26
end subroutine rgbyiq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     closest_color_name(3f) - [M_pixel:COLOR] returns the closest name
!!     for the given RGB values.
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine closest_color_name(r,g,b,closestname)
!!
!!     real,intent(in)               :: r,g,b
!!     character(len=20),intent(out) :: closestname
!!
!!##DESCRIPTION
!!     closest_color_name() returns the closest name for the given RGB
!!     values. Most X11 Windows color names are supported.
!!
!!##OPTIONS
!!     R   red component, range of 0 to 100
!!     G   green component, range of 0 to 100
!!     B   blue component, range of 0 to 100
!!
!!##RETURNS
!!     CLOSESTNAME   name of color found closest to given RGB value</li>
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!        program demo_closest_color_name
!!        use M_pixel, only : closest_color_name
!!        implicit none
!!        character(len=100) :: string ! at least 20 characters
!!           string=' '
!!
!!           call closest_color_name(100.0,  0.0,  0.0,string)
!!           write(*,*)trim(string)
!!
!!           call closest_color_name(  0.0,100.0,  0.0,string)
!!           write(*,*)trim(string)
!!
!!           call closest_color_name(  0.0,  0.0,100.0,string)
!!           write(*,*)trim(string)
!!
!!        end program demo_closest_color_name
!!
!!    Results:
!!
!!        red
!!        green
!!        blue
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE closest_color_name(r,g,b,closestname)

! ident_72="@(#) M_pixel closest_color_name(3f) given RGB values try to find closest named color"

real,intent(in)               :: r,g,b
character(len=*),intent(out) :: closestname
real                          :: rn,gn,bn
real                          :: distance, minimum_distance
character(len=20)             :: echoname
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   minimum_distance=1000.0
   closestname='Unknown'
   INFINITE: do i=1,1000
      call color_name2rgb(i2s(i),rn,gn,bn,echoname)       ! get next color
      if(echoname.eq.'Unknown') exit INFINITE
      distance=sqrt( (r-rn)**2 + (g-gn)**2 + (b-bn)**2 )
      if(distance.lt.minimum_distance)then
         closestname=echoname
         minimum_distance=min(minimum_distance,distance)
      endif
   enddo INFINITE
end SUBROUTINE closest_color_name
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     COLOR_NAME2RGB(3f) - [M_pixel:COLOR] returns the RGB values in the
!!                          range 0 to 100 for a given known color name.
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine color_name2rgb(name,r,g,b,echoname)
!!
!!     character(len=20),intent(in)   :: name
!!     real,intent(out)               :: r,g,b
!!     character(len=20),intent(out)  :: echoname
!!
!!##DESCRIPTION
!!     COLOR_NAME2RGB() returns the RGB values in the range 0 to 100
!!     for a given known color name. Most X11 Windows color names are
!!     supported. If the name is not found, ECHONAME is set to "Unknown".
!!
!!##EXAMPLE
!!
!!    A sample program:
!!
!!     program demo_color_name2rgb
!!     use M_pixel, only : hue, color_name2rgb
!!     implicit none
!!     !
!!     ! list colors known to colorname2rgb(3f) & corresponding RGB values
!!     !
!!     character(len=20) :: name
!!     character(len=20) :: echoname
!!     real              :: red,green,blue
!!     integer           :: i
!!     TRYALL: do i=1,10000
!!        ! weird little thing where the color names have aliases
!!        ! that are numeric strings
!!        write(name,'(i0)')i
!!        ! get the RGB values and English name of the color
!!        call color_name2rgb(name,red,green,blue,echoname)
!!        ! the last color name is "Unknown" so the loop should exit
!!        if(echoname.eq.'Unknown')exit TRYALL
!!        ! display the English name and RGB values for the name
!!        write(*,*)echoname,int([red,green,blue])
!!     enddo TRYALL
!!     !write(*,*)'Number of colors found is ',i-1
!!     end program demo_color_name2rgb
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine color_name2rgb(name,r,g,b,echoname)

! ident_73="@(#) M_pixel color_name2rgb(3f) given a color name return rgb color values in range 0 to 100"

character(len=*),intent(in)            :: name
real,intent(out)                       :: r,g,b
character(len=*),intent(out),optional  :: echoname
character(len=20)                      :: newname
!-----------------------------------------------------------------------------------------------------------------------------------
! returns name in ECHONAME; which is usually not useful unless NAME represents an integer string.
! Note that an integer converted to a string can be used to go sequentially thru the names until NEWNAME="Unknown"
! Color names can generally be listed using showrgb(1) in GNU/1 and Unix environments that support X11 Windows:

! A structure would normally be used for the data; but a large SELECT is easy to maintain.
! a numeric name is an alias for each color to facilitate going thru them sequentially since they are not an array.
   SELECT case(TRIM(lower(name)))

   CASE("1",   "snow")                  ;  newname="snow"                  ;  r=255  ;  g=250  ;  b=250
   CASE("2",   "ghostwhite")            ;  newname="ghostwhite"            ;  r=248  ;  g=248  ;  b=255
   CASE("3",   "whitesmoke")            ;  newname="whitesmoke"            ;  r=245  ;  g=245  ;  b=245
   CASE("4",   "gainsboro")             ;  newname="gainsboro"             ;  r=220  ;  g=220  ;  b=220
   CASE("5",   "floralwhite")           ;  newname="floralwhite"           ;  r=255  ;  g=250  ;  b=240
   CASE("6",   "oldlace")               ;  newname="oldlace"               ;  r=253  ;  g=245  ;  b=230
   CASE("7",   "linen")                 ;  newname="linen"                 ;  r=250  ;  g=240  ;  b=230
   CASE("8",   "antiquewhite")          ;  newname="antiquewhite"          ;  r=250  ;  g=235  ;  b=215
   CASE("9",   "papayawhip")            ;  newname="papayawhip"            ;  r=255  ;  g=239  ;  b=213
   CASE("10",  "blanchedalmond")        ;  newname="blanchedalmond"        ;  r=255  ;  g=235  ;  b=205
   CASE("11",  "bisque")                ;  newname="bisque"                ;  r=255  ;  g=228  ;  b=196
   CASE("12",  "peachpuff")             ;  newname="peachpuff"             ;  r=255  ;  g=218  ;  b=185
   CASE("13",  "navajowhite")           ;  newname="navajowhite"           ;  r=255  ;  g=222  ;  b=173
   CASE("14",  "moccasin")              ;  newname="moccasin"              ;  r=255  ;  g=228  ;  b=181
   CASE("15",  "cornsilk")              ;  newname="cornsilk"              ;  r=255  ;  g=248  ;  b=220
   CASE("16",  "ivory")                 ;  newname="ivory"                 ;  r=255  ;  g=255  ;  b=240
   CASE("17",  "lemonchiffon")          ;  newname="lemonchiffon"          ;  r=255  ;  g=250  ;  b=205
   CASE("18",  "seashell")              ;  newname="seashell"              ;  r=255  ;  g=245  ;  b=238
   CASE("19",  "honeydew")              ;  newname="honeydew"              ;  r=240  ;  g=255  ;  b=240
   CASE("20",  "mintcream")             ;  newname="mintcream"             ;  r=245  ;  g=255  ;  b=250
   CASE("21",  "azure")                 ;  newname="azure"                 ;  r=240  ;  g=255  ;  b=255
   CASE("22",  "aliceblue")             ;  newname="aliceblue"             ;  r=240  ;  g=248  ;  b=255
   CASE("23",  "lavender")              ;  newname="lavender"              ;  r=230  ;  g=230  ;  b=250
   CASE("24",  "lavenderblush")         ;  newname="lavenderblush"         ;  r=255  ;  g=240  ;  b=245
   CASE("25",  "mistyrose")             ;  newname="mistyrose"             ;  r=255  ;  g=228  ;  b=225
   CASE("26",  "white")                 ;  newname="white"                 ;  r=255  ;  g=255  ;  b=255
   CASE("27",  "black")                 ;  newname="black"                 ;  r=0    ;  g=0    ;  b=0
   CASE("28",  "darkslategray")         ;  newname="darkslategray"         ;  r=47   ;  g=79   ;  b=79
   CASE("29",  "dimgray")               ;  newname="dimgray"               ;  r=105  ;  g=105  ;  b=105
   CASE("30",  "slategray")             ;  newname="slategray"             ;  r=112  ;  g=128  ;  b=144
   CASE("31",  "lightslategray")        ;  newname="lightslategray"        ;  r=119  ;  g=136  ;  b=153
   CASE("32",  "gray")                  ;  newname="gray"                  ;  r=190  ;  g=190  ;  b=190
   CASE("33",  "lightgray")             ;  newname="lightgray"             ;  r=211  ;  g=211  ;  b=211
   CASE("34",  "midnightblue")          ;  newname="midnightblue"          ;  r=25   ;  g=25   ;  b=112
   CASE("35",  "navy")                  ;  newname="navy"                  ;  r=0    ;  g=0    ;  b=128
   CASE("36",  "navyblue")              ;  newname="navyblue"              ;  r=0    ;  g=0    ;  b=128
   CASE("37",  "cornflowerblue")        ;  newname="cornflowerblue"        ;  r=100  ;  g=149  ;  b=237
   CASE("38",  "darkslateblue")         ;  newname="darkslateblue"         ;  r=72   ;  g=61   ;  b=139
   CASE("39",  "slateblue")             ;  newname="slateblue"             ;  r=106  ;  g=90   ;  b=205
   CASE("40",  "mediumslateblue")       ;  newname="mediumslateblue"       ;  r=123  ;  g=104  ;  b=238
   CASE("41",  "lightslateblue")        ;  newname="lightslateblue"        ;  r=132  ;  g=112  ;  b=255
   CASE("42",  "mediumblue")            ;  newname="mediumblue"            ;  r=0    ;  g=0    ;  b=205
   CASE("43",  "royalblue")             ;  newname="royalblue"             ;  r=65   ;  g=105  ;  b=225
   CASE("44",  "blue")                  ;  newname="blue"                  ;  r=0    ;  g=0    ;  b=255
   CASE("45",  "dodgerblue")            ;  newname="dodgerblue"            ;  r=30   ;  g=144  ;  b=255
   CASE("46",  "deepskyblue")           ;  newname="deepskyblue"           ;  r=0    ;  g=191  ;  b=255
   CASE("47",  "skyblue")               ;  newname="skyblue"               ;  r=135  ;  g=206  ;  b=235
   CASE("48",  "lightskyblue")          ;  newname="lightskyblue"          ;  r=135  ;  g=206  ;  b=250
   CASE("49",  "steelblue")             ;  newname="steelblue"             ;  r=70   ;  g=130  ;  b=180
   CASE("50",  "lightsteelblue")        ;  newname="lightsteelblue"        ;  r=176  ;  g=196  ;  b=222
   CASE("51",  "lightblue")             ;  newname="lightblue"             ;  r=173  ;  g=216  ;  b=230
   CASE("52",  "powderblue")            ;  newname="powderblue"            ;  r=176  ;  g=224  ;  b=230
   CASE("53",  "paleturquoise")         ;  newname="paleturquoise"         ;  r=175  ;  g=238  ;  b=238
   CASE("54",  "darkturquoise")         ;  newname="darkturquoise"         ;  r=0    ;  g=206  ;  b=209
   CASE("55",  "mediumturquoise")       ;  newname="mediumturquoise"       ;  r=72   ;  g=209  ;  b=204
   CASE("56",  "turquoise")             ;  newname="turquoise"             ;  r=64   ;  g=224  ;  b=208
   CASE("57",  "cyan")                  ;  newname="cyan"                  ;  r=0    ;  g=255  ;  b=255
   CASE("58",  "lightcyan")             ;  newname="lightcyan"             ;  r=224  ;  g=255  ;  b=255
   CASE("59",  "cadetblue")             ;  newname="cadetblue"             ;  r=95   ;  g=158  ;  b=160
   CASE("60",  "mediumaquamarine")      ;  newname="mediumaquamarine"      ;  r=102  ;  g=205  ;  b=170
   CASE("61",  "aquamarine")            ;  newname="aquamarine"            ;  r=127  ;  g=255  ;  b=212
   CASE("62",  "darkgreen")             ;  newname="darkgreen"             ;  r=0    ;  g=100  ;  b=0
   CASE("63",  "darkolivegreen")        ;  newname="darkolivegreen"        ;  r=85   ;  g=107  ;  b=47
   CASE("64",  "darkseagreen")          ;  newname="darkseagreen"          ;  r=143  ;  g=188  ;  b=143
   CASE("65",  "seagreen")              ;  newname="seagreen"              ;  r=46   ;  g=139  ;  b=87
   CASE("66",  "mediumseagreen")        ;  newname="mediumseagreen"        ;  r=60   ;  g=179  ;  b=113
   CASE("67",  "lightseagreen")         ;  newname="lightseagreen"         ;  r=32   ;  g=178  ;  b=170
   CASE("68",  "palegreen")             ;  newname="palegreen"             ;  r=152  ;  g=251  ;  b=152
   CASE("69",  "springgreen")           ;  newname="springgreen"           ;  r=0    ;  g=255  ;  b=127
   CASE("70",  "lawngreen")             ;  newname="lawngreen"             ;  r=124  ;  g=252  ;  b=0
   CASE("71",  "green")                 ;  newname="green"                 ;  r=0    ;  g=255  ;  b=0
   CASE("72",  "chartreuse")            ;  newname="chartreuse"            ;  r=127  ;  g=255  ;  b=0
   CASE("73",  "mediumspringgreen")     ;  newname="mediumspringgreen"     ;  r=0    ;  g=250  ;  b=154
   CASE("74",  "greenyellow")           ;  newname="greenyellow"           ;  r=173  ;  g=255  ;  b=47
   CASE("75",  "limegreen")             ;  newname="limegreen"             ;  r=50   ;  g=205  ;  b=50
   CASE("76",  "yellowgreen")           ;  newname="yellowgreen"           ;  r=154  ;  g=205  ;  b=50
   CASE("77",  "forestgreen")           ;  newname="forestgreen"           ;  r=34   ;  g=139  ;  b=34
   CASE("78",  "olivedrab")             ;  newname="olivedrab"             ;  r=107  ;  g=142  ;  b=35
   CASE("79",  "darkkhaki")             ;  newname="darkkhaki"             ;  r=189  ;  g=183  ;  b=107
   CASE("80",  "khaki")                 ;  newname="khaki"                 ;  r=240  ;  g=230  ;  b=140
   CASE("81",  "palegoldenrod")         ;  newname="palegoldenrod"         ;  r=238  ;  g=232  ;  b=170
   CASE("82",  "lightgoldenrodyellow")  ;  newname="lightgoldenrodyellow"  ;  r=250  ;  g=250  ;  b=210
   CASE("83",  "lightyellow")           ;  newname="lightyellow"           ;  r=255  ;  g=255  ;  b=224
   CASE("84",  "yellow")                ;  newname="yellow"                ;  r=255  ;  g=255  ;  b=0
   CASE("85",  "gold")                  ;  newname="gold"                  ;  r=255  ;  g=215  ;  b=0
   CASE("86",  "lightgoldenrod")        ;  newname="lightgoldenrod"        ;  r=238  ;  g=221  ;  b=130
   CASE("87",  "goldenrod")             ;  newname="goldenrod"             ;  r=218  ;  g=165  ;  b=32
   CASE("88",  "darkgoldenrod")         ;  newname="darkgoldenrod"         ;  r=184  ;  g=134  ;  b=11
   CASE("89",  "rosybrown")             ;  newname="rosybrown"             ;  r=188  ;  g=143  ;  b=143
   CASE("90",  "indianred")             ;  newname="indianred"             ;  r=205  ;  g=92   ;  b=92
   CASE("91",  "saddlebrown")           ;  newname="saddlebrown"           ;  r=139  ;  g=69   ;  b=19
   CASE("92",  "sienna")                ;  newname="sienna"                ;  r=160  ;  g=82   ;  b=45
   CASE("93",  "peru")                  ;  newname="peru"                  ;  r=205  ;  g=133  ;  b=63
   CASE("94",  "burlywood")             ;  newname="burlywood"             ;  r=222  ;  g=184  ;  b=135
   CASE("95",  "beige")                 ;  newname="beige"                 ;  r=245  ;  g=245  ;  b=220
   CASE("96",  "wheat")                 ;  newname="wheat"                 ;  r=245  ;  g=222  ;  b=179
   CASE("97",  "sandybrown")            ;  newname="sandybrown"            ;  r=244  ;  g=164  ;  b=96
   CASE("98",  "tan")                   ;  newname="tan"                   ;  r=210  ;  g=180  ;  b=140
   CASE("99",  "chocolate")             ;  newname="chocolate"             ;  r=210  ;  g=105  ;  b=30
   CASE("100", "firebrick")             ;  newname="firebrick"             ;  r=178  ;  g=34   ;  b=34
   CASE("101", "brown")                 ;  newname="brown"                 ;  r=165  ;  g=42   ;  b=42
   CASE("102", "darksalmon")            ;  newname="darksalmon"            ;  r=233  ;  g=150  ;  b=122
   CASE("103", "salmon")                ;  newname="salmon"                ;  r=250  ;  g=128  ;  b=114
   CASE("104", "lightsalmon")           ;  newname="lightsalmon"           ;  r=255  ;  g=160  ;  b=122
   CASE("105", "orange")                ;  newname="orange"                ;  r=255  ;  g=165  ;  b=0
   CASE("106", "darkorange")            ;  newname="darkorange"            ;  r=255  ;  g=140  ;  b=0
   CASE("107", "coral")                 ;  newname="coral"                 ;  r=255  ;  g=127  ;  b=80
   CASE("108", "lightcoral")            ;  newname="lightcoral"            ;  r=240  ;  g=128  ;  b=128
   CASE("109", "tomato")                ;  newname="tomato"                ;  r=255  ;  g=99   ;  b=71
   CASE("110", "orangered")             ;  newname="orangered"             ;  r=255  ;  g=69   ;  b=0
   CASE("111", "red")                   ;  newname="red"                   ;  r=255  ;  g=0    ;  b=0
   CASE("116", "palevioletred")         ;  newname="palevioletred"         ;  r=219  ;  g=112  ;  b=147
   CASE("117", "maroon")                ;  newname="maroon"                ;  r=176  ;  g=48   ;  b=96
   CASE("118", "mediumvioletred")       ;  newname="mediumvioletred"       ;  r=199  ;  g=21   ;  b=133
   CASE("119", "violetred")             ;  newname="violetred"             ;  r=208  ;  g=32   ;  b=144
   CASE("120", "magenta")               ;  newname="magenta"               ;  r=255  ;  g=0    ;  b=255
   CASE("121", "violet")                ;  newname="violet"                ;  r=238  ;  g=130  ;  b=238
   CASE("122", "plum")                  ;  newname="plum"                  ;  r=221  ;  g=160  ;  b=221
   CASE("123", "orchid")                ;  newname="orchid"                ;  r=218  ;  g=112  ;  b=214
   CASE("124", "mediumorchid")          ;  newname="mediumorchid"          ;  r=186  ;  g=85   ;  b=211
   CASE("125", "darkorchid")            ;  newname="darkorchid"            ;  r=153  ;  g=50   ;  b=204
   CASE("126", "darkviolet")            ;  newname="darkviolet"            ;  r=148  ;  g=0    ;  b=211
   CASE("127", "blueviolet")            ;  newname="blueviolet"            ;  r=138  ;  g=43   ;  b=226
   CASE("128", "purple")                ;  newname="purple"                ;  r=160  ;  g=32   ;  b=240
   CASE("129", "mediumpurple")          ;  newname="mediumpurple"          ;  r=147  ;  g=112  ;  b=219
   CASE("130", "thistle")               ;  newname="thistle"               ;  r=216  ;  g=191  ;  b=216
   CASE("131", "snow1")                 ;  newname="snow1"                 ;  r=255  ;  g=250  ;  b=250
   CASE("132", "snow2")                 ;  newname="snow2"                 ;  r=238  ;  g=233  ;  b=233
   CASE("133", "snow3")                 ;  newname="snow3"                 ;  r=205  ;  g=201  ;  b=201
   CASE("134", "snow4")                 ;  newname="snow4"                 ;  r=139  ;  g=137  ;  b=137
   CASE("135", "seashell1")             ;  newname="seashell1"             ;  r=255  ;  g=245  ;  b=238
   CASE("136", "seashell2")             ;  newname="seashell2"             ;  r=238  ;  g=229  ;  b=222
   CASE("137", "seashell3")             ;  newname="seashell3"             ;  r=205  ;  g=197  ;  b=191
   CASE("138", "seashell4")             ;  newname="seashell4"             ;  r=139  ;  g=134  ;  b=130
   CASE("139", "antiquewhite1")         ;  newname="antiquewhite1"         ;  r=255  ;  g=239  ;  b=219
   CASE("140", "antiquewhite2")         ;  newname="antiquewhite2"         ;  r=238  ;  g=223  ;  b=204
   CASE("141", "antiquewhite3")         ;  newname="antiquewhite3"         ;  r=205  ;  g=192  ;  b=176
   CASE("142", "antiquewhite4")         ;  newname="antiquewhite4"         ;  r=139  ;  g=131  ;  b=120
   CASE("143", "bisque1")               ;  newname="bisque1"               ;  r=255  ;  g=228  ;  b=196
   CASE("144", "bisque2")               ;  newname="bisque2"               ;  r=238  ;  g=213  ;  b=183
   CASE("145", "bisque3")               ;  newname="bisque3"               ;  r=205  ;  g=183  ;  b=158
   CASE("146", "bisque4")               ;  newname="bisque4"               ;  r=139  ;  g=125  ;  b=107
   CASE("147", "peachpuff1")            ;  newname="peachpuff1"            ;  r=255  ;  g=218  ;  b=185
   CASE("148", "peachpuff2")            ;  newname="peachpuff2"            ;  r=238  ;  g=203  ;  b=173
   CASE("149", "peachpuff3")            ;  newname="peachpuff3"            ;  r=205  ;  g=175  ;  b=149
   CASE("150", "peachpuff4")            ;  newname="peachpuff4"            ;  r=139  ;  g=119  ;  b=101
   CASE("151", "navajowhite1")          ;  newname="navajowhite1"          ;  r=255  ;  g=222  ;  b=173
   CASE("152", "navajowhite2")          ;  newname="navajowhite2"          ;  r=238  ;  g=207  ;  b=161
   CASE("153", "navajowhite3")          ;  newname="navajowhite3"          ;  r=205  ;  g=179  ;  b=139
   CASE("154", "navajowhite4")          ;  newname="navajowhite4"          ;  r=139  ;  g=121  ;  b=94
   CASE("155", "lemonchiffon1")         ;  newname="lemonchiffon1"         ;  r=255  ;  g=250  ;  b=205
   CASE("156", "lemonchiffon2")         ;  newname="lemonchiffon2"         ;  r=238  ;  g=233  ;  b=191
   CASE("157", "lemonchiffon3")         ;  newname="lemonchiffon3"         ;  r=205  ;  g=201  ;  b=165
   CASE("158", "lemonchiffon4")         ;  newname="lemonchiffon4"         ;  r=139  ;  g=137  ;  b=112
   CASE("159", "cornsilk1")             ;  newname="cornsilk1"             ;  r=255  ;  g=248  ;  b=220
   CASE("160", "cornsilk2")             ;  newname="cornsilk2"             ;  r=238  ;  g=232  ;  b=205
   CASE("161", "cornsilk3")             ;  newname="cornsilk3"             ;  r=205  ;  g=200  ;  b=177
   CASE("162", "cornsilk4")             ;  newname="cornsilk4"             ;  r=139  ;  g=136  ;  b=120
   CASE("163", "ivory1")                ;  newname="ivory1"                ;  r=255  ;  g=255  ;  b=240
   CASE("164", "ivory2")                ;  newname="ivory2"                ;  r=238  ;  g=238  ;  b=224
   CASE("165", "ivory3")                ;  newname="ivory3"                ;  r=205  ;  g=205  ;  b=193
   CASE("166", "ivory4")                ;  newname="ivory4"                ;  r=139  ;  g=139  ;  b=131
   CASE("167", "honeydew1")             ;  newname="honeydew1"             ;  r=240  ;  g=255  ;  b=240
   CASE("168", "honeydew2")             ;  newname="honeydew2"             ;  r=224  ;  g=238  ;  b=224
   CASE("169", "honeydew3")             ;  newname="honeydew3"             ;  r=193  ;  g=205  ;  b=193
   CASE("170", "honeydew4")             ;  newname="honeydew4"             ;  r=131  ;  g=139  ;  b=131
   CASE("171", "lavenderblush1")        ;  newname="lavenderblush1"        ;  r=255  ;  g=240  ;  b=245
   CASE("172", "lavenderblush2")        ;  newname="lavenderblush2"        ;  r=238  ;  g=224  ;  b=229
   CASE("173", "lavenderblush3")        ;  newname="lavenderblush3"        ;  r=205  ;  g=193  ;  b=197
   CASE("174", "lavenderblush4")        ;  newname="lavenderblush4"        ;  r=139  ;  g=131  ;  b=134
   CASE("175", "mistyrose1")            ;  newname="mistyrose1"            ;  r=255  ;  g=228  ;  b=225
   CASE("176", "mistyrose2")            ;  newname="mistyrose2"            ;  r=238  ;  g=213  ;  b=210
   CASE("177", "mistyrose3")            ;  newname="mistyrose3"            ;  r=205  ;  g=183  ;  b=181
   CASE("178", "mistyrose4")            ;  newname="mistyrose4"            ;  r=139  ;  g=125  ;  b=123
   CASE("179", "azure1")                ;  newname="azure1"                ;  r=240  ;  g=255  ;  b=255
   CASE("180", "azure2")                ;  newname="azure2"                ;  r=224  ;  g=238  ;  b=238
   CASE("181", "azure3")                ;  newname="azure3"                ;  r=193  ;  g=205  ;  b=205
   CASE("182", "azure4")                ;  newname="azure4"                ;  r=131  ;  g=139  ;  b=139
   CASE("183", "slateblue1")            ;  newname="slateblue1"            ;  r=131  ;  g=111  ;  b=255
   CASE("184", "slateblue2")            ;  newname="slateblue2"            ;  r=122  ;  g=103  ;  b=238
   CASE("185", "slateblue3")            ;  newname="slateblue3"            ;  r=105  ;  g=89   ;  b=205
   CASE("186", "slateblue4")            ;  newname="slateblue4"            ;  r=71   ;  g=60   ;  b=139
   CASE("187", "royalblue1")            ;  newname="royalblue1"            ;  r=72   ;  g=118  ;  b=255
   CASE("188", "royalblue2")            ;  newname="royalblue2"            ;  r=67   ;  g=110  ;  b=238
   CASE("189", "royalblue3")            ;  newname="royalblue3"            ;  r=58   ;  g=95   ;  b=205
   CASE("190", "royalblue4")            ;  newname="royalblue4"            ;  r=39   ;  g=64   ;  b=139
   CASE("191", "blue1")                 ;  newname="blue1"                 ;  r=0    ;  g=0    ;  b=255
   CASE("192", "blue2")                 ;  newname="blue2"                 ;  r=0    ;  g=0    ;  b=238
   CASE("193", "blue3")                 ;  newname="blue3"                 ;  r=0    ;  g=0    ;  b=205
   CASE("194", "blue4")                 ;  newname="blue4"                 ;  r=0    ;  g=0    ;  b=139
   CASE("195", "dodgerblue1")           ;  newname="dodgerblue1"           ;  r=30   ;  g=144  ;  b=255
   CASE("196", "dodgerblue2")           ;  newname="dodgerblue2"           ;  r=28   ;  g=134  ;  b=238
   CASE("197", "dodgerblue3")           ;  newname="dodgerblue3"           ;  r=24   ;  g=116  ;  b=205
   CASE("198", "dodgerblue4")           ;  newname="dodgerblue4"           ;  r=16   ;  g=78   ;  b=139
   CASE("199", "steelblue1")            ;  newname="steelblue1"            ;  r=99   ;  g=184  ;  b=255
   CASE("200", "steelblue2")            ;  newname="steelblue2"            ;  r=92   ;  g=172  ;  b=238
   CASE("201", "steelblue3")            ;  newname="steelblue3"            ;  r=79   ;  g=148  ;  b=205
   CASE("202", "steelblue4")            ;  newname="steelblue4"            ;  r=54   ;  g=100  ;  b=139
   CASE("203", "deepskyblue1")          ;  newname="deepskyblue1"          ;  r=0    ;  g=191  ;  b=255
   CASE("204", "deepskyblue2")          ;  newname="deepskyblue2"          ;  r=0    ;  g=178  ;  b=238
   CASE("205", "deepskyblue3")          ;  newname="deepskyblue3"          ;  r=0    ;  g=154  ;  b=205
   CASE("206", "deepskyblue4")          ;  newname="deepskyblue4"          ;  r=0    ;  g=104  ;  b=139
   CASE("207", "skyblue1")              ;  newname="skyblue1"              ;  r=135  ;  g=206  ;  b=255
   CASE("208", "skyblue2")              ;  newname="skyblue2"              ;  r=126  ;  g=192  ;  b=238
   CASE("209", "skyblue3")              ;  newname="skyblue3"              ;  r=108  ;  g=166  ;  b=205
   CASE("210", "skyblue4")              ;  newname="skyblue4"              ;  r=74   ;  g=112  ;  b=139
   CASE("211", "lightskyblue1")         ;  newname="lightskyblue1"         ;  r=176  ;  g=226  ;  b=255
   CASE("212", "lightskyblue2")         ;  newname="lightskyblue2"         ;  r=164  ;  g=211  ;  b=238
   CASE("213", "lightskyblue3")         ;  newname="lightskyblue3"         ;  r=141  ;  g=182  ;  b=205
   CASE("214", "lightskyblue4")         ;  newname="lightskyblue4"         ;  r=96   ;  g=123  ;  b=139
   CASE("215", "slategray1")            ;  newname="slategray1"            ;  r=198  ;  g=226  ;  b=255
   CASE("216", "slategray2")            ;  newname="slategray2"            ;  r=185  ;  g=211  ;  b=238
   CASE("217", "slategray3")            ;  newname="slategray3"            ;  r=159  ;  g=182  ;  b=205
   CASE("218", "slategray4")            ;  newname="slategray4"            ;  r=108  ;  g=123  ;  b=139
   CASE("219", "lightsteelblue1")       ;  newname="lightsteelblue1"       ;  r=202  ;  g=225  ;  b=255
   CASE("220", "lightsteelblue2")       ;  newname="lightsteelblue2"       ;  r=188  ;  g=210  ;  b=238
   CASE("221", "lightsteelblue3")       ;  newname="lightsteelblue3"       ;  r=162  ;  g=181  ;  b=205
   CASE("222", "lightsteelblue4")       ;  newname="lightsteelblue4"       ;  r=110  ;  g=123  ;  b=139
   CASE("223", "lightblue1")            ;  newname="lightblue1"            ;  r=191  ;  g=239  ;  b=255
   CASE("224", "lightblue2")            ;  newname="lightblue2"            ;  r=178  ;  g=223  ;  b=238
   CASE("225", "lightblue3")            ;  newname="lightblue3"            ;  r=154  ;  g=192  ;  b=205
   CASE("226", "lightblue4")            ;  newname="lightblue4"            ;  r=104  ;  g=131  ;  b=139
   CASE("227", "lightcyan1")            ;  newname="lightcyan1"            ;  r=224  ;  g=255  ;  b=255
   CASE("228", "lightcyan2")            ;  newname="lightcyan2"            ;  r=209  ;  g=238  ;  b=238
   CASE("229", "lightcyan3")            ;  newname="lightcyan3"            ;  r=180  ;  g=205  ;  b=205
   CASE("230", "lightcyan4")            ;  newname="lightcyan4"            ;  r=122  ;  g=139  ;  b=139
   CASE("231", "paleturquoise1")        ;  newname="paleturquoise1"        ;  r=187  ;  g=255  ;  b=255
   CASE("232", "paleturquoise2")        ;  newname="paleturquoise2"        ;  r=174  ;  g=238  ;  b=238
   CASE("233", "paleturquoise3")        ;  newname="paleturquoise3"        ;  r=150  ;  g=205  ;  b=205
   CASE("234", "paleturquoise4")        ;  newname="paleturquoise4"        ;  r=102  ;  g=139  ;  b=139
   CASE("235", "cadetblue1")            ;  newname="cadetblue1"            ;  r=152  ;  g=245  ;  b=255
   CASE("236", "cadetblue2")            ;  newname="cadetblue2"            ;  r=142  ;  g=229  ;  b=238
   CASE("237", "cadetblue3")            ;  newname="cadetblue3"            ;  r=122  ;  g=197  ;  b=205
   CASE("238", "cadetblue4")            ;  newname="cadetblue4"            ;  r=83   ;  g=134  ;  b=139
   CASE("239", "turquoise1")            ;  newname="turquoise1"            ;  r=0    ;  g=245  ;  b=255
   CASE("240", "turquoise2")            ;  newname="turquoise2"            ;  r=0    ;  g=229  ;  b=238
   CASE("241", "turquoise3")            ;  newname="turquoise3"            ;  r=0    ;  g=197  ;  b=205
   CASE("242", "turquoise4")            ;  newname="turquoise4"            ;  r=0    ;  g=134  ;  b=139
   CASE("243", "cyan1")                 ;  newname="cyan1"                 ;  r=0    ;  g=255  ;  b=255
   CASE("244", "cyan2")                 ;  newname="cyan2"                 ;  r=0    ;  g=238  ;  b=238
   CASE("245", "cyan3")                 ;  newname="cyan3"                 ;  r=0    ;  g=205  ;  b=205
   CASE("246", "cyan4")                 ;  newname="cyan4"                 ;  r=0    ;  g=139  ;  b=139
   CASE("247", "darkslategray1")        ;  newname="darkslategray1"        ;  r=151  ;  g=255  ;  b=255
   CASE("248", "darkslategray2")        ;  newname="darkslategray2"        ;  r=141  ;  g=238  ;  b=238
   CASE("249", "darkslategray3")        ;  newname="darkslategray3"        ;  r=121  ;  g=205  ;  b=205
   CASE("250", "darkslategray4")        ;  newname="darkslategray4"        ;  r=82   ;  g=139  ;  b=139
   CASE("251", "aquamarine1")           ;  newname="aquamarine1"           ;  r=127  ;  g=255  ;  b=212
   CASE("252", "aquamarine2")           ;  newname="aquamarine2"           ;  r=118  ;  g=238  ;  b=198
   CASE("253", "aquamarine3")           ;  newname="aquamarine3"           ;  r=102  ;  g=205  ;  b=170
   CASE("254", "aquamarine4")           ;  newname="aquamarine4"           ;  r=69   ;  g=139  ;  b=116
   CASE("255", "darkseagreen1")         ;  newname="darkseagreen1"         ;  r=193  ;  g=255  ;  b=193
   CASE("256", "darkseagreen2")         ;  newname="darkseagreen2"         ;  r=180  ;  g=238  ;  b=180
   CASE("257", "darkseagreen3")         ;  newname="darkseagreen3"         ;  r=155  ;  g=205  ;  b=155
   CASE("258", "darkseagreen4")         ;  newname="darkseagreen4"         ;  r=105  ;  g=139  ;  b=105
   CASE("259", "seagreen1")             ;  newname="seagreen1"             ;  r=84   ;  g=255  ;  b=159
   CASE("260", "seagreen2")             ;  newname="seagreen2"             ;  r=78   ;  g=238  ;  b=148
   CASE("261", "seagreen3")             ;  newname="seagreen3"             ;  r=67   ;  g=205  ;  b=128
   CASE("262", "seagreen4")             ;  newname="seagreen4"             ;  r=46   ;  g=139  ;  b=87
   CASE("263", "palegreen1")            ;  newname="palegreen1"            ;  r=154  ;  g=255  ;  b=154
   CASE("264", "palegreen2")            ;  newname="palegreen2"            ;  r=144  ;  g=238  ;  b=144
   CASE("265", "palegreen3")            ;  newname="palegreen3"            ;  r=124  ;  g=205  ;  b=124
   CASE("266", "palegreen4")            ;  newname="palegreen4"            ;  r=84   ;  g=139  ;  b=84
   CASE("267", "springgreen1")          ;  newname="springgreen1"          ;  r=0    ;  g=255  ;  b=127
   CASE("268", "springgreen2")          ;  newname="springgreen2"          ;  r=0    ;  g=238  ;  b=118
   CASE("269", "springgreen3")          ;  newname="springgreen3"          ;  r=0    ;  g=205  ;  b=102
   CASE("270", "springgreen4")          ;  newname="springgreen4"          ;  r=0    ;  g=139  ;  b=69
   CASE("271", "green1")                ;  newname="green1"                ;  r=0    ;  g=255  ;  b=0
   CASE("272", "green2")                ;  newname="green2"                ;  r=0    ;  g=238  ;  b=0
   CASE("273", "green3")                ;  newname="green3"                ;  r=0    ;  g=205  ;  b=0
   CASE("274", "green4")                ;  newname="green4"                ;  r=0    ;  g=139  ;  b=0
   CASE("275", "chartreuse1")           ;  newname="chartreuse1"           ;  r=127  ;  g=255  ;  b=0
   CASE("276", "chartreuse2")           ;  newname="chartreuse2"           ;  r=118  ;  g=238  ;  b=0
   CASE("277", "chartreuse3")           ;  newname="chartreuse3"           ;  r=102  ;  g=205  ;  b=0
   CASE("278", "chartreuse4")           ;  newname="chartreuse4"           ;  r=69   ;  g=139  ;  b=0
   CASE("279", "olivedrab1")            ;  newname="olivedrab1"            ;  r=192  ;  g=255  ;  b=62
   CASE("280", "olivedrab2")            ;  newname="olivedrab2"            ;  r=179  ;  g=238  ;  b=58
   CASE("281", "olivedrab3")            ;  newname="olivedrab3"            ;  r=154  ;  g=205  ;  b=50
   CASE("282", "olivedrab4")            ;  newname="olivedrab4"            ;  r=105  ;  g=139  ;  b=34
   CASE("283", "darkolivegreen1")       ;  newname="darkolivegreen1"       ;  r=202  ;  g=255  ;  b=112
   CASE("284", "darkolivegreen2")       ;  newname="darkolivegreen2"       ;  r=188  ;  g=238  ;  b=104
   CASE("285", "darkolivegreen3")       ;  newname="darkolivegreen3"       ;  r=162  ;  g=205  ;  b=90
   CASE("286", "darkolivegreen4")       ;  newname="darkolivegreen4"       ;  r=110  ;  g=139  ;  b=61
   CASE("287", "khaki1")                ;  newname="khaki1"                ;  r=255  ;  g=246  ;  b=143
   CASE("288", "khaki2")                ;  newname="khaki2"                ;  r=238  ;  g=230  ;  b=133
   CASE("289", "khaki3")                ;  newname="khaki3"                ;  r=205  ;  g=198  ;  b=115
   CASE("290", "khaki4")                ;  newname="khaki4"                ;  r=139  ;  g=134  ;  b=78
   CASE("291", "lightgoldenrod1")       ;  newname="lightgoldenrod1"       ;  r=255  ;  g=236  ;  b=139
   CASE("292", "lightgoldenrod2")       ;  newname="lightgoldenrod2"       ;  r=238  ;  g=220  ;  b=130
   CASE("293", "lightgoldenrod3")       ;  newname="lightgoldenrod3"       ;  r=205  ;  g=190  ;  b=112
   CASE("294", "lightgoldenrod4")       ;  newname="lightgoldenrod4"       ;  r=139  ;  g=129  ;  b=76
   CASE("295", "lightyellow1")          ;  newname="lightyellow1"          ;  r=255  ;  g=255  ;  b=224
   CASE("296", "lightyellow2")          ;  newname="lightyellow2"          ;  r=238  ;  g=238  ;  b=209
   CASE("297", "lightyellow3")          ;  newname="lightyellow3"          ;  r=205  ;  g=205  ;  b=180
   CASE("298", "lightyellow4")          ;  newname="lightyellow4"          ;  r=139  ;  g=139  ;  b=122
   CASE("299", "yellow1")               ;  newname="yellow1"               ;  r=255  ;  g=255  ;  b=0
   CASE("300", "yellow2")               ;  newname="yellow2"               ;  r=238  ;  g=238  ;  b=0
   CASE("301", "yellow3")               ;  newname="yellow3"               ;  r=205  ;  g=205  ;  b=0
   CASE("302", "yellow4")               ;  newname="yellow4"               ;  r=139  ;  g=139  ;  b=0
   CASE("303", "gold1")                 ;  newname="gold1"                 ;  r=255  ;  g=215  ;  b=0
   CASE("304", "gold2")                 ;  newname="gold2"                 ;  r=238  ;  g=201  ;  b=0
   CASE("305", "gold3")                 ;  newname="gold3"                 ;  r=205  ;  g=173  ;  b=0
   CASE("306", "gold4")                 ;  newname="gold4"                 ;  r=139  ;  g=117  ;  b=0
   CASE("307", "goldenrod1")            ;  newname="goldenrod1"            ;  r=255  ;  g=193  ;  b=37
   CASE("308", "goldenrod2")            ;  newname="goldenrod2"            ;  r=238  ;  g=180  ;  b=34
   CASE("309", "goldenrod3")            ;  newname="goldenrod3"            ;  r=205  ;  g=155  ;  b=29
   CASE("310", "goldenrod4")            ;  newname="goldenrod4"            ;  r=139  ;  g=105  ;  b=20
   CASE("311", "darkgoldenrod1")        ;  newname="darkgoldenrod1"        ;  r=255  ;  g=185  ;  b=15
   CASE("312", "darkgoldenrod2")        ;  newname="darkgoldenrod2"        ;  r=238  ;  g=173  ;  b=14
   CASE("313", "darkgoldenrod3")        ;  newname="darkgoldenrod3"        ;  r=205  ;  g=149  ;  b=12
   CASE("314", "darkgoldenrod4")        ;  newname="darkgoldenrod4"        ;  r=139  ;  g=101  ;  b=8
   CASE("315", "rosybrown1")            ;  newname="rosybrown1"            ;  r=255  ;  g=193  ;  b=193
   CASE("316", "rosybrown2")            ;  newname="rosybrown2"            ;  r=238  ;  g=180  ;  b=180
   CASE("317", "rosybrown3")            ;  newname="rosybrown3"            ;  r=205  ;  g=155  ;  b=155
   CASE("318", "rosybrown4")            ;  newname="rosybrown4"            ;  r=139  ;  g=105  ;  b=105
   CASE("319", "indianred1")            ;  newname="indianred1"            ;  r=255  ;  g=106  ;  b=106
   CASE("320", "indianred2")            ;  newname="indianred2"            ;  r=238  ;  g=99   ;  b=99
   CASE("321", "indianred3")            ;  newname="indianred3"            ;  r=205  ;  g=85   ;  b=85
   CASE("322", "indianred4")            ;  newname="indianred4"            ;  r=139  ;  g=58   ;  b=58
   CASE("323", "sienna1")               ;  newname="sienna1"               ;  r=255  ;  g=130  ;  b=71
   CASE("324", "sienna2")               ;  newname="sienna2"               ;  r=238  ;  g=121  ;  b=66
   CASE("325", "sienna3")               ;  newname="sienna3"               ;  r=205  ;  g=104  ;  b=57
   CASE("326", "sienna4")               ;  newname="sienna4"               ;  r=139  ;  g=71   ;  b=38
   CASE("327", "burlywood1")            ;  newname="burlywood1"            ;  r=255  ;  g=211  ;  b=155
   CASE("328", "burlywood2")            ;  newname="burlywood2"            ;  r=238  ;  g=197  ;  b=145
   CASE("329", "burlywood3")            ;  newname="burlywood3"            ;  r=205  ;  g=170  ;  b=125
   CASE("330", "burlywood4")            ;  newname="burlywood4"            ;  r=139  ;  g=115  ;  b=85
   CASE("331", "wheat1")                ;  newname="wheat1"                ;  r=255  ;  g=231  ;  b=186
   CASE("332", "wheat2")                ;  newname="wheat2"                ;  r=238  ;  g=216  ;  b=174
   CASE("333", "wheat3")                ;  newname="wheat3"                ;  r=205  ;  g=186  ;  b=150
   CASE("334", "wheat4")                ;  newname="wheat4"                ;  r=139  ;  g=126  ;  b=102
   CASE("335", "tan1")                  ;  newname="tan1"                  ;  r=255  ;  g=165  ;  b=79
   CASE("336", "tan2")                  ;  newname="tan2"                  ;  r=238  ;  g=154  ;  b=73
   CASE("337", "tan3")                  ;  newname="tan3"                  ;  r=205  ;  g=133  ;  b=63
   CASE("338", "tan4")                  ;  newname="tan4"                  ;  r=139  ;  g=90   ;  b=43
   CASE("339", "chocolate1")            ;  newname="chocolate1"            ;  r=255  ;  g=127  ;  b=36
   CASE("340", "chocolate2")            ;  newname="chocolate2"            ;  r=238  ;  g=118  ;  b=33
   CASE("341", "chocolate3")            ;  newname="chocolate3"            ;  r=205  ;  g=102  ;  b=29
   CASE("342", "chocolate4")            ;  newname="chocolate4"            ;  r=139  ;  g=69   ;  b=19
   CASE("343", "firebrick1")            ;  newname="firebrick1"            ;  r=255  ;  g=48   ;  b=48
   CASE("344", "firebrick2")            ;  newname="firebrick2"            ;  r=238  ;  g=44   ;  b=44
   CASE("345", "firebrick3")            ;  newname="firebrick3"            ;  r=205  ;  g=38   ;  b=38
   CASE("346", "firebrick4")            ;  newname="firebrick4"            ;  r=139  ;  g=26   ;  b=26
   CASE("347", "brown1")                ;  newname="brown1"                ;  r=255  ;  g=64   ;  b=64
   CASE("348", "brown2")                ;  newname="brown2"                ;  r=238  ;  g=59   ;  b=59
   CASE("349", "brown3")                ;  newname="brown3"                ;  r=205  ;  g=51   ;  b=51
   CASE("350", "brown4")                ;  newname="brown4"                ;  r=139  ;  g=35   ;  b=35
   CASE("351", "salmon1")               ;  newname="salmon1"               ;  r=255  ;  g=140  ;  b=105
   CASE("352", "salmon2")               ;  newname="salmon2"               ;  r=238  ;  g=130  ;  b=98
   CASE("353", "salmon3")               ;  newname="salmon3"               ;  r=205  ;  g=112  ;  b=84
   CASE("354", "salmon4")               ;  newname="salmon4"               ;  r=139  ;  g=76   ;  b=57
   CASE("355", "lightsalmon1")          ;  newname="lightsalmon1"          ;  r=255  ;  g=160  ;  b=122
   CASE("356", "lightsalmon2")          ;  newname="lightsalmon2"          ;  r=238  ;  g=149  ;  b=114
   CASE("357", "lightsalmon3")          ;  newname="lightsalmon3"          ;  r=205  ;  g=129  ;  b=98
   CASE("358", "lightsalmon4")          ;  newname="lightsalmon4"          ;  r=139  ;  g=87   ;  b=66
   CASE("359", "orange1")               ;  newname="orange1"               ;  r=255  ;  g=165  ;  b=0
   CASE("360", "orange2")               ;  newname="orange2"               ;  r=238  ;  g=154  ;  b=0
   CASE("361", "orange3")               ;  newname="orange3"               ;  r=205  ;  g=133  ;  b=0
   CASE("362", "orange4")               ;  newname="orange4"               ;  r=139  ;  g=90   ;  b=0
   CASE("363", "darkorange1")           ;  newname="darkorange1"           ;  r=255  ;  g=127  ;  b=0
   CASE("364", "darkorange2")           ;  newname="darkorange2"           ;  r=238  ;  g=118  ;  b=0
   CASE("365", "darkorange3")           ;  newname="darkorange3"           ;  r=205  ;  g=102  ;  b=0
   CASE("366", "darkorange4")           ;  newname="darkorange4"           ;  r=139  ;  g=69   ;  b=0
   CASE("367", "coral1")                ;  newname="coral1"                ;  r=255  ;  g=114  ;  b=86
   CASE("368", "coral2")                ;  newname="coral2"                ;  r=238  ;  g=106  ;  b=80
   CASE("369", "coral3")                ;  newname="coral3"                ;  r=205  ;  g=91   ;  b=69
   CASE("370", "coral4")                ;  newname="coral4"                ;  r=139  ;  g=62   ;  b=47
   CASE("371", "tomato1")               ;  newname="tomato1"               ;  r=255  ;  g=99   ;  b=71
   CASE("372", "tomato2")               ;  newname="tomato2"               ;  r=238  ;  g=92   ;  b=66
   CASE("373", "tomato3")               ;  newname="tomato3"               ;  r=205  ;  g=79   ;  b=57
   CASE("374", "tomato4")               ;  newname="tomato4"               ;  r=139  ;  g=54   ;  b=38
   CASE("375", "orangered1")            ;  newname="orangered1"            ;  r=255  ;  g=69   ;  b=0
   CASE("376", "orangered2")            ;  newname="orangered2"            ;  r=238  ;  g=64   ;  b=0
   CASE("377", "orangered3")            ;  newname="orangered3"            ;  r=205  ;  g=55   ;  b=0
   CASE("378", "orangered4")            ;  newname="orangered4"            ;  r=139  ;  g=37   ;  b=0
   CASE("379", "red1")                  ;  newname="red1"                  ;  r=255  ;  g=0    ;  b=0
   CASE("380", "red2")                  ;  newname="red2"                  ;  r=238  ;  g=0    ;  b=0
   CASE("381", "red3")                  ;  newname="red3"                  ;  r=205  ;  g=0    ;  b=0
   CASE("382", "red4")                  ;  newname="red4"                  ;  r=139  ;  g=0    ;  b=0
   CASE("112", "hotpink")               ;  newname="hotpink"               ;  r=255  ;  g=105  ;  b=180
   CASE("113", "deeppink")              ;  newname="deeppink"              ;  r=255  ;  g=20   ;  b=147
   CASE("115", "lightpink")             ;  newname="lightpink"             ;  r=255  ;  g=182  ;  b=193
   CASE("383", "deeppink1")             ;  newname="deeppink1"             ;  r=255  ;  g=20   ;  b=147
   CASE("384", "deeppink2")             ;  newname="deeppink2"             ;  r=238  ;  g=18   ;  b=137
   CASE("385", "deeppink3")             ;  newname="deeppink3"             ;  r=205  ;  g=16   ;  b=118
   CASE("386", "deeppink4")             ;  newname="deeppink4"             ;  r=139  ;  g=10   ;  b=80
   CASE("387", "hotpink1")              ;  newname="hotpink1"              ;  r=255  ;  g=110  ;  b=180
   CASE("388", "hotpink2")              ;  newname="hotpink2"              ;  r=238  ;  g=106  ;  b=167
   CASE("389", "hotpink3")              ;  newname="hotpink3"              ;  r=205  ;  g=96   ;  b=144
   CASE("390", "hotpink4")              ;  newname="hotpink4"              ;  r=139  ;  g=58   ;  b=98
   CASE("114", "pink")                  ;  newname="pink"                  ;  r=255  ;  g=192  ;  b=203
   CASE("391", "pink1")                 ;  newname="pink1"                 ;  r=255  ;  g=181  ;  b=197
   CASE("392", "pink2")                 ;  newname="pink2"                 ;  r=238  ;  g=169  ;  b=184
   CASE("393", "pink3")                 ;  newname="pink3"                 ;  r=205  ;  g=145  ;  b=158
   CASE("394", "pink4")                 ;  newname="pink4"                 ;  r=139  ;  g=99   ;  b=108
   CASE("395", "lightpink1")            ;  newname="lightpink1"            ;  r=255  ;  g=174  ;  b=185
   CASE("396", "lightpink2")            ;  newname="lightpink2"            ;  r=238  ;  g=162  ;  b=173
   CASE("397", "lightpink3")            ;  newname="lightpink3"            ;  r=205  ;  g=140  ;  b=149
   CASE("398", "lightpink4")            ;  newname="lightpink4"            ;  r=139  ;  g=95   ;  b=101
   CASE("399", "palevioletred1")        ;  newname="palevioletred1"        ;  r=255  ;  g=130  ;  b=171
   CASE("400", "palevioletred2")        ;  newname="palevioletred2"        ;  r=238  ;  g=121  ;  b=159
   CASE("401", "palevioletred3")        ;  newname="palevioletred3"        ;  r=205  ;  g=104  ;  b=137
   CASE("402", "palevioletred4")        ;  newname="palevioletred4"        ;  r=139  ;  g=71   ;  b=93
   CASE("403", "maroon1")               ;  newname="maroon1"               ;  r=255  ;  g=52   ;  b=179
   CASE("404", "maroon2")               ;  newname="maroon2"               ;  r=238  ;  g=48   ;  b=167
   CASE("405", "maroon3")               ;  newname="maroon3"               ;  r=205  ;  g=41   ;  b=144
   CASE("406", "maroon4")               ;  newname="maroon4"               ;  r=139  ;  g=28   ;  b=98
   CASE("407", "violetred1")            ;  newname="violetred1"            ;  r=255  ;  g=62   ;  b=150
   CASE("408", "violetred2")            ;  newname="violetred2"            ;  r=238  ;  g=58   ;  b=140
   CASE("409", "violetred3")            ;  newname="violetred3"            ;  r=205  ;  g=50   ;  b=120
   CASE("410", "violetred4")            ;  newname="violetred4"            ;  r=139  ;  g=34   ;  b=82
   CASE("411", "magenta1")              ;  newname="magenta1"              ;  r=255  ;  g=0    ;  b=255
   CASE("412", "magenta2")              ;  newname="magenta2"              ;  r=238  ;  g=0    ;  b=238
   CASE("413", "magenta3")              ;  newname="magenta3"              ;  r=205  ;  g=0    ;  b=205
   CASE("414", "magenta4")              ;  newname="magenta4"              ;  r=139  ;  g=0    ;  b=139
   CASE("415", "orchid1")               ;  newname="orchid1"               ;  r=255  ;  g=131  ;  b=250
   CASE("416", "orchid2")               ;  newname="orchid2"               ;  r=238  ;  g=122  ;  b=233
   CASE("417", "orchid3")               ;  newname="orchid3"               ;  r=205  ;  g=105  ;  b=201
   CASE("418", "orchid4")               ;  newname="orchid4"               ;  r=139  ;  g=71   ;  b=137
   CASE("419", "plum1")                 ;  newname="plum1"                 ;  r=255  ;  g=187  ;  b=255
   CASE("420", "plum2")                 ;  newname="plum2"                 ;  r=238  ;  g=174  ;  b=238
   CASE("421", "plum3")                 ;  newname="plum3"                 ;  r=205  ;  g=150  ;  b=205
   CASE("422", "plum4")                 ;  newname="plum4"                 ;  r=139  ;  g=102  ;  b=139
   CASE("423", "mediumorchid1")         ;  newname="mediumorchid1"         ;  r=224  ;  g=102  ;  b=255
   CASE("424", "mediumorchid2")         ;  newname="mediumorchid2"         ;  r=209  ;  g=95   ;  b=238
   CASE("425", "mediumorchid3")         ;  newname="mediumorchid3"         ;  r=180  ;  g=82   ;  b=205
   CASE("426", "mediumorchid4")         ;  newname="mediumorchid4"         ;  r=122  ;  g=55   ;  b=139
   CASE("427", "darkorchid1")           ;  newname="darkorchid1"           ;  r=191  ;  g=62   ;  b=255
   CASE("428", "darkorchid2")           ;  newname="darkorchid2"           ;  r=178  ;  g=58   ;  b=238
   CASE("429", "darkorchid3")           ;  newname="darkorchid3"           ;  r=154  ;  g=50   ;  b=205
   CASE("430", "darkorchid4")           ;  newname="darkorchid4"           ;  r=104  ;  g=34   ;  b=139
   CASE("431", "purple1")               ;  newname="purple1"               ;  r=155  ;  g=48   ;  b=255
   CASE("432", "purple2")               ;  newname="purple2"               ;  r=145  ;  g=44   ;  b=238
   CASE("433", "purple3")               ;  newname="purple3"               ;  r=125  ;  g=38   ;  b=205
   CASE("434", "purple4")               ;  newname="purple4"               ;  r=85   ;  g=26   ;  b=139
   CASE("435", "mediumpurple1")         ;  newname="mediumpurple1"         ;  r=171  ;  g=130  ;  b=255
   CASE("436", "mediumpurple2")         ;  newname="mediumpurple2"         ;  r=159  ;  g=121  ;  b=238
   CASE("437", "mediumpurple3")         ;  newname="mediumpurple3"         ;  r=137  ;  g=104  ;  b=205
   CASE("438", "mediumpurple4")         ;  newname="mediumpurple4"         ;  r=93   ;  g=71   ;  b=139
   CASE("439", "thistle1")              ;  newname="thistle1"              ;  r=255  ;  g=225  ;  b=255
   CASE("440", "thistle2")              ;  newname="thistle2"              ;  r=238  ;  g=210  ;  b=238
   CASE("441", "thistle3")              ;  newname="thistle3"              ;  r=205  ;  g=181  ;  b=205
   CASE("442", "thistle4")              ;  newname="thistle4"              ;  r=139  ;  g=123  ;  b=139
   CASE("443", "gray0")                 ;  newname="gray0"                 ;  r=0    ;  g=0    ;  b=0
   CASE("444", "gray1")                 ;  newname="gray1"                 ;  r=3    ;  g=3    ;  b=3
   CASE("445", "gray2")                 ;  newname="gray2"                 ;  r=5    ;  g=5    ;  b=5
   CASE("446", "gray3")                 ;  newname="gray3"                 ;  r=8    ;  g=8    ;  b=8
   CASE("447", "gray4")                 ;  newname="gray4"                 ;  r=10   ;  g=10   ;  b=10
   CASE("448", "gray5")                 ;  newname="gray5"                 ;  r=13   ;  g=13   ;  b=13
   CASE("449", "gray6")                 ;  newname="gray6"                 ;  r=15   ;  g=15   ;  b=15
   CASE("450", "gray7")                 ;  newname="gray7"                 ;  r=18   ;  g=18   ;  b=18
   CASE("451", "gray8")                 ;  newname="gray8"                 ;  r=20   ;  g=20   ;  b=20
   CASE("452", "gray9")                 ;  newname="gray9"                 ;  r=23   ;  g=23   ;  b=23
   CASE("453", "gray10")                ;  newname="gray10"                ;  r=26   ;  g=26   ;  b=26
   CASE("454", "gray11")                ;  newname="gray11"                ;  r=28   ;  g=28   ;  b=28
   CASE("455", "gray12")                ;  newname="gray12"                ;  r=31   ;  g=31   ;  b=31
   CASE("456", "gray13")                ;  newname="gray13"                ;  r=33   ;  g=33   ;  b=33
   CASE("457", "gray14")                ;  newname="gray14"                ;  r=36   ;  g=36   ;  b=36
   CASE("458", "gray15")                ;  newname="gray15"                ;  r=38   ;  g=38   ;  b=38
   CASE("459", "gray16")                ;  newname="gray16"                ;  r=41   ;  g=41   ;  b=41
   CASE("460", "gray17")                ;  newname="gray17"                ;  r=43   ;  g=43   ;  b=43
   CASE("461", "gray18")                ;  newname="gray18"                ;  r=46   ;  g=46   ;  b=46
   CASE("462", "gray19")                ;  newname="gray19"                ;  r=48   ;  g=48   ;  b=48
   CASE("463", "gray20")                ;  newname="gray20"                ;  r=51   ;  g=51   ;  b=51
   CASE("464", "gray21")                ;  newname="gray21"                ;  r=54   ;  g=54   ;  b=54
   CASE("465", "gray22")                ;  newname="gray22"                ;  r=56   ;  g=56   ;  b=56
   CASE("466", "gray23")                ;  newname="gray23"                ;  r=59   ;  g=59   ;  b=59
   CASE("467", "gray24")                ;  newname="gray24"                ;  r=61   ;  g=61   ;  b=61
   CASE("468", "gray25")                ;  newname="gray25"                ;  r=64   ;  g=64   ;  b=64
   CASE("469", "gray26")                ;  newname="gray26"                ;  r=66   ;  g=66   ;  b=66
   CASE("470", "gray27")                ;  newname="gray27"                ;  r=69   ;  g=69   ;  b=69
   CASE("471", "gray28")                ;  newname="gray28"                ;  r=71   ;  g=71   ;  b=71
   CASE("472", "gray29")                ;  newname="gray29"                ;  r=74   ;  g=74   ;  b=74
   CASE("473", "gray30")                ;  newname="gray30"                ;  r=77   ;  g=77   ;  b=77
   CASE("474", "gray31")                ;  newname="gray31"                ;  r=79   ;  g=79   ;  b=79
   CASE("475", "gray32")                ;  newname="gray32"                ;  r=82   ;  g=82   ;  b=82
   CASE("476", "gray33")                ;  newname="gray33"                ;  r=84   ;  g=84   ;  b=84
   CASE("477", "gray34")                ;  newname="gray34"                ;  r=87   ;  g=87   ;  b=87
   CASE("478", "gray35")                ;  newname="gray35"                ;  r=89   ;  g=89   ;  b=89
   CASE("479", "gray36")                ;  newname="gray36"                ;  r=92   ;  g=92   ;  b=92
   CASE("480", "gray37")                ;  newname="gray37"                ;  r=94   ;  g=94   ;  b=94
   CASE("481", "gray38")                ;  newname="gray38"                ;  r=97   ;  g=97   ;  b=97
   CASE("482", "gray39")                ;  newname="gray39"                ;  r=99   ;  g=99   ;  b=99
   CASE("483", "gray40")                ;  newname="gray40"                ;  r=102  ;  g=102  ;  b=102
   CASE("484", "gray41")                ;  newname="gray41"                ;  r=105  ;  g=105  ;  b=105
   CASE("485", "gray42")                ;  newname="gray42"                ;  r=107  ;  g=107  ;  b=107
   CASE("486", "gray43")                ;  newname="gray43"                ;  r=110  ;  g=110  ;  b=110
   CASE("487", "gray44")                ;  newname="gray44"                ;  r=112  ;  g=112  ;  b=112
   CASE("488", "gray45")                ;  newname="gray45"                ;  r=115  ;  g=115  ;  b=115
   CASE("489", "gray46")                ;  newname="gray46"                ;  r=117  ;  g=117  ;  b=117
   CASE("490", "gray47")                ;  newname="gray47"                ;  r=120  ;  g=120  ;  b=120
   CASE("491", "gray48")                ;  newname="gray48"                ;  r=122  ;  g=122  ;  b=122
   CASE("492", "gray49")                ;  newname="gray49"                ;  r=125  ;  g=125  ;  b=125
   CASE("493", "gray50")                ;  newname="gray50"                ;  r=127  ;  g=127  ;  b=127
   CASE("494", "gray51")                ;  newname="gray51"                ;  r=130  ;  g=130  ;  b=130
   CASE("495", "gray52")                ;  newname="gray52"                ;  r=133  ;  g=133  ;  b=133
   CASE("496", "gray53")                ;  newname="gray53"                ;  r=135  ;  g=135  ;  b=135
   CASE("497", "gray54")                ;  newname="gray54"                ;  r=138  ;  g=138  ;  b=138
   CASE("498", "gray55")                ;  newname="gray55"                ;  r=140  ;  g=140  ;  b=140
   CASE("499", "gray56")                ;  newname="gray56"                ;  r=143  ;  g=143  ;  b=143
   CASE("500", "gray57")                ;  newname="gray57"                ;  r=145  ;  g=145  ;  b=145
   CASE("501", "gray58")                ;  newname="gray58"                ;  r=148  ;  g=148  ;  b=148
   CASE("502", "gray59")                ;  newname="gray59"                ;  r=150  ;  g=150  ;  b=150
   CASE("503", "gray60")                ;  newname="gray60"                ;  r=153  ;  g=153  ;  b=153
   CASE("504", "gray61")                ;  newname="gray61"                ;  r=156  ;  g=156  ;  b=156
   CASE("505", "gray62")                ;  newname="gray62"                ;  r=158  ;  g=158  ;  b=158
   CASE("506", "gray63")                ;  newname="gray63"                ;  r=161  ;  g=161  ;  b=161
   CASE("507", "gray64")                ;  newname="gray64"                ;  r=163  ;  g=163  ;  b=163
   CASE("508", "gray65")                ;  newname="gray65"                ;  r=166  ;  g=166  ;  b=166
   CASE("509", "gray66")                ;  newname="gray66"                ;  r=168  ;  g=168  ;  b=168
   CASE("510", "gray67")                ;  newname="gray67"                ;  r=171  ;  g=171  ;  b=171
   CASE("511", "gray68")                ;  newname="gray68"                ;  r=173  ;  g=173  ;  b=173
   CASE("512", "gray69")                ;  newname="gray69"                ;  r=176  ;  g=176  ;  b=176
   CASE("513", "gray70")                ;  newname="gray70"                ;  r=179  ;  g=179  ;  b=179
   CASE("514", "gray71")                ;  newname="gray71"                ;  r=181  ;  g=181  ;  b=181
   CASE("515", "gray72")                ;  newname="gray72"                ;  r=184  ;  g=184  ;  b=184
   CASE("516", "gray73")                ;  newname="gray73"                ;  r=186  ;  g=186  ;  b=186
   CASE("517", "gray74")                ;  newname="gray74"                ;  r=189  ;  g=189  ;  b=189
   CASE("518", "gray75")                ;  newname="gray75"                ;  r=191  ;  g=191  ;  b=191
   CASE("519", "gray76")                ;  newname="gray76"                ;  r=194  ;  g=194  ;  b=194
   CASE("520", "gray77")                ;  newname="gray77"                ;  r=196  ;  g=196  ;  b=196
   CASE("521", "gray78")                ;  newname="gray78"                ;  r=199  ;  g=199  ;  b=199
   CASE("522", "gray79")                ;  newname="gray79"                ;  r=201  ;  g=201  ;  b=201
   CASE("523", "gray80")                ;  newname="gray80"                ;  r=204  ;  g=204  ;  b=204
   CASE("524", "gray81")                ;  newname="gray81"                ;  r=207  ;  g=207  ;  b=207
   CASE("525", "gray82")                ;  newname="gray82"                ;  r=209  ;  g=209  ;  b=209
   CASE("526", "gray83")                ;  newname="gray83"                ;  r=212  ;  g=212  ;  b=212
   CASE("527", "gray84")                ;  newname="gray84"                ;  r=214  ;  g=214  ;  b=214
   CASE("528", "gray85")                ;  newname="gray85"                ;  r=217  ;  g=217  ;  b=217
   CASE("529", "gray86")                ;  newname="gray86"                ;  r=219  ;  g=219  ;  b=219
   CASE("530", "gray87")                ;  newname="gray87"                ;  r=222  ;  g=222  ;  b=222
   CASE("531", "gray88")                ;  newname="gray88"                ;  r=224  ;  g=224  ;  b=224
   CASE("532", "gray89")                ;  newname="gray89"                ;  r=227  ;  g=227  ;  b=227
   CASE("533", "gray90")                ;  newname="gray90"                ;  r=229  ;  g=229  ;  b=229
   CASE("534", "gray91")                ;  newname="gray91"                ;  r=232  ;  g=232  ;  b=232
   CASE("535", "gray92")                ;  newname="gray92"                ;  r=235  ;  g=235  ;  b=235
   CASE("536", "gray93")                ;  newname="gray93"                ;  r=237  ;  g=237  ;  b=237
   CASE("537", "gray94")                ;  newname="gray94"                ;  r=240  ;  g=240  ;  b=240
   CASE("538", "gray95")                ;  newname="gray95"                ;  r=242  ;  g=242  ;  b=242
   CASE("539", "gray96")                ;  newname="gray96"                ;  r=245  ;  g=245  ;  b=245
   CASE("540", "gray97")                ;  newname="gray97"                ;  r=247  ;  g=247  ;  b=247
   CASE("541", "gray98")                ;  newname="gray98"                ;  r=250  ;  g=250  ;  b=250
   CASE("542", "gray99")                ;  newname="gray99"                ;  r=252  ;  g=252  ;  b=252
   CASE("543", "gray100")               ;  newname="gray100"               ;  r=255  ;  g=255  ;  b=255
   CASE("544", "darkgray")              ;  newname="darkgray"              ;  r=169  ;  g=169  ;  b=169
   CASE("545", "darkblue")              ;  newname="darkblue"              ;  r=0    ;  g=0    ;  b=139
   CASE("546", "darkcyan")              ;  newname="darkcyan"              ;  r=0    ;  g=139  ;  b=139
   CASE("547", "darkmagenta")           ;  newname="darkmagenta"           ;  r=139  ;  g=0    ;  b=139
   CASE("548", "darkred")               ;  newname="darkred"               ;  r=139  ;  g=0    ;  b=0
   CASE("549", "lightgreen")            ;  newname="lightgreen"            ;  r=144  ;  g=238  ;  b=144
   CASE("550", "silver")                ;  newname="silver"                ;  r=192  ;  g=192  ;  b=192
   CASE("551", "teal")                  ;  newname="teal"                  ;  r=0    ;  g=128  ;  b=128
   CASE("552", "olive")                 ;  newname="olive"                 ;  r=128  ;  g=128  ;  b=0
   CASE("553", "lime")                  ;  newname="lime"                  ;  r=0    ;  g=255  ;  b=0
   CASE("554", "aqua")                  ;  newname="aqua"                  ;  r=0    ;  g=255  ;  b=255
   CASE("555", "fuchsia")               ;  newname="fuchsia"               ;  r=255  ;  g=0    ;  b=255

   case default                         ;  newname="Unknown"               ;  r=255  ;  g=255  ;  b=255 ! unknown color name

   END SELECT

   IF(PRESENT(echoname)) THEN
      echoname = newname
   ENDIF
   r=r/2.55; g=g/2.55; b=b/2.55 ! take values from range of 0 to 255 to 0 to 100
END SUBROUTINE color_name2rgb
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental pure function lower(str) result (string)

! ident_74="@(#) M_pixel lower(3f) Changes a string to lowercase"

character(*), intent(In)     :: str
character(len(str))          :: string
integer                      :: i
   string = str
   do i = 1, len(str)                                ! step thru each letter in the string in specified range
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32)     ! change letter to miniscule
      case default
      end select
   end do
end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     polar_to_cartesian(3f) - [M_pixel:TRIGONOMETRY] convert polar
!!                              coordinates to Cartesian coordinates
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine polar_to_cartesian(radius,inclination,x,y)
!!
!!     real,intent(in) :: radius,inclination
!!     real,intent(out)  :: x,y
!!
!!##DESCRIPTION
!!     Convert polar coordinate <radius, inclination > with
!!     angles in radians to cartesian point <X,Y> using the formulas
!!
!!       x=radius*cos(inclination)
!!       y=radius*sin(inclination)
!!
!!##OPTIONS
!!    RADIUS       The radial distance from the origin (O) to the point (P)
!!    INCLINATION  The INCLINATION angle in radians between the inclination
!!                 reference direction (x-axis) and the orthogonal projection
!!                 of the line OP of the reference plane (x-y plane).
!!
!!##RESULTS
!!    X  The distance along the x-axis
!!    Y  The distance along the y-axis
!!
!!##EXAMPLES
!!
!!   examples of usage
!!
!!    program demo_polar_to_cartesian
!!    use M_pixel, only : polar_to_cartesian
!!    implicit none
!!    real    :: x,y
!!    real    :: r,i
!!    !!integer :: ios
!!
!!     !!INFINITE: do
!!     !!   write(*,advance='no')'Enter radius and inclination(in radians):'
!!     !!   read(*,*,iostat=ios) r, i
!!     !!   if(ios.ne.0)exit INFINITE
!!        call polar_to_cartesian(r,i,x,y)
!!        write(*,*)'x=',x,' y=',y,'radius=',r,'inclination=',i
!!     !!enddo INFINITE
!!    end program demo_polar_to_cartesian
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine polar_to_cartesian(radius,inclination,x,y)
implicit none
! ident_75="@(#) M_pixel polar_to_cartesian(3f) convert polar coordinates to cartesian coordinates"
real,intent(in) :: radius,inclination
real,intent(out)  :: x,y
   if(radius.eq.0)then
      x=0.0
      y=0.0
   else
      x=radius*cos(inclination)
      y=radius*sin(inclination)
   endif
end subroutine polar_to_cartesian
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    d2r(3f) - [M_pixel:TRIGONOMETRY] convert degrees to radians
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental real function d2r(degrees)
!!
!!     class(*),intent(in) :: radians
!!##DESCRIPTION
!!    Converts degrees to radians using the formula:
!!
!!     radians=real(degrees*acos(-1.0d0)/180.d0)
!!##OPTIONS
!!    degrees    any standard scalar value supported by anyscalar_to_real(3f).
!!               This includes REAL, INTEGER, DOUBLEPRECISION, ... .
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_d2r
!!    use M_pixel, only :  d2r
!!    implicit none
!!       write(*,*)'With REAL array input    ', &
!!        & d2r([0.0,45.0,90.0,135.0,180.0])
!!       write(*,*)'With INTEGER array input ', &
!!        & d2r([0,  45,  90,  135,  180  ])
!!       write(*,*)'With DOUBLEPRECISION     ', &
!!        & d2r(0.0d0),d2r(45.0d0),d2r(90.0d0),d2r(135.0d0),d2r(180.0d0)
!!    end program demo_d2r
!!
!!   Results
!!
!!    With REAL array input    0.00000 0.785398185 1.57079637
!!    2.35619450 3.14159274
!!    With INTEGER array input 0.00000 0.785398185 1.57079637
!!    2.35619450 3.14159274
!!    With DOUBLEPRECISION     0.00000 0.785398185 1.57079637
!!    2.35619450 3.14159274
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
elemental real function d2r_r(degrees)

! ident_76="@(#) M_pixel d2r_r(3f) Convert degrees to radians"

real,intent(in)           :: degrees                ! input degrees to convert to radians
   d2r_r=dble(degrees)/Deg_Per_Rad                  ! do the unit conversion
end function d2r_r
!-----------------------------------------------------------------------------------------------------------------------------------
elemental doubleprecision function d2r_d(degrees)

! ident_77="@(#) M_pixel d2r_d(3f) Convert degrees to radians"

doubleprecision,intent(in) :: degrees               ! input degrees to convert to radians
   d2r_d=degrees/Deg_Per_Rad                        ! do the unit conversion
end function d2r_d
!-----------------------------------------------------------------------------------------------------------------------------------
elemental doubleprecision function d2r_i(idegrees)

! ident_78="@(#) M_pixel d2r_i(3f) Convert degrees to radians"

integer,intent(in) :: idegrees                      ! input degrees to convert to radians
   d2r_i=nint(dble(idegrees)/Deg_Per_Rad)           ! do the unit conversion
end function d2r_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_pixel
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
