










module M_xyplot
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
use M_hybrid,     only : fetch, strgar3
use M_calculator, only : stuff
use M_calculator, only : stuffa
use M_calculator, only : igetvalue, rgetvalue ! igetvalue faster than inum0?
use M_calculator, only : iclen_calc
use M_calculator, only : inum0, rnum0, snum0
use M_calculator, only : strgar2
use M_calculator, only : expression

implicit none
private
! ident_1="@(#)M_xyplot::xyplot(3f): XY plot procedures"
!===================================================================================================================================

!     contains sizes for the main global data xy_array
integer,parameter    :: IMAXQ2=3000000  ! maximum number of values
integer,parameter    :: IHNQ2=20000     ! maximum number of time curves in local buffer
                                        ! biggest curve to plot has IMAXQ2/2-2 points
                                        ! to demultiplex at all, number of curves + max points per curve <= IMAXQ2, larger is faster
!===================================================================================================================================
integer            ::  ifooq2     ! default file unit specified with the f command
!===================================================================================================================================
integer,parameter  :: NUNITSQ=2000                ! number of units in the bdunits block data
integer,parameter  :: NUNITS0Q=-20                ! space for user-defined labels
character(len=80)  :: uq(NUNITS0Q:NUNITSQ)        ! label storage
integer            :: iu1q(NUNITS0Q:NUNITSQ)      ! alternate units for relate command
integer            :: iu4q(NUNITS0Q:NUNITSQ)      ! true/false to detect whether relate conversions loop or not
real               :: u2q(NUNITS0Q:NUNITSQ)       ! multiplier for relate command
real               :: u3q(NUNITS0Q:NUNITSQ)       ! constant for relate command
!===================================================================================================================================
integer,parameter    :: ICSQ=500          ! number of text/text box/arrows added with t command , maximum number of curves per plot
!! should separate number of curves from number of text boxes

character(len=4096)  :: lgndq(-2:ICSQ)                        ! default legend labels
character(len=4096)  :: lgnduq(-2:ICSQ)                       ! user-specified legend labels

character(len=4096)  :: tuq(ICSQ)                             ! user-specified text strings
integer            :: itucq(ICSQ)                           ! text color
real               :: tuaszq(ICSQ)                          ! arrow size
integer            :: itumaxq                               ! index of highest non-blank text string
real               :: tuxq(ICSQ),tuyq(ICSQ)                 ! position of text strings
real               :: taxq(ICSQ),tayq(ICSQ),taoq(ICSQ)      ! position of arrows, origin and direction of arrow
real               :: tusq(ICSQ)                            ! text size
integer            :: itoq(ICSQ)                            ! flag if line is on or off
integer            :: itujq(ICSQ)                           ! text justification
character(len=10)  :: tufq(ICSQ)                            ! text font name
integer            :: itubq(ICSQ)                           ! flag if text is in box or not
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
! STUFF TO MOVE TO GRAPHICS AXIS COMMON, MORE THAN LIKELY
!===================================================================================================================================
integer            :: iforeq,ibackq,imidlq   ! foreground, background, middleground color
integer            :: ipenq
real               :: aymulq, ayconq         ! multiplier and constant for a right y axis scaled from the left axis
real               :: axmulq, axconq         ! multiplier and constant for a top x axis scaled from the bottom axis
character(len=255) ::  axislq(4)             ! axis labels
integer            :: icrvsq                 ! number of curves specified on last plot command
integer            :: icrvs2q                ! number of curves that go to left axis
integer            :: ilongq2                ! minimum number of digits to reserve for numeric ylabel
                                             ! (used to line up axis, 0 means automatically calculate)
real               :: RANGEQ(8)
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
integer            :: idatq           ! 0=left, 1=right  => used my jumath

integer,parameter  :: ifnq=5000       ! maximum number of attached data sets
character(len=255) :: filesq(ifnq)    ! original filenames or converted zebra filenames
character(len=255) :: filesinq(ifnq)  ! original filenames
integer,parameter  :: ifn_hq=30000    ! add this number to dataset number to get Fortran unit number for header file
integer,parameter  :: ifn_dq=50000    ! add this number to dataset number to get Fortran unit number for data file
integer            :: ifrecsq(0:ifnq) ! number of variables in a particular file
integer            :: iftitlq(0:ifnq) ! number of title lines in an attached file
character(len=10)  :: zebraq(0:ifnq)  ! close status for zebra files (delete/keep)
integer            :: isetsq(ifnq)    ! number of data values per variable in a file
integer            :: ifilsq          ! number of files initialized

integer            :: ireg2q          ! number of lines written to pseudo data file
integer            :: ireg3q          ! number of time curves in pseudo file

!                         itq:
!                                1. file number original and pseudo copy are on
!                                2. pseudo file curve number (position after original end of file)
!                                3. number of points in the curve
!                                4. original file curve number for time lines
integer            :: itq(4,IHNQ2)    ! keep track of pseudo file time curves

integer            :: icodeq      ! plot code
character(len=8)   :: varidq      ! variable id
integer            :: nodq        ! node
integer            :: isnodq      ! subnode
integer            :: itnodq      ! tertiary node
integer            :: iunitq      ! units code
character(len=80)  :: alphaq      ! alpha field
integer,parameter  :: KAQ=20      ! limit on length of alphaq string in standard file
character(len=80)  :: alpha2q     ! second alpha field in zebra file
integer            :: itimeq      ! time pointer
!     if this file has been read before, first extra integer in a header
!     line may be the number of points in curve that are significant
integer                :: ipadq(5)      ! extra integers. Not set by user, used internally by PLT
                                        !   ipadq(1)  ! number of significant points in data
                                        !   ipadq(2)  ! number of user points to ignore in zebra file
                                        !   ipadq(3)  ! zebra file line number where user data begins
                                        !   ipadq(4)  ! zebra file line number where header data goes
                                        ! extra integers. Set by user
                                        !   ipadq(5)  ! create xy_array association
real                   :: rpadq(9)      ! extra real values

integer                :: iscrq         ! number of lines printed since last clear
character(len=3)       :: exitq         ! flag to quit from paging mode

character(len=255)     :: temp1q        ! junk strings for miscellaneous use
real                   :: frameq        ! number of frames copied with hcopy command

integer                :: ipausq        ! 1=text pause, -1 = graphic pause, 0= no pause in xy_pause()
character(len=255)     :: pausq         ! pause string

integer                :: iseeq         ! unit number for see function and see command

!     width of demultiplexed header file (assumed at least 80 for title)
!               ultrix  sun   unicos
!     integers=   11    11     11
!     reals=       2     2      2        reals          11     11      11
!     characters= 28/4  28/4   28/8      characters= 168/4  168/4   168/8
!                 ___   ____   ____
!     words       20    20     17                       64     64      43
!     recl factor  1     4      8
!
!     zebra records are same as header records except alphaq is 80 chars, another alpha2q is 80 chars, added 11 real values

integer,parameter      :: IWZQ=64         ! number of words in demultiplexed zebra and header file
integer,parameter      :: IRECLZQ=256     ! 4*words is recl value on open of random files for zebra files
integer,parameter      :: IZSZQ=256       ! width of zebra file in characters so can size ztitle
                                          ! for ifort (Intel compiler -assume byterecl)

real                   :: xy_arrayq(IMAXQ2+IWZQ+IWZQ) ! store x-axis and y-axis data in, and to demultiplex and multiplex data in

integer,parameter      :: NUMPANEQ=36
real                   :: panesq(NUMPANEQ,4)   ! subwindows can be stored here
integer                :: ipanesq              ! current number of subpanes to use
integer                :: ipaneq               ! pointer to current panesq address
!===================================================================================================================================
real                   :: rcurvq(ICSQ,4)
real                   :: num_rcurvq
real                   :: CONQ(6)
!===================================================================================================================================
character(len=20)      :: LASTNAMEQ='futura.l'
logical lq2(0:21)
integer,parameter :: logleq2=5   ! flag to ignore leading values .le. 0 during plotting of logarithmic scales
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
! contains the static global values for polylines and axis routines
! EVERY VARIABLE ENDS IN Q2 SO CAN SPOT THE VARIABLE EASILY IN THE CODE AS BEING IN COMMON
!===================================================================================================================================
integer,parameter :: ICS2Q=500       ! maximum number of curves per plot
!===================================================================================================================================
! PEN IDS
type pen
   integer              :: color            !! idcolq2(-2:ICS2Q)     ! curve color
   integer              :: fill_style       !! idfillq2(-2:ICS2Q)    ! fill style
   integer              :: width            !! idwidq2(-2:ICS2Q)     ! curve width
   integer              :: dashcode         !! iddasq2(-2:ICS2Q)     ! dash code
   integer              :: marker           !! idmarq2(-2:ICS2Q)     ! curve marker
   integer              :: marker_frequency !! idfreq2(-2:ICS2Q)     ! number of markers
   real                 :: marker_size      !! idmszq2(-2:ICS2Q)     ! id marker size
   character(len=20)    :: marker_font      !! idcfonq2(-2:ICS2Q)    ! font used for markers
   integer              :: legend           !! idonq2(-2:ICS2Q)      ! flag on whether to draw legend line or not
end type pen

! create a variable of type PEN
type(pen),public :: PLOT_IDS(-2:ICS2Q)

integer              :: ipenq2                ! current pen style (color,width,dash code)
!===================================================================================================================================
! AXIS
!===================================================================================================================================
type axis
integer :: grid_style(4) !!    igridq2(4) ! grid style flags
                         !!     (1) =0  major x grid , 1 tic, 0 line, 2 none
                         !!     (2) =0  major y grid , 1 tic, 0 line, 2 none
                         !!     (3) =1  minor x grid , 1 tic, 0 line, 2 none
                         !!     (4) =1  minor y grid , 1 tic, 0 line, 2 none
integer :: background    !!    iboxq2   ! color of background of axis area, -1 is flag to not fill
integer :: xlogmode      !!    ilogxq2  ! =-1   flag that linear(-1) log(1) or log with linear decade plot_marks(0) AND CAN BE USED
integer :: ylogmode      !!    ilogyq2  ! =-1   flag that linear(-1) log(1) or log with linear decade plot_marks(0) AND CAN BE USED
end type axis

type(axis),public :: plot_axis

character(len=255)   ::  axlq2(4)     ! =' ' axis labels for x, y, top x, right y
!===================================================================================================================================
character(len=255)   ::  xlabel_fmtq2   ! =' ' format for numbers on bottom xaxis label
character(len=255)   ::  xlabel2_fmtq2  ! =' ' format for numbers on top xaxis label
character(len=255)   ::  ylabel_fmtq2   ! =' ' format for numbers on left xaxis label
character(len=255)   ::  ylabel2_fmtq2  ! =' ' format for numbers on right xaxis label
! optional user-specified labels for printing numbers
! blank means do automatic job
! 'x' means use $x(i) from calculator
! 'y' means use $u(i) from calculator
! '(g14.5)' means use the FORTRAN format (up to 20 characters long)
! ' ' generates a nice number
!===================================================================================================================================
character(len=20)   :: fontLq2         ! ='futura.m' default font for axis text labels
character(len=20)   :: fontNq2         ! ='futura.l' default font for axis numeric label
!===================================================================================================================================
real axmulq2,axconq2,ayconq2,aymulq2
!===================================================================================================================================
!     stuff set with the set command
integer iyuniq                        ! unit number of right axis label forced by aymulq, ayconq
integer ixuniq                        ! unit number of top axis label forced by axmulq, axconq
integer ixuniq2,iyuniq2
!===================================================================================================================================
real orientlq2  ! =90.0  angle to draw left y-axis label at; 90 and 270 are only reasonable values
real orientrq2  ! =90.0  angle to draw right y-axis label at; 90 and 270 are only reasonable values
!===================================================================================================================================
!     call plot_page() to initialize these
real XMIN0Q2 ! =xsmall  ! window area for entire display, leave alone
real XMAX0Q2 ! =xlarge
real YMIN0Q2 ! =ysmall
real YMAX0Q2 ! =ylarge
!     optionally call plot_set_plot_area() to shrink these to less than a full page
real XMINQ2  ! =xsmall  ! window area of current area drawing in
real XMAXQ2  ! =xlarge
real YMINQ2  ! =ysmall
real YMAXQ2  ! =ylarge
!===================================================================================================================================
integer ilabel_wq2     ! width to draw software label strings along axis
integer imin_wq2       ! width to draw software numeric strings along axis
logical isolidq2       ! override current pen style and draw a solid line; for error bars
logical ifixedlabq2    ! whether axis labels are fixed or proportional text
logical ifixednumq2    ! whether numbers are fixed or proportional text
integer ixlabel_chq2   ! default number of characters along axis for setting initial label character size
integer ixmin_chq2     ! default number of characters along axis for setting initial numeric character size
character(len=20)     ::  deviceq2  ! device driver name in case it matters
!===================================================================================================================================
!     THIS WILL PROBABLY BE CHANGED
integer icrvs2q2
integer icrvsq2        ! number of curves in last plot call, used with icrvs2q2 to see if dual axis or not
!===================================================================================================================================
!     call plot_set_nice_range to initialize these for a single-axis plot
!-----------------------------------------------------------------------------------------------------------------------------------
integer ixdivq2(4), iydivq2(4)
! ixdivq2(1)     ! number of major x tics
! ixdivq2(2)     ! number of minor x tics

! ixdivq2(3)     ! number of major x tics for second xaxis
! ixdivq2(4)     ! number of minor x tics for second xaxis

! iydivq2(1)     ! number of major y tics
! iydivq2(2)     ! number of minor y tics

! iydivq2(3)     ! number of major y tics for second yaxis
! iydivq2(4)     ! number of minor y tics for second yaxis
!===================================================================================================================================
real valsq2(11)
!     number values
!     1   marker spacing (0 means no marker)
!     2   marker scale
!     3
!     4-7 window range for scaled plot region
!     8-11 window range for second scaled plot region (right axis plots)
!     negative values are for strings
!     1   marker character
!===================================================================================================================================
real TICLNXQ2      ! the length of axis tic plot_marks calculated on a per-plot basis
real TICLNYQ2

real TICLNX2Q2     ! the length of axis minor tic plot_marks calculated on a per-plot basis
real TICLNY2Q2

real TEXTTICXQ2    ! the text offset from the axis due to tic plot_marks
real TEXTTICYQ2

real TICPRXQ2      ! the percentage of the longest axis to make the tic (TICLNQ2=TICPRXQ2*tlong)
real TICPRYQ2
real TICPRX2Q2     ! the percentage of the longest axis to make the minor tic (TICLNQ2=TICPRXQ2*tlong)
real TICPRY2Q2
!===================================================================================================================================
!     FOR MARKER PATTERNS
integer,parameter         :: ipensq2=20
integer,parameter         :: igeoq2=31 ! what to do with pens from 20 to 31?
integer,parameter         :: ipq2=7
doubleprecision,parameter :: piq2=3.14159265358979323846264338327950288419716939937510d0 ! 180 degrees
real,save                 :: shq2(ipensq2,ipq2)
!===================================================================================================================================
public plot_init
public plot_init_globals
public plot_jupage
public plot_juparea
public plot_page_aspect       ! paspect
public plot_drawplot          ! juplot
public plot_clear
public plot_axes              ! juaxis
public plot_getdatarange
public xy_setdatarange        ! setdatarange
public plot_hcopy

public plot_id                ! juid
public plot_jut               ! jut
public plot_label             ! plotlabel
public plot_line              ! plotline
public plot_marks             ! marks
public plot_page              ! plotpage
public plot_resetplot         ! resetplot
public plot_set_nice_range    ! setnicerange1
public plot_set_plot_area     ! setparea
public plot_get_plot_area     ! getparea
public plot_set_xmin          ! set_xmin
public plot_setdash           ! jusetdash
public plot_setfill           ! setfill
public plot_setmark           ! setmark
public plot_setticper         ! setticper
public plot_storage           ! storage
public plot_sz                ! jusz
public plot_title
public plot_toggle
public plot_zmode             ! zmode
public plot_z                 ! z

public xy_jumapc              ! jumapc
public xy_getscale            ! getscale
public xy_jutitlx
public xy_resetpens           ! resetpens
public xy_judraw              ! judraw
public xy_jucurv              ! jucurv
public xy_units               ! units
public xy_getmark             ! getmark
public xy_jugetwn             ! jugetwn
public xy_con_y               ! cony
public xy_setcnv              ! setcnv
public xy_con_x               ! conx
public xy_jurang              ! jurang
public xy_line                ! jupline
public xy_toa                 ! toa
public xy_getrel              ! getrel
public xy_getdat              ! getdat
public xy_juput               ! juput
public xy_ifdrawn             ! ifdrawn
public xy_setsize             ! setsize
public xy_aspct               ! aspct
public xy_pickpnt             ! pickpnt calculate value from screen for first curve in list
public xy_obj12345            ! obj12345
public xy_juprint             ! juprint

public illusion

public test_suite_M_xyplot

private priv_getpage            ! getpage
private priv_jusym              ! jusym
private priv_jufill             ! jufill
private priv_judash             ! judash
private priv_solidline          ! solidline
private priv_toscale            ! toscale
private priv_hilow              ! hilow
private priv_logrng             ! logrng
private priv_setrng             ! setrng
private priv_endgrid            ! endgrid
private priv_jugrid             ! jugrid
private priv_drawseg_using_pen  ! juseg
private priv_jubox              ! jubox
private priv_draw_y_nums        ! draw_y_nums
private priv_draw_y_label       ! draw_y_label
private priv_adjusty            ! adjusty
private priv_drawx              ! drawx
private priv_labelx             ! labelx
private priv_labely             ! labely
private priv_findlongest        ! findlongest
private priv_fontchng
private priv_doescape
private priv_zqjreset
private priv_justrlen
private priv_justr

logical,public :: LLQ(0:23)
integer,public,parameter     :: lsystemq=1   ! flag permitting passing unknown commands to system
integer,public,parameter     :: lmathq=2     ! flag indicating whether program is in calculator mode
integer,public,parameter     :: lnoisyq=3    ! flag as to display commands not typed in or not
integer,public,parameter     :: unused1q=4   !
integer,public,parameter     :: logleq=5     ! flag to ignore leading values .le. 0 during plotting of logarithmic scales
integer,public,parameter     :: livpq=6      ! flag to make initial value plots
integer,public,parameter     :: logxq=7      ! flag that linear is default or logarithmic
integer,public,parameter     :: logyq=8      ! flag that linear is default = -1; 0 is logarithmic
integer,public,parameter     :: ltitlq=9     ! flag that title is to be printed
integer,public,parameter     :: lxactq=10    ! flag exact grid values are to be used for grid, xmin, xmax, ymin ,ymax
integer,public,parameter     :: lnormalq=11  ! flag whether plot command should set up and clear or not
integer,public,parameter     :: ltextq=12    ! flag indicating whether program is in text writing mode
integer,public,parameter     :: lshowq=13    ! flag indicating whether relate is in dual mode or not
integer,public,parameter     :: luppq=14     ! flag indicating whether all units labels should be returned as uppercase
integer,public,parameter     :: iidfxedq=15  ! flag if id strings are fixed font (T) or not
integer,public,parameter     :: itifxedq=16  ! flag if title strings are fixed font (T) or not
integer,public,parameter     :: ixmfxedq=17  ! flag if xmin strings are fixed font (T) or not
integer,public,parameter     :: ixlfxedq=18  ! flag if xlabel strings are fixed font (T) or not
integer,public,parameter     :: ibafxedq=19  ! flag if ban strings are fixed font (T) or not
integer,public,parameter     :: lcommq=20    ! flag indicating whether program is in comment mode
integer,public,parameter     :: lzoomq=21    ! flag indicating whether program is zoom mode
integer,public,parameter     :: ldebugq=22   ! flag indicating whether in debug mode or not
integer,public,parameter     :: lechoq=23    ! flag to echo every line in jun() to stdout
public imaxq2
public lgndq
public lgnduq
public xy_arrayq
public filesq

public filesinq
public iforeq
public icsq
public ifn_dq
public ifn_hq
public ifnq
public ifrecsq
public ipaneq
public ipanesq
public ireg2q
public ireg3q
public iscrq
public izszq
public numpaneq
public panesq
public orientlq2, orientrq2
public axconq,    axconq2,        axmulq,        axmulq2,       ayconq,      ayconq2
public aymulq,    aymulq2,        deviceq2,      ibackq,        icrvs2q
public icrvs2q2,  icrvsq,         icrvsq2,       imidlq
public ipenq,     ipenq2,         ixdivq2,       ixlabel_chq2,  ixmin_chq2,  ixuniq
public ixuniq2,   iydivq2,        iyuniq,        iyuniq2,       logleq2
public lq2,       xlabel2_fmtq2,  xlabel_fmtq2,  xmax0q2,       xmaxq2,      xmin0q2
public xminq2,    ylabel2_fmtq2,  ylabel_fmtq2,  ymax0q2,       ymaxq2,      ymin0q2
public yminq2
public ipq2, ipensq2, shq2, igeoq2, piq2
public axlq2
public itoq
public itubq
public itucq
public itumaxq
public taoq
public tuaszq
public tufq
public tuq
public tusq
public tuxq
public tuyq
public itujq
public taxq
public tayq
public rangeq
public nunits0q, nunitsq
public ifooq2
public ilongq2
public ifilsq
public fontLq2
public fontNq2
public ifixedlabq2
public ifixednumq2
public ilabel_wq2
public imin_wq2
public axislq
public iwzq
public IRECLZQ

public kaq
public alphaq
public icodeq
public idatq
public itimeq
public iunitq
public varidq
public ipadq
public frameq

public iseeq
public exitq, ipausq
public iu1q, u2q, u3q, iftitlq
public zebraq
public nodq
public isnodq
public itnodq
public alpha2q
public pausq
public rpadq
public isetsq

public uq
public iu4q
public itq
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_init_globals(3f) - [M_xyplot] call xy_init_labels(3f) an xy_init_markers(3f)
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_init_globals()
implicit none
   call xy_init_labels()
   call xy_init_markers()
end subroutine plot_init_globals
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_init(3f) - [M_xyplot] initialize command language and graphics mode to set up starting interpretation
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_init()
implicit none
logical, save :: called=.false.
if(called)then
   call xy_init_default()
else
   called=.true.
   call xy_init_default()                                      ! initialize command language
   call plot_init_globals()
   call xy_init_graphics()                                     ! initialize graphics and more of program
endif
end subroutine plot_init
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_init_labels(3f) - [M_xyplot] data defining all unit code labels
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_init_labels()
implicit none
uq(  0)='                                        '
uq(  1)='Acceleration  (ft/s2)                   '
uq(  2)='Acceleration  (m/s2)                    '
uq(  3)='Acceleration  (g)                       '
uq(  4)='Alkalinity  (mg/kg)                     '
uq(  5)='Angular Velocity  (rad/s)               '
uq(  6)='Area  (in2)                             '
uq(  7)='Area  (ft2)                             '
uq(  8)='Area  (cm2)                             '
uq(  9)='Area  (m2)                              '
uq( 10)='Boron Concentration  (ppm)              '
uq( 11)='Boron Concentration  (mg/kg)            '
uq( 12)='Boron Worth  (pcm/ppm)                  '
uq( 13)='Break Flow  (fon)                       '
uq( 14)='Channel Heat Flux  (fon)                '
uq( 15)='Choking Index                           '
uq( 16)='Circumferential Strain  (%)             '
uq( 17)='Clad Average Temperature  (F)           '
uq( 18)='Clad Average Temperature  (K)           '
uq( 19)='Clad Average Temperature  (C)           '
uq( 20)='Clad Inner Temperature  (F)             '
uq( 21)='Collapsed Liquid Level  (ft)            '
uq( 22)='Collapsed Liquid Level  (m)             '
uq( 23)='Concentration  (ppm)                    '
uq( 24)='Concentration  (mg/kg)                  '
uq( 25)='Conductivity  (mu-ohm/cm)               '
uq( 26)='Coolant Activity  (mr/hr)               '
uq( 27)='Core Average Temperature  (F)           '
uq( 28)='Core Flow  (Fraction of Initial)        '
uq( 29)='Cpu Seconds                             '
uq( 30)='Critical Heat Flux  (Btu/s-ft2)         '
uq( 31)='Critical Heat Flux  (Btu/hr-ft2)        '
uq( 32)='Critical Heat Flux  (W/m2)              '
uq( 33)='Critical Heat Flux  (kW/m2)             '
uq( 34)='Cumulative Heat Generation  (Btu)       '
uq( 35)='Cumulative Heat Generation  (kW-hr)     '
uq( 36)='Cumulative Heat Generation  (kJ)        '
uq( 37)='Cumulative Heat Generation  (Btu/ft)    '
uq( 38)='Cumulative Heat Generation  (kJ/m)      '
uq( 39)='Cumulative Heat Transfer  (Btu)         '
uq( 40)='Cumulative Heat Transfer  (kW-hr)       '
uq( 41)='Cumulative Heat Transfer  (Btu/ft)      '
uq( 42)='Cumulative Heat Transfer  (kW-s/ft)     '
uq( 43)='Cumulative Heat Transfer  (kJ/m)        '
uq( 44)='Current  (mA)                           '
uq( 45)='Current  (A)                            '
uq( 46)='Decibels  (db)                          '
uq( 47)='Density  (lbm/ft3)                      '
uq( 48)='Density  (g/cc)                         '
uq( 49)='Density  (kg/m3)                        '
uq( 50)='Depressurization Rate  (psi/s)          '
uq( 51)='Depressurization Rate  (kPa/s)          '
uq( 52)='Depth  (in)                             '
uq( 53)='Depth  (ft)                             '
uq( 54)='Depth  (cm)                             '
uq( 55)='Depth  (m)                              '
uq( 56)='Derivative                              '
uq( 57)='Dh/dt  (Btu/lbm-s)                      '
uq( 58)='Dh/dv  (Btu/ft3)                        '
uq( 59)='D(hw)/dw  (Btu/lbm)                     '
uq( 60)='Diameter  (in)                          '
uq( 61)='Diameter  (ft)                          '
uq( 62)='Diameter  (cm)                          '
uq( 63)='Diameter  (m)                           '
uq( 64)='Diametral Gap  (in)                     '
uq( 65)='Diametral Gap  (ft)                     '
uq( 66)='Diametral Gap  (cm)                     '
uq( 67)='Diametral Gap  (m)                      '
uq( 68)='Difference                              '
uq( 69)='Differential Pressure  (in-h2o)         '
uq( 70)='Differential Pressure  (psi)            '
uq( 71)='Differential Pressure  (mbar)           '
uq( 72)='Differential Pressure  (bar)            '
uq( 73)='Differential Pressure  (m-h2o)          '
uq( 74)='Differential Pressure  (Pa)             '
uq( 75)='Differential Pressure  (kPa)            '
uq( 76)='Differential Pressure  (MPa)            '
uq( 77)='Differential Pressure  (in-Hg)          '
uq( 78)='Differential Pressure  (mm-Hg)          '
uq( 79)='Differential Rod Worth                  '
uq( 80)='Discharge Coefficient                   '
uq( 81)='Displacement  (in)                      '
uq( 82)='Displacement  (ft)                      '
uq( 83)='Displacement  (mm)                      '
uq( 84)='Displacement  (cm)                      '
uq( 85)='Displacement  (m)                       '
uq( 86)='Distance  (in)                          '
uq( 87)='Distance  (ft)                          '
uq( 88)='Distance  (cm)                          '
uq( 89)='Distance  (m)                           '
uq( 90)='DNB Ratio                               '
uq( 91)='Eckert Number                           '
uq( 92)='Elevation  (in)                         '
uq( 93)='Elevation  (ft)                         '
uq( 94)='Elevation  (cm)                         '
uq( 95)='Elevation  (m)                          '
uq( 96)='Elongation  (in)                        '
uq( 97)='Elongation  (mm)                        '
uq( 98)='Elongation  (%)                         '
uq( 99)='Energy  (Btu)                           '
uq(100)='Energy  (kJ)                            '
uq(101)='Energy  (MW-hr)                         '
uq(102)='Energy Flow Rate  (Btu/s)               '
uq(103)='Energy Flow Rate  (MBtu/s)              '
uq(104)='Energy Flow Rate  (kJ/s)                '
uq(105)='Energy Flow Rate  (MJ/s)                '
uq(106)='Enthalpy  (Btu/lbm)                     '
uq(107)='Enthalpy  (kJ/kg)                       '
uq(108)='Enthalpy Flow Rate  (Btu/s)             '
uq(109)='Equilibrium Quality                     '
uq(110)='Euler Number                            '
uq(111)='Fission Count Rate  (count/s)           '
uq(112)='Flooding Rate  (in/s)                   '
uq(113)='Flooding Rate  (ft/s)                   '
uq(114)='Flooding Rate  (cm/s)                   '
uq(115)='Flooding Rate  (m/s)                    '
uq(116)='Flow Quality                            '
uq(117)='Flow Regime                             '
uq(118)='Fluid Temperature  (F)                  '
uq(119)='Fluid Temperature  (K)                  '
uq(120)='Fluid Temperature  (C)                  '
uq(121)='Force  (lbf)                            '
uq(122)='Force  (N)                              '
uq(123)='Fraction of Nominal                     '
uq(124)='Frequency  (Hz)                         '
uq(125)='Frequency  (kHz)                        '
uq(126)='Friction Factor                         '
uq(127)='Friction Factor  (ft/gpm2)              '
uq(128)='Froude Number                           '
uq(129)='Fuel Average Temperature  (F)           '
uq(130)='Fuel Average Temperature  (K)           '
uq(131)='Fuel Average Temperature  (C)           '
uq(132)='Gap  (in)                               '
uq(133)='Gap  (cm)                               '
uq(134)='Gap Conductance  (Btu/s-ft2-F)          '
uq(135)='Gap Conductance  (Btu/hr-ft2-F)         '
uq(136)='Gap Conductance  (W/m2-K)               '
uq(137)='Gap Conductance  (kW/m2-K)              '
uq(138)='Grashof Number                          '
uq(139)='Head  (ft-h2o)                          '
uq(140)='Head  (m-h2o)                           '
uq(141)='Heat Capacitance  (Btu/lbm-F)           '
uq(142)='Heat Capacitance  (kJ/kg-K)             '
uq(143)='Heat Flux  (Btu/s-ft2)                  '
uq(144)='Heat Flux  (Btu/hr-ft2)                 '
uq(145)='Heat Flux  (W/m2)                       '
uq(146)='Heat Flux  (kW/m2)                      '
uq(147)='Heat Transfer Coeff  (Btu/s-ft2-F)      '
uq(148)='Heat Transfer Coeff  (Btu/hr-ft2-F)     '
uq(149)='Heat Transfer Coeff  (W/m2-K)           '
uq(150)='Heat Transfer Coeff  (kW/m2-K)          '
uq(151)='Heat Transfer Rate  (Btu/hr)            '
uq(152)='Heat Transfer Rate  (Btu/s)             '
uq(153)='Heat Transfer Rate  (W)                 '
uq(154)='Heat Transfer Rate  (kW)                '
uq(155)='Heat Transfer Regime                    '
uq(156)='Heat of Vaporization  (Btu/lbm)         '
uq(157)='Heat of Vaporization  (kJ/kg)           '
uq(158)='Integral                                '
uq(159)='Integral Flooding Rate  (in-h2o)        '
uq(160)='Integral Flooding Rate  (ft-h2o)        '
uq(161)='Integral Flooding Rate  (cm-h2o)        '
uq(162)='Integral Flooding Rate  (m-h2o)         '
uq(163)='Integrated Heat Flux  (Btu/ft2)         '
uq(164)='Integrated Heat Flux  (kW-s/m2)         '
uq(165)='Integrated Mass Flow  (lbm)             '
uq(166)='Integrated Mass Flow  (kg)              '
uq(167)='Integrated Mass Flux  (lbm/ft2)         '
uq(168)='Integrated Mass Flux  (kg/m2)           '
uq(169)='Integrated Volumetric Flow  (ft3)       '
uq(170)='Integrated Volumetric Flow  (m3)        '
uq(171)='Interfacial Area  (ft2/ft3)             '
uq(172)='Interfacial Area  (m2/m3)               '
uq(173)='Interfacial Velocity  (ft/s)            '
uq(174)='Interfacial Velocity  (m/s)             '
uq(175)='Julian Day                              '
uq(176)='K(z)                                    '
uq(177)='Kutateladze Number                      '
uq(178)='Leidenfrost Temperature  (F)            '
uq(179)='Leidenfrost Temperature  (K)            '
uq(180)='Leidenfrost Temperature  (C)            '
uq(181)='Length  (in)                            '
uq(182)='Length  (ft)                            '
uq(183)='Length  (cm)                            '
uq(184)='Length  (m)                             '
uq(185)='Linear Heat Generation Rate  (kW/ft)    '
uq(186)='Linear Heat Generation Rate  (kW/m)     '
uq(187)='Liquid Density  (lbm/ft3)               '
uq(188)='Liquid Density  (kg/m3)                 '
uq(189)='Liquid Density  (g/cc)                  '
uq(190)='Liquid Level  (in)                      '
uq(191)='Liquid Level  (ft)                      '
uq(192)='Liquid Level  (cm)                      '
uq(193)='Liquid Level  (m)                       '
uq(194)='Liquid Specific Heat  (Btu/lbm-F)       '
uq(195)='Liquid Specific Heat  (kJ/kg-K)         '
uq(196)='Liquid Spec. Internal Energy  (Btu/lbm) '
uq(197)='Liquid Spec. Internal Energy  (kJ/kg)   '
uq(198)='Liquid Temperature  (F)                 '
uq(199)='Liquid Temperature  (K)                 '
uq(200)='Liquid Temperature  (C)                 '
uq(201)='Liquid Velocity  (ft/s)                 '
uq(202)='Liquid Velocity  (m/s)                  '
uq(203)='Liquid Viscosity  (lbm/ft-hr)           '
uq(204)='Liquid Viscosity  (cp)                  '   ! centipoise
uq(205)='Loop Temperature  (F)                   '
uq(206)='Loop Temperature Differential  (F)      '
uq(207)='Mach Number                             '
uq(208)='Martinelli Number                       '
uq(209)='Mass  (lbm)                             '
uq(210)='Mass  (kg)                              '
uq(211)='Mass Flow Rate  (lbm/s)                 '
uq(212)='Mass Flow Rate  (lbm/hr)                '
uq(213)='Mass Flow Rate  (klbm/s)                '
uq(214)='Mass Flow Rate  (Mlbm/hr)               '
uq(215)='Mass Flow Rate  (kg/s)                  '
uq(216)='Mass Flow Rate  (kg/hr)                 '
uq(217)='Mass Flow Rate  (% Nominal)             '
uq(218)='Mass Flux  (lbm/s-ft2)                  '
uq(219)='Mass Flux  (lbm/hr-ft2)                 '
uq(220)='Mass Flux  (Mlbm/hr-ft2)                '
uq(221)='Mass Flux  (kg/s-m2)                    '
uq(222)='Mass Flux  (kg/hr-m2)                   '
uq(223)='Mixture Enthalpy  (Btu/lbm)             '
uq(224)='Mixture Enthalpy  (kJ/kg)               '
uq(225)='Mixture Level  (in)                     '
uq(226)='Mixture Level  (ft)                     '
uq(227)='Mixture Level  (cm)                     '
uq(228)='Mixture Level  (m)                      '
uq(229)='Mixture Quality                         '
uq(230)='Mixture Velocity  (ft/s)                '
uq(231)='Mixture Velocity  (m/s)                 '
uq(232)='Mixture Void Fraction                   '
uq(233)='Moment  (in-lbf)                        '
uq(234)='Moment  (ft-lbf)                        '
uq(235)='Moment  (N-m)                           '
uq(236)='Momentum Flux  (lbm/ft-s2)              '
uq(237)='Momentum Flux  (klbm/ft-s2)             '
uq(238)='Momentum Flux  (kg/m-s2)                '
uq(239)='Momentum Flux  (mg/m-s2)                '
uq(240)='Neutron Flux  (N/cm2-s)                 '
uq(241)='Neutron Flux  (10e13 N/cm2-s)           '
uq(242)='Normalized Power                        '
uq(243)='Nuclear Power  (fon)                    '
uq(244)='Nusselt Number                          '
uq(245)='Oxidation Reduction Potential  (MV)     '
uq(246)='Oxidation Rate  (%/s)                   '
uq(247)='Oxide Thickness  (mil)                  '
uq(248)='Oxide Thickness  (mm)                   '
uq(249)='Oxide Thickness  (%)                    '
uq(250)='Peaking Factor                          '
uq(251)='Percent                                 '
uq(252)='Period  (s)                             '
uq(253)='Power  (Btu/hr)                         '
uq(254)='Power  (hp)                             '
uq(255)='Power  (kW)                             '
uq(256)='Power  (MW)                             '
uq(257)='Power  (gw)                             '
uq(258)='Prandtl Number                          '
uq(259)='Pressure  (psia)                        '
uq(260)='Pressure  (psig)                        '
uq(261)='Pressure  (psi)                         '
uq(262)='Pressure  (bar)                         '
uq(263)='Pressure  (Pa)                          '
uq(264)='Pressure  (kPa)                         '
uq(265)='Pressure  (MPa)                         '
uq(266)='Product                                 '
uq(267)='Pump Inertia  (rpm/ft-s-lbf)            '
uq(268)='Pump Speed  (rpm)                       '
uq(269)='Pump Speed  (rad/s)                     '
uq(270)='Pump Speed  (% Nominal)                 '
uq(271)='Pump Torque  (ft-lbf)                   '
uq(272)='Pump Torque  (%)                        '
uq(273)='Quality                                 '
uq(274)='Quality  (%)                            '
uq(275)='Quotient                                '
uq(276)='Radians                                 '
uq(277)='Radius  (in)                            '
uq(278)='Radius  (ft)                            '
uq(279)='Radius  (cm)                            '
uq(280)='Radius  (m)                             '
uq(281)='Rayleigh Number                         '
uq(282)='Ratio                                   '
uq(283)='Reactivity  (dk/K)                      '
uq(284)='Reactivity  (pcm)                       '
uq(285)='Reactivity  ($)                         '
uq(286)='Reactivity Defect  (dk)                 '
uq(287)='Reynolds Number                         '
uq(288)='Rod Position  (steps)                   '
uq(289)='Rod Speed  (steps/s)                    '
uq(290)='Rotational Speed  (rpm)                 '
uq(291)='Rotational Speed  (rad/s)               '
uq(292)='Saturated Liquid Enthalpy  (Btu/lbm)    '
uq(293)='Saturated Vapor Enthalpy  (Btu/lbm)     '
uq(294)='Saturated Liquid Specific Vol  (ft3/lbm)'
uq(295)='Saturated Vapor Specific Vol  (ft3/lbm) '
uq(296)='Saturation Pressure  (psia)             '
uq(297)='Saturation Pressure  (Pa)               '
uq(298)='Saturation Pressure  (kPa)              '
uq(299)='Saturation Pressure  (MPa)              '
uq(300)='Saturation Temperature  (F)             '
uq(301)='Saturation Temperature  (K)             '
uq(302)='Saturation Temperature  (C)             '
uq(303)='Shutdown Margin  (%)                    '
uq(304)='Slip Ratio                              '
uq(305)='Specific Entropy  (kJ/kg-K)             '
uq(306)='Specific Entropy  (Btu/lbm-R)           '
uq(307)='Specific Gravity                        '
uq(308)='Specific Heat  (Btu/lbm-F)              '
uq(309)='Specific Heat  (J/kg-K)                 '
uq(310)='Specific Internal Energy  (Btu/lbm)     '
uq(311)='Specific Internal Energy  (kJ/kg)       '
uq(312)='Specific Volume  (ft3/lbm)              '
uq(313)='Specific Volume  (m3/kg)                '
uq(314)='Specific Volume  (cc/g)                 '
uq(315)='Stanton Number                          '
uq(316)='Static Quality                          '
uq(317)='Stored Energy  (Btu)                    '
uq(318)='Stored Energy  (kJ)                     '
uq(319)='Strain                                  '
uq(320)='Strain  (in/in)                         '
uq(321)='Strain  (mm/m)                          '
uq(322)='Strain  (%)                             '
uq(323)='Subcooling  (F)                         '
uq(324)='Subcooling  (K)                         '
uq(325)='Subcooling  (C)                         '
uq(326)='Subcooling  (Btu/lbm)                   '
uq(327)='Sum                                     '
uq(328)='Superficial Velocity  (ft/s)            '
uq(329)='Superficial Velocity  (m/s)             '
uq(330)='Superheating  (F)                       '
uq(331)='Superheating  (K)                       '
uq(332)='Superheating  (C)                       '
uq(333)='Surface Temperature  (F)                '
uq(334)='Surface Temperature  (K)                '
uq(335)='Surface Temperature  (C)                '
uq(336)='Surface Tension  (lbf/ft)               '
uq(337)='Surface Tension  (N/m)                  '
uq(338)='Temperature  (F)                        '
uq(339)='Temperature  (K)                        '
uq(340)='Temperature  (C)                        '
uq(341)='Temperature Differential  (F)           '
uq(342)='Temperature Differential  (K)           '
uq(343)='Thermal Conductivity  (Btu/hr-ft-F)     '
uq(344)='Thermal Conductivity  (kW/m-K)          '
uq(345)='Thermal Diffusivity  (ft2/s)            '
uq(346)='Thermal Diffusivity  (m2/s)             '
uq(347)='Thermodynamic Quality                   '
uq(348)='Thickness  (mil)                        '
uq(349)='Thickness  (in)                         '
uq(350)='Thickness  (ft)                         '
uq(351)='Thickness  (cm)                         '
uq(352)='Thickness  (m)                          '
uq(353)='Thickness  (%)                          '
uq(354)='Time  (ms)                              '
uq(355)='Time  (s)                               '
uq(356)='Time  (min)                             '
uq(357)='Time  (hr)                              '
uq(358)='Time  (days)                            '
uq(359)='Time After Reflood  (s)                 '
uq(360)='Timestep  (s)                           '
uq(361)='Torque  (lbf-ft)                        '
uq(362)='Torque  (N-m)                           '
uq(363)='Torque  (%)                             '
uq(364)='Twall - Tsat  (F)                       '
uq(365)='Twall - Tsat  (K)                       '
uq(366)='Two-phase Multiplier                    '
uq(367)='Valve Position  (% Open)                '
uq(368)='Valve Position  (% Closed)              '
uq(369)='Vapor Density  (lbm/ft3)                '
uq(370)='Vapor Density  (kg/m3)                  '
uq(371)='Vapor Density  (g/cc)                   '
uq(372)='Vapor Specific Heat  (Btu/lbm-F)        '
uq(373)='Vapor Specific Heat  (kJ/kg-K)          '
uq(374)='Vapor Spec. Internal Energy  (Btu/lbm)  '
uq(375)='Vapor Spec. Internal Energy  (J/kg)     '
uq(376)='Vapor Temperature  (F)                  '
uq(377)='Vapor Temperature  (K)                  '
uq(378)='Vapor Temperature  (C)                  '
uq(379)='Vapor Velocity  (ft/s)                  '
uq(380)='Vapor Velocity  (m/s)                   '
uq(381)='Vapor Viscosity  (cp)                   '   ! centipoise
uq(382)='Vapor Viscosity  (lbm/ft-hr)            '
uq(383)='Velocity  (in/s)                        '
uq(384)='Velocity  (ft/s)                        '
uq(385)='Velocity  (cm/s)                        '
uq(386)='Velocity  (m/s)                         '
uq(387)='Velocity  (furlongs/fortnight)          '
uq(388)='Viscosity  (lbm/ft-hr)                  '
uq(389)='Viscosity  (cp)                         '    ! centipoise
uq(390)='Void Fraction                           '
uq(391)='Void Fraction  (%)                      '
uq(392)='Voltage  (mV)                           '
uq(393)='Voltage  (V)                            '
uq(394)='Voltage  (Volts RMS)                    '
uq(395)='Volume  (in3)                           '
uq(396)='Volume  (ft3)                           '
uq(397)='Volume  (cc)                            '
uq(398)='Volume  (mm3)                           '
uq(399)='Volume  (l)                             '
uq(400)='Volume  (m3)                            '
uq(401)='Volumetric Flow Rate  (ft3/s)           '
uq(402)='Volumetric Flow Rate  (gpm)             '
uq(403)='Volumetric Flow Rate  (l/s)             '
uq(404)='Volumetric Flow Rate  (m3/s)            '
uq(405)='Vol. Heat Generation Rate  (Btu/s-ft3)  '
uq(406)='Vol. Heat Generation Rate  (Btu/hr-ft3) '
uq(407)='Vol. Heat Generation Rate  (kW/m3)      '
uq(408)='Volumetric Heating  (Btu/s-ft3)         '
uq(409)='Volumetric Heating  (Btu/hr-ft3)        '
uq(410)='Volumetric Heating  (kW/m3)             '
uq(411)='Weber Number                            '
uq(412)='Wetted Area Fraction                    '
uq(413)='Wetted Area  (%)                        '
uq(414)='ZrO2 Total Mass  (%)                    '
uq(415)='                                        '
uq(416)='Average Channel Heat Flux  (fon)        '
uq(417)='Faulted Loop Temperature  (F)           '
uq(418)='Hot Channel Heat Flux  (fon)            '
uq(419)='Intact Loop Temperature  (F)            '
uq(420)='Pressurizer Pressure  (psia)            '
uq(421)='Pressurizer Pressure  (psig)            '
uq(422)='Pressurizer Relief Flow  (ft3/s)        '
uq(423)='Pressurizer Water Volume  (ft3)         '
uq(424)='Steam Generator Pressure  (psia)        '
uq(425)='Steam Generator Mass  (lbm)             '
uq(426)='Vessel Inlet Temperature  (F)           '
uq(427)='Fraction of Initial                     '
uq(428)='(10**6 Btu/s)                           '
uq(429)='(10**3 lbm)                             '
uq(430)='Span  (%)                               '
uq(431)='Reactivity Insertion Rate  (pcm/s)      '
uq(432)='Core Power  (Fraction of Nominal)       '
uq(433)='Ips                                     '
uq(434)='Creep                                   '
uq(435)='Creep  (%)                              '
uq(436)='Gap  (ft)                               '
uq(437)='Internal Energy  (Btu/lbm)              '
uq(438)='Sonic Velocity  (ft/s)                  '
uq(439)='Sonic Velocity  (m/s)                   '
uq(440)='Derivative of Enthalpy  (Btu/lbm-s)     '
uq(441)='Derivative of Mass  (lbm/s)             '
uq(442)='Derivative of Flow  (lbm/s2)            '
uq(443)='Derivative of Int. Energy  (Btu/lbm-s)  '
uq(444)='Derivative of Void Fraction  (s-1)      '
uq(445)='Derivative of Temperature  (F/s)        '
uq(446)='Derivative of Quality  (s-1)            '
uq(447)='Derivative of Density  (lbm/ft3-s)      '
uq(448)='Derivative of Elevation  (ft/s)         '
uq(449)='Derivative                              '
uq(450)='Electric Power  (kW)                    '
uq(451)='Water Height  (ft)                      '
uq(452)='Water Height  (in)                      '
uq(453)='Water Height  (m)                       '
uq(454)='Water Height  (cm)                      '
uq(455)='Hydraulic Head  (ft-h2o)                '
uq(456)='Hydraulic Head  (in-h2o)                '
uq(457)='Hydraulic Head  (m-h2o)                 '
uq(458)='Hydraulic Head  (cm-h2o)                '
uq(459)='Liquid Fraction                         '
uq(460)='Liquid Fraction  (%)                    '
uq(461)='Valve Position  (Fraction Open)         '
uq(462)='Valve Position  (Fraction Closed)       '
uq(463)='Volumetric Flow Rate  (cfm)             '
uq(464)='Rod Position  (steps)                   '
uq(465)='Rod Speed  (steps/min)                  '
uq(466)='Integrated Mass Release  (klbm)         '
uq(467)='Integrated Energy Release  (MBtu)       '
uq(468)='Equivalent Cladding Reacted (%)         '
uq(469)='                                        '
uq(470)='                                        '
uq(471:NUNITSQ)='                            '

!>
!! Tue May  7 07:19:05 EDT 2002
!!
!!  Boron Worth  (pcm/pcm)                 Boron Worth  (pcm/ppm)
!!  Conductivity  (mu-mho/cm)              Conductivity  (mu-ohm/cm)
!!  Current  (MA)                          Current  (mA)
!!  Dnb Ratio                              DNB Ratio
!!  Voltage  (MV)                          Voltage  (mV)
!!  Zro2 Total Mass  (%)                   ZrO2 Total Mass  (%)
!!  Reactivity Insertion  (pcm/s)          Reactivity Insertion Rate (pcm/s)
!!  Internal Energy  (Btu/lbm-s)           Internal Energy  (Btu/lbm)

end subroutine xy_init_labels
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_init_markers(3f) - [M_xyplot] part of plot_axes(3f), used to define default geometric markers
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_init_markers()
implicit none
!                              sun planet offset lines turns dia. fill
! filled
   shq2( 1,1:ipq2)=[96.0, 1.0, 96.0, 3.0, 0.250, 1.0, 1.0] ! triangle
   shq2( 2,1:ipq2)=[96.0, 1.0, 96.0, 4.0, 0.125, 1.0, 1.0] ! square
   shq2( 3,1:ipq2)=[95.0, 1.0, 95.0, 5.0, 0.850, 1.0, 1.0] ! pentagon
   shq2( 4,1:ipq2)=[96.0, 1.0, 96.0, 6.0, 0.000, 1.0, 1.0] ! hexagon
   shq2( 5,1:ipq2)=[10.0, 2.0,  4.0,10.0, 0.250, 1.0, 1.0] ! star 5
   shq2( 6,1:ipq2)=[96.0, 1.0, 96.0, 3.0, 0.750, 1.0, 1.0] ! up triangle
   shq2( 7,1:ipq2)=[20.0, 1.0, 20.0, 4.0, 0.000, 1.0, 1.0] ! square diamond
   shq2( 8,1:ipq2)=[20.0, 1.0,  0.0,20.0, 0.000, 1.0, 1.0] ! circle
   shq2( 9,1:ipq2)=[20.0,10.0,  3.0, 4.0, 0.000, 1.0, 1.0] ! wide diamond
   shq2(10,1:ipq2)=[20.0,10.0,  3.0, 4.0, 0.250, 1.0, 1.0] ! tall diamond
! not filled
   shq2(11,1:ipq2)=[96.0, 1.0, 96.0, 3.0, 0.250, 1.0, 0.0] ! triangle
   shq2(12,1:ipq2)=[96.0, 1.0, 96.0, 4.0, 0.125, 1.0, 0.0] ! square
   shq2(13,1:ipq2)=[95.0, 1.0, 95.0, 5.0, 0.850, 1.0, 0.0] ! pentagon
   shq2(14,1:ipq2)=[96.0, 1.0, 96.0, 6.0, 0.000, 1.0, 0.0] ! hexagon
   shq2(15,1:ipq2)=[10.0, 2.0,  4.0,10.0, 0.250, 1.0, 0.0] ! star 5
   shq2(16,1:ipq2)=[96.0, 1.0, 96.0, 3.0, 0.750, 1.0, 0.0] ! up triangle
   shq2(17,1:ipq2)=[20.0, 1.0, 20.0, 4.0, 0.000, 1.0, 0.0] ! square diamond
   shq2(18,1:ipq2)=[20.0, 1.0,  0.0,20.0, 0.000, 1.0, 0.0] ! circle
   shq2(19,1:ipq2)=[20.0,10.0,  3.0, 4.0, 0.000, 1.0, 0.0] ! wide diamond
   shq2(20,1:ipq2)=[20.0,10.0,  3.0, 4.0, 0.250, 1.0, 0.0] ! tall diamond

!   data(shq2(11,jj),jj=1,ipq2)/20.0,10.0,  6.0, 8.0, 0.250, 1.0, 1.0/ ! ver. ellipse
!   data(shq2(10,jj),jj=1,ipq2)/20.0,10.0,  6.0, 8.0, 0.500, 1.0, 1.0/ ! hor. ellipse
!   data(shq2( 5,jj),jj=1,ipq2)/ 4.0, 1.0,  2.0, 8.0, 0.125, 1.0, 1.0/ ! star 4
!   data(shq2(12,jj),jj=1,ipq2)/ 8.0, 1.0,  4.0,16.0, 0.000, 1.0, 1.0/ ! star 8
!   data(shq2(13,jj),jj=1,ipq2)/ 6.0, 1.0,  4.0,12.0, 0.000, 1.0, 1.0/ ! star 6
!   data(shq2(14,jj),jj=1,ipq2)/11.0, 1.0,  7.0, 4.0, 0.125, 1.0, 1.0/ ! arrow head
!   data(shq2(15,jj),jj=1,ipq2)/18.0, 1.0,  9.0,15.0, 0.000, 1.0, 1.0/ ! trefoil

end subroutine xy_init_markers
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    plot_setfill(3f) - [M_xyplot] set background, middleground, and foreground colors for plot
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_setfill()
implicit none

! ident_2="@(#)M_xyplot::plot_setfill(3f): set background, middleground, and foreground colors for plot"

integer :: i440
integer :: ifore0

   ifore0=iforeq
   iforeq=inum0(fetch('grid_fg'))   ! check foreground color value
   ibackq=inum0(fetch('grid_bg'))   ! check background color value
   imidlq=inum0(fetch('grid_mg'))   ! check middleground color value
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ifore0.ne.iforeq)then              ! if changed foreground color, change color of all text (maybe good, maybe not)
      do i440=1,ICSQ                     ! set all text lines from t command to foreground color
         itucq(i440)=iforeq
      enddo
      plot_ids(-2)%color=iforeq          ! set minor grids to foreground color
      plot_ids(-1)%color=iforeq          ! set major grids and axis labels to foreground color
   endif
end subroutine plot_setfill
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_init_default(3f) - [M_xyplot] initialize XY plot routines
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!
!!##DESCRIPTION
!!
!!    This subroutine initializes or resets global values and the command
!!    dictionary. It is used at program startup and when the reset(1)
!!    command is used.
!!
!!    An xy_array of character strings containing the prototype for most
!!    commands is used as input to the parse routine. On the first call
!!    to xy_init_default this defines the command dictionary and sets the
!!    initial value for most of the dictionary. Each command name must
!!    be defined in order for the main program to distinguish between PLT
!!    commands and system commands. The main program tries to look up the
!!    command name in the dictionary to decide if it is a legitimate command.
!!
!!    In addition, the global logical LLQ() xy_array values are set to
!!    their defaults. This xy_array holds the switches used to control
!!    the many submodes of the program.
!!
!!    A large addition of global values (almost all of which end in the
!!    letter Q) are also set as necessary to ensure the program is reset
!!    to the proper initial modes.
!!
!!    A few environment UNIX variables are also read to establish the
!!    initial page length and banner strings and so on.
!!
!!   NOTE: Compaq/Digital Tru64 compiler warned about continued strings
!!         on lines not padded to column 72 with blanks so added
!!         line on end of continued lines; no longer required using free-format
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
subroutine xy_init_default()
use M_kracken,         only: parse, store
implicit none

! ident_3="@(#)M_xyplot::xy_init_default(3f): Initialize command dictionary and global values"

!  1989 John S. Urban
!
!  define the allowable command verbs and associated parameters and
!  set all the parameters to appropriate initial values
!
!  used at initialization and for a reset
!
character(len=24) :: bannames
character(len=6)  :: bname
integer,save      :: icalls=0 ! number of times this routine has been called
integer           :: i10
integer           :: i100
integer           :: i15
integer           :: i80
integer           :: i90
integer           :: ier
integer           :: iii
real              :: rdum
real              :: rtemp
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   iseeq=5
   ifooq2=0   ! default file number specified on f command
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   call parse('aspect   ','-oo 1 ','define')
   call parse('c        ','-oo ','define')
   call parse('exact    ','-oo off ','define')
   call parse('grid     ',' -xl 5 -xt 4 -yl 5 -yt 4 -minor off-oo on -tx 1 -ty 1 -txx 1 -tyy 1 &
                          & -major on -bg 0 -mg 0 -fg 7 -box off -w 7','define')
   call parse('right    ','-oo -m 0 -b 0 -units 0   ' ,'define')
   call parse('hcopy    ','-oo ppostscript -f "$STR(""hcopy_%."",$HDEVICE)" -sz -obj' ,'define')
   call parse('zoom     ','-oo off ','define')
   call parse('top      ','-oo -m 0 -b 0 -units 0   ' ,'define')
   call parse('le       ','-oo off  ','define')
   call parse('ivp      ','-oo off  ','define')
   call parse('id       ','-oo -t -c -w -m -d -n -on -off -fx on -fn futura.l -ch ID_CHARS -f &
                           & -W 2 -l "#N#" -sz  -start 1 -grid .false.','define')
   call parse('page     ','-oo 0 8.5 0 11','define')
   call parse('parea    ','-oo -rows 0 -cols 0 -a 1 -g 0','define')
   call parse('plot     ','-oo -vs -f -z -idonly -v -c -e 0%','define')
   call parse('relate   ','-oo -dual off -m 1 -b 0','define')
   call parse('setdash  ','-oo -p ','define')
   call parse('setmark  ','-oo -p -fill -sz -fn futura.l','define')
   call parse('sz       ','-oo 800 600 -p 0','define')
   call parse('title    ','-oo -l 1 -fn futura.l -off "#N#" -c 7 -fx off -ch TITLE_CHARS -w 4','define')
   call parse('xlabel   ','-oo -fn futura.l -ch 40 -fx off -fmt -off "#N#" -w 3 -O 0','define')
   call parse('xlog     ','-oo off  -type decade','define')
   call parse('xmax     ','-oo 123.456 -fn futura.l -w 3','define')
   call parse('xmin     ','-oo 123.456 -fn futura.l -ch 60 -fx off -w 3 -R "#N#" -r "#N#" -> "#N#"','define')
   call parse('ylabel   ','-oo -fn futura.l -fmt -w 3 -O 90 -off "#N#"','define')
   call parse('ylog     ','-oo off -type decade ','define')
   call parse('ymax     ','-oo 123.456 -fn futura.l -w 3','define')
   call parse('ymin     ','-oo 123.456 -fn futura.l -w 3','define')
   call parse('xlabel2  ','-oo -fn futura.l -fmt -off "#N#" -O 0','define')
   call parse('xmax2    ','-oo 123.456 -fn futura.l','define')
   call parse('xmin2    ','-oo 123.456 -fn futura.l','define')
   call parse('ylabel2  ','-oo -fn futura.l -off "#N#" -w 3 -fmt -O 90 ','define')
   call parse('ymax2    ','-oo 123.456 -fn futura.l -w 3','define')
   call parse('ymin2    ','-oo 123.456 -fn futura.l -w 3','define')
   call parse('t        ','-oo -l -p -c -j -box -sz -on -off -fn -a -asz 3','define') ! print text in graphics area
   call parse('d        ','-oo -f ','define')
   call parse('idbox    ','-oo -p default -c 0 -s -box -999 ','define')
   call parse('ban      ','-oo -ch 80 -w 4 -fx off -fn futura.l -br $PLTBR -bm $PLTBM -bl $PLTBL &
                                                              & -tr $PLTTR -tm $PLTTM -tl $PLTTL &
                                                           & -lt $PLTLT -lm $PLTLM -lb $PLTLB &
                                                           & -rt $PLTRT -rm $PLTRM -rb $PLTRB' ,'define')
   call parse('echoall  ','-oo off','define')
   call parse('print    ','-oo ','define')
   call parse('reset    ','-oo ','define')
   call parse('header   ','-oo -code "#N#" -name "#N# " -node "#N#" -subnode "#N#" -ternode "#N#" -units "#N#" &
                          & -alpha "#N#" -time "#N#" -points "#N#" -stat "#N#" -f -msg 6 -alpha2 "#N#" -alpha3 "#N#"','define')
   call parse('help     ','-oo -t -f " " ->','define')
   call parse('hue      ','-oo -rgb -hls -hsv ','define')
   call parse('math     ','-oo -t -o -rec 0 ->','define')
   call parse('p        ','-oo -type text','define')
   call parse('read     ','-oo ','define')
   call parse('save     ','-oo " " -t -list -type binary -ver 2','define')
   call parse('set      ','-oo','define')
   call parse('units    ','-oo -t -upper "#N#"','define')
   call parse('seefont  ','-oo ','define')
   call parse('report   ','-oo on ','define')
   call parse(';        ','-oo ','define')

   call parse('quit     ','-oo -name skull','define')
   call parse('show     ','-oo -verbs .false. -width 0','define')
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   LLQ(ldebugq)=.false.     ! debug mode
   LLQ(ibafxedq)=.false.    ! ban    text is proportionally spaced
   LLQ(iidfxedq)=.true.     ! id     text is fixed spaced
   LLQ(itifxedq)=.true.     ! title  text is fixed spaced
   LLQ(ixlfxedq)=.true.     ! xlabel text is fixed spaced
   LLQ(ixmfxedq)=.true.     ! xmin   text is fixed spaced
   LLQ(livpq)=.false.       ! for ivp
   LLQ(logleq)=.false.      ! for le
   LLQ(logxq)=.false.       ! for xlog
   LLQ(logyq)=.false.       ! for ylog
   LLQ(lshowq)=.false.      ! flag whether relate is in dual mode or not
   LLQ(ltitlq)=.true.       ! print the title
   LLQ(luppq)=.false.       ! retrieve all axis labels from units as uppercase strings
   LLQ(lxactq)=.false.      ! For exact
   LLQ(lnormalq)=.true.     ! flag whether plot command should set up and clear or not
   LLQ(lzoomq)=.false.      ! for zoom
   itumaxq=0              ! index of highest non-blank text line for t command
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  special direct stores into the command dictionary using STORE instead of PARSE
   call store('label_w','3','define',ier)
   call store('min_w','3','define',ier)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   call store('p_oo',char(07),'define',ier) ! default pause string is bell character
   call store('title_01',' ','define',ier)  ! just make sure enough room in dictionary
   call store('title_02',' ','define',ier)
   call store('title_03',' ','define',ier)
   call store('title_04',' ','define',ier)
   call store('title_05',' ','define',ier)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  set up pen defaults
   call xy_resetpens()
   do i10=1,ICSQ
      lgnduq(i10)=' '             ! use default legend
      ! set colors to basic primaries except skip yellow and background

!     text string defaults
      tuq(i10)=' '  ! string text
      itucq(i10)=7  ! string color
      tuaszq(i10)=3.0  ! arrow size
      tuxq(i10)=0.0 ! string x-position
      tuyq(i10)=0.0 ! string y-position
      tusq(i10)=1.0 ! string size
      itujq(i10)=0  ! left justify text strings
      tufq(i10)='futura.l'
      itubq(i10)=-1  ! flag to not put text in boxes
      itoq(i10)=1    ! flag that text line is on
      taoq(i10)=5.0  ! flag that arrow is not present
      taxq(i10)=0.0  ! arrow initial x value
      tayq(i10)=0.0  ! arrow initial y value
   enddo
   do i15=-2,0
      lgnduq(i15)=' '       ! use default legend
   enddo
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  set grid style values to match strings set with grid command
   call plot_resetplot()
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   iforeq=7                          ! default foreground color is white
   ibackq=0                          ! default background color is black
   imidlq=0                          ! default middleground color value
   call store('label_fn','futura.l','define',ier)
   call store('min_fn','futura.l','define',ier)
   call store('idbox_p','default','define',ier)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   ilongq2=0                             ! number of places reserved for numeric y-labels
   call stuff('FALSE',1.0d0,'')         ! a handy mnemonic
   call stuff('TRUE',0.0d0,'')          ! a handy mnemonic
   call stuff('VFALSE',0.0d0,'')        ! a handy mnemonic for M_plot logicals
   call stuff('VTRUE',1.0d0,'')         ! a handy mnemonic for M_plot logicals
   call stuff('PLTOBJECT',-1.0d0,'')    ! number of object hcopy command should copy if positive
   call stuffa('$PAUSE',' ','')    ! set by p command;
   call stuff('DETACH',ifnq,'')          ! unit number to detach if run out of room
   call stuff('LABELPLACES',0.0d0,'')   ! calculator variable that really sets ilongq2
   call stuff('PAUSE',1.0d0,'')         ! set by p command; can be used to except parcel loops
   call stuff('STATUS',0.0d0,'')        ! sometimes set by a command to say it went OK
   call stuff('ZOOM_FACTOR',10.0d0,'')  ! what percent to translate/pan when in zoom mode
   call stuff('SCALE_CHARS',0.0d0,'')   ! how many characters wide total output of $scale should be
   call stuffa('$COL_SAVE_FORMAT','(10000(g14.7,1x):)','')  ! format statement for save -type col
   call stuffa('$HDEVICE','ppsc','')  ! calculator variable for hcopy device
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   ! multiplier and constant for a top x axis scaled from the bottom axis
   axmulq=0
   axconq=0
   ! multiplier and constant for a right y axis scaled from the left axis
   aymulq=0
   ayconq=0
!  stuff set with the set command
   ixuniq=0 ! unit number of top axis label forced by axmulq, axconq
   iyuniq=0 ! unit number of right axis label forced by aymulq, ayconq
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  default number of characters used to control sizes
   rtemp=5+5+1+8+5+(5*3+20)
   call stuff('ID_CHARS',rtemp,'')
   call stuff('TITLE_CHARS',20.0d0,'')
   call stuff('LABEL_CHARS',40.0d0,'')
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   bannames='BMBRBLTMTRTLRBRMRTLBLMLT'
   do iii=1,24,2
      bname='$PLT'//bannames(iii:iii+1)
      call get_environment_variable(bname(2:6),temp1q)        ! get default string for ban command from environment variable
      call stuffa(bname,temp1q,'')
   enddo
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   call stuffa('$VERSION','6.0.0','')      ! might be handy
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  Allow abbreviation of commonly used Font control such as call stuffa('$sup','v=0.5 s=0.5','')  ! Superscript
   call stuffa('$sub','v=-0.5 s=0.5','') ! Subscript
   call stuffa('$rp','b=0 S=1','')       ! Return position/size
   call stuffa('$g','f=9','')            ! Greek
   call stuffa('$y','f=16','')           ! Symbol
   call stuffa('$rf','f=0','')           ! Return font
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   call stuff('smoothmode',0.0d0,'')
   call stuff('smoothpts',10.0d0,'')
   call stuff('PI',3.14159265358979323846d0,'')
   call stuff('E',2.71828d0,'')
   call stuff('GOLDEN_RATIO',(1.0d0+sqrt(5.0d0))/2.0d0,'')        ! Golden Ratio, around 1.61803
   rdum=rnum0('PI=3.14159265358979323846')
   rdum=rnum0('E=2.7182818284590452353602874')
   ! 1.61803398874989484820458683436563811772030917980576286213544862270526046281890...
   rdum=rnum0('GOLDEN_RATIO=(1+sqrt(5))/2')        ! Golden Ratio, around 1.61803
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   ipausq=1              ! by default, pauses are from a carriage return from standard input
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   if(icalls.ne.0)then
      call xy_jumapc('reset')
      return ! if not first call to this routine end
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  THIS CODE IS ONLY EXECUTED THE FIRST TIME THE ROUTINE IS CALLED
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   frameq=0  ! number of frames copied with the hcopy command
   iscrq=0
   exitq=' '
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   call stuff('PUT',0.0d0,'') ! initialize the PUT variable to 0
   do i80=NUNITS0Q,NUNITSQ ! blank the flag xy_array used to look for recursive unit conversion
      iu4q(i80)=0
      iu1q(i80)=0
      u2q(i80)=1.0
      u3q(i80)=0.0
   enddo
   do i90=1,ifnq ! number of title lines in each attached file
   iftitlq(i90)=0
   enddo
!  on first call to this routine set up allowable command verbs
   icalls=1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  set ranges to magic value indicating to auto max-min axis scales
   do i100=1,8
      RANGEQ(i100)=123.456
   enddo
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  aliases for commands or simple commands with no keyword  parameters
!  look up name verb_oo to see if it is in dictionary or not, so need these stub names
   call store('info_oo    ','13 ','define',ier)  ! info command
   call store('cd_oo      ',' 8 ','define',ier)  ! alias for chdir
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
end subroutine xy_init_default
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_hcopy(3f) - [M_xyplot] generate copy of the current plot for XY plot routines
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!
!!    This is the routine that generates a copy of the current plot by closing the
!!    current device and opening another, calling the same routine as the PLOT
!!    command, and then closing the new device and opening up one like you started
!!    from.
!!
!!    This works best when the current device is an interactive device such as X11 or
!!    tek; as exiting and reinitializing usually does not effect an interactive
!!    device much; but for file oriented devices the output file is restarted (as the
!!    current version of M_plot is written).
!!
!!    So, one limitation here is that if you running a batch job that is making
!!    PostScript output and you call the plot_hcopy command to generate a bitmap file, for
!!    example, your current PostScript output file is going to be restarted.
!!
!!    Consider making M_plot support concurrent open devices, or a special exit that
!!    is "temporary" and lets you resume output. Actually, I think Bernie's last
!!    version does that?
!!
!!    Note that if the -obj option is used, instead of calling the equivalent of PLOT
!!    a special object is called. This could be used by a sophisticated user, but is
!!    used to copy what shows on screen from ID, SHOWFONT, ... commands too.
subroutine plot_hcopy()
use M_journal,    only: journal
use M_strings,    only: change
use M_draw
use M_system,     only: set_environment_variable
implicit none

! ident_4="@(#)M_xyplot::plot_hcopy(3f): process hcopy command"

integer,parameter            :: dn=20                             ! device name length
character(len=dn)            :: checkdev                          ! used to check a good device name has been used
character(len=dn)            :: hold_current_dev                  ! device in use before this routine is called
character(len=255)           :: hcopy_F
character(len=20)            :: cframe

character(len=255),save      :: SVD_hcopy_SZ='-1 -1 -1 -1'
character(len=255),save      :: SVD_hcopy_F='$STR("hcopy_%.",$HDEVICE)' ! initial filename prefix
character(len=dn),save       :: SVD_hcopy_OO='ppsm'           ! default output device saved by this routine from last call
character(len=dn)            :: hcopy_OO                      ! output device to make hardcopy with

integer                      :: ier
integer                      :: lennum
integer                      :: iobj
integer                      :: ii
!-----------------------------------------------------------------------------------------------------------------------------------
! need to gather up what device is being used now,
! what device is needed for the hcopy command,
! what the hcopy counter is,
! what the desired output filename should be,
! then set up the new device, do the plot, and return to original device
!-----------------------------------------------------------------------------------------------------------------------------------
DEVICE: BLOCK
character(len=dn)           :: temp_dev
   hcopy_OO=' '
   hcopy_OO(1:dn)=snum0('$HDEVICE')                 ! ask calculator last hcopy device used (or what user set variable to)
   if(hcopy_OO.ne.' ')then
      SVD_hcopy_OO=hcopy_OO                    ! NOTE: should be checking for a valid output device name
   endif
   call xy_retrv2('hcopy_oo',temp_dev,lennum,ier)        ! check if a new device name has been requested on hcopy(1) command
   if(lennum.eq.0)then
      hcopy_OO=SVD_hcopy_OO                    ! if no device name on command line, use last one specified
   else                                                  ! device name is specified on command
      if(SVD_hcopy_OO.ne.temp_dev)then              ! if a new one clear saved sizes
         SVD_hcopy_SZ='-1 -1 -1 -1'
      endif
      SVD_hcopy_OO=temp_dev                         ! store value specified on command  as new default
      hcopy_OO=temp_dev
   endif
   call stuffa('$HDEVICE',hcopy_OO,'')         ! let calculator know last hcopy device used to use as default
END BLOCK DEVICE
!-----------------------------------------------------------------------------------------------------------------------------------
SIZE: BLOCK
character(len=255)           :: temp_sz
   call xy_retrv2('hcopy_sz',temp_sz,lennum,ier)    ! if new size was explicitly set use new new value
   if(lennum.ne.0)then
      SVD_hcopy_SZ=temp_sz
   endif
ENDBLOCK SIZE
!-----------------------------------------------------------------------------------------------------------------------------------
   frameq=frameq+1.0                                   ! number of frames copied in this program run including this one
!-----------------------------------------------------------------------------------------------------------------------------------
FILENAME: BLOCK
character(len=255)           :: temp_f
integer                      :: ilen
! build filename
   call xy_retrv2('hcopy_f',temp_f,ilen,ier)           ! get -f option from hcopy command
   if(temp_f.eq.' ')then                               ! check user-specified filename
                                                       ! no user -f value was specified
      hcopy_F=SVD_hcopy_F                    ! use current default
   else

                                                       ! see if a new default has been chosen
      if(temp_f(ilen:ilen).eq.'.')then                 ! ends in period, so make it new prefix and still add number (old method)
         SVD_hcopy_F=temp_f                       ! make new default and add % to end of name
         SVD_hcopy_F(ilen+1:ilen+1)='%'
         hcopy_F=SVD_hcopy_F                 ! use current default
      elseif(index(temp_f(:ilen),'%').ne.0) then
         SVD_hcopy_F=temp_f                       ! make new default, assuming if has a % this should be a new default (a flaw)
         hcopy_F=SVD_hcopy_F                 ! use current default
      else
         hcopy_F=temp_f                           ! literally use user filename if does not end in . or contain %
      endif
   endif
END BLOCK FILENAME
!-----------------------------------------------------------------------------------------------------------------------------------
   ! change the % character to the count
   if(frameq.le.9999)then
      write(cframe,'(''c/%/'',i4.4,''/'')')int(frameq)
      ii=9
   else
      write(cframe,'(''c/%/'',i8.8,''/'')')int(frameq)
      ii=13
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   hcopy_F(1:255)=snum0(hcopy_F)                                ! expand variable names
   call change(hcopy_F,cframe(:ii),ier)                  ! change % to number if present
!-----------------------------------------------------------------------------------------------------------------------------------
! unfortunately, cannot append to plotter file, so cannot use vnewdev.
! vnewdev also does not obey voutput; it would be nice if it would
   call vgetdev(hold_current_dev)                      ! get name of current device for restoring at end
!-----------------------------------------------------------------------------------------------------------------------------------
   if( xy_noclose(hold_current_dev) )then                   ! check if actually want to close
      call set_environment_variable('M_DRAW_DEVICE','NOCLOSE')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
LINE: BLOCK
character(len=255*4) :: temp_line
   call voutput(trim(hcopy_F))
   write(temp_line,'(6a)')' *hcopy* creating ',trim(hcopy_F),' with device ',trim(hcopy_OO),' ',trim(SVD_hcopy_SZ)
   call journal(trim(temp_line))
END BLOCK LINE
!-----------------------------------------------------------------------------------------------------------------------------------
   call xy_setsize(SVD_hcopy_SZ)                            ! process user-specified size of the output device
   call pushdev(hcopy_OO)
   call vgetdev(checkdev)                                ! if do not get back same value then the device probably did not work
   if(checkdev.eq.'nil'.or.checkdev.eq.'') then
      call journal('*hcopy* incorrect device name '//trim(hcopy_OO))
      call popdev()
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call xy_jumapc('reload')                         ! reestablish color map
   call centertext(.false.)                         ! do not center text
   call vsetflush(.false.)
   call color(ibackq)
   call clear()
   call color(iforeq)
!-----------------------------------------------------------------------------------------------------------------------------------
   iobj=inum0(fetch('hcopy_obj'))                   ! object number from hcopy command
   if(iobj.lt.0)then                                ! if not specified on command check special variable set by ID, plot_setmark, ...
      iobj=inum0('PLTOBJECT')                       ! this means hcopy should copy this object and not the plot
   endif
   if(iobj.lt.0)then
      call plot_drawplot(.true.)
   else
      call callobj(iobj)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call popdev()                                    ! set back to initial device
   call xy_jumapc('reload')                         ! reestablish color map
!-----------------------------------------------------------------------------------------------------------------------------------
   if (xy_noclose(hold_current_dev))then                    ! if this is the X11 driver, refresh the plot
      call set_environment_variable('M_DRAW_DEVICE','CLOSE')
      if(iobj.lt.0)then
         call plot_drawplot(.true.)
      else
         call callobj(iobj)
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine plot_hcopy
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_noclose(3f) - [M_xyplot] flag whether to keep current device open while switching to alternate device
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
logical function xy_noclose(dev_name)
implicit none

! ident_5="@(#)M_xyplot::plot_noclose(3f): flag whether to keep current device open while switching to alternate device"

character(len=*),intent(in) ::  dev_name
!-----------------------------------------------------------------------------------------------------------------------------------
   if (dev_name(1:3).eq.'X11'.or.dev_name(1:3).eq.'x11')then
      xy_noclose=.true.
   elseif (dev_name(1:2).eq.'PC')then
      xy_noclose=.true.
   else
      xy_noclose=.false.
   endif
end function xy_noclose
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_set_xmin(3f) - [M_xyplot] set value for xmin/xmax/ymin/... command
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_set_xmin(name,imaxv)
use M_journal, only : journal
use M_kracken, only: store, IPvalue
implicit none

! ident_6="@(#)M_xyplot::plot_set_xmin(3f): set value for xmin/xmax/ymin/... command"

character(len=IPvalue) :: temp1 ! IPvalue= length of input command or parameter
character(len=IPvalue) :: temp2
character(len=100)     :: string
character(len=*)       :: name
real                   :: rcurv1(8)
logical                :: nooutput
integer                :: i10
integer                :: i2
integer                :: i20
integer                :: i3
integer                :: i30
integer                :: i40
integer                :: ich
integer                :: ier
integer                :: ierr
integer                :: ii
integer                :: imaxv
integer                :: inums
integer                :: istart
integer                :: lennum
!-----------------------------------------------------------------------------------------------------------------------------------
   string='xmin xmax ymin ymax xmin2xmax2ymin2ymax2'
   nooutput=.true.  ! output flag -> assumed not present till found
!-----------------------------------------------------------------------------------------------------------------------------------
!  sort out which command and set ich to the correct (starting) index into RANGEQ
   ich=index(string,name(:imaxv))/5+1
   !!write(*,*)'keyword key=',ich
   if(ich.lt.1.or.ich.gt.8)then            ! string should always find a match
      call journal('*xmin* internal error')
      return
   endif
   i3=imaxv+3
   i2=imaxv+2
!-----------------------------------------------------------------------------------------------------------------------------------
! process reset switch
   temp2=name(:imaxv)//'_R'
   temp1=fetch(temp2(:i2))
   if(temp1.eq.' ')then                        ! RESET flag was present with no value
      do i30=1,8
         !!write(*,*)'reset all ',i30
         RANGEQ(i30)=123.456
      enddo
   elseif(temp1.ne.'#N#')then                  ! RESET flag was present with numeric value
      inums=inum0(temp1)
      do i40=ich,ich+inums-1
         !!write(*,*)'reset ',mod(i40-1,8)+1
         RANGEQ(mod(i40-1,8)+1)=123.456        ! reset specified number of values
      enddo
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  test if output flag is present so do not reset on command "xmin ->"
   temp1=name(:imaxv)//'_>'
   if(fetch(temp1(:i2)).ne.'#N#')then       ! if output flag present do nothing but show values
      nooutput=.false.
      !!write(*,*)'output flag present'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
! process numeric values
   temp1=name(:imaxv)//'_oo'
   call xy_retrv2(temp1(:i3),temp2,lennum,ier) ! get user values

   if(lennum.eq.0.and.nooutput)then ! if no numbers and no output flag reset by verb name
      !!write(*,*)'default reset of ',ich
      RANGEQ(ich)=123.456
   else
      call strgar2(temp2,8,rcurv1,inums,' ',ierr)
      inums=min(inums,8) ! inums counts how many are there, even if more than 8
      do i10=ich,ich+inums-1
         ii=mod(i10-1,8)+1
         !!write(*,*)'setting',ii,' to ',rcurv1(i10-ich+1),i10-ich+1,i10
         RANGEQ(ii)=rcurv1(i10-ich+1)        ! check that all values are valid numbers
      enddo
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
! process output flag AFTER any changes
   temp1=name(:imaxv)//'_>'
   if(fetch(temp1(:i2)).ne.'#N#')then       ! if output flag present do nothing but show values
   nooutput=.false.
      do i20=1,8
         istart=(i20-1)*5+1
         if(RANGEQ(i20).eq.123.456)then
            write(temp1,'(i2,a8,a)')i20,string(istart:istart+4),'     AUTO-SCALE'
         else
            write(temp1,'(i2,a8,g20.13)')i20,string(istart:istart+4),RANGEQ(i20)
         endif
         call journal(temp1)
      enddo
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   temp1=name(:imaxv)//'_fn'                         ! create xmin_fn so easy to find font
   call store('min_fn',fetch(temp1(:i3)),'replace',ier)
!-----------------------------------------------------------------------------------------------------------------------------------
   temp1=name(:imaxv)//'_w'                          ! create min_w so easy to find width
   call store('min_w',fetch(temp1(:i2)),'replace',ier)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine plot_set_xmin
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_storage(3f) - [M_xyplot] if on a storage tube, clear the screen to unclutter it
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_storage()
use M_draw, only : vgetdev
implicit none

! ident_7="@(#)M_xyplot::plot_storage(3f): if on a storage tube, clear the screen to unclutter it"

character(len=20) :: devname
   call vgetdev(devname)                          ! get name of current device
   if(devname.eq.'tek')then                       ! if on a tektronix 4014 that is not an xterm
      if(LLQ(lnormalq))call plot_clear('front')
   endif
end subroutine plot_storage
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_setmark(3f) - [M_xyplot] set or display marker geometries
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_setmark(parms,pat,ipen)
!  1992 John S. Urban
!  parms=' '     # list geometric pen information
!                # process a user plot_setmark command and store data for marker ipen
!        see xy_getmark to retrieve information for a pen
!
use M_journal, only : journal
use M_strings, only : listout
implicit none

! ident_8="@(#)M_xyplot::plot_setmark(3f): set or display marker geometries"

character(len=*)         :: parms
real                     :: pat(ipq2)
integer                  :: ipen

real                     :: rpat(ipq2)
character(len=3),save    :: onoff(0:1) =(/'off',' on'/)
real                     :: rcurv1(ipensq2)
integer                  :: icurv(ipensq2)
integer                  :: ichars
integer                  :: i10
integer                  :: i20
integer                  :: i30
integer                  :: i40
integer                  :: iend
integer                  :: ier
integer                  :: ierr
integer                  :: ierr2
integer                  :: igot
integer                  :: ii
integer                  :: ioo
integer                  :: j
integer                  :: lennum
integer                  :: lnum
real                     :: xmax0t
real                     :: xmin0t
real                     :: ymax0dum
real                     :: ymin0dum
character(len=255)       :: temp1
!===================================================================================================================================
   if(parms.eq.' '.or.fetch('setmark_>').ne.'#N#')then ! if no parameters are present list non-default legend labels
      call plot_storage()
      call journal('#-------------------------------------------------')
      call journal('#               (sun  planet  offset  lines turns)')
      call journal('#-------------------------------------------------')
      do i10=1,ipensq2
         ioo=int(shq2(i10,ipq2))
         write(temp1,101)i10,(shq2(i10,ii),ii=1,ipq2-1),onoff(ioo)
101      format('setmark ',i2,' -p ',3(1x,f6.1),2f7.3,' -sz ',f7.3,' -fill ',a)
         call journal(temp1)
      enddo
      call journal('#-------------------------------------------------')
      call plot_marks('setmark') ! draw a sample of the plot_marks
      return
!===================================================================================================================================
   elseif(parms.eq.'fetch')then
      call priv_getpage(xmin0t,xmax0t,ymin0dum,ymax0dum)
      if(ipen.le.igeoq2)then ! geometric markers
         j=max(1,mod(ipen-1,ipensq2)+1)
         do i20=1,ipq2
            pat(i20)=shq2(j,i20)
         enddo
         pat(5)=2*piq2*shq2(j,5)                    ! convert from fraction of a circle to radians
         pat(6)=shq2(j,6)*(xmax0t-xmin0t)/100.0   ! convert diameter from percent of full x window to a number
      endif
      return
   endif
!===================================================================================================================================
   temp1=fetch('setmark_fn')   ! change font used for markers
   plot_ids%marker_font=temp1
   call xy_retrv2('setmark_oo',temp1,lennum,ier)
   if(lennum.eq.0)then
      call journal('*plot_setmark* changing all marker numbers')
      iend=ipensq2
      do i40=1,iend
         icurv(i40)=i40
      enddo
   else
     ierr=0
     call strgar2(fetch('setmark_oo'),ipensq2,rcurv1,igot,' ',ierr)
     if(ierr.ne.0.or.igot.le.0)then
        call journal('*plot_setmark* stopped: undecipherable id number(s)')
        return
     endif
     call listout(int(rcurv1(:igot)),icurv,igot,ierr2) ! read up to ipensq2
     iend=igot
   endif
!===================================================================================================================================
   do i30=1,iend
      if(icurv(i30).le.igeoq2.and.icurv(i30).ge.1)then ! geometric markers
         ! get pattern number
         lnum=min(max(1,icurv(i30)),ipensq2) ! get a pattern number to replace, should yell about out-of-range numbers
         call xy_retrv2('setmark_p',temp1,ichars  ,ier)  ! get new pattern
         if(temp1.ne.' ')then
            call strgar2(fetch('setmark_p'),ipq2,rpat,igot,' ',ierr)
            igot=min(5,igot)
            shq2(lnum,1:igot)=rpat(1:igot) ! store pattern numbers
         endif

         call xy_retrv2('setmark_fill',temp1,ichars  ,ier)  ! fill flag
         if(temp1.ne.' ')then
            if(temp1.eq.'off')then
               shq2(lnum,7)=0 ! set fill off
            else
               shq2(lnum,7)=1 ! set fill on
            endif
         endif

         call xy_retrv2('setmark_sz',temp1,ichars  ,ier)  ! marker size
         if(temp1.ne.' ')then
            shq2(lnum,6)=rnum0(fetch('setmark_sz'))
         endif
      endif
   enddo
!===================================================================================================================================
end subroutine plot_setmark
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_page_aspect(3f) - [M_xyplot] Get aspect ratio from ASPECT command and call xy_aspct
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_page_aspect()
!     1992 John S. Urban
use M_journal,         only : journal
use M_kracken,         only : store
implicit none

! ident_9="@(#)M_xyplot::plot_page_aspect(3f): Get aspect ratio from ASPECT command and call xy_aspct"

integer           :: ier
real              :: shape

   shape=rnum0(fetch('aspect_oo'))
   if(shape.le.0)then
      call journal('*aspect* ratio <= 0, reset to 1')
      call store('aspect_oo','1','replace',ier)
      call xy_aspct(0.0,1.0,0.0,1.0)
   else
      call xy_aspct(0.0,shape,0.0,1.0)
   endif
end subroutine plot_page_aspect
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_marks(3f) - [M_xyplot] draw examples of dash codes, pen styles and screen markers for XY plots
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!
!!    Draw examples of dash codes, pen styles, and screen markers for the
!!    dashes(1), id(1), and markers(1) commands as reserved M_plot object 12345
!!    so they can not only be displayed, but copied by the hcopy(1) command if
!!    it is used before any other graphics-generating commands.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program testmarks
!!    use M_draw
!!    ! testmarks(1f): draw examples of dash codes, pen styles, and screen markers
!!       call vinit('')
!!       call plot_marks('setmark')   ! draw screen of current line markers
!!       idum=getkey()
!!       call plot_marks('font')      ! draw screen of current font
!!       idum=getkey()
!!       call plot_marks('id')        ! draw screen of current pen styles
!!       idum=getkey()
!!       call plot_marks('dash')      ! draw screen of current dash codes
!!       idum=getkey()
!!       call plot_marks('hue')       ! draw colors
!!       idum=getkey()
!!       call vexit()
!!    end program testmarks
subroutine plot_marks(nameit)
! Copyright(C) 1995 John S. Urban. All rights reserved
use M_journal,         only : journal
use M_draw
use M_drawplus,        only : spirograph
implicit none

! ident_10="@(#)M_xyplot::plot_marks(3f): draw screen of markers or pen styles or dash codes or color table or fonts"

character(len=*),intent(in)     :: nameit       ! drawing name type

character(len=6)                :: cnum
character(len=20)               :: devname
logical                         :: ldummy
real                            :: sh(7)
integer,parameter               :: IDSHS=20     ! number of normal pen styles
real                            :: xes(2)       ! hold x coordinates for drawing a line
real                            :: yes(2)       ! hold y coordinates for drawing a line
integer                         :: ibuf         ! flag to call swapbuffers
real                            :: xsmall,xbig,ysmall,ybig   ! drawing surface range
integer                         :: icolor
integer                         :: i10, i20, i80, i90
real                            :: box
real                            :: ycorner
real                            :: xcorner
integer            :: icols
integer            :: ido
integer            :: idummy
integer            :: im
integer            :: ipen
integer            :: irows
integer            :: istart
integer            :: jcols
integer            :: jrows
real               :: rr
real               :: sizem
real               :: tsize
real               :: xcol
real               :: xgap
real               :: xline
real               :: xnum
real               :: xrange
real               :: xtext
real               :: xx
real               :: y
real               :: yinc
real               :: yrange
real               :: yy
!-----------------------------------------------------------------------------------------------------------------------------------
   call push()
   ibuf=0
   if(LLQ(lnormalq))then          ! if called normally, make object for hcopy
      call makeobj(12345)
      call plot_page_aspect()     ! ensure set to current viewport/window
      idummy=backbuffer()
      call plot_clear('front')    ! clear graphic surface
   else                           ! called from a page command
      call color(iforeq)          ! set to foreground color
      ibuf=-1                     ! set flag to not call swapbuffers at the end
   endif
   call plot_get_plot_area(xsmall,xbig,ysmall,ybig)
   ldummy=xy_ifdrawn()            ! let other routines know something has been drawn
!-----------------------------------------------------------------------------------------------------------------------------------
   CHOOSE_PICTURE: SELECT CASE(nameit)
!-----------------------------------------------------------------------------------------------------------------------------------
    CASE('setmark')                            ! draw screen of geometric markers
      im=1                                    ! geometric marker counter
      xrange=xbig-xsmall
      yrange=ybig-ysmall
      yy=ybig-(yrange/5.0)
      rr=min(yrange/5.0,xrange/6.0)/2.0*0.88
      do i20=350,0,-100
         xx=xsmall+(xrange/6.0)
         do i10=50,500,100
            call xy_getmark(sh,im,0) ! get numbers needed to draw a symbol
            call spirograph(xx,yy,sh(1),sh(2),sh(3),rr,int(sh(4)),0.0,sh(5),int(sh(7)))
            im=im+1
            xx=xx+xrange/6.0
         enddo
         yy=yy-yrange/5.0
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
    CASE('font')                     ! draw screen of current font markers
      im=32                         ! geometric marker counter
      call xy_getmark(sh,im,0)      ! get numbers needed to draw a symbol
      call textsize(sh(6),sh(6))    ! assume the size should be constant
      YLP: do i90=350,50,-50
         XLP: do i80=50,450,40
            call move2(real(i80),real(i90))
            call drawstr(char(im))
            im=im+1
            if(im.gt.126)exit YLP
         enddo XLP
      enddo YLP
!-----------------------------------------------------------------------------------------------------------------------------------
    CASE('id')                                       ! draw screen of current pen styles
      call fixedwidth(.false.)
      call xy_rasters(2)

      istart=inum0(fetch('id_start'))               ! get pen number to start with
      istart=max(-2,istart)                         ! if nonsense value set to reasonable default
      irows=inum0(fetch('id_rows'))                 ! how many rows of pens to draw
      if(irows.le.0)irows=25                        ! if nonsense value set to reasonable default
      icols=inum0(fetch('id_cols'))                 ! get number of columns of pens to draw
      if(icols.le.0)icols=4                         ! if nonsense value set to reasonable default
      yinc=(ybig-ysmall)/real(irows+1)              ! get increment to space lines with
!        _____________________________________________________________________________________________________
      ! use current marker size to guess at a character size to use and show
      sizem=real(plot_ids(1)%marker_size)/100.0*(xbig-xsmall)    ! percent diameter of marker 1 scaled
      tsize=min(yinc,sizem)                         ! make sure text size not taller than y increment
      tsize=max(tsize,(xbig-xsmall)/80.0)
!        _____________________________________________________________________________________________________
!        loosely assuming six character widths needed for label, gaps between columns, symbols and space. and symbols near nominal
      xcol=(xbig-xsmall)/real(icols)                ! width of column
      call textsize(tsize,tsize)                    ! set text size
      xline=xcol-sizem-4.0*tsize                    ! how long to draw line (column - text label, marker size guess, white space)
      xes(1)=xsmall+sizem/2+tsize/2.0               ! where to start line in x direction
      xes(2)=xes(1)+xcol-sizem/2-3.5*tsize          ! where to end line in x direction

      COLS: do JCOLS=1,icols
         y=ybig-yinc                                ! Y height for drawing this pen line set to top
         ROWS: do JROWS=1,irows
            yes(1)=y                                ! set Y height of endpoints of line to draw
            yes(2)=y                                ! set Y height of endpoints of line to draw
            ipen=JROWS+(JCOLS-1)*irows+(istart-1)   ! number of pen style to draw
            if(ipen.lt.-2)cycle ROWS                ! if nonsense value set to reasonable default
            if(ipen.gt.ICSQ)exit COLS                ! quit if out of pens
            call xy_line(ipen,2,'toframe',xes,yes)  ! draw pen line to current frame dimensions
            write(cnum,fmt='(i4)')ipen              ! create string that matches pen number
            call color(iforeq)                      ! set pen color for drawing pen number
            call move2(xes(2)+sizem/2.0,y-tsize/2.0)! move to end of line
            call drawstr(cnum)
            y=y-yinc                                ! drop down to draw next line
         enddo ROWS
         xes(1)=xes(1)+xcol                         ! shift over for next column
         xes(2)=xes(1)+xcol-sizem/2-3.5*tsize       ! where to end line in x direction
      enddo COLS
!-----------------------------------------------------------------------------------------------------------------------------------
    CASE('hue')                                      ! draw screen of current color table
      !     draw screen of current colors
      call xy_aspct(0.0,256.0,0.0,256.0) ! ensure set to current viewport/window
      call polyfill(.true.)
      box=16.0
      call textsize(4.0,4.0)
      call centertext(.true.)
      ycorner=256.0
      icolor=0
      do i20=0,15
         xcorner=0.0
         do i10=0,15
            call color(icolor)
            call makepoly()
            call move2(xcorner,ycorner)
            call draw2(xcorner,ycorner-box)
            call draw2(xcorner+box,ycorner-box)
            call draw2(xcorner+box,ycorner)
            call draw2(xcorner,ycorner)
            call closepoly()
            write(cnum,'(i3)')icolor
            call color(iforeq)
            call move2(xcorner+box/2.0,ycorner-box/2.0)
            call drawstr(cnum)
            icolor=icolor+1
            xcorner=xcorner+box
         enddo
         ycorner=ycorner-box
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
    CASE('dash')                                     ! draw screen of current dash codes
      xgap=(xbig-xsmall)* 0.02                      ! xrange of display area * .020 is left gap
      xnum=(xbig-xsmall)* 0.08                      ! decide on length to use for label
      xline=(xbig-xsmall)*0.80                      ! length of line
      xes(1)=xsmall+xgap                            ! where to start line in x direction
      xes(2)=xes(1)+xline                           ! where to end line in x direction
      xtext=xes(2)                                  ! where to start text at end of line
      yinc=(ybig-ysmall)/23.0                       ! how far to move down after drawing a line
      call textsize(xnum/3.0,xnum/3.0)              ! set reasonable text size to fit 4 characters
      y=ybig-2*yinc                                 ! where to start top line

      do ido=1,IDSHS
         yes(1)=y                                    ! set y values for line ends for xy_line(3f)
         yes(2)=y                                    ! set y values for line ends for xy_line(3f)
         call xy_line(ido,2,'dash',xes,yes)          ! draw the line with special call to xy_line(3f) just for this routine
         write(cnum,fmt='(1x,i4)')ido                ! create string for line label
         call color(iforeq)                          ! set text color
         call move2(xtext,y-xnum/6.0)                ! move to desired position for label
         call textsize(xnum/3.0,xnum/3.0)            ! RESET? Bug in push()/pop()?
         call drawstr(cnum)                          ! draw label
         y=y-yinc                                    ! increment line height
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
    CASE DEFAULT
      call journal('*plot_marks* unknown diagram type')
      call journal(nameit)
   END SELECT CHOOSE_PICTURE
!-----------------------------------------------------------------------------------------------------------------------------------
   if(LLQ(lnormalq))then
      call closeobj()
      call callobj(12345)
      call stuff('PLTOBJECT',12345.0d0,'')
   endif
   if(ibuf.ne.-1)call swapbuffers()
   call vflush()                                    ! forces a flush
   call color(iforeq)                               ! now set the color to white
   call xy_rasters(1)
   call vgetdev(devname)                            ! get name of current device
   if(devname.eq.'tek')call xy_pause()
   call pop()
end subroutine plot_marks
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_title(3f) - [M_xyplot] allow user to add extra title lines by number
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!    allow user to add extra title lines by number
!!    store them in unused part of language dictionary
!!    NOTE: assume they are stored sequentially when printing
!!    and stop printing when a blank title line is encountered
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_title()
!     1992 John S. Urban
use M_kracken, only: store,current_command_length
implicit none

! ident_11="@(#)M_xyplot::plot_title(3f): allow user to add extra title lines by number"

character(len=20)   :: name
integer             :: i10
integer             :: ier
integer             :: iline
integer             :: lennum
character(len=4096) :: temp1
!-----------------------------------------------------------------------------------------------------------------------------------
!     use fetch and retrev instead of fetch and retrev2 so expansion occurs at plot time
!     an option for both behaviors might be nice
!-----------------------------------------------------------------------------------------------------------------------------------
      LLQ(ltitlq)=.true.    ! print the title
!-----------------------------------------------------------------------------------------------------------------------------------
!     a title line all by itself means to clear title lines
      if(current_command_length.le.0)then ! no parameters were specified, clear titles
         do i10=1,10  ! assume no more than 10 title lines
            write(name,'(''title_'',i2.2)')i10
            call store(name,' ','define',ier)
         enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      elseif( fetch('title_off').eq.' ')then   ! flag to no display normal title lines
         LLQ(ltitlq)=.false.   ! do not print the title
                             ! IGNORE OTHER PARAMETERS
!-----------------------------------------------------------------------------------------------------------------------------------
      else
!        if command is not blank and -off flag not present
         iline=inum0(fetch('title_l'))
         call xy_retrv2('title_oo',temp1,lennum,ier)
         if(iline.lt.0.and.lennum.le.0)then
            ! no -l and no -oo so store nothing, must be something like title -ch 30
         else
            iline=max(1,iline)
            write(name,'(''title_'',i2.2)')iline
            call store(name,trim(temp1),'define',ier)
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
      endif
end subroutine plot_title
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_jut(3f) - [M_xyplot] allow user to place text on screen
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_jut(parms)
! 1993 John S. Urban
use M_journal,         only : journal
use M_strings,         only : listout
implicit none

! ident_12="@(#)M_xyplot::plot_jut(3f): allow user to place text on screen"

character(len=*)        :: parms
real                    :: xyz(3)
character(len=3),save   :: onoff(2)=['on ','off']
real                    :: rcurv1(ICSQ)
integer                 :: icurv(ICSQ)
integer,save            :: lastlin=0    ! last line number used
character(len=1)        :: star         ! put an asterisk beside current line number
integer                 :: ichars
integer                 :: ii
integer                 :: notnull
integer                 :: isaw
integer                 :: i10, i20, i30, i50
integer                 :: iimax
integer                 :: iend  ! count of lines to change is set to 1
integer                 :: inext ! true that this is a "continued" string
integer                 :: ierr
integer                 :: lnum
integer                 :: iset
integer                 :: ibut
integer                 :: ier
integer                 :: ierr2
integer                 :: lennum
real                    :: xpick
real                    :: ypick
!------------------------------------------------------------------------------------------------------------------------------------------
      ii=1
      if(parms.eq.' ')then ! if no parameters are present list non-default legend labels
         notnull=0
         isaw=0
         call journal('-----------------------------------------------------------')
         do i10=1,ICSQ
            if(tuq(i10).ne.' ')then
               if(i10.eq.lastlin)then
                  star='*'
               else
                  star=' '
               endif
               notnull=notnull+1
               iimax=max(1,min(len_trim(tuq(i10)),255-4))
               write(temp1q,'(a1,i2,1x,a)')star,i10,tuq(i10)(:iimax)
               call journal(temp1q(:iimax+4))
            endif
         enddo
         if(notnull.gt.0)then
          call journal('-----------------------------------------------------------')
          call journal(' Line Position (x)           (y)     SiZe  Color BOX ON/OFF')
          call journal('-----------------------------------------------------------')
          do i20=1,ICSQ
               if(tuq(i20).ne.' ')then
               if(i20.eq.lastlin)then
                  star='*'
                  isaw=1
               else
                  star=' '
               endif

               write(temp1q,'(a1,i3,1x,2(g15.8e3,1x),f6.3,i5,i5,a5)')     &
     &            star,i20,tuxq(i20),tuyq(i20),tusq(i20),itucq(i20),      &
     &            itubq(i20),onoff(itoq(i20))

               call journal(temp1q(:74))
               if(taoq(i20).le.4.5)then
                  write(*,*)'arrow=',taxq(i20),tayq(i20),taoq(i20)
               endif
            endif
          enddo
          call journal('-----------------------------------------------------------')
         else
          call journal('*t* no non-null text labels')
         endif
         if(isaw.eq.0)then
            write(temp1q,'(a,i3)') 'current line is ',lastlin   ! if did not mark current line, show the number
            call journal(temp1q)
         endif
         return
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_retrv2('t_l',temp1q,lennum,ier)                      ! determine which lines to apply command to
      if(lennum.eq.0)then                                       ! if text line not specified, apply to next text line
         iend=1                                                 ! count of lines to change is set to 1
         inext=1                                                ! true that this is a "continued" string
         ii=max(1,lastlin)
         if(lastlin+1.ge.ICSQ)lastlin=0
         icurv(1)=lastlin+1                                     ! store line number to change in the generic place for it
      else                                                      ! explicitly listed line numbers to change
         ierr=0
         call strgar2(fetch('t_l'),ICSQ,rcurv1,iend,' ',ierr)    ! convert string list to an xy_array of numbers in rcurv1
         call listout(int(rcurv1(:iend)),icurv,iend,ierr2)      ! expand xy_array if it contains ranges up to ICSQ values
         if(ierr.ne.0.or.iend.le.0)then
            call journal('*t* stopped: undecipherable t number(s)')
            return
         endif
         inext=0 ! false that this is a "continued" string
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      lastlin=icurv(iend)   ! store index of last line number that will try to change
!-----------------------------------------------------------------------------------------------------------------------------------
      do 30 i30=1,iend
      lnum=icurv(i30)
      iset=0 ! flag that have changed something, so know what to do if string is blank (change attributes and leave alone, or clear)
!-----------------------------------------------------------------------------------------------------------------------------------
      if((lnum.lt.1).or.(lnum.gt.ICSQ))then
         call journal('*t* text number out of allowable range')
         goto 30
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_retrv2('t_off',temp1q,ichars  ,ier) ! suppress this text
      if(temp1q.ne.'#N#')then
         itoq(lnum)=2
         iset=iset+1
      elseif(inext.eq.1)then ! this is "continued" text
         itoq(lnum)=itoq(ii)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_retrv2('t_on',temp1q,ichars  ,ier)  ! activate this text
      if(temp1q.ne.'#N#')then
         itoq(lnum)=1
         iset=iset+1
      elseif(inext.eq.1)then ! this is "continued" text
         itoq(lnum)=itoq(ii)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_retrv2('t_c',temp1q,ichars  ,ier)  ! text color
      if(temp1q.ne.'#N#')then
         itucq(lnum)=inum0(temp1q)
         iset=iset+1
      elseif(inext.eq.1)then ! use previous color because this is "continued" text
         itucq(lnum)=itucq(ii)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_retrv2('t_asz',temp1q,ichars  ,ier)  ! arrow size
      if(temp1q.ne.'#N#')then
         tuaszq(lnum)=rnum0(temp1q)
         iset=iset+1
      elseif(inext.eq.1)then ! use previous size because this is "continued" text
         tuaszq(lnum)=tuaszq(ii)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_retrv2('t_box',temp1q,ichars  ,ier)  ! text box flag
      if(temp1q.ne.'#N#')then
         itubq(lnum)=inum0(temp1q)
         if(itubq(lnum).lt.0)itubq(lnum)=-1
         iset=iset+1
      elseif(inext.eq.1)then ! use previous flag because this is "continued" text
         itubq(lnum)=itubq(ii)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_retrv2('t_sz',temp1q,ichars  ,ier)  ! text size
      if(temp1q.ne.'#N#')then
         tusq(lnum)=abs(rnum0(temp1q))
         if(tusq(lnum).eq.0)tusq(lnum)=1.0
         iset=iset+1
      elseif(inext.eq.1)then ! use previous size because this is "continued" text
         tusq(lnum)=tusq(ii)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_retrv2('t_j',temp1q,ichars  ,ier)  ! text justification; non-zero is center
      if(temp1q.ne.'#N#')then
         if(temp1q(1:1).eq.'c')then
            itujq(lnum)=1 ! center justify the string
         else
            itujq(lnum)=0 ! no center justification
         endif
         iset=iset+1
      elseif(inext.eq.1)then ! use previous mode because this is "continued" text
         itujq(lnum)=itujq(ii)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_retrv2('t_p',temp1q,ichars  ,ier)  ! text position
      if(temp1q.ne.'#N#')then
         if(ichars.gt.0)then  ! values have been specified
            call strgar2(fetch('t_p'),2,xyz,iend,' ',ierr)
            if(ierr.ne.0.or.iend.eq.1)then
               call journal('*t* undecipherable text position')
            else
               tuxq(lnum)=xyz(1)
               tuyq(lnum)=xyz(2)
            endif
         else                                          ! -p keyword with no values
            if(i30.eq.1)then
               call journal('query position from screen')
               call xy_pickpnt(1,xpick,ypick,ibut)   ! calculate value from screen for first curve in list
            endif
            tuxq(lnum)=xpick
            tuyq(lnum)=ypick
         endif
         iset=iset+1
      elseif(inext.eq.1)then ! use previous coordinates but down one because  this is "continued" text
           tuxq(lnum)=tuxq(ii)
           tuyq(lnum)=tuyq(ii) !when printed, an identical coordinate to one above it means print below it
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_retrv2('t_a',temp1q,ichars  ,ier)  ! text arrow
      if(temp1q.ne.'#N#')then
         call strgar2(fetch('t_a'),3,xyz,iend,' ',ierr)
         if(ierr.ne.0)then
            call journal('*t* undecipherable arrow position')
         else
            if(iend.eq.0)then
               taoq(lnum)=5.0      ! if no number, just keyword turn off arrow
            elseif(iend.eq.1)then
               taoq(lnum)=xyz(1)    ! if just one number, use it for the origin indicator
            elseif(iend.eq.2)then  ! use two numbers for end point and figure out a nice place to start from
               taxq(lnum)=xyz(1)
               tayq(lnum)=xyz(2)
               taoq(lnum)=0.0      ! flag that should figure out where arrow starts at
            else                   ! the end point and origin indicator were explicitly set
               taxq(lnum)=xyz(1)
               tayq(lnum)=xyz(2)
               taoq(lnum)=xyz(3)
            endif
            iset=iset+1
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      temp1q=' '
      call xy_retrv2('t_fn',temp1q,ichars  ,ier)  ! text font
      if(temp1q(1:3).ne.'#N#')then
         tufq(lnum)=temp1q
         if(tufq(lnum).eq.' ')tufq(lnum)='futura.l'
         iset=iset+1
      elseif(inext.eq.1)then ! use previous font because this is "continued" text
         tufq(lnum)=tufq(ii)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_retrv2('t_oo',temp1q,ichars  ,ier) ! text label
      if(ichars.ne.0)then
         tuq(lnum)=temp1q   ! if text not blank, store it.
      elseif(iset.eq.0)then ! string is blank; but only if nothing was changed clear the string
         tuq(lnum)=temp1q   ! even with text blank store it to blank it out
         taoq(lnum)=5.0     ! turn arrow off for this text line too
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
30    continue
!-----------------------------------------------------------------------------------------------------------------------------------
      itumaxq=0
      do i50=1,ICSQ              ! so things are faster in other places, record highest non-blank subscript
         if(tuq(i50).ne.' ')itumaxq=i50
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine plot_jut
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_sz(3f) - [M_xyplot] change the size of the display surface
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!    subroutine plot_sz()
!!##DESCRIPTION
!!    This routine changes the device size on devices where this is
!!    supported. It is typically used with the X11 windows driver. The size
!!    and position are stored as local variables. They are changed by the
!!    sz_oo and sz_p strings queried from the command language dictionary.
!!
!!    Most devices are left open and just a new aspect area is selected based
!!    on the ratio of the size parameters. The xy_noclose() function flags
!!    whether to actually change the display device or not. For devices
!!    like X11 and PC the device is actually closed and then restarted at
!!    the new size because the M_plot graphics library does not in general
!!    support changing the device size and position except when the device
!!    is initialized.
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine plot_sz()
use M_journal,         only : journal
use M_system,          only : set_environment_variable
use M_draw
use M_kracken,         only : store
implicit none

! ident_13="@(#)M_xyplot::plot_sz(3f): close and reopen window to new size if possible"

character(len=20)           :: dev
real                        :: rvals(2)
integer,save                :: iwidth=800, iheight=600
integer                     :: ix=0, iy=0
integer                     :: inums
integer                     :: ierr
integer                     :: lenoo, lenp
character(len=255)          :: scratch_string
real                        :: shape
!-----------------------------------------------------------------------------------------------------------------------------------
!  extract preferred size
   call xy_retrv2('sz_oo',scratch_string,lenoo,ierr)

   if(lenoo.ne.0)then
      call strgar2(scratch_string(:lenoo),2,rvals,inums,' ',ierr)
   else
      inums=0
   endif

   if(inums.eq.1)then
      iwidth=int(rvals(1))
      iheight=iwidth
   elseif(inums.eq.2)then
      iwidth=max(int(rvals(1)),32)
      iheight=max(int(rvals(2)),32)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  extract preferred position ( this code requires values >= 0 )
   call xy_retrv2('sz_p',scratch_string,lenp,ierr)

   if(lenp.ne.0)then
      call strgar2(scratch_string(:lenp),2,rvals,inums,' ',ierr)
   else
      inums=0
   endif

   if(inums.eq.1)then
      ix=int(rvals(1))
      iy=0
   elseif(inums.eq.2)then
      ix=int(rvals(1))
      iy=int(rvals(2))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(fetch('sz_>').ne.'#N#')then
      call journal('sc','width.......',iwidth)
      call journal('sc','height......',iheight)
      call journal('sc','xposition...',ix)
      call journal('sc','yposition...',iy)
      if(lenp.eq.0.and.lenoo.eq.0)return   ! if no options return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call vgetdev(dev)                       ! get the name of the current device
   if(xy_noclose(dev))then
      call set_environment_variable('M_DRAW_DEVICE','CLOSE')  ! set variable so device driver knows this will be reopened
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  should be checking for a valid output device name
   call prefposition(ix, iy)            ! Specify the preferred position of the window opened  by the *next* vinit.
   if(xy_noclose(dev))then              ! restart the device now to make the new size active
      iwidth=min(2000,iwidth)
      iheight=min(2000,iheight)
      call prefsize(iwidth, iheight)    ! Specify the preferred width and height  of  the  window opened by the *next* vinit.
      call vnewdev(dev)                 ! reinitialize to a new device using current attributes
      call vflush()
   else
      call prefsize(iwidth, iheight)    ! Specify the preferred width and height  of  the  window opened by the *next* vinit.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   shape=real(iwidth)/real(iheight)
   call xy_aspct(0.0,shape,0.0,1.0)
   call store('aspect_oo',shape,'replace',ierr)
   call plot_clear('all')                             ! clear and flush
   call xy_setsize('-1 -1 -1 -1')                     ! reset so X11 and tek device do not set output device sizes for hcopy
   call xy_jumapc('reload')                           ! reload PLT color table
end subroutine plot_sz
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_drawplot(3f) - [M_xyplot] draw an xy plot
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_drawplot(lclear)
use M_journal, only : journal
use M_draw
use M_drawplus, only : plain_rect
implicit none

! ident_14="@(#)M_xyplot::plot_drawplot(3f): draw an xy plot"

!     1989 John S. Urban
logical,intent(in)         :: lclear                                 ! flag to not clear if creating a segment
!-----------------------------------------------------------------------------------------------------------------------------------
real                       :: xtreme(4),ytreme(4)                    ! hold max and min of all curves
real                       :: xlow(2),xhigh(2),ylow(2),yhigh(2)
real                       :: X8(8)
character(len=20)          :: devname
integer                    :: idelx(4),idely(4)
integer                    :: iunits(4)
!-----------------------------------------------------------------------------------------------------------------------------------
real                       :: rcurv(ICSQ,4)
logical                    :: llhold
logical                    :: fulldual
real                       :: aymulhold
real                       :: conb2
real                       :: conm2
real                       :: finalb
real                       :: finalm
integer                    :: i10
integer                    :: i70
integer                    :: i75
integer                    :: i80
integer                    :: ibuf
integer                    :: ichange
integer                    :: idonly
integer                    :: ierr
integer                    :: ierr2
integer                    :: ifile
integer                    :: inums
integer                    :: iraz
integer                    :: iuniti
integer                    :: iunito
integer                    :: ixdivt
integer                    :: ixtict
integer                    :: iydivt
integer                    :: iytict
real                       :: rax
real                       :: ray
real                       :: rfile
real                       :: xmaxt
real                       :: xmint
real                       :: ymax2hold
real                       :: ymaxt
real                       :: ymin2hold
real                       :: ymint
!-----------------------------------------------------------------------------------------------------------------------------------
! hold values so can reset at end of procedure
      ymin2hold=rangeq(7)                          ! make sure ymin2 and ymax2 are ignored if fixed-scale dual axis are requested
      ymax2hold=rangeq(8)
      llhold=LLQ(lshowq)                           ! flag for whether DUAL -RELATE is on or off
      aymulhold=aymulq                             ! if not zero, RIGHT command is active
!-----------------------------------------------------------------------------------------------------------------------------------
      ifile=ifooq2                                 ! file number default specified with the f command
!-----------------------------------------------------------------------------------------------------------------------------------
!     determine whether dual independent axis are to be used or not
      rfile=ifile
      call strgar3(fetch('plot_oo'),ICSQ,rfile,rcurv,inums,' ',' ',ierr2) ! get the list of curve numbers
      fulldual=.false.
      do i10=1,inums
         if(rcurv(i10,1).eq.0)then
            fulldual=.true.                          ! zero in curve list so making a full dual axis plot with independent axis
         endif
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
! make sure ymin2 and ymax2 are ignored if fixed-scale dual axis are requested
      if(fulldual) then                                     ! a plot with independent dual axis, check for fixed y-axis relationship
        ! ensure independent y-axis mode overrides fixed dual y-axis relationship
        if(fulldual)then                                                                   ! independent dual axis with "plot n 0 m"
          if(aymulq.ne.0)then
             call journal('*plot_drawplot* warning: fixed y-axis relationship')
             call journal('         request from RIGHT ignored due to ')
             call journal('         independent y-axis plot request')
             aymulq=0.0                                                                    ! turn off RIGHT command
          endif
          if(LLQ(lshowq))then
            call journal('*plot_drawplot* warning: fixed y-axis relationship')
            call journal('         request from RELATE -DUAL ignored due')
            call journal('         to independent y-axis plot request')
            LLQ(lshowq)=.false.                                                              ! turn off RELATE -DUAL
          endif
        endif
      else                                                        ! not independent dual axis, but other dual-axis modes might be on
        if( (aymulq.ne.0) .or. (LLQ(lshowq)) )then                                           ! RIGHT on or RELATE -DUAL on
           if(rangeq(7).ne.123.456)then                                                    ! YMIN2 is set so clear it
              call journal('*plot_drawplot* warning:fixed dual y-axis scaling overrides YMIN2')
              rangeq(7)=123.456
           endif
           if(rangeq(8).ne.123.456)then                                                    ! YMAX2 is set so clear it
              call journal('*plot_drawplot* warning:fixed dual y-axis scaling overrides YMAX2')
              rangeq(8)=123.456
           endif
            if(LLQ(lshowq).and.(aymulq.ne.0))then                                            ! DUAL -RELATE and RIGHT both on
               call journal('*plot_drawplot* warning:"relate -dual" overrides "right"')
               aymulq=0.0
            endif
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      ibuf=-1                                                     ! if no curves or file found, we move to 999 before setting ibuf
      ilongq2=inum0('LABELPLACES')
!-----------------------------------------------------------------------------------------------------------------------------------
!     initialize xy_array of extremes
      do i70=1,4
         xtreme(i70)=0.0
         ytreme(i70)=0.0
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
!     set flags as to whether certain text should be fixedwidth or proportional
      call xy_fxed('title_fx',LLQ(itifxedq))
      call xy_fxed('xmin_fx',LLQ(ixmfxedq))
      call xy_fxed('xlabel_fx',LLQ(ixlfxedq))
      call xy_fxed('id_fx',LLQ(iidfxedq))
      call xy_fxed('ban_fx',LLQ(ibafxedq))
      call fixedwidth(.true.)                 ! start off assuming in fixed-width font mode
!-----------------------------------------------------------------------------------------------------------------------------------
      ixdivT=max(1,inum0(fetch('grid_xl')))   ! approximate number of x grid labels
      ixticT=max(1,inum0(fetch('grid_xt')))   ! approximate number of x grid tics between labels
      iydivT=max(1,inum0(fetch('grid_yl')))   ! approximate number of y grid labels
      iyticT=max(1,inum0(fetch('grid_yt')))   ! approximate number of y grid tics between labels
      if(ixdivT.le.0)ixdivT=5                 ! reset to default if bad value
      ixdivT=max(1,ixdivT)                    ! approximate number of x grid labels

      if(ixticT.le.0)ixticT=4                 ! reset to default if bad value
      ixticT=max(1,ixticT)                    ! approximate number of x grid tics between labels

      if(iydivT.le.0)iydivT=5                 ! reset to default
      iydivT=max(1,iydivT)                    ! approximate number of y grid labels

      if(iyticT.le.0)iyticT=4                 ! reset to default
      iyticT=max(1,iyticT)                    ! approximate number of y grid tics between labels
!-----------------------------------------------------------------------------------------------------------------------------------
!     find extrema for group of curves to be plotted and set legend defaults if legends are blank
      plot_axis%ylogmode=-1 ! assume axis must be linear unless log is set and it checks out OK
      plot_axis%xlogmode=-1 ! assume axis must be linear unless log is set and it checks out OK
      call xy_jucurv(xlow,xhigh,ylow,yhigh,iunits,ierr) ! set iunits for use when relate is in dual mode
      if(ierr.ne.0)goto 999
      ! find nice ranges for first set of axes
      call xy_jurang(1,xlow(1),ylow(1),xhigh(1),yhigh(1),xtreme,ytreme,idelx,idely,  &
     &   RANGEQ,plot_axis%xlogmode,plot_axis%ylogmode,                               &
     &   LLQ(logxq),LLQ(logyq),LLQ(lxactq),                                          &
     &   ixdivT,ixticT,iydivT,iyticT,                                                &
     &   fetch('xlog_type'),fetch('ylog_type'))
!-----------------------------------------------------------------------------------------------------------------------------------
      if(LLQ(lshowq).and.aymulq.eq.0.and.(.not.fulldual))then ! no forced label set with RIGHT and relate is in dual mode
!        collect linear conversion factors based on automatic table set up relate command so only have to call xy_convert once
         ichange=0                              ! flag as to whether any conversions were found
         finalm=1.0                             ! initial multiplication factor
         finalb=0.0                             ! initial constant factor
         iuniti=iunits(2)
         !==============================================================
         do i75=NUNITS0Q,NUNITSQ                  ! reblank the flag xy_array used to look for recursive unit conversion
            iu4q(i75)=0
         enddo
         !==============================================================
         GATHER: do                             ! top of loop gathering the conversion factors
         call xy_getrel(iuniti,iunito,conm2,conb2) ! see if this unit is flagged as needing conversion
            if(iunito.ne.iuniti)then          ! accumulate the linear conversion factors until the chain stops or is recursive
               if(iu4q(iuniti).eq.-1)then     ! iu4q is an xy_array of flags indicating whether this conversion has been used or not
                  call journal('*plot_drawplot* recursive unit conversion detected')
               else
                  iu4q(iuniti)=-1               ! set flag went thru this conversion so can avoid recursive loops
                  finalb=conm2*finalb+conb2     ! accumulate linear conversion factors
                  finalm=finalm*conm2
                  ichange=ichange+1             ! flag that a conversion call is needed
                  iuniti=iunito
                  cycle GATHER                  ! look for another conversion in a chain
               endif
            endif
         exit GATHER
         enddo GATHER
         !==============================================================
         if(ichange.ne.0)then
            do i80=NUNITS0Q,NUNITSQ               ! reblank the flag xy_array used to look for recursive unit conversion
              iu4q(i80)=0
            enddo
         endif
         !==============================================================
         aymulq=finalm
         ayconq=finalb
         iyuniq=iunito
         iunits(4)=iyuniq
      else
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_setlbl(iunits)                                         ! select axis labels from unit codes or user-specified strings
!-----------------------------------------------------------------------------------------------------------------------------------
      if(aymulq.ne.0)then                                         ! right scale is to be an exact multiple of left scale
         ylow(2)=ytreme(1)*aymulq+ayconq
         yhigh(2)=aymulq*ytreme(2)+ayconq
         ! find nice ranges for second axis
         call xy_jurang(2,xlow(2),ylow(2),xhigh(2),yhigh(2),            &
     &      xtreme(3),ytreme(3),idelx(3),idely(3),                      &
     &      RANGEQ,plot_axis%xlogmode,plot_axis%ylogmode,               &
     &      LLQ(logxq),LLQ(logyq),LLQ(lxactq),                          &
     &      ixdivT,ixticT,iydivT,iyticT,                                &
     &      fetch('xlog_type'),fetch('ylog_type'))
      else                                                        ! normal calculation of right axis scale
         if(fulldual)then                                         ! ranges for second y axis, should not be needed if no right axis
            ! find nice ranges for second axis
            call xy_jurang(2,xlow(2),ylow(2),xhigh(2),yhigh(2),         &
     &         xtreme(3),ytreme(3),idelx(3),idely(3),                   &
     &         RANGEQ,plot_axis%xlogmode,plot_axis%ylogmode,            &
     &         LLQ(logxq),LLQ(logyq),LLQ(lxactq),                       &
     &         ixdivT,ixticT,iydivT,iyticT,                             &
     &         fetch('xlog_type'),fetch('ylog_type'))
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     ensure set to current viewport/window in case new device does not
!     have same physical viewport shape and to negate changes made for
!     legend or whatever
      if(LLQ(lnormalq))then
      call plot_page_aspect() ! ensure set to current viewport/window
         ibuf=backbuffer()
      else
         call color(iforeq)
         ibuf=-1
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     set up character sizes and marker size and tic size
!     scale characters as a percent of longest window side
!     so that no matter what the window size, a good attempt is made
!     to select properly scaled character sizes.
      call plot_get_plot_area(xmint,xmaxt,ymint,ymaxt)
      !tlong=max(xmaxt-xmint,ymaxt-ymint)
      call plot_setticper(rnum0(fetch('grid_tx')),                           &
     &               rnum0(fetch('grid_ty')),                           &
     &               rnum0(fetch('grid_txx')),                          &
     &               rnum0(fetch('grid_tyy')),'set')
!-----------------------------------------------------------------------------------------------------------------------------------
      if(xy_ifdrawn())then
         ! first page of output so do not clear or might put blank page of paper out
      else
         if(lclear.and.LLQ(lnormalq))call plot_clear('front')
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(lclear)call plot_storage()
!-----------------------------------------------------------------------------------------------------------------------------------
      if(fetch('grid_box').eq.'on')then
         iraz=max(1,inum0(fetch('grid_w')))
         call xy_rasters(iraz)
         rax=(xmaxt-xmint)/1000.0          ! find a small amount to move in so roundoff does not remove outline in M_plot package
         ray=(ymaxt-ymint)/1000.0          ! find a small amount to move in so roundoff does not remove outline in M_plot package
         call plain_rect(xmint+rax,ymint+ray,xmaxt-rax,ymaxt-ray)  ! overall box around entire plot_area
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(fetch('plot_idonly').eq.' ')then
         idonly=0
      else
         idonly=1
      endif
      call clipping(.false.)        ! turn off clipping to avoid bug on calculating length of proportional hardware fonts
      call xy_bans()                ! draw banner text around plot area and adjust window margins inward
      call xy_jutitl(ifile,idonly)  ! add title and legend to top of plot, adjust window margins down
      call clipping(.true.)         ! when hardware proportional text of SoftText=2 is fully supported, can allow clipping again
      if(idonly.eq.0)goto 999       ! do not draw rest of plot
!-----------------------------------------------------------------------------------------------------------------------------------
      call color(iforeq)            ! now set the color to foreground
!-----------------------------------------------------------------------------------------------------------------------------------
!     set data ranges
      x8(1)=xtreme(1)
      x8(2)=xtreme(2)
      x8(3)=ytreme(1)
      x8(4)=ytreme(2)
      x8(5)=xtreme(3)
      x8(6)=xtreme(4)
      x8(7)=ytreme(3)
      x8(8)=ytreme(4)
      call xy_setdatarange(x8)
      call xy_rasters(2)
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_laxis(idelx,idely)             ! draw x axis and y axis and determine size of curve area
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_rasters(1)
      call xy_judraw(ifile)                  ! draw the curves
!-----------------------------------------------------------------------------------------------------------------------------------
      ! initialize conversion factors for going from plot to surface coordinates
      call xy_setcnv(plot_axis%xlogmode,plot_axis%ylogmode)
      call xy_iftext()                       ! add user text strings
      call xy_tidybox()
      call color(iforeq)                     ! now set the color to white
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ibuf.ne.-1)call swapbuffers()
      call vflush()                          ! forces a flush
      call vgetdev(devname)                  ! get name of current device
      if(devname.eq.'tek')call xy_pause()
      !-------------------------------
      LLQ(lshowq)=llhold                     ! restore dual axis state
      aymulq=aymulhold                       ! restore dual axis state
      rangeq(7)=ymin2hold
      rangeq(8)=ymax2hold
      !-------------------------------
      return
999   continue
      !-------------------------------
      LLQ(lshowq)=llhold                     ! restore dual axis state
      aymulq=aymulhold                       ! restore dual axis state
      rangeq(7)=ymin2hold
      rangeq(8)=ymax2hold
      !-------------------------------
      if(ibuf.ne.-1)call swapbuffers()
      call color(iforeq)                     ! now set the color to white
      call vflush()                          ! forces a flush
      call fixedwidth(.true.)                ! text is fixed space after leaving plot_drawplot
end subroutine plot_drawplot
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_juparea(3f) - [M_xyplot] parse parea command
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_juparea()
!      'parea -oo -a 1 -rows 0 -cols 0 -g 0'
!       this routine is not finished.
!       only parea xmin xmax ymin ymax works
!       does not work for hcopy command
!       g is goto nth pane so can fill out of order
!       a is pane area number being defined or cleared
!       oo is 4 numbers to define area or none to clear an area
!       rows and cols is a shortcut to define a matrix of windows
!
use M_journal, only : journal
implicit none

! ident_15="@(#)M_xyplot::plot_juparea(3f): parse parea command"

real           :: rpat(4)
integer        :: ichars
integer        :: irows
integer        :: icols
integer        :: irowsa
integer        :: icolsa
integer        :: ier
integer        :: ierr
integer        :: igot
!-----------------------------------------------------------------------------------------------------------------------------------
! if batch, want to set to maximum; but if interactive might want to ignore
      irows=inum0(fetch('parea_rows'))
      icols=inum0(fetch('parea_cols'))
      irowsa=abs(irows)
      icolsa=abs(icols)
      if(icolsa.gt.0.or.irowsa.gt.0)then
         irowsa=max(1,irowsa)
         icolsa=max(1,icolsa)
         if(irowsa*icolsa.gt.NUMPANEQ)then ! CAUTION: ignoring  -p and -oo
           call journal('sc','*parea* number of panes cannot exceed ',NUMPANEQ)
           return
         endif
           ipaneq=1            ! set pointer to first pane
           ipanesq=irows*icols ! number of panes now this
         return
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_retrv2('parea_oo',temp1q,ichars  ,ier)
      if(temp1q.ne.' ')then
         call strgar2(temp1q,4,rpat,igot,' ',ierr)
         LLQ(lnormalq)=.false.
         ! need to add a lot of checking for good values
         call plot_set_plot_area(rpat(1),rpat(2),rpat(3),rpat(4))
      endif
end subroutine plot_juparea
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_jupage(3f) - [M_xyplot] a good try at allowing mixing of PLT commands and M_plot commands
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_jupage()
use M_journal, only : journal
use M_draw
implicit none

! ident_16="@(#)M_xyplot::plot_jupage(3f): a good try at allowing mixing of PLT commands and M_plot commands"

real,save          :: rpat(4)=[0.0, 8.5, 0.0, 11.0]
integer            :: ichars
integer            :: ier
integer            :: ierr
integer            :: igot
integer            :: icolor
character(len=255) :: ctemp1

   call xy_retrv2('page_oo',ctemp1,ichars,ier)
   if(ctemp1.eq.' ')then
      igot=4                                                  ! use statically saved values from last call or initial state
   else
      call strgar2(ctemp1,4,rpat,igot,' ',ierr)
   endif

   select case(igot)
   case(0)                              ! do something equivalent to a page advance
   case(1)                              ! return to normal PLT mode
      LLQ(lnormalq)=.true.
      call centertext(.false.)          ! do not center text
      call vsetflush(.false.)
      call vflush()
      icolor=rpat(1)
      if(icolor.ge.0)then
         if(icolor.eq.0)then
            call color(ibackq)
         else
            call color(icolor)
         endif
         call clear()
         call color(iforeq)
      else
         call color(iforeq)
      endif
   case(2)                              ! return to normal PLT mode setting background and foreground color
      LLQ(lnormalq)=.true.
      call centertext(.false.)          ! do not center text
      call vsetflush(.false.)
      call vflush()
      icolor=rpat(1)
      if(icolor.ge.0)then
         call color(icolor)
      else
         call color(ibackq)
      endif
      call clear()
      icolor=rpat(2)
      if(icolor.ge.0)then
         call color(icolor)
      else
         call color(iforeq)
      endif
   case(4)                              ! go to special "M_plot mode" in way most PLT commands still work.
      LLQ(lnormalq)=.false.
      call xy_aspct(rpat(1),rpat(2),rpat(3),rpat(4))
      call frontbuffer()
      call vsetflush(.true.)
      call color(ibackq)
      call clear()
      call color(iforeq)
   case default
      call journal('*page* insufficient parameters for page command')
   end select

end subroutine plot_jupage
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_id(3f) - [M_xyplot] render the id(1) command for XY plots
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!   subroutine plot_id(icmdlen)
!!
!!    integer,intent(in)          :: icmdlen
!!##DESCRIPTION
!!
!!    This routine implements the line style information set with the ID
!!    command. It is used to change the attributes of the pens used to draw
!!    the user data. Essentially everything that is used to identify a curve
!!    can be changed by this command. Line thickness, color, marker number,
!!    marker frequency, dash code ID, marker size, and curve fill style can
!!    be set. Also, text can be set to override the default legend label,
!!    and the legend entry can be turned off. A full description of the
!!    command can be found in the user manual.
!!
!!    If no parameters are given a chart showing curve styles is displayed
!!    and the current pen settings are written out.
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_id(icmdlen)
! 1992 John S. Urban

use M_journal, only : journal
use M_kracken, only: lget
use M_strings, only : listout
implicit none

! ident_17="@(#)M_xyplot::plot_id(3f): allow user to alter legend strings by number"

integer,intent(in)          :: icmdlen

character(len=7),save       :: flag(2) =(/' -on  ',' -off '/)
character(len=255)          :: ctemp1
character(len=4096)         :: ctemp2

integer,parameter           :: igrid=3    ! number of special pens for grid style (the pen values from -igrid+1 to 0)
real                        :: rcurv1(ICS2Q+igrid)
integer                     :: icurv(ICS2Q+igrid)
integer                     :: i10, i20, i25, i30
integer                     :: ichars
integer                     :: icols, irows
integer                     :: istart, iend
integer                     :: ier, ierr, ierr2
integer                     :: ifill
integer                     :: ishow
integer                     :: isub
integer                     :: ival
integer                     :: lennum
integer                     :: lnum
!===================================================================================================================================
   istart=inum0(fetch('id_start'))   ! see if display options for the pen sample plot have been specified
   icols=inum0(fetch('id_cols'))
   irows=inum0(fetch('id_rows'))
   ishow=max(istart,icols,irows)     ! see if -start, -cols, -rows were specified, as the default for all three is zero
!===================================================================================================================================
! list non-default legend labels and pen descriptions if no command options were specified
   if(icmdlen.le.0.or.ishow.gt.-999.or.fetch('id_>').ne.'#N#')then  ! no parameters are present or output file specified
      call plot_storage()
      do i10=1,ICS2Q
         if(lgnduq(i10).ne.' ')then
            call journal('sc','id',i10,'-t ','"'//trim(lgnduq(i10))//'"')
            if(exitq.eq.'END')goto 999  ! exit on non-null entry at cont... prompt
         endif
      enddo
      call journal('#--------------------------------------------------------------')
      if(exitq.eq.'END')goto 999  ! exit on non-null entry at cont... prompt
      call journal('#id pen -color -width -marker -dash_ID -number_of_markers -filltype  -size -on|-off')
      if(exitq.eq.'END')goto 999
      call journal('#--------------------------------------------------------------')
      if(exitq.eq.'END')goto 999
      do i20=1,ICS2Q
         202 format ('id ',i0,' -c ',i0,' -w ',i0,' -m ',i0,' -d ',i0,' -n ',i0,' -f ',i0,' -sz ',f4.1,' ',a)
         write(ctemp1,202) &
  &         i20,plot_ids(i20)%color,plot_ids(i20)%width,plot_ids(i20)%marker,plot_ids(i20)%dashcode, &
  &         plot_ids(i20)%marker_frequency,plot_ids(i20)%fill_style,plot_ids(i20)%marker_size,flag(plot_ids(i20)%legend+1)
         call journal(ctemp1(:70))
         if(exitq.eq.'END')goto 999  ! exit on non-null entry at cont... prompt
      enddo
      call journal('#--------------------------------------------------------------')
      if(exitq.eq.'END')goto 999  ! exit on non-null entry at cont... prompt
      call journal('# Special: 0 grid box, -1 major grid,-2 minor grid')
      if(exitq.eq.'END')goto 999  ! exit on non-null entry at cont... prompt
      do i25=-2,0
         write(ctemp1,202) &
  &         i25,plot_ids(i25)%color,plot_ids(i25)%width,plot_ids(i25)%marker,plot_ids(i25)%dashcode, &
  &         plot_ids(i25)%marker_frequency,plot_ids(i25)%fill_style,plot_ids(i25)%marker_size,flag(plot_ids(i25)%legend+1)
         call journal(ctemp1(:70))
         if(exitq.eq.'END')goto 999  ! exit on non-null entry at cont... prompt
      enddo
      call journal('#--------------------------------------------------------------')
      if(exitq.eq.'END')goto 999  ! exit on non-null entry at cont... prompt
      if(icmdlen.le.0.or.ishow.gt.-999)then  ! no parameters are present
         call plot_marks('id') ! draw a sample of the lines
         goto 999
      endif
   endif
!===================================================================================================================================
   call xy_retrv2('id_oo',ctemp1,lennum,ier)   ! get list of curve numbers as a string to check length
   if(lennum.eq.0)then                      ! if list is blank, assume all curves
      if(lget('id_grid'))then
          call journal('*id* changing all special pens')
          if(exitq.eq.'END')goto 999  ! exit on non-null entry at cont... prompt
          iend=igrid
          isub=1
      else
          call journal('*id* changing all standard pens')
          if(exitq.eq.'END')goto 999  ! exit on non-null entry at cont... prompt
          iend=ICS2Q
          isub=0
      endif
      FILL: do IFILL=1,iend
         icurv(IFILL)=IFILL-isub
      enddo FILL
   else                                     ! a list of pens was specified
      ierr=0
      call strgar2(fetch('id_oo'),ICS2Q+igrid,rcurv1,iend,' ',ierr)  ! get list of curve numbers expanded through calculator
      if(ierr.ne.0.or.iend.le.0)then
         call journal('*id* stopped: undecipherable id number(s)')
         if(exitq.eq.'END')goto 999  ! exit on non-null entry at cont... prompt
         goto 999
      endif
      call listout(int(rcurv1(:iend)),icurv,iend,ierr2)   ! expand any ranges in the list
   endif
!===================================================================================================================================
   if(lget('id_grid'))then           ! special grid line numbers are actually negative values
      icurv(:iend)=-icurv(:iend)
   endif
!===================================================================================================================================
   CHANGE: do i30=1,iend
      lnum=icurv(i30)
!===================================================================================================================================
      if((lnum.lt.-2).or.(lnum.gt.ICS2Q))then
         call journal('sc','*id* legend number out of allowable range:',lnum)
         if(exitq.eq.'END')goto 999  ! exit on non-null entry at cont... prompt
         exit CHANGE
      endif
!===================================================================================================================================
      call xy_retrv2('id_t',ctemp2,ichars  ,ier)                   ! legend label
      if(ctemp2.ne.'#N#')lgnduq(lnum)=ctemp2
!===================================================================================================================================
      ival=0
      call xy_retrv2('id_off',ctemp1,ichars  ,ier)                 ! suppress this id
      if(ctemp1.ne.'#N#')ival=1
      call xy_retrv2('id_on',ctemp1,ichars  ,ier)                  ! activate this id
      if(ctemp1.ne.'#N#')ival=0
      plot_ids(lnum)%legend=ival
!===================================================================================================================================
! check for valid values for line style values
      call xy_retrv2('id_c',ctemp1,ichars  ,ier)                              ! line color
      if(ctemp1.ne.'#N#')plot_ids(lnum)%color=abs(inum0(ctemp1))
      if(ctemp1.eq.' ')then
         if(lnum.gt.0)then
            plot_ids(lnum)%color=mod(lnum-1,7)+1                              ! if just keyword reset to default
         else ! for special line styles such as grid line styles
            plot_ids(lnum)%color=iforeq                                       ! if just keyword reset to default
         endif
      endif
!===================================================================================================================================
! check for valid values for line style values
      call xy_retrv2('id_f',ctemp1,ichars  ,ier)                              ! fill style/color
      if(ctemp1.eq.'#N#') then                                                ! keyword was not specified
      elseif(ctemp1.eq.' ')then
         plot_ids(lnum)%fill_style=0                                          ! if just keyword reset to default
      else
         plot_ids(lnum)%fill_style=abs(inum0(ctemp1))                         ! convert to number
      endif
!===================================================================================================================================
      call xy_retrv2('id_n',ctemp1,ichars  ,ier)                              ! number of markers
      if(ctemp1.ne.'#N#')plot_ids(lnum)%marker_frequency=abs(inum0(ctemp1))
      if(ctemp1.eq.' ')then
         plot_ids(lnum)%marker_frequency=0                                    ! if just keyword reset to default
      endif
!===================================================================================================================================
      call xy_retrv2('id_w',ctemp1,ichars  ,ier)                       ! line width
      if(ctemp1.ne.'#N#')plot_ids(lnum)%width=abs(inum0(ctemp1))
      if(ctemp1.eq.' ')then
         if(lnum.gt.0)then
            plot_ids(lnum)%width=3                                     ! if just keyword reset to default
         else
            plot_ids(lnum)%width=1                                     ! if just keyword reset to default
            if(lnum.eq. 0)plot_ids(lnum)%width=1
            if(lnum.eq.-1)plot_ids(lnum)%width=3                       ! major grid line style
            if(lnum.eq.-2)plot_ids(lnum)%width=1                       ! minor grid line style
         endif
      endif
!===================================================================================================================================
      call xy_retrv2('id_sz',ctemp1,ichars  ,ier)                      ! line marker size
      if(ctemp1.ne.'#N#')plot_ids(lnum)%marker_size=abs(rnum0(ctemp1))
      if(ctemp1.eq.' ')then
         if(lnum.gt.0)then
            plot_ids(lnum)%marker_size=2                               ! if just keyword reset to default
         else
            if(lnum.eq. 0)plot_ids(lnum)%width=10                      ! for plot_setmark command
            if(lnum.eq.-1)plot_ids(lnum)%width=1                       ! for letters in dash codes
            if(lnum.eq.-2)plot_ids(lnum)%width=2
         endif
      endif
!===================================================================================================================================
      call xy_retrv2('id_m',ctemp1,ichars  ,ier)                ! line marker number
      select case(ctemp1)                                       ! process different cases of the marker string
      case('#N#')                                               ! special value means the keyword was not specified
      case(' ')                                                 ! the keyword was specified with no value
         !plot_ids(lnum)%marker=abs(mod(lnum-1,20)+1)           ! if just keyword reset to default restricted to a nice range
         plot_ids(lnum)%marker=abs(lnum)                        ! if just keyword reset to default
      case default                                              ! the string is assumed to be a numeric expression
         plot_ids(lnum)%marker=inum0(ctemp1)                    ! get a numeric value from the string
      end select
!===================================================================================================================================
      call xy_retrv2('id_l',ctemp1,ichars  ,ier)                ! line marker letter
      if(ctemp1.eq.' ')then
         !plot_ids(lnum)%marker=abs(mod(lnum-1,20)+1)           ! if just keyword reset to default
         plot_ids(lnum)%marker=abs(lnum)                        ! if just keyword reset to default
      elseif(ctemp1.ne.'#N#')then
         plot_ids(lnum)%marker=ichar(ctemp1(1:1))
      endif
!===================================================================================================================================
      call xy_retrv2('id_d',ctemp1,ichars  ,ier)                ! line dash code
      if(ctemp1.ne.'#N#')plot_ids(lnum)%dashcode=inum0(ctemp1)
      if(ctemp1.eq.' ')then
         if(lnum.gt.0)then
            plot_ids(lnum)%dashcode=abs(lnum-1)                 ! if just keyword reset to default
         else
            plot_ids(lnum)%dashcode=0
         endif
      endif
!===================================================================================================================================
   enddo CHANGE
999   continue
end subroutine plot_id
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_clear(3f) - [M_xyplot] clear graphics area and ensure in graphics mode
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_clear(ctype)
use M_draw
implicit none

! ident_18="@(#)M_xyplot::plot_clear(3f): clear graphics area and ensure in graphics mode"

character(len=*),intent(in) :: ctype
integer                     :: ibuf
      iscrq=0
!     clear up screen even when double buffering
      if(LLQ(lnormalq))then
         ibuf=backbuffer()
      else
         ibuf=-1
      endif
      if(ibuf.ne.-1.and.ctype(1:3).eq.'all')then
         call color(ibackq)
         call clear()
         call color(iforeq)
         call swapbuffers()
         call color(ibackq)
         call clear()
         call color(iforeq)
      else
         call color(ibackq)
         call clear()
         call color(iforeq)
      endif
      call vflush()
end subroutine plot_clear
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_aspct(3f) - [M_xyplot] Store plot window size in global variables and call plot_page()
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!    Store plot window size in global variables and call BIGGEST_ORTHO2()
!!    bug occurs if a window edge (at least xsmall) is zero in determining
!!    length of hardware characters.
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_aspct(xsmall,xlarge,ysmall,ylarge)
implicit none

! ident_19="@(#)M_xyplot::xy_aspct(3f): Store plot window size in global variables and call BIGGEST_ORTHO2()"

real,intent(in)   :: xsmall
real,intent(in)   :: xlarge
real,intent(in)   :: ysmall
real,intent(in)   :: ylarge
   call stuff('PWIN_XMIN',xsmall,'')
   call stuff('PWIN_XMAX',xlarge,'')
   call stuff('PWIN_YMIN',ysmall,'')
   call stuff('PWIN_YMAX',ylarge,'')
   call plot_page(xsmall,xlarge,ysmall,ylarge)
end subroutine xy_aspct
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_bans(3f) - [M_xyplot] draw banner lines from PLT ban command
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_bans()
use M_draw
implicit none

! ident_20="@(#)M_xyplot::xy_bans(3f): draw banner lines from PLT ban command"

real               :: bothi
real               :: centr
real               :: height
integer            :: ibchars
integer            :: iprntb
integer            :: iprntl
integer            :: iprntr
integer            :: iprntt
integer            :: irazl
integer            :: irazt
real               :: tophi
real               :: width
real               :: xmaxt
real               :: xmint
real               :: xtemp
real               :: ymaxt
real               :: ymint
   call centertext(.false.) ! turn off text centering
   call plot_get_plot_area(xmint,xmaxt,ymint,ymaxt)
   centr=(xmaxt+xmint)/2.0   ! find center of drawing area for centering text
!-----------------------------------------------------------------------------------------------------------------------------------
!     add banner line
   call fixedwidth(LLQ(ibafxedq))              ! text is proportionally spaced for banner or not
   ibchars=max(1,inum0(fetch('ban_ch')))     ! get number of characters that must fit across banner line
   width=(xmaxt-xmint)/ibchars               ! fixed text character width
   height=width*1.2                          ! height of banner characters
   call font(fetch('ban_fn'))                ! select font to use for banner strings
   call textsize(width,height)               ! set font size
   irazt=max(1,inum0(fetch('ban_w')))        ! calculate width of lines to draw software text with
   irazl=max(1,inum0(fetch('grid_w')))       ! calculate width of lines to draw box, lines
!-----------------------------------------------------------------------------------------------------------------------------------
!     vertical text
!-----------------------------------------------------------------------------------------------------------------------------------
   call textang(90.0)
   tophi=ymaxt-height*2.0                    ! vertical placement of top text
   bothi=ymint+height*2.0                    ! vertical placement of bottom text
!-----------------------------------------------------------------------------------------------------------------------------------
   iprntl=0                                  ! flag if a string was drawn in left border or not
   xtemp=xmint+height*1.5 ! bad form to pass variable in common to a subroutine
   call xy_rasters(irazt)                       ! set text width
   call xy_printbanv('ban_lt',xtemp,tophi,-1.0,            iprntl)
   call xy_printbanv('ban_lm',xtemp,(tophi+bothi)/2.0,-0.5,iprntl)
   call xy_printbanv('ban_lb',xtemp,bothi, 0.0,            iprntl)
   if(iprntl.ne.0)then
      xmint=xmint+2.0*height
      call xy_rasters(irazl)                        ! set line width
      call move2(xmint,tophi)
      call draw2(xmint,bothi)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   iprntr=0                                  ! flag if a string was drawn in right border or not
   xtemp=xmaxt-height*0.5 ! bad form to pass variable in common to a subroutine
   call xy_rasters(irazt)                        ! set text width
   call xy_printbanv('ban_rt',xtemp,tophi,-1.0,            iprntr)
   call xy_printbanv('ban_rm',xtemp,(tophi+bothi)/2.0,-0.5,iprntr)
   call xy_printbanv('ban_rb',xtemp,bothi, 0.0,            iprntr)
   if(iprntr.ne.0)then
      call xy_rasters(irazl)                        ! set line width
      xmaxt=xmaxt-2.0*height
      call move2(xmaxt,tophi)
      call draw2(xmaxt,bothi)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  horizontal text
!-----------------------------------------------------------------------------------------------------------------------------------
   call textang(0.0)
   tophi=ymaxt-height*1.5     ! vertical placement of top text
   bothi=ymint+height*0.5     ! vertical placement of bottom text
!-----------------------------------------------------------------------------------------------------------------------------------
   iprntb=0                           ! flag whether a string was drawn at bottom or not
   call xy_rasters(irazt)                        ! set text width
   call xy_printban('ban_bl',xmint+width,bothi, 0.0,iprntb)
   call xy_printban('ban_bm',centr,      bothi,-0.5,iprntb)
   call xy_printban('ban_br',xmaxt-width,bothi,-1.0,iprntb)
   if(iprntb.ne.0)then
      call xy_rasters(irazl)                        ! set line width
      bothi=bothi+height*1.5
      call move2(xmint,bothi)
      call draw2(xmaxt,bothi)
      ymint=ymint+2.0*height
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   iprntt=0                           ! flag whether a string was drawn at top or not
   call xy_rasters(irazt)                        ! set text width
   call xy_printban('ban_tl',xmint+width,tophi, 0.0,iprntt)
   call xy_printban('ban_tm',centr,      tophi,-0.5,iprntt)
   call xy_printban('ban_tr',xmaxt-width,tophi,-1.0,iprntt)

   if(iprntt.ne.0)then
      call xy_rasters(irazl)                        ! set line width
      tophi=tophi-height*0.5
      call move2(xmint,tophi)
      call draw2(xmaxt,tophi)
      ymaxt=ymaxt-2.5*height
   else
      ymaxt=ymaxt-(ymaxt-ymint)*0.02   ! move title a little down from the top to put space over the title
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call xy_rasters(irazt)                 ! set text width
   call plot_set_plot_area(xmint,xmaxt,ymint,ymaxt)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine xy_bans
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_printban(3f) - [M_xyplot] plot horizontal banner string at specified location
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_printban(name,startx,starty,slenfac,iprinted)
use M_draw
implicit none

! ident_21="@(#)M_xyplot::xy_printban(3f): plot horizontal banner string at specified location"

character(len=*),intent(in)    :: name
real,intent(in)                :: startx
real,intent(in)                :: starty
real,intent(in)                :: slenfac
integer,intent(inout)          :: iprinted
character(len=255)             :: temp1
integer                        :: ilen
integer                        :: ier
real                           :: slen
real                           :: place
      call xy_retrv2(name,temp1,ilen,ier)
      if(ilen.gt.0)then                   ! if string not blank
         slen=xy_ustrlen(temp1(:ilen))    ! find length of string
         place=startx+(slenfac*slen)      ! left, right, or center justify
         call move2(place,starty)         ! move to desired beginning of string
         call drawstr(temp1)              ! draw the string
         iprinted=iprinted+1              ! increment count of non-blank strings
      endif
end subroutine xy_printban
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_printbanv(3f) - [M_xyplot] plot vertical banner string
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_printbanv(name,startx,starty,slenfac,iprinted)
use M_draw
implicit none

! ident_22="@(#)M_xyplot::xy_printbanv(3f): plot vertical banner string"

character(len=*),intent(in)    :: name
real,intent(in)                :: startx
real,intent(in)                :: starty
real,intent(in)                :: slenfac
integer,intent(inout)          :: iprinted

character(len=255)             :: temp1
integer                        :: ilen
integer                        :: ier
real                           :: slen
real                           :: place
   call xy_retrv2(name,temp1,ilen,ier)
   if(ilen.gt.0)then
      slen=xy_ustrlen(temp1(:ilen))
      place=starty+(slenfac*slen)
      call move2(startx,place)
      call drawstr(temp1)
      iprinted=iprinted+1
   endif
end subroutine xy_printbanv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_fxed(3f) - [M_xyplot] select whether a string is fixed-space or not
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_fxed(string,llisub)
implicit none

! ident_23="@(#)M_xyplot::xy_fxed(3f): select whether a string is fixed-space or not"

character(len=*),intent(in)      :: string
logical,intent(out)              :: llisub
character(len=255)               :: temp1
integer                          :: lennum
integer                          :: ier
   call xy_retrv2(string,temp1,lennum,ier)
   if(temp1.eq.'on')then
      llisub=.true.
   elseif(temp1.eq.'off')then
      llisub=.false.
   endif
end subroutine xy_fxed
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_init_graphics(3f) - [M_xyplot] Initialize PLT graphics environment
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_init_graphics()
use M_journal,    only : journal
use M_kracken,    only : iget, sget, store
use M_draw
implicit none

! ident_24="@(#)M_xyplot::xy_init_graphics(3f): Initialize PLT graphics environment"

character(len=256) :: value
character(len=256) :: cmd_d
character(len=256) :: hcopyfile
integer            :: i
integer            :: idum
integer            :: ierr
integer            :: ios
integer            :: ix
integer            :: iy
integer            :: lll2
real               :: shape
real               :: xx
real               :: yy
!-----------------------------------------------------------------------------------------------------------------------------------
   value=' '                                               ! DEVICE [XRASTERS YRASTERS LEFT_EDGE TOP_EDGE ]
!-----------------------------------------------------------------------------------------------------------------------------------
   call get_environment_variable('M_DRAW_DEVICE',value)    ! use device name from environment and do not prompt if set
!-----------------------------------------------------------------------------------------------------------------------------------
   cmd_d=sget('plt_d')                                     ! check for command line argument
   if(cmd_d.ne.' ')then
      value=cmd_d
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ios=0
   do i=1,3                                                ! if still blank try to read a value up to three times
      if(value.eq.' ')then
         read(*,'(a)',iostat=ios) value
      else
         exit
      endif
   enddo
   if(ios.ne.0.or.value.eq.' ')stop
!-----------------------------------------------------------------------------------------------------------------------------------
   value=adjustl(value)
   lll2=index(value(:len_trim(value)),' ')         ! if a space is in the name; size parameters are present
   if(lll2.gt.1) then
      call xy_setsize(value(lll2:))                ! set output device size and border
      value=value(:lll2)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  command arguments for overall size
   ix=iget('plt_x')
   iy=iget('plt_y')
   if(ix.gt.0.and.iy.gt.0)then
      call prefsize(ix,iy)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  set up output file if a batch device
   CHECK_INTERACTIVE: SELECT CASE ( value )
   CASE ('X11','x11','tek','xtek','PC','xt')    ! interactive device
      call journal('s','interactive device')
   CASE ('nil')                                 ! null device
      ipausq=0                                  ! p command off
      call journal('s','null device')
   CASE DEFAULT                                 ! batch device
      ipausq=0                                  ! p command off
      hcopyfile=' '
      call get_environment_variable('PLTHCOPY',hcopyfile)
      if(hcopyfile.eq.' ')then
         hcopyfile='hcopy.plt'
      endif
      call voutput(trim(hcopyfile))             ! open output file for a batch output device run
      call journal('s','batch mode')
   END SELECT CHECK_INTERACTIVE
!-----------------------------------------------------------------------------------------------------------------------------------
   call vinit(value)
!-----------------------------------------------------------------------------------------------------------------------------------
   call vflush()                                ! forces a flush
   call stuffa('$DEVICE',value,'')         ! set calculator variable so user can know what device is
   call getdisplaysize(xx,yy)                   ! get screen size in terms of raster units
   shape=xx/yy                                  ! determine initial aspect ratio of drawing surface and set drawing area accordingly
   call xy_aspct(0.0,shape,0.0,1.0)
   call store('aspect_oo',shape,'replace',ierr)              ! store current shape into dictionary
   call centertext(.false.)                                  ! do not center text
   call vsetflush(.false.)                                   ! turn off automatic flushing
   call vflush()                                             ! forces a flush
   if(value.eq.'x11'.or.value.eq.'X11'.or.value.eq.'PC')then
      idum=checkkey()
      call plot_clear('all')                                 ! as recommended, we clear after initializing graphics
      call frontbuffer()
      call vflush()                                          ! forces a flush
   else
      call plot_clear('all')                                 ! as recommended, we clear after initializing graphics
   endif

   call font('futura.l')                                     ! set preferred default font
   call vflush()                                             ! forces a flush
   !call xy_setsize('-1 -1 -1 -1')                           ! so an output device does not pick up this size
   call xy_jumapc('reload')                                  ! load PLT color table for first device
end subroutine xy_init_graphics
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_ifdrawn(3f) - [M_xyplot] xy_ifdrawn() is used to see if first page drawn or not
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
logical function xy_ifdrawn()
implicit none

! ident_25="@(#)M_xyplot::xy_ifdrawn(3f): xy_ifdrawn() is used to see if first page drawn or not"

! if never called before in this program execution return .false. else return .true.
logical,save :: ifcalled=.false.

   if(ifcalled .eqv. .false.) then
      ifcalled=.true.
      xy_ifdrawn=.true.
   else
      xy_ifdrawn=.false.
   endif
end function xy_ifdrawn
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_iftext(3f) - [M_xyplot] Add user-specified text strings to plot
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_iftext()
use M_journal, only : journal
use M_draw
use M_drawplus, only : plain_rect
implicit none

! ident_26="@(#)M_xyplot::xy_iftext(3f): Add user-specified text strings to plot"

real        :: asz
integer     :: i20
integer     :: i30
integer     :: iboxc
integer     :: icenter
integer     :: icount
integer     :: ilines
real        :: rlen
real        :: rscale
real        :: rxleft
real        :: rxright
real        :: rxto
real        :: ry1
real        :: rybot
real        :: ryto
real        :: rytop
real        :: ticlen
real        :: tlong
real        :: where
real        :: x1
real        :: xleft
real        :: xmaxt
real        :: xmint
real        :: xput
real        :: xright
real        :: y1
real        :: ymaxt
real        :: ymint

!      tuq(ICSQ)                 user-specified text strings
!      itucq(ICSQ)               text color
!      itumaxq                  index of highest non-blank text string
!      tuxq(ICSQ),tuyq(ICSQ)      position of text strings
!      tusq(ICSQ)                text size
!      tufq(ICSQ)                text font name
!      itubq(ICSQ)               flag if to box text or not and if so, what color
!      tuaszq(ICSQ)              arrow size
   if(itumaxq.le.0)return  ! index of highest non-blank text string
   ! if kept track, could eliminate a lot of redundant calls when color, font name, and such not changing
!-----------------------------------------------------------------------------------------------------------------------------------
   call push()                               ! Set the view up
   call centertext(.false.)                        ! do not center text
   call xy_rasters(plot_ids(0)%width)                   ! set curve width to pen 0
!-----------------------------------------------------------------------------------------------------------------------------------
!     set up character sizes and marker size
   call plot_get_plot_area(xmint,xmaxt,ymint,ymaxt)
   tlong=max(xmaxt-xmint,ymaxt-ymint)
   TICLEN=0.020*tlong  ! marker size
!-----------------------------------------------------------------------------------------------------------------------------------
   icount=1
   rscale=TICLEN*tusq(1)                    ! marker size * user size
   do 20 i20=1,itumaxq
      if(tuq(i20).eq.' ')goto 20                ! skip blank lines
      if(itoq(i20).eq.2)goto 20                 ! this line is off, skip it
!-----------------------------------------------------------------------------------------------------------------------------------
      x1=tuxq(i20)                              ! get text position
      y1=tuyq(i20)
      Ry1=xy_con_y(tuyq(i20))
      if(i20.gt.1)then
         ! if same coordinates as above, this is a block of text
         if(x1.eq.tuxq(i20-1).and.y1.eq.tuyq(i20-1).and.tuq(i20-1).ne.' ')then
            Ry1=Ry1-rscale*icount
            icount=icount+1
         else
            icount=1
         endif
      else
         icount=1
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(tufq(i20).ne.' ')then
         call xy_jufont(tufq(i20))                 ! set font
      else
         call journal('sc','*xy_iftext* blank font for ',i20)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     size the text
      rscale=TICLEN*tusq(i20)             ! marker size * user size
      call textsize(rscale,rscale)           ! set size after selecting font
!-----------------------------------------------------------------------------------------------------------------------------------
      if(icount.eq.1)then ! first line in this block so save first set of numbers and box color in case boxing the text
         iboxc=itubq(i20)
!        need the box under text so have to look ahead
         ilines=0
         xput=tuxq(i20)   ! x position in rscaled units
         xright=0.0       ! length of string in unscaled units to right of xput
         xleft=0.0        ! length of string in unscaled units to left of xput
         do i30=i20,itumaxq
            if(tuxq(i30).ne.tuxq(i20).or.tuyq(i30).ne.tuyq(i20))goto 40 ! encountered a line not part of this text block
            ilines=ilines+1
            if(tuq(i30).ne.' ')then
               rlen=xy_ustrlen(tuq(i30))
               if(itujq(i30).eq.0)then        ! left justify the text
                  xright=max(xright,rlen)
               else                           ! centered text
                  xright=max(xright,rlen/2.0)
                  xleft=min(xleft,-rlen/2.0)
               endif
            endif
         enddo
40       continue

         Rytop=xy_con_y(tuyq(i20))+rscale*3.0/2.0
         Rybot=Rytop-(ilines+1)*rscale
         if(itujq(i30).eq.0)then ! text is center middle justified, so make some room
            Rybot=Rybot-rscale/2.0
         endif
         Rxleft=xy_con_x(xput)+xleft-rscale/2.0
         Rxright=xy_con_x(xput)+xright+rscale/2.0
         if(iboxc.ge.0.and.i20.ne.0)then
            call color(iboxc)
            call polyfill(.true.)
            call makepoly()
            call plain_rect(Rxleft,Rybot,Rxright,Rytop)
            call closepoly()
            call polyfill(.false.)
            call color(iforeq)           ! draw box in foreground color around the region
            call plain_rect(Rxleft,Rybot,Rxright,Rytop)
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call color(iforeq)
      where=taoq(i20)
      Rxto=xy_con_x(taxq(i20))
      Ryto=xy_con_y(tayq(i20))
      asz=tuaszq(i20)*tlong/100.0
      call xy_arrow(Rxleft,Rybot,Rxright,Rytop,Rxto,Ryto,where,asz)
!-----------------------------------------------------------------------------------------------------------------------------------
      if(tufq(i20).eq.'markers')then
         call centertext(.true.) ! always turn centering on for markers
         Ry1=Ry1+(1.0-0.70)/2.0*rscale ! adjust M_plot hershey font for middle vertical justification for marker font
      else
         if(itujq(i20).eq.0)then     ! xy_juprint does not recognize centertext
            !call centertext(.false.)
            icenter=2
         else
            !call centertext(.true.)
            icenter=1
         endif
      endif
      call color(itucq(i20))                   ! set color
      if(i20.ne.0)then             ! test flag to see if this is an inside legend box
         call xy_juprint(xy_con_x(x1),Ry1,tuq(i20),icenter)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
20 continue
!-----------------------------------------------------------------------------------------------------------------------------------
   call centertext(.false.) ! do not center text
   call pop()
end subroutine xy_iftext
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_jucurv(3f) - [M_xyplot] find extrema for group of curves and set legend label defaults
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!    find extrema for group of curves to be plotted and set legend label defaults if labels are blank
!!    set icrvsq and icrvs2q
!!    note that you have to "skip over" a 0 curve when assigning line attributes and legend labels
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_jucurv(xlowa,xhigha,ylowa,yhigha,iunits,ierr)
use M_journal,         only : journal
use M_draw,            only : vgetdev
implicit none

! ident_27="@(#)M_xyplot::xy_jucurv(3f): find extrema for group of curves and set legend label defaults"

real,intent(out)            :: xlowa(2),xhigha(2),ylowa(2),yhigha(2)
integer,intent(out)         :: iunits(4)
integer,intent(out)         :: ierr

integer                     :: inums
character(len=20)           :: devname
character(len=255)          :: msg
real                        :: rcurv1(ICSQ,4),rcurv(ICSQ,4)
integer                     :: i10, i15, i20, i30, i40, i50
integer                     :: icurve
integer                     :: icurve3
integer                     :: icurve4
integer                     :: idum
integer                     :: ierr2
integer                     :: ifile
integer                     :: ifile2
integer                     :: ifile3
integer                     :: ifilevs
integer                     :: iput
integer                     :: ipvs
integer                     :: iside
integer                     :: itime
integer                     :: itime3
integer                     :: itime4
integer                     :: itimevs
integer                     :: ivals
integer                     :: iworked
integer                     :: izeros
real                        :: rfile
real                        :: xhigh
real                        :: xlow
real                        :: yhigh
real                        :: yhigh3
real                        :: yhigh4
real                        :: ylow
real                        :: ylow3
real                        :: ylow4
!-----------------------------------------------------------------------------------------------------------------------------------
      ierr=0                            ! change to non-zero if nothing worth plotting
      iworked=0                         ! number of curves actually displayed
!-----------------------------------------------------------------------------------------------------------------------------------
      temp1q=fetch('plot_f')       ! figure out what file to read the plots from
      if(temp1q.eq.' ')then             ! explicitly set the file on the plot command
         ifile=ifooq2                   ! file number to read curves from or 0, as default from f command
      else                              ! not explicitly set on the plot command, so use the default
         ifile=inum0(temp1q)            ! file number to read curves from or 0
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      do i15=1,4                        ! blank out axis labels for unit codes above 1000
         axislq(i15)=' '                ! used in xy_setlbl()
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      do i20=1,2
         xlowa(i20)=0.0
         xhigha(i20)=0.0
         ylowa(i20)=0.0
         yhigha(i20)=0.0
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
!     blank out storage of data labels
      do i50=1,ICSQ
         lgndq(i50)=' '
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      rfile=ifile
      call strgar3(fetch('plot_oo'),ICSQ,rfile,rcurv1,inums,' ',' ',ierr2) ! get the list of curve numbers
!-----------------------------------------------------------------------------------------------------------------------------------
      if(inums.le.0)then  ! must specify a list of curves
         call journal('*xy_jucurv* must specify list of curves')
         ierr=-1
         goto 999
      endif
      ! expand list of curve numbers up to ICSQ curves (a -b means from curve a to curve b)
      call xy_listout3(rcurv1,ICSQ,rcurv,ICSQ,ICSQ,inums)
!-----------------------------------------------------------------------------------------------------------------------------------
!     ignore trailing and duplicate zeros in curve list
      izeros=0                                               ! number of zero curves encountered
      icrvs2q=0
      if(inums.gt.1)then
         iput=1                                          ! where to put next curve to keep (any curve not the 2nd or higher 0 curve)
         do i40=2,inums
            icurve=rcurv(i40,1)
            if(icurve.ne.0)then
               iput=iput+1
               rcurv(iput,1)=icurve
               rcurv(iput,2)=rcurv(i40,2)
            elseif(izeros.eq.0)then
               icrvs2q=iput                                  ! keep track of where the last curve on left side is stored
               izeros=1
            else
               izeros=izeros+1
            endif
         enddo
         if(izeros.gt.1)then                                 ! a nice little reminder message something looked suspicious
            call journal('*xy_jucurv* multiple zeros ignored')
         endif
         if(izeros.gt.0.and.icrvs2q.ge.iput)then             ! first zero was a trailing zero
            izeros=0
         endif
         inums=iput
      endif
      icrvsq=inums                                      ! number of curves read off last plot command including up to one zero curve
      if(izeros.eq.0)icrvs2q=icrvsq
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_iflou(1)                                     ! set flag on to ignore values .le. 0 in logarithmic plots
!-----------------------------------------------------------------------------------------------------------------------------------
      icurve=rcurv(1,1)                                 ! get file number and record number first curve on
      ifile2=rcurv(1,2)                                 ! get file number and record number first curve on
      call xy_getdat(ifile2,icurve,ylow,yhigh,itime,idum,1)
      !-------------------------------------------------------  include extrema from error bar curves if any
      if(rcurv(1,3).ne.0)then
         icurve3=rcurv(1,3)
         call xy_getdat(ifile2,icurve3,ylow3,yhigh3,itime3,idum,0)
         ylow=min(ylow3,ylow)
         yhigh=max(yhigh3,yhigh)
         if(itime.ne.itime3)then
            call journal('*xy_jucurv* warning: top error bar X values do not match curve')
         endif
      endif
      if(rcurv(1,4).ne.0)then
         icurve4=rcurv(1,4)
         call xy_getdat(ifile2,icurve4,ylow4,yhigh4,itime4,idum,0)
         ylow=min(ylow4,ylow)
         yhigh=max(yhigh4,yhigh)
         if(itime.ne.itime4)then
            call journal('*xy_jucurv* warning: bottom error bar X values do not match curve')
         endif
      endif
      !-------------------------------------------------------
      iunits(2)=iunitq                                              ! store units for xy_setlbl procedure
      iunits(4)=iunitq
      axislq(2)=alpha2q
      axislq(4)=alpha2q

      if(itime.le.0)then                                            ! requested data not found
         write(msg,'(a,i5,a,i2)')'curve ',icurve,' not on file ',ifile2
         call journal(msg)
         goto 999                                                   ! could not get curve requested
      endif
      call xy_getdat(ifile2,itime,xlow,xhigh,idum,ivals,-1)            ! get x-axis data, should check for not found
      iunits(1)=iunitq                                              ! store units for xy_setlbl procedure
      iunits(3)=iunitq
      axislq(1)=alpha2q
      axislq(3)=alpha2q
      iworked=1                                                     ! increment number of curves ok
!-----------------------------------------------------------------------------------------------------------------------------------
      call vgetdev(devname)                      ! get name of current device
      if(devname.eq.'tek')then ! bug in xterm emulator - need to move down two lines
         call journal('s',' ')
         call journal('s',' ')
      endif
      write(msg,101)ifile2,int(rcurv(1,1)),ivals,xlow,xhigh,ylow,yhigh
101   format('f=',i3,';curve=',i5,';pts=',i7,';range=',4(g11.4,1x))
      call journal('s',msg)
!-----------------------------------------------------------------------------------------------------------------------------------
      do i30=1,2
         xlowa(i30)=xlow
         xhigha(i30)=xhigh
         ylowa(i30)=ylow
         yhigha(i30)=yhigh
      enddo
      iside=1
!-----------------------------------------------------------------------------------------------------------------------------------
      if(inums.gt.1)then
         do i10=2,inums
            ! get file number and record number i10 curve stored on
            icurve=rcurv(i10,1)
            ifile3=rcurv(i10,2)
            call xy_getdat(ifile3,icurve,ylow,yhigh,itime,idum,i10)               ! check for not found
            !-------------------------------------------------------  include extrema from error bar curves if any
            if(rcurv(i10,3).ne.0)then
               icurve3=rcurv(i10,3)
               call xy_getdat(ifile2,icurve3,ylow3,yhigh3,itime3,idum,0)
               ylow=min(ylow3,ylow)
               yhigh=max(yhigh3,yhigh)
               if(itime.ne.itime3)then
                  call journal('*xy_jucurv* warning: top error bar X values do not match curve')
               endif
            endif
            if(rcurv(i10,4).ne.0)then
               icurve4=rcurv(i10,4)
               call xy_getdat(ifile2,icurve4,ylow4,yhigh4,itime4,idum,0)
               ylow=min(ylow4,ylow)
               yhigh=max(yhigh4,yhigh)
               if(itime.ne.itime4)then
                  call journal('*xy_jucurv* warning: bottom error bar X values do not match curve')
               endif
            endif
            !-------------------------------------------------------
            if(itime.le.0)then
               write(msg,'(a,i5,a,i2)')'curve ',icurve,' not on file ',ifile3
               call journal(msg)
               continue                                                              ! could not get curve requested
            else
               iworked=iworked+1                                                     ! increment number of curves ok
            endif
            if(i10.eq.icrvs2q+1)then                                                 ! first curve of left group
               iunits(4)=iunitq                                                      ! store axis unit value
               axislq(4)=alpha2q
               iside=2                                                               ! flag now working with second set of values
               ylowa(iside)=ylow                         ! extremes of first curve after 0 become initial extremes for right axis
               yhigha(iside)=yhigh
            endif
            ! see if time range has changed
            call xy_getdat(ifile3,itime,xlow,xhigh,idum,ivals,-1)                    ! get x-axis data, should check for not found
            ylowa(iside)=min(ylow,ylowa(iside))
            yhigha(iside)=max(yhigh,yhigha(iside))
            xlowa(1)=min(xlow,xlowa(iside))
            xhigha(1)=max(xhigh,xhigha(iside))
            write(msg,101)ifile3,int(rcurv(i10,1)),ivals,xlow,xhigh,ylow,yhigh
            call journal('s',msg)
         enddo
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     if forcing a curve to be the x-axis values, then all curves
!     are to be checked to make sure they have the same time values
!     this check is done in xy_judraw
      ipvs=inum0(fetch('plot_vs'))
      if(ipvs.gt.0)then
         ifilevs=ifile
         call xy_getdat(ifilevs,ipvs,xlow,xhigh,itimevs,idum,-1)            ! get x-axis data
         if(itimevs.lt.0)then
            call journal('*xy_jucurv* user specified x-axis data not found')
            goto 999
         endif
         xlowa(1)=xlow
         xhigha(1)=xhigh
         iunits(1)=iunitq                                                ! store units for xy_setlbl procedure
         axislq(1)=alpha2q
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(aymulq.ne.0.and.iyuniq.ge.0)then
         iunits(4)=iyuniq                                                ! forced right units for right axis scaled from left
         axislq(4)=alpha2q
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue
      if(iworked.eq.0)ierr=-1                                            ! if no curves ok, return a flag to not draw anything
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_iflou(2)                                                   ! set flag off to ignore values .le. 0 in logarithmic plots
!-----------------------------------------------------------------------------------------------------------------------------------
      if(icrvs2q.eq.icrvsq)then                                       ! no zero in curve list to cause a second y-axis to be created
         ylowa(2)=ylowa(1)
         yhigha(2)=yhigha(1)
         iunits(4)=iunits(2)
         axislq(4)=axislq(2)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      xlowa(2)=xlowa(1)                                                  ! only doing one x-axis scale for now
      xhigha(2)=xhigha(1)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine xy_jucurv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_jumapc(3f) - [M_xyplot] map colors using HUE command
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_jumapc(mode)
use M_journal, only : journal
use m_color, only: hue, color_name2rgb
use M_draw, only : mapcolor
use M_strings, only: v2s, listout
implicit none

! ident_28="@(#)M_xyplot::xy_jumapc(3f): map colors using HUE command"

! 1989 John S. Urban

character(len=20)             :: echoname
character(len=:),allocatable  :: fmt
real                          :: red,green,blue
real                          :: rr,gg,bb
integer                       :: i

character(len=*)     :: mode      ! 'reset' or 'reload' or 'define'

real :: vals(6)   ! array to store color values into
real :: factor
real :: plus1
real :: plus2
real :: plus3
real :: steps
real :: valb
real :: valg
real :: valr
real :: rlist(256)

integer :: ilist(256)
integer :: i_findend
integer :: i_noneg
integer :: i_print
integer :: i_reload
integer :: i_reset
integer :: i_toobig
integer :: iblue
integer :: ici
integer :: iend
integer :: ier
integer :: ifound
integer :: inumbers
integer :: igreen
integer :: ilen
integer :: ilen2
integer :: ipen_number
integer :: ired
integer :: ist
integer :: itemp
integer :: itypes
integer :: ix
integer,parameter :: imax=255 ! number of hues

character(len=255)   :: ctemp
character(len=255)   :: cnums
character(len=3)     :: modl
character(len=100)   :: line

integer,save         :: ir(0:IMAX),ig(0:IMAX),ib(0:IMAX)
integer,save         :: jr(0:IMAX),jg(0:IMAX),jb(0:IMAX)
integer,save         :: nmax=15

data ir/-255, 255,   0, 255,   0, 255,   0,  -1, 155,   0, 155, 155,   0, 155,   0, 100, 240*255 /
data ig/ 255,   0, 255, 255,   0,   0, 255,   0,   0, 155, 255, 155,   0,   0, 155, 100, 240*255 /
data ib/ 255,   0,   0,   0, 255, 255, 255,   0,   0,   0, 255,   0, 155, 155, 155, 100, 240*255 /
data jr/-255, 255,   0, 255,   0, 255,   0,  -1, 155,   0, 155, 155,   0, 155,   0, 100, 240*255 /
data jg/ 255,   0, 255, 255,   0,   0, 255,   0,   0, 155, 255, 155,   0,   0, 155, 100, 240*255 /
data jb/ 255,   0,   0,   0, 255, 255, 255,   0,   0,   0, 255,   0, 155, 155, 155, 100, 240*255 /
!-----------------------------------------------------------------------------------------------------------------------------------
! reload all pens except pens 0 and 7 when drivers change;
! M_DRAW does not normally retain color maps between driver changes
!-----------------------------------------------------------------------------------------------------------------------------------
   if(mode.eq.'reload')then
      RELOAD: do i_RELOAD=0,nmax
         if((ir(i_RELOAD).ge.0).and.(ig(i_RELOAD).ge.0).and.(ib(i_RELOAD).ge.0))then
            call mapcolor(i_RELOAD,abs(ir(i_RELOAD)),abs(ig(i_RELOAD)),abs(ib(i_RELOAD)))
         endif
      enddo RELOAD
      return
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(mode.eq.'reset')then
      RESET: do i_RESET=0,nmax
         ir(i_RESET)=jr(i_RESET)
         ig(i_RESET)=jg(i_RESET)
         ib(i_RESET)=jb(i_RESET)
         if((ir(i_RESET).ge.0).and.(ig(i_RESET).ge.0).and.(ib(i_RESET).ge.0))then
            call mapcolor(i_RESET,abs(ir(i_RESET)),abs(ig(i_RESET)),abs(ib(i_RESET)))
         endif
      enddo RESET
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call xy_retrv2('hue_oo',ctemp,ilen,ier)
   itypes=0  ! check number of models specified on this command
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen.eq.0)then          ! color index missing on color command so show chart and (optionally)  named colors
      !-------------------------------------------------------------
      ! find last non-black pen
      iend=IMAX
      FINDEND: do i_FINDEND=IMAX,0,-1
         if((ir(i_FINDEND).eq.IMAX).and.(ig(i_FINDEND).eq.IMAX).and.(ib(i_FINDEND).eq.IMAX))then
            iend=i_FINDEND
         else
            exit
         endif
      enddo FINDEND
      continue
      !-------------------------------------------------------------
      call xy_retrv2('hue_name',ctemp,ilen,ier)
      if(ilen.eq.0)then                                         ! list recognized color names
         TRYALL: do i=1,10000
            ! weird little thing where the color names have aliases that are numeric strings
            call color_name2rgb(v2s(i),red,green,blue,echoname) ! get the RGB values and English name of the color
            if(echoname.eq.'Unknown')exit TRYALL                ! the last color name is "Unknown" so the loop should exit
            fmt='(a,3(i3,"%",1x),"#",3(z2.2),1x,3(i3,1x))'
            ! display the English name and RGB values for the name
            write(line,fmt)echoname,int([red,green,blue]/1.00), int([red,green,blue]*2.55+0.5) ,int([red,green,blue]*2.55+0.5)
            call journal(line)
         enddo TRYALL
      else                                                      ! print current pen definitions
         PRINT: do i_PRINT=0,iend
            if(min(ir(i_PRINT),ig(i_PRINT),ib(i_PRINT)).ge.0)then
               write(line,101)i_PRINT,ir(i_PRINT)*100/255, ig(i_PRINT)*100/255,ib(i_PRINT)*100/255, ' '
            else
               write(line,101)i_PRINT,ir(i_PRINT)*100/255, ig(i_PRINT)*100/255, ib(i_PRINT)*100/255,' # ignored'
            endif
            call journal(line)
         enddo PRINT
101      format('hue ',i0,' -rgb ',3(i4.3,1x),a)
         if(i_PRINT.ne.IMAX)then
            call journal('#... the rest (up to 255) are currently black')
         endif
      endif
      !-------------------------------------------------------------
      call plot_marks('hue')
      return
!-----------------------------------------------------------------------------------------------------------------------------------
   else  ! some color numbers were specified
      call strgar2(ctemp,imax,rlist,inumbers,' ,',ier)  ! put list of hues selected into ilist(:inumbers)
      call listout(int(rlist(1:inumbers)),ilist,inumbers,ier)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen2=1
   cnums=' '
!-----------------------------------------------------------------------------------------------------------------------------------
   call xy_retrv2('hue_rgb',ctemp,ilen,ier)
   if(ilen.ne.0)then
      ilen2=ilen
      cnums=ctemp
      itypes=1
      modl='rgb'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call xy_retrv2('hue_name',ctemp,ilen,ier)                    ! use color name to get RGB values and proceed as if -rgb used
   if(ctemp.ne.'#N#')then                                    ! try to find RGB values of name
      call color_name2rgb(ctemp,rr,gg,bb,echoname)           ! get the RGB values for the English color name
      if(echoname.ne.'Unknown')then
         ctemp=v2s(rr)//' '//v2s(gg)//' '//v2s(bb)           ! make string of RGB values so can proceed as if -rgb used
      else
         call journal('*xy_jumapc*: error: unknown name '//trim(ctemp) )
         ctemp='0 0 0 '
      endif
      ilen2=len_trim(ctemp)
      cnums=ctemp
      itypes=1
      modl='rgb'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call xy_retrv2('hue_hls',ctemp,ilen,ier)
   if(ilen.ne.0)then
      if(itypes.ne.0)then
         call journal('*xy_jumapc* more than one color model, rgb ignored')
      endif
      itypes=2
      ilen2=ilen
      cnums=ctemp
      modl='hls'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call xy_retrv2('hue_hsv',ctemp,ilen,ier)
   if(ilen.ne.0)then
      if(itypes.ne.0)then
         call journal('*xy_jumapc* more than one color model, using hsv')
      endif
      itypes=3
      ilen2=ilen
      cnums=ctemp
      modl='hsv'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(itypes.eq.0)then  ! just pen numbers but no model+values specified, so reset these pens if < nmax or warn
      RESETN: do i=1,inumbers
         ipen_number=ilist(i)
         !if(ipen_number.le.nmax.and.ipen_number.ge.0)then
         if(ipen_number.le.imax.and.ipen_number.ge.0)then
            ir(ipen_number)=jr(ipen_number) ! copy from constant values to current values
            ig(ipen_number)=jg(ipen_number)
            ib(ipen_number)=jb(ipen_number)
            call mapcolor(ipen_number,abs(ir(ipen_number)),abs(ig(ipen_number)),abs(ib(ipen_number)))
         else
            call journal('*xy_jumapc* hue number out of range, 0 <= '//v2s(ipen_number)//' <= '//v2s(imax))
         endif
      enddo RESETN
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call strgar2(cnums(:ilen2),6,vals,ifound,' ,',ier)  ! parse the color numbers
   if(ifound.lt.3)then
      call journal('*xy_jumapc* must have three values on color command')
      return
   endif

!  if more than three values assume defining a stepped range of colors
   steps=max(1,ilist(inumbers)-ilist(1))
   plus1=0.0
   plus2=0.0
   plus3=0.0

   if(ifound.gt.3)then
      plus1=(vals(4)-vals(1))/steps
   endif

   if(ifound.gt.4)then
      plus2=(vals(5)-vals(2))/steps
   endif

   if(ifound.gt.5)then
      plus3=(vals(6)-vals(3))/steps
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  factor to multiply input values by
!  useful if you have values done to a different scale than 0-100
   call xy_retrv2('hue_m',ctemp,ilen,ier)
   factor=rnum0(ctemp)
   if(factor.le.0)factor=1.0
!-----------------------------------------------------------------------------------------------------------------------------------
   SETPENS: do i=1,inumbers
      ici=ilist(i)
!-----------------------------------------------------------------------------------------------------------------------------------
      if(itypes.ne.1)then
         ix=2
         !     bring hue angle into range of 0 to 360 degrees for hue() routine
         if(vals(1).gt.360.0)then
            vals(1)=mod(vals(1),360.0)
         elseif(vals(1).lt.0)then
            itemp=abs(int(vals(1))/360)+1
            vals(1)=vals(1)+itemp*360.0
         endif
      else
         ix=1
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      TOOBIG: do i_TOOBIG=ix,3                    ! bring remaining values into range 0 to 100
         if(vals(i_TOOBIG).gt.100.0)then
            vals(i_TOOBIG)=aint(vals(i_TOOBIG)*factor)
            if(vals(i_TOOBIG).gt.100.0)then
               call journal('*xy_jumapc* color value too high('//v2s(vals(i_TOOBIG))//'), set to 100')
               vals(i_TOOBIG)=100.0
            endif
         endif
      enddo TOOBIG
!-----------------------------------------------------------------------------------------------------------------------------------
      NONEG: do i_NONEG=1,3                     ! no negative values allowed
         if(vals(i_NONEG).lt.0)then
            vals(i_NONEG)=0.0
            call journal('*xy_jumapc* negative color value set to zero')
         endif
      enddo NONEG
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ici.lt.0.or.ici.gt.imax)then
         call journal('*xy_jumapc* color index out of range 0 <= '//v2s(ici)//' <= '//v2s(imax))
         cycle setpens
      else
         nmax=max(nmax,ici) ! top color user is interested in in a reload
         nmax=min(nmax,imax) ! top color user is interested in in a reload
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call hue(modl,vals(1),vals(2),vals(3),'rgb',valr,valg,valb,ist)
      ired=int(valr*2.55)
      igreen=int(valg*2.55)
      iblue=int(valb*2.55)
      ir(ici)=ired
      ig(ici)=igreen
      ib(ici)=iblue
      call mapcolor(ici,ired,igreen,iblue)
      vals(1)=vals(1)+plus1
      vals(2)=vals(2)+plus2
      vals(3)=vals(3)+plus3
   enddo SETPENS
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine xy_jumapc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_pause(3f) - [M_xyplot] conditionally  pause until graphic or text-window response
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!    would prefer to use errin but not all FORTRAN implementations
!!    support it; even in 1 environments.
!!
!!    if a null line is entered, set calculator value PAUSE to 1,
!!    else set PAUSE to 0. This can be handy for creating break points
!!
!!    The calculator variable $PAUSE is set to the character entered, except
!!    that characters less than ADE 32 are set to a blank.
!!##OPTIONS
!!  constants:
!!
!!    ipausq=1 beep and pause until carriage return entered from standard input.
!!    ipausq=2          pause until carriage return entered from standard input.
!!    ipausq=0 ignore pause request
!!    ipausq=-1  pause until a character is entered from graphics input.
!!
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_pause()
!-----------------------------------------------------------------------------------------------------------------------------------
use M_journal,    only : journal
use M_draw
implicit none

! ident_29="@(#)M_xyplot::xy_pause(3f): conditionally  pause until graphic or text-window response"

character(len=1)       :: cjunk
integer                :: i10
integer                :: idum
integer                :: ios
integer                :: ivalue
!-----------------------------------------------------------------------------------------------------------------------------------
      cjunk=' '
      idum=0
      if(ipausq.eq.0)then
!        you may wish to turn page mode off
!        this could occur if you are reading files in batch mode
!        that you typically use for interactive mode but you do
!        not wish to make a copy of just to use without pauses.
         call stuff('PAUSE',1.0d0,'')
         return
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     call point2(XMIN0Q2,YMAX0Q2) ! home cursor - kludge
      call point2(0.0,0.0) ! home cursor - kludge
!-----
      call vflush()              ! flush graphics buffers
      call journal('s',pausq)          ! send bell character or user string
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ipausq.eq.-1)then
!       if standard  input is not coming from a terminal then you may want to do
!       a pause  from the  graphic  display  so that the  user can  still  pause
!       interactively  this would typically  occur if commands are coming from a
!       1 here document
        do i10=1,1000 ! flush key buffer
           if(checkkey().eq.0)exit
        enddo
        ivalue=getkey()           ! wait till keypress is read in graphic window
        ivalue=max(ivalue,32)     ! convert non-printable characters to a space
                                   ! (including carriage return in particular)
        cjunk=char(ivalue)
!-----------------------------------------------------------------------------------------------------------------------------------
      elseif(ipausq.eq.2)then
         read(*,'(a)',end=999,iostat=ios,err=999)cjunk
!-----------------------------------------------------------------------------------------------------------------------------------
      else
         read(*,'(a)',end=999,iostat=ios,err=999)cjunk
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(cjunk.eq.' ')then
         call stuff('PAUSE',1.0d0,'')
      else  ! flag to quit
         call stuff('PAUSE',0.0d0,'')
      endif
      call stuffa('$PAUSE',cjunk,'')
      return
999   continue
      call stuffa('$PAUSE',cjunk,'')
      call stuff('PAUSE',1.0d0,'')
end subroutine xy_pause
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_jutitl(3f) - [M_xyplot] add title lines to plot
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_jutitl(ifile,idonly)
! making xy_jutitlX PLT-independent to make generic plotting routines
use M_journal, only : journal
use M_draw
implicit none

! ident_30="@(#)M_xyplot::xy_jutitl(3f): add title lines to plot"

integer,intent(in)      :: ifile
integer,intent(in)      :: idonly
integer,parameter       :: isize=10
character(len=4096)     :: ftitle(isize)                ! hold title lines from the file
integer                 :: ititles

! grab up to 10 title lines from file to use as title or to use as substitution strings for user-specified title
   call xy_loadtl(ifile,ftitle,isize,ititles)
   ititles=max(ititles,1)
   call xy_jutitlX(idonly,ftitle,ititles)

end subroutine xy_jutitl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_jutitlX(3f) - [M_xyplot] draw plot titling information (titles and legend block)
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_jutitlX(idonly,ftitle,isize)
! not printing title lines beginning with #,$ allows comment information to be stored in input files
use M_journal, only : journal
use M_draw
implicit none

! ident_31="@(#)M_xyplot::xy_jutitlX(3f): draw plot titling information (titles and legend block)"

character(len=4096)  :: l
integer              :: isize
character(len=4096)  :: ftitle(isize)                ! hold title lines from the file
character(len=1)     :: cwhere
real                 :: centr
real                 :: factor
real                 :: height
integer              :: ichars
integer              :: icols
integer              :: icount
integer              :: idonly
integer              :: itchars
real                 :: r10
real                 :: tallest
real                 :: width
real                 :: xlong
real                 :: xmaxt
real                 :: xmint
real                 :: ydown
real                 :: ymaxt
real                 :: ymindum
real                 :: ymint
real                 :: ytall
real                 :: yup

   call centertext(.false.) ! turn off text centering
   call plot_get_plot_area(xmint,xmaxt,YMINDUM,ymaxt)
   centr=(xmaxt+xmint)/2.0  ! find center of drawing area for centering text
!-----------------------------------------------------------------------------------------------------------------------------------
!  initialize title line section EVEN if no title lines
   call xy_rasters(max(1,inum0(fetch('title_w'))))  ! calculate width of lines to draw title software text
   call color(max(0,inum0(fetch('title_c'))))    ! calculate color of title lines
   r10=ymaxt                                     ! vertical position to print the next title line
   call fixedwidth(LLQ(itifxedq))                  ! text is fixed space or not for title
   itchars=max(1,inum0(fetch('title_ch')))       ! get number of characters that must fit across title line
!-----------------------------------------------------------------------------------------------------------------------------------
   ! set up text size, line width, and font in case first title line is a comment or titles are off
   width=(xmaxt-xmint)/itchars                   ! biggest width of characters if fixed space
   width=width*0.95                              ! a little margin
   tallest=width*1.20                            ! max height whether fixed or proportional
   call font(fetch('title_fn'))
   call textsize(width,tallest)                  ! use factor of 1.2 between height and width
!-----------------------------------------------------------------------------------------------------------------------------------
!      if(.not.LLQ(ltitlq))then                        ! if title is off, go to section on legend labels
!         goto 50
!      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     set up first title line, coming up with a size that fits
   if(isize.gt.0)then
      l=ftitle(1)
      ichars=len_trim(l)                             ! number of characters in string
   else
      l=' '
      ichars=0
   endif
   if(ichars.ge.1)then
      if(.not.LLQ(itifxedq))then              ! resize if text is proportional
         width=(xmaxt-xmint)/itchars        ! calculate maximum character width at largest allowed size
         width=width*.95  ! a little margin
         height=width*1.2                   ! calculate nice character height
         call textsize(width,height)        ! set to largest allowed size
         !---------------------------------------------
         call priv_justrlen(l(:ichars),xlong,ytall,ydown,yup)
         !---------------------------------------------
         factor=(xmaxt-xmint)/xlong         ! make fit across entire width if used
         factor=0.95*factor
         call xy_set_bigger(tallest,width,height,factor)
         !---------------------------------------------
         call priv_justrlen(l(:ichars),xlong,ytall,ydown,yup)
         !---------------------------------------------
      else
         width=(xmaxt-xmint)/max(itchars,ichars)   ! calculate maximum character width
         width=width*.95  ! a little margin
         height=width*1.2                          ! calculate nice character height
         call textsize(width,height)
         !---------------------------------------------
         call priv_justrlen(l(:ichars),xlong,ytall,ydown,yup)
         !---------------------------------------------
      endif
      r10=r10-yup
      !-----------------------------------
      call xy_juprint(centr,r10,l(:ichars),1)
      !-----------------------------------
      r10=r10-ydown
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     print rest of title lines, making them smaller as required
   do icount=2,isize
      l=ftitle(icount)
      ichars=len_trim(l)                    ! number of characters in string plus some border characters
      if(ichars.ge.1)then
         !---------------------------------------------
         call priv_justrlen(l(:ichars),xlong,ytall,ydown,yup)
         !---------------------------------------------
         factor=(xmaxt-xmint)/xlong
         if(factor.lt.1.0)then           ! if longer than allowed will shorten it (and set character size smaller for all lines)
            factor=factor*0.95
            call xy_set_bigger(tallest,width,height,factor)
            !---------------------------------------------
            call priv_justrlen(l(:ichars),xlong,ytall,ydown,yup)
            !---------------------------------------------
         endif
         r10=r10-yup
         !-----------------------------------
         call xy_juprint(centr,r10,l(:ichars),1)
         !-----------------------------------
         r10=r10-ydown
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
!50    continue
   call fixedwidth(LLQ(iidfxedq))                      ! text is fixed space or not for id labels
   cwhere(1:1)=fetch('idbox_p')
   if(index('pae',cwhere).gt.0)then                    ! use xy_tidybox to draw legend box inside box area later overlaying plot
      call plot_get_plot_area(xmint,xmaxt,ymint,ymaxt) ! space down a little so title is not crowded against plot
      ymaxt=r10
      call plot_set_plot_area(xmint,xmaxt,ymint,ymaxt)
   elseif(cwhere.eq.'b')then
      icols=inum0(fetch('idbox_c'))
      icols=max(1,icols)
      call xy_idbox(r10,icols,idonly)                  ! legend below plot
   elseif(cwhere.eq.'d')then
      call xy_idbox0(r10,idonly)                       ! default legend above plot
   else
      call journal('*xy_jutitl* unknown xy_idbox location')
   endif
   call fixedwidth(.false.)                            ! text is not fixed space
!     call color(max(0,inum0(fetch('title_c'))))       ! reset color after drawing lines to title line color
   call color(iforeq)                                  ! reset color after drawing lines
end subroutine xy_jutitlX
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_set_bigger(3f) - [M_xyplot] change title line size
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_set_bigger(tallest,width,height,factor)
use M_draw
implicit none

! ident_32="@(#)M_xyplot::xy_set_bigger(3f): change title line size"

real,intent(in)    :: tallest
real,intent(out)   :: width
real,intent(out)   :: height
real,intent(in)    :: factor

   if(height*factor.le.tallest)then ! if new height less than tallest allowed
      width=width*factor
      height=height*factor
      call textsize(width,height)
   endif
end subroutine xy_set_bigger
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_laxis(3f) - [M_xyplot] draw linear axis and logarithmic axis for PLT
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_laxis(ixdiv,iydiv) ! draw the axes
use M_journal, only : journal
use M_draw
implicit none

! ident_33="@(#)M_xyplot::xy_laxis(3f): draw linear axis and logarithmic axis for PLT"

!-----------------------------------------------------------------------------------------------------------------------------------
!     ixdiv and iydiv are number of major and minor axis divisions
integer                     :: ixdiv(4), iydiv(4)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=20)           :: hold1
character(len=20)           :: hold2
character(len=20)           :: hold3
character(len=20)           :: devname
integer                     :: i10
!-----------------------------------------------------------------------------------------------------------------------------------
! GET DATA FROM COMMONS AND DICTIONARIES
      ORIENTLq2=rnum0(fetch('ylabel_O'))
      ORIENTRq2=rnum0(fetch('ylabel2_O'))

      ilabel_wq2=max(1,inum0(fetch('label_w')))           ! calculate width of lines to draw software text
      imin_wq2=max(1,inum0(fetch('min_w')))               ! calculate width of lines to draw software text
      ifixedlabq2=LLQ(ixlfxedq)                                ! text is fixed space or not for xlabel labels
      ifixednumq2=LLQ(ixmfxedq)                                ! text is fixed space or not for xmin labels


      fontNq2=fetch('min_fn')                             ! get fontname from MIN command
      fontLq2=fetch('label_fn')                           ! get fontname from XLABEL/YLABEL command
      ixlabel_chq2=max(1,inum0(fetch('xlabel_ch')))       ! user-desired height of characters to make axis labels
      ixmin_chq2=inum0(fetch('xmin_ch'))
!-----------------------------------------------------------------------------------------------------------------------------------
!     grid on|off -major on|off|xon|xoff|none -minor on|off|xon|xoff|none

      hold1=fetch('grid_oo')
      hold2=fetch('grid_minor')
      hold3=fetch('grid_major')
!
!  plot_axis%grid_style(0)  !  plot_axis%grid_style(1)  !
!  xaxis       |  yaxis       !
!  0=off       !  0=off       !
!  1=tic       !  1=tic       !
!  2=line      !  2=line      !
      !
      if(hold1.eq.'on')then   ! turn on both x and y major grid
          plot_axis%grid_style(1)=0 !major x grid
          plot_axis%grid_style(2)=0 !major y grid
      elseif(hold1.eq.'off')then
         hold2='off'  ! IF MAJOR GRID IS OFF ASSUME MINOR IS OFF TOO
         hold3='off'  ! IF MAJOR GRID IS OFF ASSUME MINOR IS OFF TOO
         plot_axis%grid_style(1)=1 !major x tic
         plot_axis%grid_style(2)=1 !major y tic
      else
         call journal('*xy_laxis* unknown grid_oo')
         call journal(hold1)
         hold2='off'  ! IF MAJOR GRID IS OFF ASSUME MINOR IS OFF TOO
         hold3='off'  ! IF MAJOR GRID IS OFF ASSUME MINOR IS OFF TOO
         plot_axis%grid_style(1)=2 !major x tic
         plot_axis%grid_style(2)=2 !major y tic
      endif
      if(    hold3.eq.'off')then
         plot_axis%grid_style(1)=1 !minor x tic
         plot_axis%grid_style(2)=1 !minor y tic
         hold2='off'
      elseif(hold3.eq.'on')then
         plot_axis%grid_style(1)=0 !minor x grid
         plot_axis%grid_style(2)=0 !minor y grid
      elseif(hold3.eq.'xon')then
         plot_axis%grid_style(1)=0 !minor x tic
         plot_axis%grid_style(2)=1 !minor y tic
      elseif(hold3.eq.'yon')then
         plot_axis%grid_style(1)=1 !minor x tic
         plot_axis%grid_style(2)=0 !minor y tic
      elseif(hold3.eq.'none')then
         plot_axis%grid_style(1)=2 !minor x null
         plot_axis%grid_style(2)=2 !minor y null
      else
         call journal('*xy_laxis* unknown grid_major')
         call journal(hold3)
         plot_axis%grid_style(3)=2 !minor x grid
         plot_axis%grid_style(4)=2 !minor y grid
      endif
      if(    hold2.eq.'off')then
         plot_axis%grid_style(3)=1 !minor x tic
         plot_axis%grid_style(4)=1 !minor y tic
      elseif(hold2.eq.'on')then
         plot_axis%grid_style(3)=0 !minor x grid
         plot_axis%grid_style(4)=0 !minor y grid
      elseif(hold2.eq.'xon')then
         plot_axis%grid_style(3)=0 !minor x tic
         plot_axis%grid_style(4)=1 !minor y tic
      elseif(hold2.eq.'yon')then
         plot_axis%grid_style(3)=1 !minor x tic
         plot_axis%grid_style(4)=0 !minor y tic
      elseif(hold2.eq.'none')then
         plot_axis%grid_style(3)=2 !minor x null
         plot_axis%grid_style(4)=2 !minor y null
      else
         call journal('*xy_laxis* unknown grid_minor')
         call journal(hold2)
         plot_axis%grid_style(3)=2 !minor x grid
         plot_axis%grid_style(4)=2 !minor y grid
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!      if(fetch('grid_oo').eq.'on')then
!          plot_axis%grid_style(1)=0                       ! major x  grid
!          plot_axis%grid_style(2)=0                       ! major y grid
!          if(fetch('grid_minor').eq.'off')then
!             plot_axis%grid_style(3)=1                    ! minor x tic
!             plot_axis%grid_style(4)=1                    ! minor y tic
!          elseif(fetch('grid_minor').eq.'none')then
!             plot_axis%grid_style(3)=2                    ! minor x null
!             plot_axis%grid_style(4)=2                    ! minor y null
!          else
!             plot_axis%grid_style(3)=0                    ! minor x grid
!             plot_axis%grid_style(4)=0                    ! minor y grid
!          endif
!      else
!         plot_axis%grid_style(1)=1                        ! major x tic
!         plot_axis%grid_style(2)=1                        ! major y tic
!         plot_axis%grid_style(3)=1                        ! minor x tic
!         plot_axis%grid_style(4)=1                        ! minor y tic
!      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      xlabel_fmtq2=fetch('xlabel_fmt')
      xlabel2_fmtq2=fetch('xlabel2_fmt')
      ylabel_fmtq2=fetch('ylabel_fmt')
      ylabel2_fmtq2=fetch('ylabel2_fmt')
!-----------------------------------------------------------------------------------------------------------------------------------
      call color(plot_ids(-1)%color)        ! set color
      if(ibackq.ne.imidlq)then
         plot_axis%background=imidlq        ! flag to fill grid with middle color
      else
         plot_axis%background=-1            ! flag to not fill box with color
      endif

      lq2(logleq2)=LLQ(logleq)              ! KLUDGE TILL CLEAN
      ipenq2=ipenq                          ! KLUDGE TILL CLEAN

      do i10=1,4                            ! transfer grid spacing calculated in call to xy_laxis to plotting common
          ixdivq2(i10)=ixdiv(i10)
          iydivq2(i10)=iydiv(i10)
      enddo

      aymulq2=aymulq
      axmulq2=axmulq
      ayconq2=ayconq
      axconq2=axconq
      ixuniq2=ixuniq
      iyuniq2=iyuniq
      icrvs2q2=icrvs2q
      icrvsq2=icrvsq
      call vgetdev(devname)                 ! get name of current device
      deviceq2=devname

      call plot_axes()

      ipenq=ipenq2              ! KLUDGE TILL CLEAN
!-----------------------------------------------------------------------------------------------------------------------------------
      if(XMINQ2.ge.XMAXQ2)then
         call journal('*xy_laxis* no room for curves -adjust aspect or area')
         XMINQ2=XMIN0Q2   ! an ungraceful way to prevent big problems when set viewport and window to draw user curves
         XMAXQ2=XMAX0Q2
      endif
      if(YMINQ2.ge.YMAXQ2)then
         call journal('*xy_laxis* no room for curves -adjust aspect or area')
         YMINQ2=YMIN0Q2   ! an ungraceful way to prevent big problems when set viewport and window to draw user curves
         YMAXQ2=YMAX0Q2
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine xy_laxis
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_pickpnt(3f) - [M_xyplot] interactively draw a line to current left scale and store the curve or return point
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_pickpnt(itype,xecho,yecho,ibut)
use M_journal, only : journal
use M_draw
use M_drawplus, only : rdpnt
implicit none

! ident_34="@(#)M_xyplot::xy_pickpnt(3f): interactively draw a line to current left scale and store the curve or return point"

!  if itype is 0, store a curve into the pseudo file
!  if itype is 1, return point in xecho, yecho
integer,intent(in)  :: itype
real                :: xecho
real                :: yecho
integer,intent(out) :: ibut
real                :: con(4)
integer,save        :: icalls=0
integer             :: isize
real                :: x1, y1
integer             :: i20, i30
real                :: x2, y2
real                :: xminv, xmaxv, yminv, ymaxv

!  set to scale of left axis and pick a point and return its coordinates
   call push()
   call frontbuffer()
   call xy_getscale(0,con(1),con(2),con(3),con(4))  ! get scaling factors
   call getgp2(xecho,yecho)
   isize=0
   ibut=-1
   call rdpnt(xecho,yecho,x1,y1,ibut)  ! first point
   if(ibut.lt.0.or.ibut.gt.2)goto 999     ! no locator device found if lt 0; asked to quit if greater than 3
!-----------------------------------------------------------------------------------------------------------------------------------
!  establish initial min and max
   xminv=(x1-con(2))/con(1)           ! scale x value
   yminv=(y1-con(4))/con(3)           ! scale y value
   xmaxv=xminv
   ymaxv=yminv
!-----------------------------------------------------------------------------------------------------------------------------------
   if(itype.eq.1)then    ! if just getting one point, you have it so return
         xecho=xminv
         yecho=yminv
         goto 999
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call move2(x1,y1)
   OUTER: do ! continue in a loop
      isize=isize+1
      call draw2(x1,y1)
      ! making the assumption con(1) and con(3) are not zero or would not have a plot
      x2=(x1-con(2))/con(1)           ! scale x value
      y2=(y1-con(4))/con(3)           ! scale y value
      xminv=min(x2,xminv)
      yminv=min(y2,yminv)
      xmaxv=max(x2,xmaxv)
      ymaxv=max(y2,ymaxv)
      xy_arrayq(isize)=x2                ! store x value
      xy_arrayq(isize+IMAXQ2/2)=y2        ! store y value
      write(*,*)xy_arrayq(isize),xy_arrayq(isize+IMAXQ2/2)
      xecho=x1
      yecho=y1
      INNER: do
         call rdpnt(xecho,yecho,x1,y1,ibut)
         if(ibut.eq.1)then
            cycle OUTER
         elseif(ibut.eq.2)then       ! back up one index to ignore last point
            call journal('s','*rdpnt* backing up')
            isize=max(1,isize-1)
            xecho=xy_arrayq(isize)*con(1)+con(2)  ! recalculate point to move back too
            yecho=xy_arrayq(isize+IMAXQ2/2)*con(3)+con(4)
            cycle INNER
         endif
      exit INNER
      enddo INNER
      exit OUTER
   enddo OUTER
!  any other button exits the draw loop
!-----------------------------------------------------------------------------------------------------------------------------------
!  create a nearly null time curve of specified size that can be changed with header and math
   if(isize.le.0)then
      call journal('*xy_pickpnt* size must be .ge. 1)')
      goto 999
   endif
!  build a dummy header record
   icodeq=icalls
   varidq='XDRAWN'
   nodq=0
   isnodq=0
   itnodq=0
   iunitq=0
   alphaq(:KAQ)=' '
   alphaq(KAQ+1:)=' '
   alpha2q=' '
   itimeq=0
   ipadq(1)=isize  ! store number of points in the curve
   do i20=2,5
      ipadq(i20)=0
   enddo
!  NEED TO CONVERT THESE POINTS TO PROPER SCALE
   call xy_juput(0,0,xminv,xmaxv,0,'sc')
! inefficient shift might be avoided quite easily with a small change to xy_juput
   do i30=1,isize   ! shift the y values to where they are stored from
      xy_arrayq(i30)=xy_arrayq(i30+IMAXQ2/2)
   enddo
   itimeq=ifrecsq(0) ! last curve stored on pseudo file was for this curve
   varidq='YDRAWN'
   call xy_juput(0,0,yminv,ymaxv,0,'sc')
999   continue
   icalls=icalls+1
   call pop()
end subroutine xy_pickpnt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_setlbl(3f) - [M_xyplot] decide axis label strings
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_setlbl(ii)
!     1992 John S. Urban
implicit none

! ident_35="@(#)M_xyplot::xy_setlbl(3f): decide axis label strings"

!     If axis label string is empty, use units to get default string
!----- PUT IN CHECKS FOR LABELS THAT ARE TOO LONG
character(len=255)          :: temp1
character(len=255)          :: temp2
character(len=10),save      :: lnames(4)
integer                     :: ii(4)
integer                     :: i10
integer                     :: ier
integer                     :: iii
integer                     :: ilag
integer                     :: ilen
integer                     :: len1
data lnames/'xlabel_oo','ylabel_oo','xlabel2_oo','ylabel2_oo'/
!-----------------------------------------------------------------------------------------------------------------------------------
      ilag=inum0(fetch('plot_lag'))
      do i10=1,4
         temp1=' '
         temp2=' '
         call xy_retrv2(lnames(i10),temp1,len1,ier) ! use user-specified label
         if(len1.eq.0)then
            if(ilag.eq.0)then
               iii=ii(i10)
            else
               if(i10.eq.1)then
                  iii=ii(2)
               elseif(i10.eq.3)then
                  iii=ii(4)
               else
                  iii=ii(i10)
               endif
            endif
            if(iii.ge.1000)then       ! new style where alpha2 is axis label
               axlq2(i10)=axislq(i10)
               ilen=len_trim(axlq2(i10))+1
               call xy_units(iii,temp2)  ! use label based on units
               temp1=axlq2(i10)(1:ilen)  ! start with label in alpha2q
               temp1(ilen+1:)=temp2       ! add unit (which may be converted)
            else                          ! old style unit labels
               call xy_units(iii,temp1)  ! use label based on units
            endif
         endif
         axlq2(i10)=temp1                 ! save axis label somewhere
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ilag.ne.0)then
         axlq2(1)=axlq2(2)
         axlq2(3)=axlq2(4)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     if -off switch is present blank out labels after all
      if(fetch('xlabel_off').ne.'#N#') axlq2(1)=' '
      if(fetch('ylabel_off').ne.'#N#') axlq2(2)=' '
      if(fetch('xlabel2_off').ne.'#N#') axlq2(3)=' '
      if(fetch('ylabel2_off').ne.'#N#') axlq2(4)=' '
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine xy_setlbl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_setsize(3f) - [M_xyplot] use values to set size and border on output device
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_setsize(string)
use M_draw
use M_verify
implicit none

! ident_36="@(#)M_xyplot::xy_setsize(3f): use values to set size and border on output device"

!     note that a value of '-1 -1 -1 -1' will reset to default driver size
!     assumes prefsize(3) and prefposition(3) will complain about bad values and take corrective action, as it is hard to say what
!     values are bad for particular output devices.
character(len=*),intent(in) :: string
real                        :: vals(4)
integer                     :: ier
integer                     :: ifound
   vals=[-1.0,-1.0,-1.0,-1.0]                        ! special values that indicate driver should use build-in defaults
   if(string.ne.' ')then
      vals(3)=0.0                                    ! initial x offset
      vals(4)=0.0                                    ! initial y offset
      ier=0                                          ! flag as to whether had trouble converting string to numbers or not
      call strgar2(string,4,vals,ifound,' ,',ier)    ! set preferred size and border of display output area
      if(ier.eq.0)then                               ! if no errors occurred
         if(ifound.eq.1)vals(2)=vals(1)              ! if found one value, make it square
         call prefsize(int(vals(1)),int(vals(2)))
         call prefposition(int(vals(3)),int(vals(4)))
      endif
   endif
end subroutine xy_setsize
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_tidybox(3f) - [M_xyplot] draw legend lines inside box specified on call
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_tidybox()
use M_journal,         only : journal
use M_draw
implicit none

! ident_37="@(#)M_xyplot::xy_tidybox(3f): draw legend lines inside box specified on call"

character(len=255) :: corners
character(len=1)   :: cwhere
real               :: plot_setmark_size
integer            :: itextc
integer            :: ichars
integer            :: iwchar
integer            :: icchar
real               :: xmint, xmaxt, ymint, ymaxt
integer            :: i10
real               :: xmark
integer            :: ibox
!-----------------------------------------------------------------------------------------------------------------------------------
!     set up legend attributes
      ichars=1+5+1+8+5+(5*3+20)                           ! maximum expected number of characters in the string
      ichars=max(1,inum0(fetch('id_ch')))                 ! get number of characters estimated needed
      iwchar=max(1,inum0(fetch('id_tw')))                 ! get character width
      itextc=inum0(fetch('id_tc'))                        ! get character color
      if(itextc.lt.0)then
         itextc=max(0,inum0(fetch('title_c')))            ! calculate color of title lines to get color for ID labels
         itextc=iforeq                                    ! calculate color of title lines
      endif
      call push()                                         ! Set the view up
      call polyfill(.true.)
      call xy_jufont(fetch('id_fn'))                      ! always call font before textsize
      ibox=inum0(fetch('idbox_box'))                      ! color of box
      call fixedwidth(LLQ(iidfxedq))                      ! text is fixed space or not
      call plot_get_plot_area(xmint,xmaxt,ymint,ymaxt)
!-----------------------------------------------------------------------------------------------------------------------------------
      plot_setmark_size=0.0                               ! max size of markers as percent of x width ASSUMING all fit in 1x1 box
      do i10=1,100
         plot_setmark_size=max(plot_setmark_size,plot_ids(i10)%marker_size)
      enddo
      xmark=2.0*(xmaxt-xmint)*plot_setmark_size/100.0     ! max size of markers in percent of X display range
      plot_setmark_size=xmark
!-----------------------------------------------------------------------------------------------------------------------------------
      corners=fetch('idbox_oo')
      if(corners.eq.'off') goto 999
!-----------------------------------------------------------------------------------------------------------------------------------
      cwhere(1:1)=fetch('idbox_p')
      if(index('pa',cwhere).ne.0)then
         call xy_tidybox2(itextc,ichars,iwchar,ibox,plot_setmark_size,cwhere,xmint,xmaxt,ymint,ymaxt,xmark)
      elseif(cwhere.eq.'e')then
         call xy_tidybox1(itextc,ichars,iwchar,ibox,plot_setmark_size,cwhere,xmint,xmaxt,ymint,ymaxt,xmark)
      elseif(cwhere.eq.'d')then ! default
      else
         call journal('*xy_tidybox* unknown positioning keyword')
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_rasters(0)
999   continue
      call pop()
end subroutine xy_tidybox
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_arrow(3f) - [M_xyplot] Add xy_arrow from T text box to specified point
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_arrow(xleft,ybot,xright,ytop,xto,yto,where,asz)
use M_drawplus, only : arrowhead
implicit none

! ident_38="@(#)M_xyplot::xy_arrow(3f): Add xy_arrow from T text box to specified point"

real             :: asz
real             :: size
real             :: where
real             :: where2
real             :: xcen
real             :: xdel
real             :: xdraw
real             :: xleft
real             :: xright
real             :: xto
real             :: ybot
real             :: ycen
real             :: ydel
real             :: ydraw
real             :: yto
real             :: ytop
   size=asz                   ! use size from above call
   where2=abs(where)
   if(where2.gt.4.5)then    ! flag to not draw
      return
   elseif(where2.lt.0.50)then ! figure out where to put it at
      xcen=(xright+xleft)/2.0
      ycen=(ytop+ybot)/2.0
      xdel=xto-xcen
      ydel=yto-ycen
      if(xdel.ge.0.and.ydel.ge.0)then       ! quadrant I
         where2=1.5
         if(xto.le.xright)where2=1.0
         if(yto.lt.ytop)  where2=2.0
      elseif(xdel.le.0.and.ydel.ge.0)then   ! quadrant II
         where2=4.5
         if(xto.gt.xleft) where2=1.0
         if(yto.lt.ytop)  where2=4.0
      elseif(xdel.le.0.and.ydel.le.0)then   ! quadrant III
         where2=3.5
         if(xto.gt.xleft) where2=3.0
         if(yto.ge.ybot)  where2=4.0
      else                                  ! quadrant IV
         where2=2.5
         if(xto.lt.xright)where2=3.0
         if(yto.gt.ybot)  where2=2.0
      endif
   endif
!     .5 to 1.5 is top, 1.5 to 2.5 is right, 2.5 to 3.5 is bottom, 3.5 to 4.5 is left
!     + is away from box, - is towards box
   if(where2.le.1.5.and.where2.ge.0.5)then
      ydraw=ytop
      xdraw=xy_slide(xleft,xright,where2,0.5)
   elseif(where2.le.2.5)then
      xdraw=xright
      ydraw=xy_slide(ytop,ybot,where2,1.5)
   elseif(where2.le.3.5)then
      ydraw=ybot
      xdraw=xy_slide(xright,xleft,where2,2.5)
   else
      xdraw=xleft
      ydraw=xy_slide(ybot,ytop,where2,3.5)
   endif
   if(where.ge.0)then
      call arrowhead(xto,yto,xdraw,ydraw,size,0)
   else
      call arrowhead(xdraw,ydraw,xto,yto,size,0)
   endif
end subroutine xy_arrow
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_idbox(3f) - [M_xyplot] Draw ID legend box specified with LEGEND command
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_idbox(r10a,icolsa,idonly)
use M_journal, only : journal
use M_draw
implicit none

! ident_39="@(#)M_xyplot::xy_idbox(3f): Draw ID legend box specified with LEGEND command"

integer,parameter :: lbllen=4096     ! maximum number of characters in a label
real              :: factor
real              :: height
integer           :: i10
integer           :: i20
integer           :: ialpha
integer           :: ichars
integer           :: icols
integer           :: icolsa
integer           :: idonly
integer           :: ilines
integer           :: ilines2
integer           :: iprintl
integer           :: iprintr
integer           :: itemp
integer           :: itimes
real              :: r10
real              :: r10a
real              :: ratio
real              :: rblock
real              :: rmax
real              :: rmaxstr
real              :: rsave
real              :: rtall
real              :: rx
real              :: rxmax
real              :: s2
real              :: space
real              :: width
real              :: x1
real              :: x2
real              :: xcol
real              :: xleft
real              :: xmaxt
real              :: xmid
real              :: xmint
real              :: xshift
real              :: y1
real              :: y2
real              :: ymaxt
real              :: ymint
!-----------------------------------------------------------------------------------------------------------------------------------
      r10=r10a  ! not polite to change input values that have no reason to be returned
!-----------------------------------------------------------------------------------------------------------------------------------
!     calculate a size for legend labels
      ichars=5+5+1+8+5+(5*3+20)                 ! offset to beginning of line + length of line + gap + max length of string
      ichars=inum0('ID_CHARS')
      call plot_get_plot_area(xmint,xmaxt,ymint,ymaxt)
      width=(xmaxt-xmint)/real(ichars)          ! calculate maximum character width assuming 1 column
      height=width*1.2                          ! calculate nice character height
      call xy_jufont(fetch('id_fn'))               ! always call font before textsize
!-----------------------------------------------------------------------------------------------------------------------------------
!     put some reasonable restrictions on the area that the id labels take up
!     should do an overall clean look at title, plot, and id areas
      ilines=0
      space=1.3                                 ! initial wide spacing between id lines
      rmaxstr=0.0
      call textsize(width,height)
!-----------------------------------------------------------------------------------------------------------------------------------
      if(index(fetch('idbox_s'),'alpha').ne.0)then   ! maybe build into temp1q from left to right as keywords appear?
         ialpha=8+1+5+1+5+1+5+1
      else
         ialpha=1
      endif
      do  i20=1,icrvsq                       ! find length at assumed font size of longest legend text string
         if(plot_ids(i20)%legend.eq.0)then
            ilines=ilines+1                        ! get a count of number of legend lines to print
            if(lgnduq(i20).ne.' ')then
               temp1q=lgnduq(i20)                  ! user legend supersedes
            elseif(ialpha.ne.1)then    ! (a8,1x,i5,1x,i5,1x,i5,1x,a) varidq,nodq,isnodq,itnodq,alphaq(:KAQ)
               temp1q=lgndq(i20)(ialpha:)
            else                       ! all of default legend label
               temp1q=lgndq(i20)
            endif
            rmaxstr=max(xy_ustrlen(temp1q),rmaxstr)
         endif
      enddo
      if(ilines.eq.0)return           ! if no lines then leave this routine
      icols=min(icolsa,icrvsq,ilines) ! use the least number of columns needed
!-----------------------------------------------------------------------------------------------------------------------------------
      ratio=(rmaxstr+2*width)*icols/xmaxt-xmint
      if(ratio.gt.1)then
         width=width/ratio
         height=height/ratio
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      ilines2=ceiling(real(ilines)/real(icols))*2  ! one for each text line, one for each line segment (not counting axis labels)
      if(ilines2.ne.0)then                         ! id lines are going to be used
         rblock=height*space*(ilines2+1)           ! height of id block if current text height is used
         rtall=r10-ymint                           ! height of remaining area after titles have been drawn
         if(idonly.ne.0)then
            rsave=(ymaxt-ymint)*0.40               ! try and leave at least 40 percent of height for plot
         else
            rsave=0.0
         endif
         rmax=rtall-rsave                          ! max area to use for id block
         if(rtall.gt.ymint.and.rmax.gt.ymint)then  ! if titles didn't take up entire plot area and 1/3 of plot left after titles
            if(rmax-ymint.lt.rblock)then           ! first, pack the id lines tighter and see if that helps
               space=1.0
               rblock=height*ilines2*space
            endif
            if((rmax-ymint).lt.rblock)then         ! make id text small enough to leave some plot area
              factor=rmax/space/ilines2
              rmaxstr=rmaxstr*height/factor
              height=factor
              width=height/1.2
            endif
         else
            call journal('*xy_idbox* yech! change aspect or use less title lines')
         endif
      else                                         ! no id lines are going to be used
         rblock=0.0                                ! height of id block if current text height is used
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_jufont(fetch('id_fn'))
      call textsize(width,height)
!-----------------------------------------------------------------------------------------------------------------------------------
      ! set ymaxt to unused portion of window
      call plot_get_plot_area(x1,x2,y1,y2)
      call plot_set_plot_area(x1,x2,y1,r10)

      rx=ymint+rblock-height*space/2.0 ! position at top of area
      rxmax=rx
      iprintr=0                         ! which axis side you are describing (right) and have you yet?
      iprintl=0                         ! which axis side you are describing (left) and have you yet?
      xmid=(xmint+xmaxt)/2.0            ! centerline to put legend box on
      xcol=rmaxstr+2*width              ! width of a column
      xshift=(xcol)*real(icols)/2.0     ! half of width of entire id box
      xleft=xmid-xshift+xcol/2.0        ! centerline of first column
      call rect(xleft-xcol/2.0,rx-rblock,xleft-xcol/2.0+2*xshift,rxmax)
      call centertext(.true.) ! turn on text centering
      itimes=0
      do  10 i10=1,icrvsq
      if(plot_ids(i10)%legend.eq.0)then ! only print legend if flag is on
         if(itimes.ge.ilines2/2)then
            itimes=1
            rx=rxmax
            xleft=xleft+xcol
         else
            itimes=itimes+1
         endif
         if(i10.le.icrvs2q.and.iprintl.eq.0.and.icrvsq.ne.icrvs2q)then  ! a zero curve number was used to label left axis curves
            iprintl=iprintl+1
            rx=rx-height*space
            call xy_rasters(2)
            call color(iforeq)
            if(axlq2(2) .ne. ' ')then                           !     draw y-axis label if one is present
               call xy_juprint(xleft,rx,axlq2(2),2)
            else
               call xy_juprint(xleft,rx,'LEFT AXIS:',2)
            endif
         elseif(i10.gt.icrvs2q.and.iprintr.eq.0)then  ! a zero curve number was used to break between left and right axis
            iprintr=iprintr+1
            rx=rx-height*space
            call xy_rasters(2)
            call color(iforeq)
            if(axlq2(4) .ne. ' ')then                           !     draw y-axis label if one is present
               call xy_juprint(xleft,rx,axlq2(4),2)
            else
               call xy_juprint(xleft,rx,'RIGHT AXIS:',2)
            endif
         endif
         rx=rx-2.0*height*space
         s2=xcol/2.0-width
         call color(plot_ids(i10)%color)        ! set color
         call xy_rasters(plot_ids(i10)%width)      ! set curve width
         call priv_drawseg_using_pen(i10,xleft-s2,rx,xleft+s2,rx) ! draw line using pen style ipen NOT to axis scale for legend box
!        call color(iforeq)
         call color(max(0,inum0(fetch('title_c'))))    ! calculate color of ti
         call xy_rasters(2)
         if(lgnduq(i10).eq.' ')then
         else
            lgndq(i10)=lgnduq(i10) !user legend supersedes
         endif
         call xy_juprint(xleft,rx+height*space,lgndq(i10)(ialpha:),2)
         call xy_rasters(0)
      endif
10    continue
      call centertext(.false.) ! turn off text centering
      ymint=rxmax+height/2.0! set ymint to unused portion of window
      call plot_get_plot_area(x1,x2,y1,y2)
      call plot_set_plot_area(x1,x2,ymint,y2)
end subroutine xy_idbox
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_idbox0(3f) - [M_xyplot] draw legend lines
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_idbox0(r10,idonly)
use M_journal, only : journal
use M_draw
implicit none

! ident_40="@(#)M_xyplot::xy_idbox0(3f): draw legend lines"

character(len=4096) :: line
integer,parameter   :: lbllen=4096 ! maximum number of characters in a label
real                :: plot_setmark_size
real                :: height
integer             :: i10
integer             :: i20
integer             :: ichars
integer             :: idonly
integer             :: ilines
integer             :: iprintl
integer             :: iprintr
integer             :: iwchar
integer             :: icchar
real                :: r10
real                :: rblock
real                :: rlength
real                :: rmax
real                :: rsave
real                :: rtall
real                :: spacing
real                :: width
real                :: x1
real                :: x2
real                :: xlong
real                :: xmaxt
real                :: xmint
real                :: xper
real                :: y1
real                :: y2
real                :: ydown
real                :: ymaxt
real                :: ymint
real                :: ytall
real                :: yup
!-----------------------------------------------------------------------------------------------------------------------------------
!     r10 is bottom of title lines
!-----------------------------------------------------------------------------------------------------------------------------------
!     calculate a size for legend labels
!     offset to beginning of line + length of line + gap + max length of string
   ichars=max(1,inum0(fetch('id_ch')))       ! get number of characters estimated
   iwchar=max(1,inum0(fetch('id_tw')))       ! get character line width
   icchar=inum0(fetch('id_tc'))              ! get character color
   if(icchar.lt.0)then
      icchar=max(0,inum0(fetch('title_c')))  ! calculate color as title lines
   endif
   call plot_get_plot_area(xmint,xmaxt,ymint,ymaxt)
   width=(xmaxt-xmint)/ichars                ! calculate maximum character width
   xper=(xmaxt-xmint)/100.0                  ! one x percent unit
   height=width*1.2                          ! calculate nice character height
   call fixedwidth(LLQ(iidfxedq))            ! text is fixed space or not
   call xy_jufont(fetch('id_fn'))            ! always call font before textsize
!-----------------------------------------------------------------------------------------------------------------------------------
   ! plot_setmark_size=(xmaxt-xmint)*plot_setmark_size/100.0/2.0
   ! max size of marker/2 in percent of X display range
!-----------------------------------------------------------------------------------------------------------------------------------
!     put some reasonable restrictions on the area that the id labels take up
!     should do an overall clean look at title, plot, and id areas
   ilines=0
   spacing=0.3*xper ! initial spacing between id lines
   ! max radius of markers as percent of xwidth ASSUMING all fit in 1x1 box
   plot_setmark_size=0.0
   do  i20=1,icrvsq
      if(plot_ids(i20)%legend.eq.0)then
         ! get a count of number of legend lines to print + a spacing
         ilines=ilines+1
         if(plot_ids(i20)%marker_frequency.gt.0)then
            ! if marker used, use size for spacing
            plot_setmark_size=max(plot_setmark_size,xper*plot_ids(i20)%marker_size/2.0)
         endif
      endif
   enddo
   if(ilines.eq.0)goto 30          ! no id lines are going to be used
!-----------------------------------------------------------------------------------------------------------------------------------
!     try to resize height is required
   rblock=(height*spacing)*ilines
   ! height of id block if current text height is used
   rtall=r10-ymint    ! height of remaining area after titles have been drawn
   if(idonly.ne.0)then
      rsave=(ymaxt-ymint)*0.40 ! leave at least 40 percent of height for plot
   else
      rsave=0.0
   endif
   rmax=rtall-rsave                ! max area to use for id block
   if(rtall.gt.ymint.and.rmax.gt.0)then
      ! if titles didn't take up entire plot area and 1/3 of area still left
      if(rmax.lt.rblock)then
         ! first, pack the id lines tighter and see if that helps
         spacing=spacing/2.0
         rblock=(height+spacing)*ilines
      endif
      if(rmax.lt.rblock)then
         ! make id text small enough to leave some plot area
         height=rmax/rmax*height
         width=height/1.2
      endif
   else
      call journal('*xy_idbox0* yech! change aspect or use less title lines')
   endif
30 continue
!-----------------------------------------------------------------------------------------------------------------------------------
   call xy_jufont(fetch('id_fn'))
   call textsize(width,height)
   xy_arrayq(IMAXQ2/2)=xmint+5*xper+plot_setmark_size ! set left margin
   rlength=max(10*xper,plot_setmark_size+3*xper)
   ! big marker should not cover line, point to point line length
   xy_arrayq(IMAXQ2/2+1)=xy_arrayq(IMAXQ2/2)+rlength
!-----------------------------------------------------------------------------------------------------------------------------------
   iprintr=0   ! right print
   iprintl=0   ! left print
   call xy_rasters(iwchar)
   !call color(iforeq)
   r10=r10-spacing   ! put a little space after the title
   do  i10=1,icrvsq
      call color(icchar)
      if(plot_ids(i10)%legend.eq.0)then ! only print legend if flag is on
         if(i10.le.icrvs2q.and.iprintl.eq.0.and.icrvsq.ne.icrvs2q)then
            ! zero curve number was used to label left axis curves
            iprintl=iprintl+1
            if(plot_ids(-1)%legend.eq.0)then
               ! use id 0 -off to turn off copying axis labels
               if(axlq2(2) .ne. ' ')then  ! draw y-axis label if one is present
                  if(lgnduq(-1).ne.' ')then
                     line=lgnduq(-1)
                  else
                     line=axlq2(2)
                  endif
                  call priv_justrlen(line,xlong,ytall,ydown,yup)
                  r10=min(r10-yup,r10-plot_setmark_size)
                  call xy_juprint(xy_arrayq(IMAXQ2/2),r10,line,2)
                  r10=r10-max(ydown,plot_setmark_size)
               else
                  r10=r10-height
                  call xy_juprint(xy_arrayq(IMAXQ2/2),r10,'LEFT AXIS:',2)
                  r10=r10-max(plot_setmark_size,spacing)
               endif
            endif
         elseif(i10.gt.icrvs2q.and.iprintr.eq.0)then
            ! a zero curve number was used to break between left and right axis
            iprintr=iprintr+1
            if(plot_ids(-2)%legend.eq.0)then ! use id 0 -off to turn off axis labels
               if(axlq2(4) .ne. ' ')then   ! draw y-axis label if one is present
                  if(lgnduq(-2).ne.' ')then
                     line=lgnduq(-2)
                  else
                     line=axlq2(4)
                  endif
                  call priv_justrlen(line,xlong,ytall,ydown,yup)
                  r10=min(r10-yup,r10-plot_setmark_size)
                  call xy_juprint(xy_arrayq(IMAXQ2/2),r10,line,2)
                  r10=r10-max(ydown,plot_setmark_size)
               else
                  r10=r10-height
                  call xy_juprint(xy_arrayq(IMAXQ2/2),r10,'RIGHT AXIS:',2)
                  r10=r10-max(plot_setmark_size,spacing)
               endif
            endif
         endif
         if(lgnduq(i10).ne.' ')lgndq(i10)=lgnduq(i10) !user legend supersedes
         call priv_justrlen(trim(lgndq(i10)),xlong,ytall,ydown,yup)
         r10=min(r10-yup,r10-plot_setmark_size)
         xy_arrayq(1)=r10+height/2.0
         xy_arrayq(2)=xy_arrayq(1)
         call xy_line(i10,2,'toframe',xy_arrayq(IMAXQ2/2),xy_arrayq)
         ! might set color and line width so make sure
         call xy_rasters(iwchar)
         call textsize(width,height) !BUG? NEEDS RESET AFTER xy_line
         call color(icchar) ! same color as title lines
         call xy_juprint(xy_arrayq(IMAXQ2/2+1)+max(width,plot_setmark_size),r10,lgndq(i10),2)
         r10=r10-max(ydown,plot_setmark_size)
      endif
   enddo
   call xy_rasters(0)
   ymaxt=r10 ! set YMAXQ to unused portion of window
   call plot_get_plot_area(x1,x2,y1,y2)
   call plot_set_plot_area(x1,x2,y1,ymaxt)
end subroutine xy_idbox0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_iflou(3f) - [M_xyplot] set global flag for Leading Edge trimming (see LE command)
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_iflou(iflag)
use M_journal, only : journal
implicit none

! ident_41="@(#)M_xyplot::xy_iflou(3f): set global flag for Leading Edge trimming (see LE command)"

integer,intent(in)  :: iflag
!     look up in the dictionary whether xy_jucurv and priv_toscale want leading values
!     .le. 0 removed from curves when trying to plot them on logarithmic scales.
!     the common values are looked at in the xy_getdat procedure. This is a special
!     request of Lou Englehardt's.
!     John S. Urban, 1993
      if(iflag.eq.2)then
         LLQ(logleq)=.false.
      elseif(iflag.eq.1)then
         if(fetch('le_oo').ne.'off')then
            LLQ(logleq)=.true.
         else
            LLQ(logleq)=.false.
         endif
      else
         LLQ(logleq)=.false.
         call journal('*xy_iflou* should not get here')
      endif
end subroutine xy_iflou
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_judraw(3f) - [M_xyplot] draw curves to plot scales using plot_ values
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_judraw(ifile0)
use M_journal, only : journal
implicit none

! ident_42="@(#)M_xyplot::xy_judraw(3f): draw curves to plot scales using plot_ values"
!-----------------------------------------------------------------------------------------------------------------------------------
real, dimension(:), allocatable :: holdx3,holdx4
real, dimension(:), allocatable :: holdy3,holdy4
real                            :: xs(2)
real                            :: ys(2)
!-----------------------------------------------------------------------------------------------------------------------------------
integer,intent(in)              :: ifile0                      ! default file to assume data is on for strgar3() call
!-----------------------------------------------------------------------------------------------------------------------------------
real                            :: rcurv1(ICSQ,4),rcurv(ICSQ,4)  ! curve numbers to plot on this frame
character(len=255)              :: error_type
character(len=7)                :: side                        ! designate if curve is drawn to left scale or to right scale
!-----------------------------------------------------------------------------------------------------------------------------------
integer                         :: inums                       ! number of values found to plot
integer                         :: ierr                        ! error flag returned by strgar3()
real                            :: x8(8)                       ! extremes
!-----------------------------------------------------------------------------------------------------------------------------------
integer              :: i10
integer              :: i30
integer              :: i40
integer              :: icurve
integer              :: icurve3
integer              :: icurve4
integer              :: idum
integer              :: ifile
integer              :: iii
integer              :: iput
integer              :: ipvs
integer              :: itime
integer              :: itimevs
integer              :: ivals
integer              :: ivalsx
integer              :: ivalsy
real                 :: error_width
real                 :: xhigh
real                 :: xlow
real                 :: yhigh
real                 :: ylow
!-----------------------------------------------------------------------------------------------------------------------------------
!     read up to ICSQ curve numbers
      if(fetch('plot_if').eq.'#N#'.and.fetch('plot_v').eq.' '.and.fetch('plot_c').eq.' ')then
         ! curves to plot are a simple numeric list of curves, no additional conditionals are used to select curves
         call strgar3(fetch('plot_oo'),ICSQ,real(ifile0),rcurv1,inums,' ',' ',ierr)
         call xy_listout3(rcurv1,ICSQ,rcurv,ICSQ,ICSQ,inums)               ! expand any ranges found in the list
      else                                                           ! selected using -if -v -c switches
         rcurv(:,:)=rcurvq(:,:)
         inums=num_rcurvq                                            ! number of curves that match the -if -v -c switch
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(inums.gt.1)then                                             ! remove trailing and duplicate zeros in curve list
         iput=1
         do i40=2,inums
            icurve=rcurv(i40,1)
            if(icurve.ne.0)then
               iput=iput+1
               rcurv(iput,1)=icurve                                  ! curve numbers
               rcurv(iput,2)=rcurv(i40,2)                            ! curve number files
               rcurv(iput,3)=rcurv(i40,3)                            ! curve number top error if any
               rcurv(iput,4)=rcurv(i40,4)                            ! curve number bottom error if any
            endif
         enddo
         inums=iput                                                  ! new number of curves with extraneous zeros removed
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      ipvs=inum0(fetch('plot_vs'))                          ! get alternate x curve to force all y values to be plotted against
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_iflou(1)                                           ! set flag on to ignore values .le. 0 in logarithmic plots
!-----------------------------------------------------------------------------------------------------------------------------------
      error_type=fetch('plot_e')                            ! type of error bar
      error_width=0.0
      iii=index(error_type,'%')                                  ! percent value for width of "T" on error bars
      if(iii.ne.0)then
         error_width=rnum0(error_type(:iii-1))                   ! get T bar width as a percent
         call plot_getdatarange(X8)
         error_width=(x8(2)-x8(1))*error_width/100.0             ! percent of total x range in user plot scale units
      else
         error_width=rnum0(error_type)                           ! get T bar width as a numeric value
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      do i10=1,inums                                             ! inums is number of curves read off plot command ignoring zeros
         icurve=rcurv(i10,1)                                     ! record number i10 curve stored on or pseudo number
         ifile=int(rcurv(i10,2)+0.5)

         call xy_getdat(ifile,icurve,ylow,yhigh,itime,ivalsy,i10)   ! get y values for a curve
         if(itime.le.0)exit                                      ! could not get curve requested

         call getxvals(ierr)
         if(ierr.ne.0)exit
         !-----------------------------------------------------------------------------------------------------------------------------------
         ! to allow multiple curves to share the same time curve data even if they don't use all the values
         ! (that is, if the y values are truncated) per request of Mark Malone
         if(ivalsy.gt.0)then                                     ! use y-count of points to override x-count of points
            ivals=min(ivalsy,ivalsx)
         else
            ivals=ivalsx
         endif
         !-----------------------------------------------------------------------------------------------------------------------------------
         if(i10.gt.icrvs2q)then
            side='toright'
         else
            side='toleft'
         endif
         call xy_line(i10,ivals,side,xy_arrayq(IMAXQ2/2),xy_arrayq)  ! draw a polyline to left or right scale
!-----------------------------------------------------------------------------------------------------------------------------------
         ! draw error bars if present
         icurve3=rcurv(i10,3)                     ! record curve number for top error values, assumed on same file as base curve
         icurve4=rcurv(i10,4)                     ! record curve number for bottom error values, assumed on same file as base curve
         call priv_solidline(.true.)
         if(icurve3.gt.0.and.icurve4.gt.0)then
            !-------------------------------------
            call xy_getdat(ifile,icurve3,ylow,yhigh,itime,ivalsy,i10)     ! get top error X values
            ALLOCATE(holdy3(ivals))
            holdy3=xy_arrayq(:ivals)
            call getxvals(ierr)
            if(ierr.ne.0)exit
            ALLOCATE(holdx3(ivals))
            holdx3=xy_arrayq(IMAXQ2/2:IMAXQ2/2+ivals-1)
            !-------------------------------------
            call xy_getdat(ifile,icurve4,ylow,yhigh,itime,ivalsy,i10)     ! get bottom error X values
            ALLOCATE(holdy4(ivals))
            holdy4=xy_arrayq(:ivals)
            call getxvals(ierr)
            ALLOCATE(holdx4(ivals))
            holdx4=xy_arrayq(IMAXQ2/2:IMAXQ2/2+ivals-1)
            !-------------------------------------
            do i30=1,ivals
               xs(1)=holdx3(i30)
               ys(1)=holdy3(i30)
               xs(2)=holdx4(i30)
               ys(2)=holdy4(i30)
               call xy_line(i10,2,side,xs,ys)        ! draw polyline to left or right scale
               if(error_width.ne.0)then
                  xs(1)=holdx3(i30)-error_width
                  xs(2)=holdx3(i30)+error_width
                  ys(1)=holdy3(i30)
                  ys(2)=holdy3(i30)
                  call xy_line(i10,2,side,xs,ys)     ! draw polyline T bar
                  !call xy_line(i10,2,side,(/xs(2)-error_width,xs(2)+error_width/),(/ys(2),ys(2)/))     ! draw polyline T bar
                  xs(1)=holdx4(i30)-error_width
                  xs(2)=holdx4(i30)+error_width
                  ys(1)=holdy4(i30)
                  ys(2)=holdy4(i30)
                  call xy_line(i10,2,side,xs,ys)     ! draw polyline T bar
               endif
            enddo
            DEALLOCATE(holdx3)
            DEALLOCATE(holdx4)
            DEALLOCATE(holdy3)
            DEALLOCATE(holdy4)
         endif
         call priv_solidline(.false.)
!-----------------------------------------------------------------------------------------------------------------------------------
      enddo
      call xy_iflou(2) ! set flag off to ignore values .le. 0 in logarithmic plots
!-----------------------------------------------------------------------------------------------------------------------------------
      contains
         subroutine getxvals(ierr)
         integer :: ierr
         ierr=0

         if(ipvs.le.0)then                                       ! using default x-axis data
            call xy_getdat(ifile,itime,xlow,xhigh,idum,ivalsx,-1)   ! get x-axis data, should check for not found
         else                                                    ! have to get data each time because xy_line destroys the data
            call xy_getdat(ifile,ipvs,xlow,xhigh,itimevs,ivalsx,-1) ! get x-axis data
            if(itimevs.lt.0)then
               call journal('*xy_judraw* user specified x-axis data not found')
               ierr=-1
            elseif(itime.ne.itimevs)then                         ! if forcing a curve to be the x-axis values, then all curves
                                                                 ! are to be checked to make sure they have the same time  values
               call journal('s','*xy_judraw* forcing use of alternate x values')
                                                                 ! let them go ahead, even though it is dangerous
            endif
         endif
         end subroutine getxvals
end subroutine xy_judraw
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_loadtl(3f) - [M_xyplot] load title xy_array
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_loadtl(ifile,ftitle,isize,ititles)
!     not printing title lines beginning with #,$ allows comment information to be stored in input files

!     ititles = out, number of title lines set to be used

!     1993 John S. Urban
use M_journal, only : journal
implicit none

! ident_43="@(#)M_xyplot::xy_loadtl(3f): load title xy_array"

character(len=20)         :: name
character(len=IZSZQ)      :: ztitle
character(len=4096)       :: ctemp
integer                   :: isize
character(len=4096)       :: ftitle(isize)     ! hold title lines from the file
integer,parameter         :: isizeX=20
character(len=4096)       :: ftitleX(isizeX)   ! scratch hold of title lines from the file
logical                   :: more
character(len=4096),save  :: savet
integer,save              :: icalls=0
integer     :: i10
integer     :: i15
integer     :: i20
integer     :: i30
integer     :: i35
integer     :: iadd
integer     :: ichars
integer     :: ifile
integer     :: ilen
integer     :: ilen1
integer     :: imax
integer     :: ios
integer     :: irec
integer     :: isub
integer     :: ititles
!-----------------------------------------------------------------------------------------------------------------------------------
!     if system environment variable PLTTITLE1 is set always use it for first title line
!     so can force a string like 'UNCONFIGURED VERSION' out via the plt(1) script
      if(icalls.eq.0)then
         savet=' '
         call get_environment_variable('PLTTITLE1',savet) ! do this once and store away, it seems to be slow on some systems
         icalls=icalls+1
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
         ititles=0    ! how many title lines to return
!-----------------------------------------------------------------------------------------------------------------------------------
!     blank out title line xy_array for clarity
      do i10=1,isize
         ftitle(i10)=' '
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      do i15=1,isizeX        ! for && variables
         ftitleX(i15)=' '
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      if(.not.LLQ(ltitlq))then      ! if title is off, skip except PLT1
        goto 25
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     make sure the file ifile is attached to read titles from
      imax=0
!-----------------------------------------------------------------------------------------------------------------------------------
!     grab up to isize title lines from file for title or to use as substitution strings for user-specified title
      ios=0        ! I/O error flag
      irec=0       ! record to read from zebra file from
      more=.true.  ! flag as to whether more title lines to look at. ASSUMING ALWAYS AT LEAST ONE TITLE LINE
50    continue
      irec=irec+1  ! move down record pointer into zebra file
      read(ifn_dq+imax,rec=irec,iostat=ios,err=777)ztitle
      ilen=len_trim(ztitle)
      if(ilen.le.0)then  ! blank title line
         call journal('*xy_loadt* blank title lines not permitted')
         more=.false.
      elseif(ztitle(ilen:ilen).eq.char(92))then ! title  line ending in \ continues title block
         ztitle(ilen:ilen)=' '
         more=.true.
      else ! title line not ending in \ ends title block
         more=.false.
      endif
      if(ztitle(1:1).ne.'#'.and.ztitle(1:1).ne.'$')then  ! allows comment information in input files
         ! store title lines where calculator can get to them
         if(ititles+1.le.isize)then
            ititles=ititles+1                     ! increment number of title lines in file found
            ftitle(ititles)=ztitle                ! store printable file title line
         else
            call journal('*xy_loadtl* too many title lines')
         endif
      endif
      if(more) goto 50
!     file title lines are now stored (note that comment title lines are NOT stored)
!-----------------------------------------------------------------------------------------------------------------------------------
!     if user-specified title is present use it instead of file lines.
!     must have read file though so that $TITLE_xxx variables are set (high overhead)
!     expand &&N variables

      do i35=1,ititles
            ftitleX(i35)=ftitle(i35)
      enddo

      do i20=1,isize
         write(name,'(''title_'',i2.2)')i20
         ctemp=fetch(name)                           ! obtain title string
         if(ctemp.ne.' ')then                        ! if specified use it instead of file
            ititles=i20
            if(ctemp(1:2).eq.'&&')then               ! support &&N syntax
               ichars=len_trim(ctemp)                ! number of characters in string
               isub=inum0(ctemp(3:max(ichars,3)))
               if(isub.eq.0)then
                  call get_environment_variable('PLTTITLE',ftitleX(ititles))
               elseif(isub.lt.0.or.isub.gt.isize)then  ! set out-of-range numbers to a blank string
                  ftitleX(ititles)=' '
               else
                  ftitleX(ititles)=ftitle(isub)   ! replace with Nth file title line
               endif
            else
               ftitleX(ititles)=ctemp             ! store regular user file title line
            endif
         else                                     ! once a blank line is encountered stop
            exit                                  ! if hit a blank quit storing
         endif
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
25    continue
      if(savet.ne.' ')then   ! make the first title line PLTTITLE1 if present
         ftitle(1)=savet
         ititles=ititles+1
         iadd=1
      else
         iadd=0
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      do i30=1,min(isize-iadd,ititles)              ! watch the subscripts. Assuming > isize+1
            ftitle(i30+iadd)=ftitleX(i30)
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      return
!-----------------------------------------------------------------------------------------------------------------------------------
777   continue
      call journal('sc','*xy_loadtl* I/O error reading titles =',ios)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine xy_loadtl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_tidybox1(3f) - [M_xyplot] draw legend lines in box with legend box autosized and positioned by edge number
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_tidybox1(itextc,ichars,iwchar,ibox,plot_setmark_size,cwhere,xmint0,xmaxt0,ymint0,ymaxt0,xmark)
use M_journal, only : journal
use M_draw
use M_drawplus, only : plain_rect
implicit none

! ident_44="@(#)M_xyplot::xy_tidybox1(3f):draw legend lines in box with legend box autosized and positioned by edge number"

real               :: gap
real               :: gapx
real               :: gapy
real               :: height
integer            :: i10
integer            :: i20
integer            :: ibox
integer            :: ichars
integer            :: idid1
integer            :: idid2
integer            :: ierr
integer            :: igot
integer            :: itextc
integer            :: iwchar
real               :: plot_setmark_size
real               :: r10
real               :: r10start
real               :: right2
real               :: rline
real               :: space
real               :: width
real               :: xdelta
real               :: xdelta0
real               :: xfar
real               :: xleft
real               :: xlong
real               :: xmark
real               :: xmaxt
real               :: xmaxt0
real               :: xmint
real               :: xmint0
real               :: xper
real               :: xright
real               :: xshift
real               :: xstart
real               :: xwidest
real               :: xxx
real               :: ybot
real               :: ydelta
real               :: ydown
real               :: yfar
real               :: ymaxt
real               :: ymaxt0
real               :: ymint
real               :: ymint0
real               :: yper
real               :: ytall
real               :: ytop
real               :: yup
real               :: yyy
character*1 cwhere
character*255 corners
real xx(2)
real yy(2)
real values(4)
save values
data values/0.0,0.0,0.0,0.0/ ! initialize xy_array of numeric values
!-----------------------------------------------------------------------------------------------------------------------------------
   xmint=xmint0
   xmaxt=xmaxt0
   ymint=ymint0
   ymaxt=ymaxt0
!-----------------------------------------------------------------------------------------------------------------------------------
   corners=fetch('idbox_oo')
   if(corners.ne.' ')then
      ierr=0
      call strgar2(corners,4,values,igot,' ',ierr)
      if(ierr.ne.0.or.(igot.lt.1.or.igot.gt.2))then
         call journal('*xy_tidybox1* bad numbers for box area')
         goto 999
      endif
   else      ! reuse last set of values
      igot=0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
! reduce area by a border (or INCREASE if NEGATIVE !!!! )
   if(igot.ge.2)then                       ! how far to push back from edge
      !values(2)=min(50.0,values(2))        ! lets not get silly
      gapx=(xmaxt-xmint)/100.0*values(2)
      if(igot.gt.2)then
         gapy=(ymaxt-ymint)/100.0*values(3)
      else
         gapy=(ymaxt-ymint)/100.0*values(2)
      endif
      xmint=xmint+gapx
      xmaxt=xmaxt-gapx
      ymint=ymint+gapy
      ymaxt=ymaxt-gapy
   elseif(igot.eq.1)then               ! default border is two character widths
      gap=(xmaxt-xmint)/ichars*2.0
      xmint=xmint+gap
      xmaxt=xmaxt-gap
      ymint=ymint+gap
      ymaxt=ymaxt-gap
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ! find maximum box area of same size with upper right corner positioned
   xdelta0=xmaxt0-xmint0
   xdelta=xmaxt-xmint
   ydelta=ymaxt-ymint
   if(values(1).ge.0.5.and.values(1).le.1.5)then       ! top edge
      xper=(values(1)-0.5)
      xleft =xmint+xper*xdelta
      xright=xleft+xdelta
      ytop=ymaxt
      ybot=ymint
   elseif(values(1).ge.1.5.and.values(1).le.2.5)then       ! left edge
      yper=values(1)-1.5
      ytop=ymaxt-yper*ydelta
      ybot=ytop-ydelta
      xleft=xmaxt
      xright=xleft+xdelta
   elseif(values(1).ge.2.5.and.values(1).le.3.5)then       ! bottom edge
      xper=3.5-values(1)
      xleft =xmint+xper*xdelta
      xright=xleft+xdelta
      ytop=ymint
      ybot=ymint-ydelta
   elseif(values(1).ge.3.5.and.values(1).le.4.5)then       ! right edge
      yper=(values(1)-3.5)
      ytop=ymint+yper*ydelta
      ybot=ytop-ydelta
      xleft=xmint
      xright=xleft+xdelta
   else
      call journal('*xy_tidybox1* unknown edge location')
      xper=0.0
      xleft =xmint+xper*xdelta
      xright=xleft+xdelta
      ytop=ymaxt
      ybot=ymint
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     calculate a first guess size for legend labels and length of sample line
!     based on ORIGINAL plot area before GAP
!     offset to beginning of line + length of line + gap + max length of string
   width=(xdelta0)/ichars   ! calculate maximum character width
   height=width*1.2              ! calculate nice character height
   space=height*.05
   space=0.0
   xwidest=0.0
   xx(1)=xleft+1.5*width+xmark/2.0  ! a little margin from left edge before drawing line segment
   rline=4.0*width
   rline=max(rline,2*width+xmark) ! draw line 4 characters long or 2 characters+marker size
   xx(2)=xx(1)+rline  ! sample line is 5 characters
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,2      ! once to set box size to fit, once to draw
      call textsize(width,height)  ! odd bug with font sizes
      call getfontsize(xxx,yyy)    ! odd bug with font sizes
      call xy_rasters(iwchar)
      call color(itextc)
      r10start=ytop
      r10=ytop-width*0.80   ! a little margin at the top
      idid1=0
      idid2=0
      do  i10=1,icrvsq
         if(plot_ids(i10)%legend.eq.0)then ! only print legend if flag is on
            !--------------------------------------------------------------
            ! PUT AXIS LABELS OUT WHEN DOING MULTI-AXIS PLOTS
            if(i10.le.icrvs2q.and.icrvsq.ne.icrvs2q.and.idid1.eq.0)then    ! zero curve number was used to label left axis curves
               idid1=1                                                     ! only do this once
               if(axlq2(2) .ne. ' ')then                                   !     draw y-axis label if one is present
                  call priv_justrlen(axlq2(2),xlong,ytall,ydown,yup)
                  !YDOWN=max(plot_setmark_size/2.0,ydown)
                  YUP=max(plot_setmark_size/2.0,yup)
                  xwidest=max(xwidest,xlong)
                  r10=r10-YUP
                  if(i20.eq.2)call xy_juprint(xx(1),r10,axlq2(2),2)
               else
                  call priv_justrlen('LEFT AXIS:',xlong,ytall,ydown,yup)
                  !YDOWN=max(plot_setmark_size/2.0,ydown)
                  YUP=max(plot_setmark_size/2.0,yup)
                  xwidest=max(xwidest,xlong)
                  r10=r10-YUP
                  if(i20.eq.2)call xy_juprint(xx(1),r10,'LEFT AXIS:',2)
               endif
               r10=r10-YDOWN
               r10=r10-space
            elseif(i10.gt.icrvs2q.and.idid2.eq.0)then  ! a zero curve number was used to break between left and right axis
               idid2=1
               if(axlq2(4) .ne. ' ')then                           !     draw y-axis label if one is present
                  call priv_justrlen(axlq2(4),xlong,ytall,ydown,yup)
                  !YDOWN=max(plot_setmark_size/2.0,ydown)
                  YUP=max(plot_setmark_size/2.0,yup)
                  xwidest=max(xwidest,xlong)
                  r10=r10-YUP
                  if(i20.eq.2)call xy_juprint(xx(1),r10,axlq2(4),2)
               else
                  call priv_justrlen('RIGHT AXIS:',xlong,ytall,ydown,yup)
                  !YDOWN=max(plot_setmark_size/2.0,ydown)
                  YUP=max(plot_setmark_size/2.0,yup)
                  xwidest=max(xwidest,xlong)
                  r10=r10-YUP
                  if(i20.eq.2)call xy_juprint(xx(1),r10,'RIGHT AXIS:',2)
               endif
               r10=r10-YDOWN
               r10=r10-space
            endif
            !--------------------------------------------------------------
            ! put out individual lines
            if(lgnduq(i10).ne.' ')lgndq(i10)=lgnduq(i10) !user legend supersedes
            call priv_justrlen(lgndq(i10),xlong,ytall,ydown,yup)
            !YDOWN=max(plot_setmark_size/2.0,ydown)
            YUP=max(plot_setmark_size/2.0,yup)
            xwidest=max(xwidest,xlong)
            r10=r10-YUP
            if(i20.eq.2)then
               yy(1)=r10-YDOWN+ytall/2.0
               yy(2)=yy(1)
               call xy_line(i10,2,'toframe',xx,yy) ! might set color and line width
               call textsize(width,height) !BUG? NEEDS RESET AFTER xy_line
               call xy_rasters(iwchar)
               call color(itextc)
               xstart=xx(2)+xmark/2.0+width
               call xy_juprint(xstart,r10,lgndq(i10),2)
            endif
            r10=r10-YDOWN
            !--------------------------------------------------------------
         endif
      enddo
      r10=r10-1.2*width   ! gap below last legend line
      !--------------------------------------------------------------
      ! when using one number use character size of id command and
      ! place along edge with correct percentage gap if specified
      ! move boxes into plotting area that went out bottom or right
      ! DOES NOT CHECK THAT OTHER EDGES WERE FORCED OUT OF PLOT AREA

      xshift=0.0
      xfar=xx(2)+width+xwidest+width+xmark/2.0
      if(xfar.gt.xmaxt)then  ! farthest right drawn out of box
         xshift=xmaxt-xfar
         xx(1)=xx(1)+xshift
         xx(2)=xx(2)+xshift
      endif
      yfar=ymint-r10
      if(r10.lt.ymint)then   ! bottom drawn out of box
         ytop=ytop+yfar
      else if(r10start.gt.ymaxt)then   ! bottom drawn out of box
         ytop=ymaxt
      endif
      !--------------------------------------------------------------
      ! draw box around edge-positioned label box (NOT FACTORED)
      if(i20.eq.1)then
         ! find box sides
         ybot=ytop-(r10start-r10)
         right2=xshift+xfar
         if(ibox.ge.0)then                                   ! filled box
            call color(ibox)
            call rect(xleft+xshift,ybot,right2,ytop)
            call color(plot_ids(0)%color)
            call xy_rasters(plot_ids(0)%width)
            call plain_rect(xleft+xshift,ybot,right2,ytop)
         else if(ibox.le.-999) then                          ! no box
         else                                                ! unfilled box
            call color(abs(ibox))
            call plain_rect(xleft+xshift,ybot,right2,ytop)
         endif
      endif
      !---------------------------------------------------------------
   enddo
999 continue
end subroutine xy_tidybox1
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_tidybox2(3f) - [M_xyplot] draw legend lines inside box specified on call
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_tidybox2(itextc,ichars,iwchar,ibox,plot_setmark_size,cwhere,xmint,xmaxt,ymint,ymaxt,xmark)
use M_journal, only : journal
use M_draw
use M_drawplus, only : plain_rect
implicit none

! ident_45="@(#)M_xyplot::xy_tidybox2(3f): draw legend lines inside box specified on call"

real                 :: factor
real                 :: gap1
real                 :: height
integer              :: i10
integer              :: i20
integer              :: ibox
integer              :: ichars
integer              :: idid1
integer              :: idid2
integer              :: ierr
integer              :: igot
integer              :: itextc
integer              :: iwchar
real                 :: plot_setmark_size
real                 :: r10
real                 :: r10start
real                 :: rline
real                 :: space
real                 :: width
real                 :: xfactor
real                 :: xleft
real                 :: xlong
real                 :: xmark
real                 :: xmaxt
real                 :: xmint
real                 :: xright
real                 :: xshift
real                 :: xstart
real                 :: xtext
real                 :: xwidest
real                 :: ybottom
real                 :: ybox
real                 :: ydown
real                 :: yfactor
real                 :: ymaxt
real                 :: ymint
real                 :: yshift
real                 :: ytall
real                 :: ytop
real                 :: yup
character(len=1)     :: cwhere
character(len=255)   :: corners
real                 :: xx(2)
real                 :: yy(2)
real,save            :: values(4)=[0.0, 0.0, 0.0, 0.0] ! initialize xy_array of numeric values
!-----------------------------------------------------------------------------------------------------------------------------------
!  set up legend attributes
   gap1=0
   corners=fetch('idbox_oo')
!-----------------------------------------------------------------------------------------------------------------------------------
   if(corners.ne.' ')then
      ierr=0
      call strgar2(corners,4,values,igot,' ',ierr)
      if(ierr.ne.0.or.igot.ne.4)then
         call journal('*xy_tidybox2* bad numbers for box area')
         goto 999
      endif
   else      ! reuse last set of values
      igot=0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ! convert from Axis values to Window values
   if(cwhere.eq.'a')then     ! relative to axis
      xleft=xy_con_x(values(1))
      xright=xy_con_x(values(2))
      ybottom=xy_con_y(values(3))
      ytop=xy_con_y(values(4))
   elseif(cwhere.eq.'p')then  ! relative to percentage of axis
      xleft =xmint+values(1)/100.0*(xmaxt-xmint)
      xright=xmint+values(2)/100.0*(xmaxt-xmint)
      ybottom=ymint+values(3)/100.0*(ymaxt-ymint)
      ytop=   ymint+values(4)/100.0*(ymaxt-ymint)
   else
      call journal('*xy_tidybox2* unknown position request')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ibox.ge.0)then                                   ! filled box
      call color(ibox)
      call rect(xleft,ybottom,xright,ytop)             ! draw fill
      call xy_rasters(plot_ids(0)%width)
      call color(plot_ids(0)%color)
      call plain_rect(xleft,ybottom,xright,ytop)            ! draw border
   else if(ibox.le.-999) then                          ! no box
   else                                                ! unfilled box
      call color(abs(ibox))
      call plain_rect(xleft,ybottom,xright,ytop)
   endif
   ybox=ytop-ybottom
!-----------------------------------------------------------------------------------------------------------------------------------
!  calculate a first guess size for legend labels and length of sample line
!  offset to beginning of line + length of line + gap + max length of string
   width=(xright-xleft)/ichars   ! calculate maximum character width
   height=width*1.2              ! calculate nice character height
   space=height*.05
   space=0.0
   xwidest=0.0
   xx(1)=xleft+1.5*width+xmark/2.0  ! a little margin from left edge before drawing line segment
   rline=4.0*width
   rline=max(rline,2*width+xmark) ! draw line 4 characters long or 2 characters+marker size
   xx(2)=xx(1)+rline  ! sample line is 5 characters
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,2      ! once to set size to fit, once to draw
      call textsize(width,height)
      call xy_rasters(iwchar)
      call color(iforeq)
      r10start=ytop
      r10=ytop-width*0.80   ! a little margin at the top
      idid1=0
      idid2=0
      do  i10=1,icrvsq
         if(plot_ids(i10)%legend.eq.0)then ! only print legend if flag is on
            !--------------------------------------------------------------
            ! PUT AXIS LABELS OUT WHEN DOING MULTI-AXIS PLOTS
            if(i10.le.icrvs2q.and.icrvsq.ne.icrvs2q.and.idid1.eq.0)then    ! zero curve number was used to label left axis curves
               idid1=1                                                     ! only do this once
               if(axlq2(2) .ne. ' ')then                                   !     draw y-axis label if one is present
                  call priv_justrlen(axlq2(2),xlong,ytall,ydown,yup)
                  xwidest=max(xwidest,xlong)
                  YUP=max(plot_setmark_size/2.0,yup) ! check for big markers
                  !YDOWN=max(plot_setmark_size/2.0,ydown) ! check for big markers
                  r10=r10-YUP
                  if(i20.eq.2)call xy_juprint(xx(1),r10,axlq2(2),2)
               else
                  call priv_justrlen('LEFT AXIS:',xlong,ytall,ydown,yup)
                  YUP=max(plot_setmark_size/2.0,yup) ! check for big markers
                  !YDOWN=max(plot_setmark_size/2.0,ydown) ! check for big markers
                  xwidest=max(xwidest,xlong)
                  r10=r10-YUP
                  if(i20.eq.2) call xy_juprint(xx(1),r10,'LEFT AXIS:',2)
               endif
               r10=r10-YDOWN
               r10=r10-space
            elseif(i10.gt.icrvs2q.and.idid2.eq.0)then  ! a zero curve number was used to break between left and right axis
               idid2=1
               if(axlq2(4) .ne. ' ')then                           !     draw y-axis label if one is present
                  call priv_justrlen(axlq2(4),xlong,ytall,ydown,yup)
                  YUP=max(plot_setmark_size/2.0,yup) ! check for big markers
                  !YDOWN=max(plot_setmark_size/2.0,ydown) ! check for big markers
                  xwidest=max(xwidest,xlong)
                  r10=r10-YUP
                  if(i20.eq.2)call xy_juprint(xx(1),r10,axlq2(4),2)
               else
                  call priv_justrlen('RIGHT AXIS:',xlong,ytall,ydown,yup)
                  YUP=max(plot_setmark_size/2.0,yup) ! check for big markers
                  !YDOWN=max(plot_setmark_size/2.0,ydown) ! check for big markers
                  xwidest=max(xwidest,xlong)
                  r10=r10-YUP
                  if(i20.eq.2) call xy_juprint(xx(1),r10,'RIGHT AXIS:',2)
               endif
               r10=r10-YDOWN
               r10=r10-space
            endif
            !--------------------------------------------------------------
            ! put out individual lines
            if(lgnduq(i10).ne.' ')lgndq(i10)=lgnduq(i10) !user legend supersedes
            call priv_justrlen(lgndq(i10),xlong,ytall,ydown,yup)
            YUP=max(plot_setmark_size/2.0,yup) ! check for big markers
            !YDOWN=max(plot_setmark_size/2.0,ydown) ! check for big markers
            xwidest=max(xwidest,xlong)
            r10=r10-YUP
            if(i20.eq.2)then
               yy(1)=r10-YDOWN+ytall/2.0
               yy(2)=yy(1)
               call xy_line(i10,2,'toframe',xx,yy) ! might set color and line width
               call textsize(width,height) !BUG? NEEDS RESET AFTER xy_line
               call xy_rasters(iwchar)
               call color(itextc)    ! color of title lines
               xstart=xx(2)+xmark/2.0+width
               call xy_juprint(xstart,r10,lgndq(i10),2)
            endif
            r10=r10-YDOWN
            !--------------------------------------------------------------
         endif
      enddo
      r10=r10-max(1.2*width,xmark/1.5)  ! gap below last legend line

      if(ybox.ne.0)then
         yfactor=(r10start-r10)/ybox
      else
         yfactor=1.0
      endif
      xtext=xright-xx(2)-2*width-xmark/2.0  ! space left for text (IF ANY)
      if(xtext.ne.0)then
         xfactor=(xwidest)/xtext
      else
         xfactor=1.0
      endif
      factor=max(xfactor,yfactor)
      !------------------------------------------------------------
      ! THIS IS WRONG FOR CENTERING SOMEHOW
      if(xfactor.ne.factor)then ! center in x
         xshift=xright-xx(2)-xmark/2.0-(width+xwidest)/factor
         xshift=xshift/2.0
         xx(1)=xx(1)+xshift
         xx(2)=xx(2)+xshift
      endif
      if(yfactor.ne.factor)then ! center in y
         yshift=(r10-ybottom)/factor
         yshift=yshift/2
         !ytop=ytop-yshift
      endif
      !--------------------------------------------------------------
   enddo
   call xy_rasters(0)
999 continue
   call pop()
end subroutine xy_tidybox2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_listout3(3f) - [M_xyplot] copy rcurve_in to rcurve_out expanding negative curve numbers to ranges (1 -10 means 1 thru 10)
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_listout3(rcurve_in,iszin,rcurve_out,iszout,isize,inums)
!===================================================================================================================================
use M_journal, only : journal
implicit none

! ident_46="@(#)M_xyplot::xy_listout3(3f): copy rcurve_in to rcurve_out expanding negative curve numbers to ranges (1 -10 means 1 thru 10)"

!     use rcurve(:,1) for list of numbers to expand
!     use rcurve(:,2) expand values in this column only
!     so
!     1 -10  20 30 40   becomes 1,2,3,4,5,6,7,8,9,10,20,30,40 in column 1
!
integer               :: iszin
real,intent(in)       :: rcurve_in(iszin,4)   ! rcurve_in is input xy_array
                                              ! row 1 is curve numbers
                                              ! row 2 is file numbers
                                              ! row 3 is top error curve numbers
                                              ! row 4 is bottom error curve numbers

integer               :: iszout
real,intent(out)      :: rcurve_out(iszout,4) ! rcurve_out is output xy_array
integer,intent(in)    :: isize                ! isize is maximum numbers to put into rcurve_out
integer,intent(inout) :: inums          ! inums is number of rcurve_in values on input, number of rcurve_out numbers on output
integer               :: icurve
integer               :: idec
integer               :: ifile
integer               :: iincounter
integer               :: imax
integer               :: imin
integer               :: ioutcounter
!===================================================================================================================================
      rcurve_out(1,1)=rcurve_in(1,1)
      rcurve_out(1,2)=rcurve_in(1,2)
      rcurve_out(1,3)=rcurve_in(1,3)
      rcurve_out(1,4)=rcurve_in(1,4)
      ioutcounter=2
      if(inums.ge.2)then
         do iincounter=2,inums
            if(ioutcounter.gt.isize) then
               call journal('sc','*xy_listout3* only',isize,'values allowed')
               inums=ioutcounter-1
               return
            endif
            if(rcurve_in(iincounter,1).lt.0)then     ! if a negative number this is a range to expand
               imax=abs(rcurve_in(iincounter,1))     ! the number is the end of the range without the negative sign
               imin=abs(rcurve_in(iincounter-1,1))   ! previous number is assumed to be beginning of range
               ifile=rcurve_in(iincounter,2)
               if(rcurve_in(iincounter-1,2).ne.rcurve_in(iincounter,2))then
                  call journal('sc','*xy_listout3* warning: filenumber of range ends differs, using ',ifile)
                  rcurve_out(ioutcounter-1,2)=ifile  ! stuff the range end file back to the beginning of the range
               endif
               if(imin.gt.imax)then                  ! from big to small or small to big
                  idec=-1                            ! big to small
                  imin=imin-1
               elseif(imax.gt.imin)then
                  idec=1                             ! small to big
                  imin=imin+1
               else                                  ! min and max are the same
                  idec=1
               endif
               do icurve=imin,imax,idec
                  if(ioutcounter.gt.isize) then
                     call journal('sc','*xy_listout3* only',isize,'values allowed')
                     inums=ioutcounter-1
                     return
                  endif
                  rcurve_out(ioutcounter,1)=icurve
                  rcurve_out(ioutcounter,2)=rcurve_in(iincounter,2)
                  rcurve_out(ioutcounter,3)=rcurve_in(iincounter,3)
                  rcurve_out(ioutcounter,4)=rcurve_in(iincounter,4)
                  ioutcounter=ioutcounter+1
               enddo
            else                                     ! value is positive, so not the end of a range
               rcurve_out(ioutcounter,1)=rcurve_in(iincounter,1)
               rcurve_out(ioutcounter,2)=rcurve_in(iincounter,2)
               rcurve_out(ioutcounter,3)=rcurve_in(iincounter,3)
               rcurve_out(ioutcounter,4)=rcurve_in(iincounter,4)
               ioutcounter=ioutcounter+1
            endif
         enddo
      else
         ioutcounter=inums+1
      endif
      inums=ioutcounter-1
end subroutine xy_listout3
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!   xy_retrv2(3f) - [M_xyplot] a special version of RETREV() that integrates the Calculator and Language library for XY plots
!!   (LICENSE:MIT)
!!##SYNOPSIS
!!
!!    subroutine xy_retrv2(name,val,ilen,ier)
!!    character, intent=(in) :: name*(*)
!!    character, intent=(out) :: val*(*)
!!    integer, intent=(out) :: ilen
!!    integer, intent=(out) :: ier
!!##DESCRIPTION
!!      + RETREV() is called to look up the value of the specified name in the
!!        dictionary.
!!      + If the returned value is a string expression(ie. it starts with a $ or
!!        " character) then evaluate the expression using the Calculator libray
!!        and return the expanded value.
!!      + If the calculator expansion of a string expression fails, the
!!        expression itself is returned as blank.
!!      + If the name does not appear in the dictionary a blank string is
!!        returned.
!!##OPTIONS
!!    NAME  is the entry name to look up. From 1 to 20 characters long.
!!    VAL   is the returned value of the entry that was looked up. From 1 to 255 characters long.
!!    ILEN  is the significant length of the string VAL.
!!    IER   is zero(0) if no error occurred.
!!
!!##DEPENDENCIES
!!      + expression
!!      + len_trim
!!      + retrev
!!##SEE ALSO
!!    see parse(3f), retrev(3f), fetch(3f), store(3f), plot_toggle(3f).
!!
!!##REFERENCES
!!##AUTHOR
!!      + John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine xy_retrv2(name,val,ilen,ier)
! 1994 John S. Urban
use M_kracken, only    : retrev
implicit none

! ident_47="@(#)M_xyplot::xy_retrv2(3f): Call RETREV and then expand string variables with Calculator"

character(len=*),intent(in)  :: name
character(len=*),intent(out) :: val
integer,intent(out)          :: ilen
integer,intent(out)          :: ier
character(len=iclen_calc)    :: outlin0
doubleprecision              :: dvalue2
integer                      :: lenn
integer                      :: lenv
integer                      :: ier2
integer                      :: ilen2
!-----------------------------------------------------------------------------------------------------------------------------------
   ier=0
   lenn=len(name)
   lenv=len(val)
   call retrev(name(1:lenn),val(1:lenv),ilen,ier)
   if(val(1:1).eq.'$'.or.val(1:1).eq.'"')then
      ! evaluate a calculator string expression
      call expression(val(:lenv),dvalue2,outlin0,ier2,ilen2)
      if(ier2.eq.2)then
         val=outlin0
         ilen=len_trim(outlin0)
      else   ! judgement call as to leave it as-is or blank it out if calculator failed
         val=' '
         ilen=0
      endif
   endif
end subroutine xy_retrv2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_toggle(3f) - [M_xyplot] special version of toggle(3f) to integrate calculator and language libraries
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!   subroutine plot_toggle(verb,pars)
!!
!!     character, intent=(in) :: verb*(*)
!!     character, intent=(in) :: pars*(*)
!!
!!##DESCRIPTION
!!
!!     For users of the Language library TOGGLE() is an example for the common
!!     problem of commands that have no parameters but are merely 'ON' or 'OFF'.
!!     This routine helps you to easily create a command of the form:
!!
!!          command     # toggle from current state to opposite state.
!!          command on  # explicitly turn this command on
!!          command off # explicitly turn this command off
!!
!!     plot_toggle() does the same thing as TOGGLE() except that now an additional
!!     command syntax is permitted:
!!
!!          command $MODE
!!
!!     where $MODE is a valid Calculator string variable name or expression.
!!
!!       + If value is explicitly on or off, set appropriate string.
!!       + if no value is specified, toggle on/off value.
!!
!!     plot_toggle() is a copy of the TOGGLE() routine that uses xy_retrv2() instead of
!!     RETREV() so that the 'on' and 'off' keywords can be specified as Calculator
!!     string expressions instead of fixed strings.
!!
!!     After this routine is called the RETREV or FETCH procedures can be used to
!!     query the dictionary entry 'verb_oo'. It will either be the string "on" or
!!     the string "off".
!!
!!##OPTIONS
!!     VERB  is the command verb name, from 1 to 20 characters.
!!     PARS  is the remaining parameter part of the command.
!!
!!##DEPENDENCIES
!!       + xy_retrv2
!!       + parse
!!       + store
!!       + len_trim
!!       + jun
!!
!!##EXAMPLES
!!
!!     See the Programmer's example for a simple code with a call to TOGGLE() in
!!     it. plot_toggle() is used in much the same way but with the Calculator library
!!     loaded.
!!
!!##SEE ALSO
!!     see PARSE().
!!
!!##REFERENCES
!!       + NONE.
!!
!!##AUTHOR
!!       + John S. Urban
subroutine plot_toggle(verb,pars)
use M_journal, only : journal
use M_kracken, only: parse, store, IPvalue, IPverb
implicit none

! ident_48="@(#)M_xyplot::plot_toggle(3f): copy of TOGGLE that uses xy_retrv2 instead of RETREV"

! If value is explicitly on or off, set appropriate string,
! if no value is specified, toggle on/off value.
! 1989 John S. Urban
character value*(IPvalue),oldval*(IPvalue)
character tempv*(IPverb+3), verb*(*),pars*(*)
integer            :: ier
integer            :: ilen
integer            :: ilenp
integer            :: ilenv
      ilenv=len_trim(verb)
      ilenp=len(pars)
      tempv=verb(:ilenv)//'_oo'
      call xy_retrv2(tempv,oldval,ilen,ier)             ! store current value
      call parse(verb(:ilenv),' -oo','replace')       ! clear any old value
      call parse(verb(:ilenv),pars(:ilenp),'replace') ! process user command
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      call xy_retrv2(tempv(:ilenv+3),value,ilen,ier) ! get parameter string verb_oo
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!     if length of parameter value string is 0 only the command 'verb -oo'
!     so toggle value
      if(ilen.eq.0)then  ! toggle the value
         if(oldval.eq.'off')then
            value='on'
            write(oldval,'(a,'' set on'')')verb(:ilenv)
            call journal(oldval)
         elseif(oldval.eq.'on')then
            value='off'
            write(oldval,'(a,'' set off'')')verb(:ilenv)
            call journal(oldval)
         else
            call journal('*toggle* invalid old state (not off or on)')
            call journal('*toggle* setting value off')
            value='off'
         endif
         call store(tempv,value,'noadd',ier)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      elseif(value.ne.'off'.and.value.ne.'on')then   ! new value was specified
        call journal('*toggle* invalid new state ignored (not off or on)')
        call store(tempv,oldval,'noadd',ier)
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
end subroutine plot_toggle
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_getdat(3f) - [M_xyplot] place data from specified file and curve into xy_arrayQ xy_array
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!   place data from specified file and curve into xy_arrayQ xy_array
!!
!!   given a file number (ifile)  and curve number (iff)  to extract, place data into
!!   xy_arrayq xy_array in common, and return min and max values; number of values; and the
!!   number of the time xy_array this data is calibrated to.
!!
!!    o header data is put into q variables in common
!!
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_getdat(ifile,iff,ymin,ymax,itime,iread,icurve)
!     1992 John S. Urban
use M_journal, only : journal
use M_math, only    : extremum
implicit none

! ident_49="@(#)M_xyplot::xy_getdat(3f): place data from specified file and curve into xy_arrayQ xy_array"

!===================================================================================================================================
integer,intent(in)            :: ifile   ! file number to extract data from
integer,intent(in)            :: iff     ! curve number to extract from file
real,intent(out)              :: ymin    ! minimum value in selected vector
real,intent(out)              :: ymax    ! maximum value in selected vector
integer,intent(out)           :: itime   ! points to the vector number containing the X values if Y values are being extracted.
                                         ! If itime is set to -1 an error occurred
integer,intent(out)           :: iread   ! iread is the number of significant points read but a multiple of IWZQ where changed
integer,intent(in)            :: icurve  ! icurve is a flag value, as this routine needs several flavors
            ! >= 1  make default label string for label(icurve) if label(icurve) is blank, this is a plot call
            ! = 0  getting data for purpose other than plotting, do not generate label
            ! =-1  this is x-axis data (instead of y-axis) so store in upper half of xy_arrayq
            ! =-2  same as 0 except load x-axis data into upper half too (leaving common values set to y value information)
            ! RELATE CONVERSION OF X VALUES WILL NOT OCCUR!!
real                   :: conb2
real                   :: conm2
real                   :: dummax
real                   :: finalb
real                   :: finalm
integer                :: i100
integer                :: i123
integer                :: i200
integer                :: i75
integer                :: i80
integer                :: ichange
integer                :: idat
integer                :: ihead
integer                :: ii
integer                :: irec
integer                :: istart
integer                :: itemp
integer                :: itimes
integer                :: iuniti
integer                :: iunito
integer                :: iwhere
real                   :: rmax
real                   :: rmin
real                   :: ymax2
real                   :: ymin2
!===================================================================================================================================
      if(iff.lt.1.or.iff.gt.ifrecsq(ifile))then !curve number out of range
         call journal('sc', '*xy_getdat* curve number out of range 1 <= n <= ',ifrecsq(ifile))
         goto 999
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      ihead=ifn_hq+ifile
      ! read a header record INTO COMMON
      read(ihead,rec=iff)icodeq,varidq,nodq,isnodq,itnodq,iunitq,alphaq,itimeq,ipadq,rmin,rmax,alpha2q,rpadq
      if(ipadq(1).gt.0)then     ! number of points to read is in header line, or number read in new file
        iread=ipadq(1)
      else
        iread=isetsq(ifile)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     set default legend label for this curve
      if(icurve.ge.1.and.icurve.le.ICSQ)then
         if(alphaq(KAQ+1:).le.' ')then
           write(lgndq(icurve),303)varidq,nodq,isnodq,itnodq,alphaq(:KAQ)
303        format(a8,1x,i5,1x,i5,1x,i5,1x,a)
         else
           write(lgndq(icurve),'(a)')alphaq(KAQ+1:)
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!      store y data at beginning of xy_array, x data in second half
       if(icurve.eq.-1)then
          istart=IMAXQ2/2
       else
          istart=1
       endif
!-----------------------------------------------------------------------------------------------------------------------------------
      idat=ifn_dq+ifile
      itimes=(ipadq(1)+IWZQ-1)/IWZQ  ! number of curves to read
      iwhere=istart                ! where to store the line read from the zebra file
      irec=ipadq(3)                ! the line number of the zebra file to read
      do i100=1,itimes
         read(idat,rec=irec)(xy_arrayq(ii),ii=iwhere,iwhere+IWZQ-1)
         iwhere=iwhere+IWZQ
         irec=irec+1
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
!     store min and max of data if it has not already been calculated
!     redundantly checking lines of constant value.
      if(rmin.eq.rmax)then
         call extremum(xy_arrayq(istart:istart+iread-1),rmin,rmax) !record min and max
         write(ihead,rec=iff)icodeq,varidq,nodq,isnodq,itnodq,iunitq,alphaq,itimeq,ipadq,rmin,rmax,alpha2q,rpadq  ! write header
      endif
      ymin=rmin
      ymax=rmax
!-----------------------------------------------------------------------------------------------------------------------------------
!     collect linear conversion factors based on automatic table set up relate command so only have to call xy_convert once
      ichange=0                         ! flag as to whether any conversions were found
      finalm=1.0                        ! initial multiplication factor
      finalb=0.0                        ! initial constant factor
      if(.not.LLQ(lshowq))then            ! convert if relate is used actively instead of passively (i.e. to make a right y axis)
         !==============================================================
         do i75=NUNITS0Q,NUNITSQ !reblank the flag xy_array used to look for recursive unit conversion
            iu4q(i75)=0
         enddo
         !==============================================================
90       continue                          ! top of loop gathering the conversion factors
         if(iunitq.gt.NUNITSQ.or.iunitq.lt.NUNITS0Q)then
            call journal('sc','*xy_getdat* unit code out of range',iunitq)
            iunitq=0
         endif
         iuniti=iunitq                     ! input  unit number
         call xy_getrel(iuniti,iunito,conm2,conb2) ! see if this unit is flagged as needing conversion
         if(iunito.ne.iunitq)then          ! accumulate the linear conversion factors until the chain stops or is recursive
            if(iu4q(iuniti).eq.-1)then     ! iu4q is an xy_array of flags indicating whether this conversion has been used or not
               call journal('*xy_getdat* recursive unit conversion detected')
            else
               iu4q(iuniti)=-1             ! set flag went thru this conversion so can avoid recursive loops
               iunitq=iunito               ! save unit number of new units converting to
               finalb=conm2*finalb+conb2   ! accumulate linear conversion factors
               finalm=finalm*conm2
               ichange=ichange+1           ! flag that a conversion call is needed
               goto 90                     ! look for another conversion in a chain
            endif
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(LLQ(livpq).and.icurve.ne.-1)then              ! if initial value mode requested, subtract first y value from all y values
         ! convert first value to converted units and adjust linear conversion constant
         finalb=finalb-(xy_arrayq(istart)*finalm+finalb)
         ichange=ichange+1                            ! flag that a conversion should be made
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ichange.ne.0)then
         call xy_convert(finalm,finalb,xy_arrayq(istart),iread)
         ymin2=finalm*ymin+finalb  ! because of possible sign change, min and max could have changed order
         ymax2=finalm*ymax+finalb
         ymin=min(ymin2,ymax2)
         ymax=max(ymin2,ymax2)
         do i80=NUNITS0Q,NUNITSQ !reblank the flag xy_array used to look for recursive unit conversion
           iu4q(i80)=0
         enddo
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     if the flag is set, remove leading values .le. 0 when calculating smallest value
!     this is not generic, intended to find max and mins for autorange of axis
!     it can cause a lot of overhead by recalculating min many times
!     ymin : actual minimum value after encounter first value .gt. 0
      if(LLQ(logleq).and.ymin.le.0)then
         do i123=0,iread-1   ! find first value not .le. 0
            if(xy_arrayq(i123+istart).gt.0) exit
         enddo
         ! since cutting off some leading values, make sure no other values .le.0 later in data
         call extremum(xy_arrayq(i123+istart:istart+iread-1),ymin,dummax)
         call journal('sc','*xy_jucurv* number of leading points trimmed=',i123)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(icurve.eq.-2)then            ! store x values in upper half per special request
         if(itimeq.eq.0)itimeq=iff   ! special case where reading x values in the first place
         ! trusting itimeq record exists
         ! do not see a need to change number of points if they differ
         ! not converting x values by relate set-up
         itemp=itimeq
         ! set header record data to x-value data
         read(ihead,rec=itemp)icodeq,varidq,nodq,isnodq,itnodq,iunitq,alphaq,itimeq,ipadq,rmin,rmax,alpha2q,rpadq
         istart=IMAXQ2/2
         iwhere=istart                ! where to store the line read from the zebra file
         irec=ipadq(3)                ! the line number of the zebra file to read
         do i200=1,itimes
            read(idat,rec=irec)(xy_arrayq(ii),ii=iwhere,iwhere+IWZQ-1)
            iwhere=iwhere+IWZQ
            irec=irec+1
         enddo
         ! reset header record data
         read(ihead,rec=iff)icodeq,varidq,nodq,isnodq,itnodq,iunitq,alphaq,itimeq,ipadq,rmin,rmax,alpha2q,rpadq
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      itime=itimeq
      return
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue
      itime=-1
      iread=0
      ymin=0.0
      ymax=0.0
end subroutine xy_getdat
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_juput(3f) - [M_xyplot] write current memory-resident curves in global variables(q) onto pseudo file
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_juput(ifile,irecord,rmin,rmax,ionend,iquiet)
use M_journal, only    : journal
use M_math, only       : extremum
implicit none

! ident_50="@(#)M_xyplot::xy_juput(3f): write current memory-resident curves in global variables(q) onto pseudo file"

integer             :: ipadz(5)
character(len=*)    :: iquiet
real                :: rpadz(9)
character(len=8)    :: varidz
character(len=80)   :: alphaz
character(len=80)   :: alpha2z
integer             :: i100
integer             :: i40
integer             :: i80
integer             :: icodez
integer             :: idat
integer             :: ifile
integer             :: ihead
integer             :: ii
integer             :: ii1
integer             :: iloop
integer             :: iloop2
integer             :: ionend
integer             :: irecord
integer             :: isnodz
integer             :: itimez
integer             :: itnodz
integer             :: iunitz
integer             :: ix
integer             :: ix2
integer             :: ixe
integer             :: izero
integer             :: nodz
integer             :: nonzero
real                :: rmax
real                :: rmaxnew
real                :: rmaxz
real                :: rmin
real                :: rminnew
real                :: rminz
real                :: zero
! adding ionend as a feature to replace a record, 0=add, otherwise record to replace
! if replace incompatible record size, would cause error. Should check for this (and append?)
!-----------------------------------------------------------------------------------------------------------------------------------
      ihead=ifn_hq                              ! pseudo file for header records
      idat=ifn_dq                               ! pseudo file for actual data
!-----------------------------------------------------------------------------------------------------------------------------------
!     ifrecsq(0)=number of curves on pseudo file and number of lines in pseudo header file
!     ireg2q=number of lines written to pseudo data file
!     ireg3q=number of time curves in pseudo file
      rminnew=rmin
      rmaxnew=rmax
      if(ionend.eq.0)then                   ! adding to psuedo file
         ix=ifrecsq(0)+1                    ! actual position to store header record in header file
         ix2=ireg2q+1                       ! actual position to store header record in zebra file
      else                                  ! replacing a record
        if(ionend.gt.ifrecsq(0).or.ionend.le.0)then
          call journal('*put* cannot replace nonexistent record')
          return
        else
          ix=ionend                       ! position of header record to replace
          read(ihead,rec=ix)icodez,varidz,nodz,isnodz,itnodz,iunitz,alphaz,itimez,ipadz,rminz,rmaxz,alpha2z,rpadz
          ix2=ipadz(4)
          if(ipadz(1).lt.ipadq(1))then  ! replacing a curve and old curve has less points in it so must truncate new curve
           call journal('*put* tryed to add points to existing record')
           call journal('sc','*put* truncated number of points is ',ipadz(1))
           ipadq(1)=ipadz(1)
           call extremum(xy_arrayq(:ipadz(1)),rminnew,rmaxnew) ! since some points were dropped, need new extremes
          endif
        endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      ipadq(3)=ix2+ceiling(real(ipadq(2)/IWZQ))+1  ! zebra line number where curve data starts
      ipadq(4)=ix2                           ! zebra line number where header data goes
      write(ihead,rec=ix)icodeq,varidq,nodq,isnodq,itnodq,iunitq,alphaq,itimeq,ipadq,rminnew,rmaxnew,alpha2q,rpadq
      write(idat,rec=ix2)icodeq,varidq,nodq,isnodq,itnodq,iunitq,alphaq,itimeq,ipadq,rminnew,rmaxnew,alpha2q,rpadq
!-----------------------------------------------------------------------------------------------------------------------------------
      iloop=(ipadq(1)+IWZQ-1)/IWZQ ! number of lines needed to write out this data curve
      iloop2=ipadq(1)/IWZQ        ! number of completely filled zebra lines that will be filled
      ii1=1
      ix2=ipadq(3)
      do i100=1,iloop2
         write(idat,rec=ix2)(xy_arrayq(ii),ii=ii1,ii1+IWZQ-1)
         ix2=ix2+1
         ii1=ii1+IWZQ
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      if(iloop.ne.iloop2)then ! pad out last line to a full line
         nonzero=ipadq(1)-iloop2*IWZQ !number of significant values in this line
         izero=IWZQ-nonzero  ! number of zeros needed to pad out to a full line
         zero=0.0
         write(idat,rec=ix2)(xy_arrayq(i40),i40=ii1,ii1+nonzero-1),(zero,i80=1,izero)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     write a terminating header line to be written over if another curve is added, else to terminate the file
!     logically easy to put this hear, but would be cleaner to put it on the end if file 0 is every detached, perhaps.
      if(ionend.eq.0)then
         ixe=ix2+1
         write(idat,rec=ixe)0,'END     ',0,0,0,0,alphaq,0,ipadq,0.0,0.0,alpha2q,rpadq
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ionend.eq.0)then          ! adding to psuedo file
         write(temp1q,101)irecord,ix
101      format('*put* ',i10,' became record ',i10,' on file 0 ')
         ifrecsq(0)=ix                  ! increment number of curves stored in pseudo file
         ireg2q=ix2
         call stuff('PUT',ifrecsq(0),'') ! store line number data in calculator for use by user in expressions
      else
         write(temp1q,202)ix
202      format('*put* replaced record ',i10,' on file 0')
      endif
      call journal(iquiet,temp1q)
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ifile.ne.0.and.itimeq.eq.0.and.ionend.eq.0)then ! this is a time curve not from pseudo file being added, not replaced
         ireg3q=ireg3q+1
         itq(1,ireg3q)=ifile            ! file number original was on
         itq(2,ireg3q)=ix               ! curve number on psuedo file
         itq(3,ireg3q)=0                ! not used
         itq(4,ireg3q)=irecord          ! original file position (for matching get curves to pseudo time curves)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine xy_juput
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_slide(3f) - [M_xyplot] slide value
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
real function xy_slide(xmin,xmax,value,bottom)
implicit none

! ident_51="@(#)M_xyplot::xy_slide(3f): slide value"

real,intent(in) :: xmin
real,intent(in) :: xmax
real,intent(in) :: value
real,intent(in) :: bottom
   xy_slide=xmin+(xmax-xmin)*(value-bottom)
end function xy_slide
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_z(3f) - [M_xyplot] Interactive ZOOM mode
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_z()
use M_journal, only : journal
use M_draw
implicit none

! ident_52="@(#)M_xyplot::plot_z(3f): Interactive ZOOM mode"

real               :: con(4)
character(len=10)  :: action
integer            :: iend
real               :: xmin, ymin, xmax, ymax
!-----------------------------------------------------------------------------------------------------------------------------------
      call journal('*plot_z* Interactive ZOOM MODE commencing. MOUSE ACTIONS:')
      call journal('*plot_z*   (1) ZOOM IN......pick a box')
      call journal('*plot_z*   (2) PAN & ZOOM...old box fitted to new box)')
      call journal('*plot_z*   (3) RESET')
      call journal('*plot_z*  double-click without moving mouse to quit')
!-----------------------------------------------------------------------------------------------------------------------------------
1     continue
      call frontbuffer()                                ! make sure drawing is visible
      call xy_getscale(0,con(1),con(2),con(3),con(4))      ! get scaling information between window and "axis scale window"
      call plot_get_plot_area(xmin,xmax,ymin,ymax)
      call vflush() ! does this flush letter buffer?
      call xy_zoom(xmin,ymin,xmax,ymax,iend)         ! changes these values with mouse
      ! remember to draw scaled plot back to next viewport/window
!-----------------------------------------------------------------------------------------------------------------------------------
      action='CSP'
      if(iend.eq.0)then                       !  normal zoom action
         action='CSP'
      elseif(iend.eq.1)then                   ! quit with zoomed ranges
         call journal('*plot_z* interactive zoom mode concluding')
         action='Q'
      elseif(iend.eq.3)then                   ! request to reset back to original values if mouse 3 pressed
         action='RSP'
      else
         write(*,*)'unknown iend command ',iend
         action=' '
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(index(action,'C').ne.0)then  ! convert
         xmin=(xmin-con(2))/con(1)
         xmax=(xmax-con(2))/con(1)
         ymin=(ymin-con(4))/con(3)
         ymax=(ymax-con(4))/con(3)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(index(action,'R').ne.0)then  ! reset
            xmin=123.456           ! special flag value of 123.456 resets to auto max-min mode
            xmax=123.456
            ymin=123.456
            ymax=123.456
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(index(action,'S').ne.0)then  ! store
         RANGEQ(1)=xmin
         RANGEQ(2)=xmax
         RANGEQ(3)=ymin
         RANGEQ(4)=ymax
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(index(action,'P').ne.0)then  ! plot
         call plot_drawplot(.true.)  ! draw scaled plot to requested viewport/window
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(index(action,'Q').eq.0)then  ! quit
         goto 1
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine plot_z
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_zmode(3f) - [M_xyplot] Interactive ZOOM mode
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!    The plot_zmode() procedure is used to allow you to use the keyboard to zoom in
!!    and around a plot.
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_zmode(percent0)
use M_journal, only : journal
use M_kracken, only: store
use M_draw
implicit none

! ident_53="@(#)M_xyplot::plot_zmode(3f): Interactive ZOOM mode"

real                  :: con(4)
character(len=10)     :: action
character(len=1)      :: letter
character(len=255)    :: temp
real              :: bigger
integer           :: ierr
integer           :: iletter
integer           :: istatus
real              :: percent
real              :: percent0
real              :: same
real              :: smaller
real              :: t
real              :: upx
real              :: upy
real              :: w
real              :: x
real              :: xdelta
real              :: xdelta0
real              :: xm
real              :: xmax
real              :: xmax0
real              :: xmaxp
real              :: xmaxt
real              :: xmin
real              :: xmin0
real              :: xminp
real              :: xmint
real              :: xprint
real              :: xshift
real              :: y
real              :: ydelta
real              :: ydelta0
real              :: ym
real              :: ymax
real              :: ymax0
real              :: ymaxp
real              :: ymaxt
real              :: ymin
real              :: ymin0
real              :: yminp
real              :: ymint
real              :: yprint
real              :: yshift

temp=' '
!-----------------------------------------------------------------------------------------------------------------------------------
   call journal('*plot_zmode* Interactive ZOOM MODE commencing.')
   call journal('*plot_zmode* (enter ? in graphics area for help).')
!-----------------------------------------------------------------------------------------------------------------------------------
   percent=100.0/percent0
   call plot_drawplot(.true.)  ! draw scaled plot to requested viewport/window
   call frontbuffer()                                ! make sure drawing is visible
   call xy_getscale(0,con(1),con(2),con(3),con(4))      ! get scaling information between window and "axis scale window"
   call plot_get_plot_area(xmint,xmaxt,ymint,ymaxt)

   xmin=(xmint-con(2))/con(1)  ! desired user axis corners (numbers to scale)
   xmax=(xmaxt-con(2))/con(1)
   ymin=(ymint-con(4))/con(3)
   ymax=(ymaxt-con(4))/con(3)
   Xdelta=(xmax-xmin)/percent
   Ydelta=(ymax-ymin)/percent

   ! hold these values for a reset
   xmin0=xmin
   xmax0=xmax
   ymin0=ymin
   ymax0=ymax
   Xdelta0=Xdelta
   Ydelta0=Ydelta

   call vflush() ! does this flush letter buffer?
2  continue
   iletter=checkkey()
   if(iletter.eq.0)then
      istatus=locator(x,y)
      if(istatus.ne.-1)call point2(x,y)
      goto 2
   endif
   letter=char(iletter)
   ! remember to draw scaled plot back to next viewport/window
!-----------------------------------------------------------------------------------------------------------------------------------
   action='SPD'                            ! default action is to STORE,  PLOT, get new DELTA
   if(letter.eq.'p')then                   ! print values
      istatus=locator(x,y)
      if(istatus.eq.-1)then
         call journal('*plot_zmode* no locator for this device')
      else
         xprint=(x-con(2))/con(1)  ! convert to scaled numbers for printing
         yprint=(y-con(4))/con(3)
         write(*,*)'p: x=',xprint,'y=',yprint
         call point2(x,y)
         action=' '
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'z'.or.letter.eq.'Z'.or.letter.eq.'c')then                   ! center,zoom in,zoom out on mouse location
      istatus=locator(x,y)
      if(istatus.eq.-1)then
         call journal('*plot_zmode* no locator for this device')
      else
         xprint=(x-con(2))/con(1)  ! convert to scaled numbers for printing
         yprint=(y-con(4))/con(3)

         xm=(xmin+xmax)/2.0
         ym=(ymin+ymax)/2.0
         Xshift=xprint-xm     ! shift locator point to center
         Yshift=yprint-ym
         xmin=xmin+Xshift
         xmax=xmax+Xshift
         ymin=ymin+Yshift
         ymax=ymax+Yshift

         if(letter.eq.'z')then
            xmin=xmin+Xdelta          ! zoom in
            xmax=xmax-Xdelta
            ymin=ymin+Ydelta
            ymax=ymax-Ydelta
         elseif(letter.eq.'Z')then
            xmin=xmin-Xdelta          ! zoom out
            xmax=xmax+Xdelta
            ymin=ymin-Ydelta
            ymax=ymax+Ydelta
         endif
         action='SPD'
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'F')then                   ! finer scaling
      action='D'
      percent=percent*1.033333333
      if(percent.lt.1./1000.0)percent=1.0/1000.0
      if(percent.gt.1000.0)percent=1000.0
      write(*,*)'f: percent=',percent
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'C')then                   ! coarser scaling
      action='D'
      percent=percent/1.03333333
      if(percent.gt.1000.0)percent=1000.0
      if(percent.lt.1./1000.0)percent=1./1000.0
      write(*,*)'c: percent=',percent
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(index('BSLRUD',letter).ne.0)then    ! bigger or smaller window

      same=1.0
      smaller=1.0/1.10
      bigger=1.10

      upx=same
      upy=same

      if(letter.eq.'B') then
         upx=bigger
         upy=bigger
      elseif(letter.eq.'S')then
         upx=smaller
         upy=smaller
      elseif(letter.eq.'L')then
         upx=smaller
      elseif(letter.eq.'R')then
         upx=bigger
      elseif(letter.eq.'U')then
         upy=bigger
      elseif(letter.eq.'D')then
         upy=smaller
      endif

      action='SP'

      !     extract preferred size, no smaller than 64x64, no bigger than 2000x2000
      !     only call getdisplaysize twice. Problem where decorations of window are not
      !     consistently considered between size request and size query. So better just
      !     to store the numbers to reduce the problem till resolve M_plot issue.
      if(temp.eq.' ')then
         call getdisplaysize(w,t)
         write(*,*)'from ',w,'x',t
      else
         write(*,*)'from ',w,'x',t
         w=max(64.0,w*upx)
         t=max(64.0,t*upy)
      endif
      write(*,*)' to ',w*upx,'x',t*upy
      write(temp(1:21),'(i10,1x,i10)') int(w),int(t)
      call store('sz_oo',temp,'replace',ierr)
      call plot_sz()
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'l')then                   ! pan left
      xmin=xmin+Xdelta
      xmax=xmax+Xdelta
      action='SP'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'r'.or.letter.eq.'h')then  ! pan right
      xmin=xmin-Xdelta
      xmax=xmax-Xdelta
      action='SP'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'u'.or.letter.eq.'k')then  ! pan up
      ymin=ymin-Ydelta
      ymax=ymax-Ydelta
      action='SP'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'d'.or.letter.eq.'j')then  ! pan down
      ymin=ymin+Ydelta
      ymax=ymax+Ydelta
      action='SP'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'i')then                   ! zoom in
      xmin=xmin+Xdelta
      xmax=xmax-Xdelta
      ymin=ymin+Ydelta
      ymax=ymax-Ydelta
      action='SPD'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'y')then                   ! zoom in on Y axis
      ymin=ymin+Ydelta
      ymax=ymax-Ydelta
      action='SPD'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'x')then                   ! zoom in on X axis
      xmin=xmin+Xdelta
      xmax=xmax-Xdelta
      action='SPD'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'1')then                   ! zoom in quadrant 1
      xmin=xmin+(xmax-xmin)/2.0
      ymin=ymin+(ymax-ymin)/2.0
      action='SPD'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'2')then                   ! zoom in quadrant 2
      xmax=xmax-(xmax-xmin)/2.0
      ymin=ymin+(ymax-ymin)/2.0
      action='SPD'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'3')then                   ! zoom in quadrant 3
      xmax=xmax-(xmax-xmin)/2.0
      ymax=ymax-(ymax-ymin)/2.0
      action='SPD'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'4')then                   ! zoom in quadrant 4
      xmin=xmin+(xmax-xmin)/2.0
      ymax=ymax-(ymax-ymin)/2.0
      action='SPD'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'o')then                   ! zoom out
      xmin=xmin-Xdelta
      xmax=xmax+Xdelta
      ymin=ymin-Ydelta
      ymax=ymax+Ydelta
      action='SPD'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'X')then                   ! zoom out
      xmin=xmin-Xdelta
      xmax=xmax+Xdelta
      action='SPD'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'Y')then                   ! zoom out
      ymin=ymin-Ydelta
      ymax=ymax+Ydelta
      action='SPD'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'?')then                   ! print help text
      action=' '
      goto 111
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'Q')then                   ! quit with zoomed ranges
      call journal('*plot_zmode* interactive zoom mode concluding.')
      action='SPQ'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'q')then                   ! quit with reset ranges
      call journal('*plot_zmode* interactive zoom mode concluding.RESET')
      action='RPQ'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(letter.eq.'n')then                   ! request to reset back to original values if mouse 3 pressed
      action='RPD'
!-----------------------------------------------------------------------------------------------------------------------------------
   else
      write(*,*)'unknown letter command ',letter
      action=' '
      goto 111
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(index(action,'S').ne.0)then  ! store
      RANGEQ(1)=xmin
      RANGEQ(2)=xmax
      RANGEQ(3)=ymin
      RANGEQ(4)=ymax
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(index(action,'R').ne.0)then  ! reset
      ! special flag value of 123.456 resets to auto max-min mode
      RANGEQ(1)=123.456
      RANGEQ(2)=123.456
      RANGEQ(3)=123.456
      RANGEQ(4)=123.456
      xmin=xmin0
      xmax=xmax0
      ymin=ymin0
      ymax=ymax0
      Xdelta=Xdelta0
      Ydelta=Ydelta0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(index(action,'P').ne.0)then  ! plot
      call plot_drawplot(.true.)  ! draw scaled plot to requested viewport/window
      call frontbuffer()                                ! make sure drawing is visible
      call xy_getscale(0,con(1),con(2),con(3),con(4))      ! get scaling information between window and "axis scale window"
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ! new delta movement values because overall range of scale has changed so can move in percent of current
   if(index(action,'D').ne.0)then
      xminP=(xmint-con(2))/con(1)  ! user axis corners (numbers to scale) from last plot command
      xmaxP=(xmaxt-con(2))/con(1)
      yminP=(ymint-con(4))/con(3)
      ymaxP=(ymaxt-con(4))/con(3)
      Xdelta=(xmaxP-xminP)/percent
      Ydelta=(ymaxP-yminP)/percent
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(index(action,'Q').eq.0)then  ! quit
      goto 2
   endif
   return
!-----------------------------------------------------------------------------------------------------------------------------------
111 continue
   call journal('LETTERS:')
   call journal('-------------------------------------------')
   call journal('Zooming via keyboard                       ')
   call journal('  (l)eft (r)ight (u)p (d)own (i)n (o)ut')
   call journal('  lhkj  also work for left,right,up,down')
   call journal('  1234  zooms up a specific quadrant')
   call journal('  x,X  zooms in on X  axis')
   call journal('  y,Y  zooms out on Y axis')
   call journal('-------------------------------------------')
   call journal('Zooming via locator position               ')
   call journal('  (c)enters point under locator')
   call journal('  z,Z  zooms in or out on locator')
   call journal('  (p)rints coordinates')
   call journal('-------------------------------------------')
   call journal('Adjust Zooming Increment                   ')
   call journal('  (C)oarser movements')
   call journal('  (F)iner movements')
   call journal('-------------------------------------------')
   call journal('Window Size                                ')
   call journal('  B,S  bigger or smaller X11 window')
   call journal('  L,R,U,D  bigger or smaller X11 window')
   call journal('-------------------------------------------')
   call journal('Reset                                      ')
   call journal('  (n)ormal default axis ranges')
   call journal('-------------------------------------------')
   call journal('Quitting                                   ')
   call journal('  (q)uits resetting to default view')
   call journal('  (Q)uits saving current zoom')
   call journal('-------------------------------------------')
   goto 2
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine plot_zmode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_getrel(3f) - [M_xyplot] return values to convert one set of units to another
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!    return values to convert one set of units to another
!!    so that a table may be set up to automatically convert one set of
!!    units to another, the relate command allows a linear translation
!!    table to be set up.
!!
!!    given an input unit, get the output unit, rmultiplier, and constant.
!!    since unit 0 is blank, assume not to convert if a 0 iunito is found.
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_getrel(iuniti,iunito,rmult,const)
implicit none

! ident_54="@(#)M_xyplot::xy_getrel(3f): return values to convert one set of units to another"

integer         :: iuniti
integer         :: iunito
integer         :: n
real            :: rmult
real            :: const
      n=iuniti
      if(iu1q(n).ne.0)then
         iunito=iu1q(n)
         rmult=u2q(n)
         const=u3q(n)
      else
         iunito=n
         rmult=1.0
         const=0.0
      endif
end subroutine xy_getrel
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_units(3f) - [M_xyplot] given unit code return string label optionally converted to uppercase
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_units(n0,eunit)
use M_strings,only: upper
implicit none

! ident_55="@(#)M_xyplot::xy_units(3f): given unit code return string label optionally converted to uppercase"

integer,intent(in)  :: n0
character(len=80)   :: eunit
integer             :: n
   n=max0(0,min0(n0,NUNITSQ))
   eunit=uq(n)
   if(LLQ(luppq))then
      eunit=upper(eunit)
   endif
end subroutine xy_units
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_juprint(3f) - [M_xyplot] print string LINE at position x,y with embedded directives
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_juprint(x,y,line,icenter)
use M_draw
implicit none

! ident_56="@(#)M_xyplot::xy_juprint(3f): print string LINE at position x,y with embedded directives"

!      icenter=1    centermode
!      icenter=2    clear centering data, print right justified
real,intent(in)             :: x
real,intent(in)             :: y
character(len=*),intent(in) :: line
integer,intent(in)          :: icenter
real                        :: xs, ys
real                        :: xl, yl
   call pushattributes()
   call centertext(.false.)
   call clipping(.false.)
   call priv_justr(0.0,0.0,line(:len(line)),icenter,xs,xl,ys,yl) ! clear centering data or get centering data
   call priv_justr(x,y,line(:len(line)),0,xs,xl,ys,yl)           ! actually print the string
   call popattributes()
end subroutine xy_juprint
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_justrlen(3f) - [M_xyplot] query string size with embedded directives
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_justrlen(line,xlen,ylen,ydown,yup)
implicit none

! ident_57="@(#)M_xyplot::priv_justrlen(3f): query string size with embedded directives"

character(len=*) :: line
integer          :: ilen
real             :: xl
real             :: xlen
real             :: xs
real             :: ydown
real             :: yl
real             :: ylen
real             :: ys
real             :: yup
   ilen=len_trim(line)
   call priv_justr(0.0,0.0,line(:ilen),1,xs,xl,ys,yl)  ! set size and centering data
   call priv_justr(0.0,0.0,line(:ilen),2,xs,xl,ys,yl)  ! clear centering data
   xlen=xl-xs
   ylen=yl-ys
   ydown=-ys
   yup=yl
end subroutine priv_justrlen
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_justr(3f) - [M_xyplot] print string l0 at position x,y with embedded directives
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!    subroutine priv_justr(x,y,l0,itype,xmin,xmax,ymin,ymax)
!!    real                :: x
!!    real                :: y
!!    character(len=*)    :: l0
!!    integer             :: itype
!!    real                :: xmax
!!    real                :: xmin
!!    real                :: ymin
!!    real                :: ynow
!!##DESCRIPTION
!! !     string is assumed to be [string]\directive\string\directive\string\directive\...
!! !     does not allow for escaped backslash in string or null string between directives
!! !     environment is RESET after each string (MAY CHANGE THIS)
!!##OPTIONS
!! !     itype=0    draw the string with centering or size data set (must be second call)
!! !     itype=1    getting string size and centering shift
!! !     itype=2    blank out centering values
!!##EXAMPLE
!!
subroutine priv_justr(x,y,l0,itype,xmin,xmax,ymin,ymax)
use M_draw
use M_journal,         only : journal
implicit none

! ident_58="@(#)M_xyplot::priv_justr(3f): print string l0 at position x,y with embedded directives"

character(len=*)    :: l0
character(len=4096) :: line
character(len=256)  :: directive
logical             :: instring
integer,parameter   :: ilens=40 ! max number of lines from a single string
real,save           :: xmins(ilens),xmaxs(ilens) ! store each string centering information
character(len=20)   :: lastfont
real                :: height
real                :: height1
real                :: height2
real                :: heightf
integer             :: i10
integer             :: i40
integer             :: ichange
integer             :: ichars
integer             :: iend
integer             :: imax
integer             :: ione
integer             :: istart
integer             :: itype
integer             :: k
real,save           :: shift=0.0
real                :: tdec
real                :: width
real                :: width1
real                :: width2
real                :: widthf
real                :: x
real                :: xmax
real                :: xmin
real                :: xnow
real                :: y
real                :: ymax
real                :: ymin
real                :: ynow
data xmins/40*0.0/
data xmaxs/40*0.0/

   !write(*,*)'itype and string=',itype,l0
   line(1:)=' '
   line=l0
   if(itype.eq.2)goto 999            ! just blank out centering values

   imax=len(l0)                      ! determine length of input string
   if(imax.le.0)goto 999             ! null strings might cause problems
   ichars=len_trim(line(:imax))      ! number of characters in string
   if(ichars.le.0)goto 999           ! if string is all blanks ignore it (causes current position to not be updated)
   xnow=x                            ! initial x print position
   ynow=y                            ! initial y print position
   call move2(xnow,ynow)

   xmin=xnow                         ! min X position for this line of string
   xmax=xnow                         ! max X position for this line of string
   !write(*,*)'xmax 1 =',xmax
   ymin=y
   ymax=y

   k=1                               ! count of number of extra lines
   istart=1                          ! where current string/directive starts
   iend=0                            ! where current string/directive ends
   ichange=0                         ! were attributes changed by directives 0=no 1=yes
   instring=.true.
   call getfontsize(width,height)    !
   if(width.eq.0.0.or.height.eq.0.0)then
      write(*,*)'*priv_justr* A) bad font size (W,H)=',width,height
      call printattribs('priv_justr A')
   endif
   call textsize(width,height)    !
   width1=width                      ! use to restore the values at end (M_DASH does not reset this properly with pop/push?)
   height1=height
   ione=0 ! not zero print P=ichar and return to directive processing
   ! negative, spliced a string back into options

   do i10=1,ichars+1
      if(itype.eq.1)then
         xmins(k)=x
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(line(i10:i10).eq.char(92).or.i10.eq.ichars+1)then          ! found beginning or end of font directives
1000     continue
         if(instring)then                                ! if was in a string now in a directive so print string and start directive
            instring=.false.
            if(iend-istart.ge.0.or.ione.ne.0)then                    ! if string is not null print it
               if(itype.eq.0)then                                    ! draw the string
                  call rmove2(-((xmaxs(k)-xmins(k))/2.0),0.0)
                  if(ione.gt.0)then
                     call xy_jucp2(char(ione),1)                     ! draw the special character from P=
                  elseif(ione.lt.0)then                              ! splicing a $string back in
                  else
                     call xy_jucp2(line(istart:iend),iend-istart+1)  ! draw the string
                  endif
                  call rmove2(((xmaxs(k)-xmins(k))/2.0),0.0)
               elseif(itype.eq.1)then                                ! just building string to get the size
                  xmins(k)=min(xmins(k),xnow)
                  !write(*,*)'building size ione=',ione,xmax
                  if(ione.eq.0)then
                     call move2(xnow+xy_ustrlen2(line(istart:iend),iend-istart+1),ynow)
                  elseif(ione.lt.0)then
                  else
                     call move2(xnow+xy_ustrlen2(char(ione),1),ynow)
                  endif
               else
                  call journal('sc','*priv_justr* bad itype',itype)
                  goto 999
               endif
               call getgp2(xnow,ynow)
               xmax=max(xnow,xmax)
               !write(*,*)'xmax 2 =',xmax
               if(itype.eq.1)then                                    ! building length
                  xmaxs(k)=max(xmaxs(k),xnow)
               endif
               call getfontsize(width2,height2)

               !tdec=getfontdec()+height2/2.0                        ! descender value
               call pushattributes()
               call xy_getxy_jufont(lastfont)
               call font(lastfont)
               call getfontsize(widthf,heightf)
               tdec=-0.22*heightf  ! descender value
               call popattributes()
               call textsize(width2,height2)
               xmin=min(xnow,xmin)
               xmax=max(xnow,xmax)
               !write(*,*)'xmax 3 =',xmax
               ymin=min(ynow-tdec-height2/2.0,ymin)
               ymax=max(ynow-tdec+height2/2.0,ymax)
            endif
         else                                                        ! end of a directive
            instring=.true.
            if(iend-istart.ge.0)then
               if(ichange.eq.0)then
                  ichange=1
                  call push()                                        ! save current environment in case change it
               endif
               if(ione.eq.0)directive=line(istart:iend)
               ione=0
               call priv_fontchng(xnow,ynow,width,height,directive,x,y,k,itype,ione)
               call move2(xnow,ynow)
               xmin=min(xnow,xmin)
               xmax=max(xnow,xmax)
               !write(*,*)'xmax 4 =',xmax
               ymin=min(ynow,ymin)
               ymax=max(ynow,ymax)
            endif
         endif
         if(ione.ne.0)then
            goto 1000
         else
            istart=i10+1                                             ! beginning of next string or directive
         endif
      else                                                           ! not a delimiter
         iend=i10                                                    ! this might be the end of the last directive or string
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
   enddo
   if(ichange.ne.0)call pop()                                        ! so things changed here do not effect other calls
   if(itype.eq.0)then
   else if(itype.eq.1)then
      shift=(xmax-xmin)/2.0
   endif

   call move2(xnow,ynow)                                             ! does pop() loose current position?
   call textsize(width1,height1)
999 continue
   if(itype.eq.0.or.itype.eq.2)then                                  ! do not keep length information
      do i40=1,ilens
         xmins(i40)=0.0
         xmaxs(i40)=0.0
      enddo
      shift=0.0
   endif
end subroutine priv_justr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_fontchng(3f) - [M_xyplot] called by priv_justr to process embedded directives in a string
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_fontchng(xnow,ynow,width,height,string,x0,y0,ilines,ii,ione)
!     does not check if STRING is longer than LINE
!     if using this feature, the variables v,h,f,s,c,w,n,a are reserved names
use M_journal,         only : journal
use M_strings,         only : delim
use M_draw
implicit none

character(len=*)           :: string  ! THIS MUST BE ABLE TO BE CHANGED AND LENGTHENED
integer,parameter          :: ifontnames=33
character(len=11)          :: fonts(ifontnames)
integer,parameter          :: NN=20
character(len=80)          :: xy_array(NN),cname
character(len=iclen_calc)  :: fetched
character(len=len(string)) :: chold
integer                    :: ibegin(NN), ITERM(NN)
doubleprecision            :: dvalue
real            :: height
real            :: heightt
integer         :: i30
integer         :: icount
integer         :: ierr
integer         :: ii
integer         :: ilen
integer         :: iline2
integer         :: ilines
integer         :: ione
integer         :: istart
real            :: rdum
real            :: width
real            :: widtht
real            :: x0
real            :: xa
real            :: xb
real            :: xbb
real            :: xc
real            :: xd
real            :: xdnew
real            :: xf
real            :: xh
real            :: xhh
real            :: xi
real            :: xn
real            :: xnow
real            :: xp
real            :: xs
real            :: xss
real            :: xsx
real            :: xsy
real            :: xv
real            :: xvv
real            :: xw
real            :: xx
real            :: xy
real            :: y0
real            :: ynow
real            :: yy
data fonts/                                                       &
     & 'astrology', 'cursive', 'cyrillic', 'futura.l', 'futura.m',      &
     &'gothic.eng', 'gothic.ger', 'gothic.ita',                         &
     &'greek', 'markers', 'math.low', 'math.upp',                       &
     &'meteorology', 'music', 'script', 'symbolic',                     &
     &'times.g', 'times.i', 'times.ib', 'times.r', 'times.rb',          &
     &'japanese', 'small', 'large',                                     &
     &'orall_aa', 'orall_ab', 'orall_ac',                               &
     &'orall_ad', 'orall_ae', 'orall_af',                               &
     &'orall_ag', 'orall_ah', 'orall_ai'/
!-----------------------------------------------------------------------------------------------------------------------------------
!     M_DRAW BUG
      call getfontsize(widthT,heightT)
      if(widthT.le.0.or.heightT.le.0)then
         call textsize(width,height)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     cache variable names
      XV =rgetvalue('v')
      XH =rgetvalue('h')
      XF =rgetvalue('f')
      XS =rgetvalue('s')
      XC =rgetvalue('c')
      XW =rgetvalue('w')
      XN =rgetvalue('n')
      XSX=rgetvalue('sx')
      XSY=rgetvalue('sy')
      XVV=rgetvalue('V')
      XHH=rgetvalue('H')
      XSS=rgetvalue('S')
      XA =rgetvalue('a')
      XB =rgetvalue('b')
      XBB=rgetvalue('B')
      XX =rgetvalue('x')
      XY =rgetvalue('y')
      XP =rgetvalue('P')
      XD =rgetvalue('D')
      XI =rgetvalue('i')

      call getgp2(XX,YY)
      call stuff('CX',XX,'')
      call stuff('CY',YY,'')
!-----------------------------------------------------------------------------------------------------------------------------------
      if(string.ne.' ')then
         call delim(string,xy_array,NN,icount,ibegin,iterm,ilen,' ') ! parse string into xy_array
         cname(1:1)='$'
         do i30=1,icount
            call priv_zqjreset()
            if(index(xy_array(i30),'=').ne.0)then   ! if an = assume a default numeric expression
               rdum=rnum0(xy_array(i30))  ! evaluate numeric expression
            else  ! string names without a $ character
               cname(2:)=xy_array(i30)
               call expression(cname,dvalue,fetched,ierr,iline2)
               if(ierr.ne.2)then
                  call journal('*xy_juprint* error in string variable')
               elseif(ierr.eq.2.and.dvalue.ge.1.0d0)then
                  chold(:)=' '
                  chold=string(iterm(i30)+1:)
                  string(:)=fetched
                  istart=len_trim(string)
                  istart=istart+1
                  string(istart:)=chold
                  ione=-1 ! flag to just redo the directive
                  goto 40
               endif
            endif
            call priv_doescape(ii,fonts,width,height,xnow,ynow,ilines,x0,y0,ione)
            if(ione.ne.0)then
               string(:iterm(i30))=' ' ! blank out already processed directives
               goto 40
            endif
         enddo
      endif
40    continue
!-----------------------------------------------------------------------------------------------------------------------------------
      XDnew=rgetvalue('D')
!-----------------------------------------------------------------------------------------------------------------------------------
!     restore variable names
      call stuff('v',XV,'')
      call stuff('h',XH,'')
      call stuff('f',XF,'')
      call stuff('s',XS,'')
      call stuff('c',XC,'')
      call stuff('w',XW,'')
      call stuff('n',XN,'')
      call stuff('sx',XSX,'')
      call stuff('sy',XSY,'')
      call stuff('P',XP,'')
      call stuff('V',XVV,'')
      call stuff('H',XHH,'')
      call stuff('S',XSS,'')
      call stuff('a',XA,'')
      call stuff('b',XB,'')
      call stuff('B',XBB,'')
      call stuff('x',XX,'')
      call stuff('y',XY,'')
      call stuff('D',XD,'')
      call stuff('i',XI,'')
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine priv_fontchng
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_zqjreset(3f) - [M_xyplot] called by priv_fontchng(3f) to process embedded directives in a string
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_zqjreset()
implicit none
call  stuff('v',     -999.0d0,'')
call  stuff('h',     -999.0d0,'')
call  stuff('f',     -999.0d0,'')
call  stuff('s',     -999.0d0,'')
call  stuff('c',     -999.0d0,'')
call  stuff('w',     -999.0d0,'')
call  stuff('n',     -999.0d0,'')
call  stuff('sx',    -999.0d0,'')
call  stuff('sy',    -999.0d0,'')
call  stuff('P',     -999.0d0,'')
call  stuff('V',     -999.0d0,'')
call  stuff('H',     -999.0d0,'')
call  stuff('S',     -999.0d0,'')
call  stuff('a',     -999.0d0,'')
call  stuff('b',     -999.0d0,'')
call  stuff('B',     -999.0d0,'')
call  stuff('x',     -999.0d0,'')
call  stuff('y',     -999.0d0,'')
call  stuff('Fx',    1234.5678d0,'')
call  stuff('Fy',    1234.5678d0,'')
call  stuff('Fcen',  -999.0d0,'')
call  stuff('Ffix',  -999.0d0,'')
call  stuff('D',     -999.0d0,'')
call  stuff('i',     -999.0d0,'')
end subroutine priv_zqjreset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_doescape(3f) - [M_xyplot] called by priv_fontchng(3f) to process embedded directives in a string
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
! too many calls to calculator. Do one pass at end like earlier version?
subroutine priv_doescape(ii,fonts,width,height,xnow,ynow,ilines,x0,y0,ione)
use M_journal,         only : journal
use M_draw
implicit none

!parameter(ifontnames=37)
integer,parameter       :: ifontnames=33
character(len=11)       :: fonts(ifontnames)
character(len=20)       :: lastfont
real                    :: a
real                    :: b
real                    :: cheight
real                    :: cwidth
real                    :: fx
real                    :: fy
real                    :: h
real                    :: height
integer                 :: i10
integer                 :: ic
integer                 :: icen
integer                 :: if
integer                 :: ifixed
integer                 :: ii
integer                 :: ilines
integer                 :: ione
integer                 :: iw
integer                 :: ixp
real                    :: ri
real                    :: rn
real                    :: s
real                    :: sx
real                    :: sy
real                    :: v
real                    :: width
real                    :: x
real                    :: x0
real                    :: xnow
real                    :: y
real                    :: y0
real                    :: ynow
      ! igetvalue faster than inum0?
!-----------------------------------------------------------------------------------------------------------------------------------
!     color selection
      if(ii.eq.0)then    ! M_DRAW BUG CHANGES COLOR IN PUSH/POP
         ic=int(igetvalue('c')) ! ic=inum0('c')
         if(ic.ne.-999)then
            call color(ic)
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     font selection by number
      if=int(igetvalue('f')) !  if=inum0('f')
      if(if.ge.1.and.if.le.ifontnames)then
         call font(fonts(if)) ! call selected font
      elseif(if.eq.0)then
         call xy_getxy_jufont(lastfont)
         call font(lastfont)
      elseif(if.ne.-999)then
         do i10=1,ifontnames
            call journal('sc',fonts(i10),i10)
         enddo
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     line width for software characters
      iw=int(igetvalue('w')) !  iw=inum0('w')
      if(iw.ne.-999)call xy_rasters(iw)
!-----------------------------------------------------------------------------------------------------------------------------------
!     vertical movement from current position in scale of original character height
      v=rgetvalue('V') !  v=rnum0('V')
      if(v.ne.-999)then
         ynow=ynow+height*v
      endif

!     vertical movement from current position in scale of current character height
      v=rgetvalue('v') !  v=rnum0('v')
      if(v.ne.-999)then
         call getfontsize(cwidth,cheight)
         ynow=ynow+cheight*v
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     horizontal movement from current position in scale of original character width
      h=rgetvalue('H') !  h=rnum0('H')
      if(h.ne.-999)then
         xnow=xnow+width*h
      endif

!     horizontal movement from current position in scale of current character width
      h=rgetvalue('h') !  h=rnum0('h')
      if(h.ne.-999)then
         call getfontsize(cwidth,cheight)
         xnow=xnow+cwidth*h
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     font size
      s=rgetvalue('S') !  s=rnum0('S')
      if(s.ne.-999)then
         call textsize(width*s,height*s)
      endif

      s=rgetvalue('s') !  s=rnum0('s')
      if(s.ne.-999)then
         call getfontsize(cwidth,cheight)
         call textsize(cwidth*s,cheight*s)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      sx=rgetvalue('sx') !  sx=rnum0('sx')
      sy=rgetvalue('sy') !  sy=rnum0('sy')
      if(sx.ne.-999.or.sy.ne.-999)then
         if(sx.eq.-999) sx=1.0
         if(sy.eq.-999) sy=1.0
         call textsize(width*sx,height*sy)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      ixp=int(igetvalue('P')) !  x=rnum0('x')
      if(ixp.ne.-999)then
           ione=ixp
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      x=rgetvalue('x') !  x=rnum0('x')
      if(x.ne.-999)then
         xnow=x
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      y=rgetvalue('y') !  y=rnum0('y')
      if(y.ne.-999)then
         ynow=y
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      a=rgetvalue('a') !  a=rnum0('a')
      if(a.ne.-999)then
         call textang(a)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      ri=rgetvalue('i') !  ri=rnum0('i')
      if(ri.ne.-999)then
         call textslant(ri)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      b=rgetvalue('b') !  b=rnum0('b')
      if(b.ne.-999)then
         call getfontsize(cwidth,cheight)
         ynow=y0+b*cheight
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      B=rgetvalue('B') !  B=rnum0('B')
      if(B.ne.-999)then
         ynow=y0+B*height
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     line feed
      rn=rgetvalue('n') !  rn=rnum0('n')
      if(rn.ne.-999)then
         call getfontsize(cwidth,cheight)
         xnow=x0
         !ynow=ynow-y0-(ilines+rn)*cheight
         ynow=ynow-rn*cheight
         ilines=ilines+1
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      ifixed=igetvalue('Ffix') !  ifixed=inum0('Ffix')
      if(ifixed.eq.0)then
         call fixedwidth(.false.)
      elseif(ifixed.eq.1)then
         call fixedwidth(.true.)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      icen=igetvalue('Fcen') !  icen=inum0('Fcen')
      if(icen.eq.0)then
         call centertext(.false.)
      elseif(icen.eq.1)then
         call centertext(.true.)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      fx=rgetvalue('Fx') !  fx=rnum0('Fx')
      if(fx.ne.1234.5678)then
         xnow=fx
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      fy=rgetvalue('Fy') !  fy=rnum0('Fy')
      if(fy.ne.1234.5678)then
         ynow=fy
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      icen=igetvalue('Fcen') !  icen=inum0('Fcen')
      if(icen.eq.0)then
         call centertext(.false.)
      elseif(icen.eq.1)then
         call centertext(.true.)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine priv_doescape
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_jucp2(3f) - [M_xyplot] plot string line at current position
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!    For some packages and machines it is best to put the string into
!!    a scratch variable and put a null character at the end
!!
!!    ignores training blanks
!!
!!    uses current text justification modes
!!
!!    M_DRAW fonts use the x value for the bottom of the descender for letters like g,
!!    so move the character down about 22% to keep it level if change size so
!!    THE CURRENT Y-VALUE IS THE HEIGHT OF THE BASE OF LETTERS WITHOUT DESCENDERS
!!    need to make a lower level option in M_DASH for this
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_jucp2(line,ilen)
use M_draw
implicit none

! ident_59="@(#)M_xyplot::xy_jucp2(3f): plot string line at current position"

character(len=*)   :: line
integer,parameter  :: imx=257
character(len=imx) :: line2  ! scratch variable for template and workstation bugs
real               :: cheight
real               :: cheight2
real               :: cheight3
real               :: cwidth
real               :: cwidth2
real               :: cwidth3
real               :: fudge
integer            :: ichars
integer            :: ilen
integer            :: imax
integer            :: ix
real               :: xnow
real               :: ynow

imax=len(line)               ! determine length of input string
                                   ! COULD WARN IF LENGTH OF INPUT TOO BIG TO STORE IN BUFFER
      if(imax.le.0)return          ! null strings might cause problems
      ! DO NOT TRIM TRAILING WHITE-SPACE
      ! ichars=len_trim(line(:imax))    ! number of characters in string
      ichars=ilen
      if(ichars.le.0)return        ! if string is all blanks ignore it (causes current position to not be updated)
      ichars=min(ichars,imx-1)
      line2=line(:ichars)          ! transfer a copy of the string to the buffer
      ix=ichars+1                  ! fixes hp/sun bug
      line2(ix:ix)= char(0)        ! put a null character at end of string
      call getgp2(xnow,ynow)

      call getfontsize(cwidth,cheight)   ! NOT GETTING EXPECTED SIZE IF DID A FONT CHANGE
      if(cwidth.eq.0.0.or.cheight.eq.0.0)then
         write(*,*)'1 bad font size=',cwidth,cheight
         call printattribs('size1')
       endif

      call pushattributes()
         call font('futura.l')
         call getfontsize(cwidth2,cheight2)   ! NOT GETTING EXPECTED SIZE IF DID A FONT CHANGE
         if(cwidth2.eq.0.0.or.cheight2.eq.0.0)then
            write(*,*)'2 bad font size=',cwidth2,cheight2
            call printattribs('size2')
          endif
      call popattributes()

      call getfontsize(cwidth3,cheight3)   ! NOT GETTING EXPECTED SIZE IF DID A FONT CHANGE
      if(cwidth3.eq.0.0.or.cheight3.eq.0.0)then
         write(*,*)'3 bad font size=',cwidth3,cheight3
         call printattribs('size3')
      endif
      call textsize(cwidth,cheight)      ! DOES NOT HAVE EXPECTED CHANGE
      fudge=-cheight/2.0+.28*cheight2
      call move2(xnow,ynow+fudge)! account for descender in font
      call clipping(.false.)
      call drawstr(line2(:ix))     ! output string with null at end
      call clipping(.true.)
      call getgp2(xnow,ynow)
      call move2(xnow,ynow-fudge)! account for descender in font
end subroutine xy_jucp2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_juprints(3f) - [M_xyplot] simple print of string l at position x,y
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_juprints(x,y,l)
use M_draw
implicit none

! ident_60="@(#)M_xyplot::xy_juprints(3f): simple print of string l at position x,y"

real,intent(in)      :: x
real,intent(in)      :: y
character(len=*)     :: l
character(len=257)   :: l2
integer              :: imax
integer              :: iend

      imax=len(l)                  ! determine length of input string
      if(imax.le.0)return          ! null strings might cause problems
      iend=len_trim(l(:imax))         ! number of characters in string
      iend=min(iend,257)
      if(iend.le.0)return          ! if string is all blanks ignore it (causes current position to not be updated)
      call move2(x,y)
      l2=l(:iend)                  ! required to work on some workstations
      l2(iend+1:iend+1)=char(0)    ! required to work on some workstations
      call xy_jucp(l2(:iend+1))
end subroutine xy_juprints
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_jucp(3f) - [M_xyplot] plot string line at current position
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!    For some packages and machines it is best to put the string into
!!    a scratch variable and put a null character at the end
!!
!!    ignores training blanks
!!
!!    uses current text justification modes
!!
!!    M_DRAW fonts use the x value for the bottom of the descender for letters like g,
!!    so move the character down about 22% to keep it level if change size so
!!    THE CURRENT Y-VALUE IS THE HEIGHT OF THE BASE OF LETTERS WITHOUT DESCENDERS
!!    need to make a lower level option in M_DASH for this
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_jucp(line)
use M_draw
implicit none

! ident_61="@(#)M_xyplot::xy_jucp(3f): plot string line at current position"

character*(*) line
integer,parameter        :: imx=257
character(len=imx)       :: line2        ! scratch variable for template and workstation bugs
real                     :: cheight
real                     :: cwidth
integer                  :: ichars
integer                  :: imax
integer                  :: ix
real                     :: tdec
real                     :: xnow
real                     :: ynow

      imax=len(line)               ! determine length of input string
                                   ! COULD WARN IF LENGTH OF INPUT TOO BIG TO STORE IN BUFFER
      if(imax.le.0)return          ! null strings might cause problems
      ichars=len_trim(line(:imax))    ! number of characters in string
      if(ichars.le.0)return        ! if string is all blanks ignore it (causes current position to not be updated)
      ichars=min(ichars,imx-1)
      line2=line(:ichars)          ! transfer a copy of the string to the buffer
      ix=ichars+1                  ! fixes hp/sun bug
      line2(ix:ix)= char(0)        ! put a null character at end of string
      call getgp2(xnow,ynow)
      call getfontsize(cwidth,cheight)
      tdec=getfontdec()+cheight/2.0
      call move2(xnow,ynow+tdec)! account for descender in font
      call drawstr(line2(:ix))     ! output string with null at end
      call getgp2(xnow,ynow)
      call move2(xnow,ynow-tdec)! account for descender in font
end subroutine xy_jucp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_jufont(3f) - [M_xyplot] check and set font and store it so can query it
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_jufont(name)
use M_journal, only : journal
use M_draw
implicit none

! ident_62="@(#)M_xyplot::xy_jufont(3f): check and set font and store it so can query it"

character(len=*)      :: name
character(len=60)     :: msg
integer,parameter     :: ifontnames=33
!  parameter (ifontnames=37)
integer               :: i10, i30
character(len=20)     :: names(ifontnames)
data names/                                                                            &
  &'astrology', 'cursive', 'cyrillic', 'futura.l', 'futura.m',                         &
  &'gothic.eng', 'gothic.ger', 'gothic.ita',                                           &
  &'greek', 'markers', 'math.low', 'math.upp',                                         &
  &'meteorology', 'music', 'script', 'symbolic',                                       &
  &'times.g', 'times.i', 'times.ib', 'times.r', 'times.rb',                            &
  &'japanese', 'small', 'large',                                                       &
  &'orall_aa', 'orall_ab', 'orall_ac',                                                 &
  &'orall_ad', 'orall_ae', 'orall_af',                                                 &
  &'orall_ag', 'orall_ah', 'orall_ai' /
! &'classic_ol', 'romanb_ol', 'romanl_ol' ,'style_ol' /

      do i10=1,ifontnames
        if(name.eq.names(i10))then
           call font(names(i10))
           LASTNAMEQ=names(i10)
           goto 999
        endif
      enddo
      call journal('*xy_jufont* unknown font')
      do i30=1,ifontnames
         write(msg,'(i3,1x,a)')i30,names(i30)
         call journal(msg)
      enddo
      goto 999
999   continue
end subroutine xy_jufont
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_jufont(3f) - [M_xyplot] check and set font and store it so can query it
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_getxy_jufont(name)
implicit none

! ident_63="@(#)M_xyplot::xy_jufont(3f): check and set font and store it so can query it"

character*(*) name
   name=LASTNAMEQ
end subroutine xy_getxy_jufont
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    xy_rasters(3f) - [M_xyplot] set line width
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_rasters(iwidth)
use M_draw
implicit none

! ident_64="@(#)M_xyplot::xy_rasters(3f): set line width"

integer,intent(in) :: iwidth
character(len=80)  :: value
integer            :: ivalue
real,save          :: factor= -1.0
   if(factor.lt.0)then
      value='5.0'
      call get_environment_variable('VLINEWIDTH',value)
      if(value.eq.' ')then
         factor=5.0
      else
         factor=rnum0(value)
         factor=max(0.0,factor)
      endif
   endif
   ivalue=nint(max(1,abs(iwidth))*factor)
   call linewidth(ivalue)
end subroutine xy_rasters
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!@(#) FORTRAN interface point for M_DASH functions that return real values
!     The FORTRAN/C Interface may return different size real values
!     from real functions. Since there are only two real functions
!     (strlength and getaspect) make a default real of each
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    xy_ustrlen(3f) - [M_xyplot] get software string length including trailing whitespace
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
real function xy_ustrlen(string)
use M_draw
implicit none

! ident_65="@(#)M_xyplot::xy_ustrlen(3f): get software string length including trailing whitespace"

character(len=*),intent(in) :: string
intrinsic len
integer :: ilen
   ilen=len(string)
   xy_ustrlen=strlength(string(1:ilen))
end function xy_ustrlen
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    xy_ustrlen2(3f) - [M_xyplot] get software string length up to specified character
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
real function xy_ustrlen2(string,ichars)
use M_draw
implicit none

! ident_66="@(#)M_xyplot::xy_ustrlen2(3f): get software string length up to specified character"

character(len=*),intent(in) :: string
integer,intent(in)    :: ichars
   xy_ustrlen2=strlength(string(1:ichars))
end function xy_ustrlen2
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    plot_get_plot_area(3f) - [M_xyplot] initialize plot page and set up common page-related values
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_get_plot_area(xsmall,xlarge,ysmall,ylarge)
implicit none

! ident_67="@(#)M_xyplot::plot_get_plot_area(3f): initialize plot page and set up common page-related values"

real,intent(out)  :: xsmall
real,intent(out)  :: xlarge
real,intent(out)  :: ysmall
real,intent(out)  :: ylarge

   xsmall=XMINQ2
   xlarge=XMAXQ2
   ysmall=YMINQ2
   ylarge=YMAXQ2
end subroutine plot_get_plot_area
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    plot_set_plot_area(3f) - [M_xyplot] initialize plot page and set up common page-related values
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_set_plot_area(xsmall,xlarge,ysmall,ylarge)
implicit none

! ident_68="@(#)M_xyplot::plot_set_plot_area(3f): initialize plot page and set up common page-related values"

real,intent(in)  :: xsmall
real,intent(in)  :: xlarge
real,intent(in)  :: ysmall
real,intent(in)  :: ylarge

   XMINQ2=xsmall
   XMAXQ2=xlarge
   YMINQ2=ysmall
   YMAXQ2=ylarge
end subroutine plot_set_plot_area
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    plot_axis(3f) - [M_xyplot] draw XY axis for XY plot routines
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!    subroutine plot_axes()
!!##DESCRIPTION
!!
!!    This is a group of routines extracted from the PLT program and being converted
!!    to a generic XY axis routine. It currently has a ridiculous number of
!!    parameters set thru the common block.
!!
!!    Using plot_page(3f), plot_set_nice_range(3f), plot_axis(3f) and xy_line(3f) you can draw simple graphs
!!    quite easily. This will be expanded upon.
!!
!!    plot_set_plot_area(3f) can be used if you are laying out other graphics with the plot.
!!
!!    These routines will change.
!!
!!     1. xy_line destroys the data if a logarithmic plot is drawn
!!     2. A generic "mode" routine will be made to set most of the values in common
!!     3. detailed usage information will be written
!!
!!      V.5.0.2 Error Correction --> DUAL AXIS failure was caused by a missing line in loop 10.
!!
!!##EXAMPLE
!!
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine plot_axes() ! draw the axes
use M_draw
implicit none

! ident_69="@(#)M_xyplot::plot_axes(3f): generically draw linear axis and logarithmic axis"

!     draw y-axis first, as it adjusts left margin of plot area
!
!     make sure ixdivq2 and iydivq2 values are reasonable
!     adjust text size to fit and/or move in y axis
!* SHOULD ENSURE ixdivq2,iydivq2 IS AT LEAST 1
real            :: aa
real            :: actor
real            :: approxx
real            :: around
real            :: bb
real            :: bigger
real            :: factor
integer         :: idrawright
integer         :: idrawtop
integer         :: ifixed
integer         :: ilngb
integer         :: ilngl
integer         :: ilngr
real            :: shift
real            :: tlong
real            :: txtbhm
real            :: txtbwm
real            :: txthil
real            :: txthim
real            :: txtlhm
real            :: txtlwm
real            :: txtmaxh
real            :: txtmaxw
real            :: txtrhm
real            :: txtrwm
real            :: txtwdl
real            :: txtwdm
real            :: widthnum
real            :: x
real            :: xlabely1
real            :: xlabely3
real            :: xleft
real            :: xlong
real            :: xlongdum
real            :: xmax
real            :: xmin
real            :: xprint
real            :: xright
real            :: xstep
real            :: xx
real            :: xxstep
real            :: y
real            :: ydown1
real            :: ydowndum
real            :: yguess1
real            :: yguess3
real            :: yleft
real            :: ymax
real            :: ymin
real            :: ystep
real            :: yup3
real            :: yupdum
real            :: yy
real            :: yystep
!-----------------------------------------------------------------------------------------------------------------------------------
      factor=1.05      ! use to add space between text and tic plot_marks
!-----------------------------------------------------------------------------------------------------------------------------------
! GET DATA FROM COMMONS AND DICTIONARIES
      call plot_get_plot_area(XMIN,XMAX,YMIN,YMAX)
      y=valsq2(7)-valsq2(6)                             ! scale height in axis units
      yy=valsq2(11)-valsq2(10)                          ! right scale height in axis units
      x=valsq2(5)-valsq2(4)                             ! scale width in axis units
      xx=valsq2(9)-valsq2(8)

      ystep=y/max(iydivq2(1),1)                         ! delta between major tic plot_marks to axis scale
      xstep=x/max(ixdivq2(1),1)                         ! delta between major tic plot_marks to axis scale

      yystep=yy/max(iydivq2(3),1)                       ! delta between major tic plot_marks to axis scale
      xxstep=xx/max(ixdivq2(3),1)                       ! delta between major tic plot_marks to axis scale

!     assuming drawing y-axis first, adjust window for labeling and such
      tlong=XMAX-XMIN                                   ! everything except yaxis label is horizontal
      TXTWDL=tlong/ixlabel_chq2                         ! user-desired height of characters to make axis labels

      TXTWDM=tlong/REAL(max(1,abs(ixmin_chq2)))         ! user-desired height of characters to make axis numeric labels

      TXTHIM=TXTWDM/0.7
      TXTHIL=TXTWDL/0.7
!-----------------------------------------------------------------------------------------------------------------------------------
!     set tic lengths each call
      tlong=max(XMAX-XMIN,YMAX-YMIN)
      call plot_setticper(0.0,0.0,0.0,0.0,'reset') ! recalculate tic lengths

      valsq2(2)=0.020*tlong  ! marker size
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ixmin_chq2.lt.0)then           ! if negative, ignore autosizing requests
         ifixed=1                       ! do not auto-size characters to shrink to fit
      else
         ifixed=0
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
! SOME NICE BORDERS
      XMIN=XMIN+1.0*TXTHIL    ! give some working room for a border and because hardware text not always the size one expects
      XMAX=XMAX-1.0*TXTHIL
      YMIN=YMIN+0.5*TXTHIL    ! room bottom of letters, a border space
      YMAX=YMAX-0.5*TXTHIL
!-----
!     put a gap out that is between ID strings and top of xlabel2 to ensure that xlabel2 is not too close to
!     ID area and that large numbers in the corners along the y axes that stick up past the corner do not run
!     into the ID area when there is no xlabel2
!     height of characters might be exaggerated; find a good guess using initially assumed sizes
      txtmaxw=(YMAX-YMIN)/(ixdivq2(1)+3.0)/3
      txtmaxh=min(TXTWDM,txtmaxw)/0.7
      YMAX=YMAX-txtmaxh       ! lower this down so large numbers do not go up into id strings in upper corners
!-----------------------------------------------------------------------------------------------------------------------------------
! FIND ROOM FOR BOTTOM XAXIS LABEL ASSUMING INITIAL TEXT SIZE IS USED
      ! assuming text is not shunk to fit, find height of x-axis labels so y-axis can be drawn short and text centered
      if(axlq2(1) .ne. ' ')then
         call fixedwidth(ifixedlabq2)                     ! text is fixed space or not for xlabel labels
         call xy_jufont(fontLq2)
         call textsize(TXTWDL,TXTHIL)
         !---------------------------------------------
         call priv_justrlen(axlq2(1),xlongdum,yguess1,ydown1,yupdum)
         xlabely1=YMIN+ydown1 ! xlabel will appear at bottom of plot area raised by working room set above
      else
         yguess1=0.0
      endif
                                                        ! approximate where the bottom of the plot area will end up
      if(plot_axis%xlogmode.eq.-1)then                             ! linear labels
                                                        ! room for tics, numeric markers, and labels
         YMIN=YMIN+TEXTTICXQ2+factor*1.2*TXTHIM+yguess1       ! adjust bottom so high enough to print x-axis label
      else                                              ! logarithmic labels
         YMIN=YMIN+TEXTTICXQ2+yguess1+1.8*factor*1.2*TXTHIM   ! adjust bottom so high enough to print x-axis label
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
! FIND ROOM FOR TOP XAXIS LABEL ASSUMING INITIAL TEXT SIZE IS USED
      idrawtop=0
      if(axmulq2.ne.0.0)then   ! plot_marks for second x axis forced in
         idrawtop=1
      endif
      yguess3=0.0
      xlabely3=YMAX
      if(idrawtop.eq.1)then
         if(axlq2(3) .ne. ' ')then
            call fixedwidth(ifixedlabq2)                   ! text is fixed space or not for xlabel labels
            call xy_jufont(fontLq2)
            call textsize(TXTWDL,TXTHIL)
            !---------------------------------------------
            call priv_justrlen(axlq2(3),xlongdum,yguess3,ydowndum,yup3)
            xlabely3=YMAX-yup3          ! xlabel will appear at top of plot area lowered by working room set above
         else
            yguess3=0.0
         endif
                                                           ! approximate where the bottom of the plot area will end up
         if(plot_axis%xlogmode.eq.-1)then                             ! linear labels
            YMAX=YMAX-TEXTTICXQ2-factor*TXTHIM-yguess3     ! adjust bottom so high enough to print x-axis label
                                                           ! (room for tics, numeric markers, and labels)
         else                                              ! logarithmic labels
            YMAX=YMAX-TEXTTICXQ2-yguess3-1.8*factor*TXTHIM ! adjust bottom so high enough to print x-axis label
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call clipping(.false.)                            ! turn clipping off to give hardware text a chance when window is very small
!-----------------------------------------------------------------------------------------------------------------------------------
! DRAW LEFT Y-AXIS TEXT LABEL
      if(axlq2(2) .ne. ' ')then                                ! draw y-axis label if one is present
        ! changes XMIN
        call xy_rasters(ilabel_wq2)                            ! set curve width
        call color(plot_ids(-2)%color)                         ! M_DRAW bug where pop does not restore color
        call priv_draw_y_label(ifixedlabq2,TXTWDL,TXTHIL,fontLq2,deviceq2,axlq2(2),XMIN,XMAX,YMIN,YMAX,ORIENTLq2,2)
        call color(plot_ids(-2)%color)                         ! M_DRAW bug where pop does not restore color
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
! DRAW RIGHT Y-AXIS TEXT LABEL
      if(icrvs2q2.lt.icrvsq2.or.aymulq2.ne.0)then              ! a right axis is to be drawn
       if(axlq2(4) .ne. ' ')then                               ! draw right y-axis label if one is present
        call xy_rasters(ilabel_wq2)                            ! set curve width
        call priv_draw_y_label(ifixedlabq2,TXTWDL,TXTHIL,fontLq2,deviceq2,axlq2(4),XMIN,XMAX,YMIN,YMAX,ORIENTRq2,4)
        call color(plot_ids(-2)%color)                         ! M_DRAW bug where pop does not restore color
       endif
      endif
      call textang(0.0)
!-----------------------------------------------------------------------------------------------------------------------------------
! CALCULATE ROOM NEEDED FOR NUMERIC LABELS ALONG Y AXIS
      call fixedwidth(ifixednumq2)                      ! text is fixed space or not for xmin labels
      TXTLHM=TXTHIM                                     ! This is the desired size
      TXTLWM=TXTWDM
      call priv_adjusty(iydivq2(1),valsq2(6),ylabel_fmtq2,ilngl,ystep,TXTLHM,TXTLWM,TEXTTICYQ2,xleft,YMIN,YMAX,plot_axis%ylogmode)
      XMIN=XMIN+xleft                                   ! make enough room for numeric labels
!-----------------------------------------------------------------------------------------------------------------------------------
      TXTRHM=TXTHIM                                     ! This is the desired size
      TXTRWM=TXTWDM
      if(icrvs2q2.lt.icrvsq2.or.aymulq2.ne.0)then       ! a right axis is to be drawn
         call priv_adjusty(iydivq2(3),valsq2(10),ylabel2_fmtq2,ilngr,yystep,TXTRHM,TXTRWM,TEXTTICYQ2, &
         & xright,YMIN,YMAX,plot_axis%ylogmode)
         XMAX=XMAX-xright                               ! make enough room for numeric labels
      else
         xright=0.0
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
! calculate room needed for numeric labels along x axis at right corner,
! make sure text size can fit between major tic plot_marks, and resize numeric text if needed.
!
!     calculate maximum number of characters in any numeric label along x-axis
      xlong=0
      call fixedwidth(ifixednumq2)                        ! text is fixed space or not for xmin labels
      call xy_jufont(fontNq2)
      call textsize(TXTWDM,TXTHIM)
      call priv_findlongest(plot_axis%xlogmode,ixdivq2(1),valsq2(4),xstep,xlabel_fmtq2,ilngb,xlong)
      widthnum=xy_ustrlen('0')                                     ! assuming numbers are all the same width
      xlong=xlong+widthnum
!     current length of x-axis is only approximately known because of adjustments for labeling,
!     but ballpark good enough for most reasonable aspect ratios
      ! approximate distance between x tic plot_marks for scaling text, 1 ylabel
      approxx=(XMAX-XMIN-TEXTTICYQ2-ilngl*TXTLWM)/max(1,ixdivq2(1))
      around=0.95                            ! assume adjustments will not change length of xaxis by more than 5 percent
      if(xlong.le.around*approxx)then        ! if longest numeric label does not fit between major tic plot_marks, make text smaller
         TXTBWM=TXTWDM
         TXTBHM=TXTHIM
      elseif(ifixed.eq.1)then                ! ignore previous calculations
         TXTBWM=TXTWDM
         TXTBHM=TXTHIM
      else
         TXTBWM=TXTWDM*approxx/xlong*around
         TXTBHM=TXTBWM*.05/.045
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      bigger=TXTBWM*ilngb/2.0-xright
      if(bigger.gt.0.0)then
         XMAX=XMAX-bigger                               ! adjust for x-axis right numeric label sticking past corner
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      xprint=XMIN-TEXTTICYQ2-0.25*TXTLWM                       ! xvalue for label to end at for left labels
      if(-TICLNYQ2.gt.XMAX-XMIN)TICLNYQ2=-(XMAX-XMIN) ! so major tic stops at top if sticks into plot
      call priv_draw_y_nums(TXTLWM,TXTLHM,xprint,iydivq2(1),YMAX,YMIN,-TICLNYQ2,TICLNY2Q2,y,ystep,valsq2(6),1,ilngl,XMIN,imin_wq2, &
     & plot_ids(-1)%width,plot_axis%ylogmode,ylabel_fmtq2,0.0,1.0)
      idrawright=0
!-----------------------------------------------------------------------------------------------------------------------------------
!     RIGHT AXIS MAJOR TICS AND NUMERIC LABELS
      if(icrvs2q2.ne.icrvsq2)then                           ! plot_marks for second y axis
         idrawright=1
         xprint=XMAX+TEXTTICYQ2+0.25*TXTLWM                  ! xvalue for label to end at for left labels
         SHIFT=0.0
         actor=1.0
      elseif(aymulq2.ne.0.0.and.icrvs2q2.eq.icrvsq2)then     ! plot_marks for second y axis forced in
         idrawright=1
!        assuming range for second axis was long enough in values to include all of first axis,
!        lining up the base point exactly, and then rescale the step and ignore plot_marks not along the existing edge
!        force right step to be correct size
         bb=valsq2(7)*aymulq2+ayconq2
         aa=valsq2(6)*aymulq2+ayconq2
         yy=valsq2(11)-valsq2(10)                           ! right scale height in axis units
         actor=(bb-aa)/yy                               ! how many units should be/are   along right edge
         ! find point that should be at bottom right corner
         ! new axis will be different length
         !! yright=valsq2(6)*aymulq2+ayconq2                  ! point to put at YMIN on right is same value as on other side
         yleft=(valsq2(10)-ayconq2)/aymulq2                   ! bottom right converted back to left
         SHIFT=(valsq2(6)-yleft)/(valsq2(7)-valsq2(6))*(YMAX-YMIN)
         yystep=yy/max(iydivq2(3),1)                      ! delta between major tic plot_marks to axis scale
         ! find how far have to shift up scale we were going to draw
         xprint=XMAX+2*TEXTTICYQ2                           ! xvalue for label to end at for left labels
      endif
      if(idrawright.eq.1)then                           ! draw the right numbers and major tics
         if(TICLNYQ2.gt.XMAX-XMIN)TICLNYQ2=-(XMAX-XMIN) ! so major tic stops at top if sticks into plot
         call priv_draw_y_nums(TXTRWM,TXTRHM,xprint,iydivq2(3),YMAX,YMIN,                  &
     &    TICLNYQ2,-TICLNY2Q2,yy,yystep,valsq2(10),2,ilngr,XMAX,imin_wq2,plot_ids(-1)%width, &
     &    plot_axis%ylogmode,ylabel2_fmtq2,SHIFT,actor)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(-TICLNXQ2.gt.YMAX-YMIN)TICLNXQ2=-(YMAX-YMIN)    ! so major tic stops at top if sticks into plot
      call priv_drawx(TXTBWM,TXTBHM,                          &
     &  xstep,                                           &
     &  TXTWDL,TXTHIL,                                   &
     &  xlabely1,                                        &
     &  ixdivq2(1),                                      & ! bottom axis numbers
     &  yguess1,                                         &
     &  factor,                                          &
     &  x,                                               &
     &  1,                                               &
     &  YMIN,                                            &
     &  plot_ids(-1)%width,                                     &
     &  XMIN,XMAX,                                       &
     &  TICLNXQ2,TEXTTICXQ2,-TICLNX2Q2,                  &
     &  valsq2(4),                                       &
     &  ilabel_wq2,                                      &
     &  imin_wq2,                                        &
     &  ifixednumq2,ifixedlabq2,                         &
     &  1,                                               &
     &  fontLq2,fontNq2,                                 &
     &  xlabel_fmtq2,                                    &
     &  plot_axis%xlogmode,                                         &
     &  axlq2)

      call color(plot_ids(-2)%color)                               ! M_DRAW bug where pop does not restore color
!-----------------------------------------------------------------------------------------------------------------------------------
      idrawtop=0
      if(axmulq2.ne.0.0)then     ! plot_marks for second x axis forced in
         idrawtop=1
      endif
      if(idrawtop.eq.1)then
         call priv_drawx(TXTBWM,TXTBHM,       &! -> text size for numeric labels
     &      xstep,                       &!
     &      TXTWDL,TXTHIL,               &! -> text size for text labels
     &      xlabely3+TEXTTICXQ2,         &! xlabely  ->
     &      ixdivq2(1),                  &! idivs ->
     &      yguess3,                     &! iguess ->
     &      factor,                      &! factor ->
     &      x,                           &! x ->
     &      3,                           &! iaxis -> which
     &      YMAX,                        &! ybase ->
     &   plot_ids(-1)%width,                    &! idwid ->
     &  XMIN,XMAX,                       &! xsmall, xlarge ->
     &   TICLNXQ2,TEXTTICXQ2,-TICLNX2Q2, &! ticln -> for drawing tics
     &   valsq2(4),                      &! vals ->
     &   ilabel_wq2,                     &! ilabel_w ->
     &   imin_wq2,                       &! imin_w ->
     &   ifixednumq2,ifixedlabq2,        &! iffixed,iffixed2 ->
     &   -1,                             &! iup ->
     &   fontLq2,fontNq2,                &! fontL, fontN -> fonts for labels and numbers
     &   xlabel2_fmtq2,                  &! fmt ->
     &   plot_axis%xlogmode,                        &! ilogx
     &   axlq2)                           ! axisl -> xy_array with axes labels
         call color(plot_ids(-2)%color)                               ! M_DRAW bug where pop does not restore color
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call priv_jubox(plot_axis%background,XMIN,YMIN,XMAX,YMAX)
!-----------------------------------------------------------------------------------------------------------------------------------
      call plot_set_plot_area(XMIN,XMAX,YMIN,YMAX)  ! grid routine has to know plot area to draw rescaled dash codes in grid lines
!-----------------------------------------------------------------------------------------------------------------------------------
      call priv_jugrid(ixdivq2,iydivq2,plot_axis%grid_style,valsq2, &
      & XMIN,XMAX,YMIN,YMAX,plot_ids%color,plot_ids%width,plot_axis%xlogmode,plot_axis%ylogmode, &
     & TICLNYQ2,TICLNXQ2,TICLNY2Q2,TICLNX2Q2,aymulq2,ayconq2,icrvs2q2,icrvsq2)
!-----------------------------------------------------------------------------------------------------------------------------------
      call clipping(.true.)                             ! turn clipping on
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine plot_axes
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine priv_draw_y_nums(xtext,ytext,xprint,iydivi,top,          & ! draw numbers and major tic lines along y axis
     & bottom,tic,ticmin,yy,yystep,valsi,iii,ilong,xmax,imin_w,idwid,ilogy,fmt,SHIFT,factor)
!      xtext   ->  width of characters
!      ytext   ->  height of characters
!      xprint  ->  x coordinate to print numbers at
!      iydivi  ->  number of divisions
!      top     ->  y window value of top of plot area
!      bottom  ->  y window value of bottom of plot area
!      tic     ->  length of major tics
!      ticmin  ->  length of minor tics
!      yy      ->
!      yystep  ->  increment for value to print
!      valsi   ->  initial value to print
!      iii     ->  =1 for left axis, =2 for right axis
!      ilong   ->
!      xmax    -> x value of border drawing along (left or right)
!      imin_w  -> width of line to draw text
!      idwid   -> width of line to draw major tics
!      ilogy   -> flag if labels are log or linear
!      fmt     ->
!      SHIFT   ->
!      factor  ->
use M_draw
implicit none

integer                  :: iydivi
character(len=*)         :: fmt
real                     :: alabel
real                     :: bottom
real                     :: factor
integer                  :: i10
integer                  :: i20
integer                  :: idwid
integer                  :: iii
integer                  :: ilogy
integer                  :: ilong
integer                  :: imin_w
real                     :: shift
real                     :: sy
real                     :: tic
real                     :: ticmin
real                     :: top
real                     :: valsi
real                     :: xmax
real                     :: xprint
real                     :: xtext
real                     :: yprint
real                     :: ytext
real                     :: yy
real                     :: yystep

         call xy_rasters(idwid)
         do i20=0,iydivi
            sy = i20 * (top - bottom) / factor / yy * yystep + bottom
            sy= sy - SHIFT
            if(sy.ge.(bottom*0.99).and.sy.le.(top*1.01))then
               call move2(xmax, sy)                       ! draw tic from axis out to numeric label
               call draw2(xmax + tic, sy)
               if(xprint.gt.xmax)then ! right axis
               else ! left axis
                 if(tic.gt.0)then  ! major tics point in, draw back same as minor tics
                   call move2(xmax, sy)                       ! draw tic from axis out to numeric label
                   call draw2(xmax + min(xmax,ticmin), sy)
                 endif
               endif
            endif
         enddo

         call textsize(xtext,ytext)
         call xy_rasters(imin_w)  ! width of lines to draw software text
         do i10=0,iydivi
            sy = i10 * (top - bottom) / factor / yy * yystep + bottom
            sy= sy - SHIFT                ! Added due to JSU's finding of DUAL axis failure (6/1/05)
            if(sy.ge.(bottom*0.99).and.sy.le.(top*1.01))then
               alabel = valsi + yystep * i10
               yprint=sy-ytext/2                          ! baseline for bottom of characters
               call priv_labely(iii,alabel,xprint,yprint,xtext,ytext,ilong,yystep,i10+1,ilogy,fmt)
            endif
         enddo

end subroutine priv_draw_y_nums
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine priv_draw_y_label(ifixedlab,TXTWDL,TXTHIL,fontL,device,caxis,XMIN,XMAX,YMIN,YMAX,ORIENT,ia)
use M_journal, only : journal
use M_draw
implicit none

character(len=*) :: fontL,device,caxis
logical          :: ifixedlab
integer          :: i75
integer          :: ia
integer          :: ii
real             :: orient
real             :: oydown
real             :: oyup
real             :: txthh2
real             :: txthil
real             :: txtwdl
real             :: txtww2
real             :: xlong
real             :: xmax
real             :: xmin
real             :: xtemp
real             :: ydown
real             :: ymax
real             :: ymin
real             :: ystring
real             :: ytall
real             :: ytemp
real             :: yup
      call fixedwidth(ifixedlab)                        ! text is fixed space or not
      TXTWW2=TXTWDL                                     ! first, assume text can be the user-requested size
      TXTHH2=TXTHIL
      !write(*,*)'=============================='
      !write(*,*)'call priv_draw_y_label:'
      !write(*,*)'   ifixedlab:',ifixedlab
      !write(*,*)'   txtwdl:',txtwdl
      !write(*,*)'   txthil:',txthil
      !write(*,*)'   fontL:',fontL
      !write(*,*)'   device:',device
      !write(*,*)'   caxis:',caxis
      !write(*,*)'   xmin:',xmin
      !write(*,*)'   xmax:',xmax
      !write(*,*)'   ymin:',ymin
      !write(*,*)'   ymax:',ymax
      !write(*,*)'   orient:',orient
      !write(*,*)'   ia:',ia
      !write(*,*)'=============================='
      call xy_jufont(fontL)
      call textsize(TXTWW2,TXTHH2)

!-----------------------------------------------------------------------------------------------------------------------------------
!     need way to query who has rotatable hardware text from M_DRAW
      ii=0
      if(device(1:4).eq.'xfig')ii=1
      if(device(1:3).eq.'svg')ii=1
      if(device(1:3).eq.'gnu')ii=1
      if(device(1:3).eq.'pos')ii=1
      if(device(1:3).eq.'pdf')ii=1
      if(device(1:3).eq.'vml')ii=1
      if(device(1:2).eq.'ps') ii=1
      if(device(1:2).eq.'pp') ii=1
      if(device(1:3).eq.'   ')ii=1
      if((fontL.eq.'small'.or.fontL.eq.'large').and.ii.eq.0)then ! one character under another if hardware text cannot rotate
        call getfontsize(TXTWW2,TXTHH2)
         ii=len_trim(caxis)
         ytemp=(YMAX+YMIN)/2.0+(ii*TXTHH2)/2.0          ! center string vertically assuming fixed-height characters
         do i75=1,ii
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if(ia.eq.2)xtemp=XMIN-TXTWW2                ! draw y-axis label
            if(ia.eq.4)xtemp=XMAX-max(TXTHH2,TXTWW2)    ! right axis label
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            call xy_juprint(xtemp,ytemp,caxis(i75:i75),2)
            ytemp=ytemp-TXTHH2
         enddo
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if(ia.eq.2)XMIN=XMIN+max(TXTHH2,TXTWW2)        ! shift in XMIN to account for label width
         if(ia.eq.4)XMAX=XMAX-max(TXTHH2,TXTWW2)        ! shift in XMAX to account for label height
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------------------------------------------------------------------
      else                                              ! string parallel to y axis
         !---------------------------------------------
         call priv_justrlen(caxis,xlong,ytall,ydown,yup)
         !---------------------------------------------
         ystring=xlong
         if(ystring.ge.(YMAX-YMIN))then                 ! if y-axis alphameric label does not fit, reduce character size
            TXTWW2=TXTWW2*(YMAX-YMIN)/ystring
            TXTHH2=TXTWW2/0.7
            call textsize(TXTWW2,TXTHH2)  ! not flipping yet, so act like printing horizontally
            call priv_justrlen(caxis,xlong,ytall,ydown,yup)
         endif
         !---------------------------------------------
         ytemp=(YMAX+YMIN)/2.0                          ! center string vertically
         !---------------------------------------------
         ! the fancy xy_juprint currently only supports horizontal text so create a rotated coordinate system
         call push()
            if(ORIENT.eq.90)then
              Oyup=yup
              Oydown=ydown
            elseif(ORIENT.eq.270)then
              Oyup=ydown
              Oydown=yup
            else ! someone playing games
              Oyup=xlong/2.0
              Oydown=xlong/2.0
            endif
            !!!!!!!!!!!!!!!!!!!!!!!!!
            if(ia.eq.2)then
               XMIN=XMIN+Oyup                           ! shift in XMIN to account for label height
               call translate(XMIN,ytemp,0.0)
            elseif(ia.eq.4)then
               XMAX=XMAX-Oydown                         ! shift in XMAX to account for label height
               call translate(XMAX,ytemp,0.0)
            else
               call journal('sc','priv_draw_y_label* *unknown ia value',ia)
            endif
            !!!!!!!!!!!!!!!!!!!!!!!!!!!
            if(ia.eq.4)XMAX=XMAX-Oydown                 ! shift in XMAX to account for label height

            call rotate(ORIENT,'z')
            call xy_juprint(0.0,0.0,caxis,1)               ! print vertical string y-axis label
            ! debug stuff
            !call font('futura.m')
            !call xy_juprint(0.0,0.0,caxis,1)               ! print vertical string y-axis label
            !call polyfill(.false.)
            !call color(1)
            !call circle(0.0,0.0,0.2)
            !call color(2)
            !call circle(0.0,0.0,0.4)
            !call color(3)
            !call circle(0.0,0.0,0.8)
            !call polyfill(.true.)

         call pop()
         !!!!!!!!!!!!!!!!!!!!!!!!!!!
         if(ia.eq.2)XMIN=XMIN+Oydown                    ! shift in XMIN to account for label height
         if(ia.eq.4)XMAX=XMAX-Oyup                      ! shift in XMAX to account for label height
         !!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine priv_draw_y_label
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine priv_adjusty(iydivi,valsi,ylabel_fmt,ilong,ystep,TXTLHEIGHT,TXTLWIDTH,tic,SHIFT,ymin,ymax,ilogy)

!     calculate maximum number of characters in any numeric label so can move y-axis over as necessary
!     and shrink text size for numeric labels if required to keep numbers from running into each other

use M_draw
implicit none

character(len=255)  :: ylabel_fmt
integer             :: iydivi
integer             :: ilogy
integer             :: ilong
real                :: big
real                :: dumhh
real                :: shift
real                :: tic
real                :: txtlheight
real                :: txtlwidth
real                :: valsi
real                :: widthnum
real                :: ymax
real                :: ymin
real                :: ystep

  call priv_findlongest(ilogy,iydivi,valsi,ystep,ylabel_fmt,ilong,big)

  if((YMAX-YMIN).lt.(TXTLheight*(iydivi+3)))then            ! If cannot stack this many labels up, have to shrink
     TXTLheight=(YMAX-YMIN)/(iydivi+3)*0.95
     TXTLwidth=0.7*TXTLheight
  endif

  call textsize(TXTLwidth,TXTLheight)
  call getcharsize('0',widthnum,dumhh)                      ! assuming numbers are all the same width

  shift= (ilong+0.5)*(widthnum)+tic                         ! length needed for longest numeric label
end subroutine priv_adjusty
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine priv_drawx(TXTBWM,TXTBHM,xstep,TXTWDL,TXTHIL,xlabely,idivs,   &   ! tic plot_marks and numeric labels for x axis
     & yguess,factor,x,iaxis,ybase,idwid,xsmall,xlarge,                   &
     & ticln,ticlnt,ticlnminor,                                           &
     & vals,ilabel_w,imin_w,iffixed,iffixed2,iup,fontL,fontN,fmt,         &
     & ilogx,axisl)
use M_draw
implicit none

logical iffixed
logical iffixed2
character axisl(4)*255
character*(*)fmt
character*(*)fontL,fontN
integer idivs
real           :: alabel
real           :: botmajor
real           :: botminor
real           :: factor
integer        :: i10
integer        :: i20
integer        :: iaxis
integer        :: idwid
integer        :: ilabel_w
integer        :: ilogx
integer        :: imin_w
integer        :: iup
real           :: sx
real           :: sy
real           :: ticln
real           :: ticlnminor
real           :: ticlnt
real           :: txtbhm
real           :: txtbwm
real           :: txthh2
real           :: txthil
real           :: txtwdl
real           :: txtww2
real           :: vals
real           :: x
real           :: xlabely
real           :: xlarge
real           :: xlong
real           :: xsmall
real           :: xstep
real           :: xstring
real           :: xx
real           :: ybase
real           :: ydown
real           :: yguess
real           :: ytall
real           :: yup

!     fontN        font for numbers
!     fontL        font for labels
!     fmt          user-definable format for numbers

!      write(*,*)TXTBWM,TXTBHM,
!     & xstep,
!     & TXTWDL,TXTHIL,
!     & xlabely,             ! tic plot_marks and numeric labels for x axis
!     & idivs,
!     & yguess,
!     & factor,
!     & x,
!     & iaxis,
!     & ybase,
!     & idwid,
!     & xsmall,xlarge,
!     & ticln,ticlnt,
!     & vals,ilabel_w,imin_w,iffixed,iffixed2,iup,fontL,fontN,fmt,
!     & ilogx,axisl
!-----------------------------------------------------------------------------------------------------------------------------------
!     draw x axis
!     should check if idivs less than 1
!-----------------------------------------------------------------------------------------------------------------------------------
      call fixedwidth(iffixed)                        ! text is fixed space or not for xmin labels
      call xy_jufont(fontN)
      call textsize(TXTBWM, TXTBHM)                   ! set text size for numeric labels
      if(iup.ge.0)then
         sy=ybase-iup*(ticlnt+factor*TXTBHM)           ! height to print x-axis numeric labels at
      else
         sy=ybase-iup*(ticlnt+factor*TXTBHM)-TXTBHM    ! height to print x-axis numeric labels at
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_rasters(idwid)                             ! set curve width
      botmajor=ybase-iup*ticln                        ! yvalue of end of major tic
      botminor=ybase-iup*ticlnminor                   ! yvalue of end of minor tic
      do i10=0,idivs                               ! draw the tic mark to the number
         sx = i10 * (xlarge-xsmall)/x * xstep + xsmall
          if(botmajor.ge.ybase)then      ! draw major tic into plot area
            call move2(sx, ybase)
            call draw2(sx, botmajor)
            call move2(sx, ybase)        ! draw to same as minor if sticks out or null line
            call draw2(sx, min(botminor+0.1*TXTBHM,ybase))
         else                          ! major sticks out, draw to it
            call move2(sx, ybase)
            call draw2(sx, min(botmajor+0.1*TXTBHM,ybase))
         endif
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_rasters(imin_w)                            ! width of lines to draw software text
      do i20=0,idivs
         sx = i20 * (xlarge-xsmall)/x * xstep + xsmall
         alabel = vals + xstep * i20                  ! calculate the number and convert it to a string
         call priv_labelx(alabel,sx,sy,TXTBWM,TXTBHM,xstep,i20+1,ilogx,fmt,iaxis)
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
!     place label on x-axis
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_rasters(imin_w)  ! width of lines to draw software text
      if(axisl(iaxis) .ne. ' ')then
         call fixedwidth(iffixed2)   ! text is fixed space or not for xlabel labels
         TXTWW2=TXTWDL
         TXTHH2=TXTHIL
         call xy_jufont(fontL)
         call textsize(TXTWW2,TXTHH2)
         !---------------------------------------------
         call priv_justrlen(axisl(iaxis),xlong,ytall,ydown,yup)
         xstring=xlong
         !---------------------------------------------
         if(xstring.ge.(xlarge-xsmall))then          ! if x-axis alphameric label does not fit, reduce character size
            TXTWW2=TXTWW2*(xlarge-xsmall)/xstring
            TXTHH2=TXTWW2*(0.05/0.035)
            call textsize(TXTWW2,TXTHH2)
         endif
         !---------------------------------------------
         call priv_justrlen(axisl(iaxis),xlong,ytall,ydown,yup)
         !---------------------------------------------
         !xx = (xlarge+xsmall)/2.0-xlong/2.0 ! center string to x value
         xx = (xlarge+xsmall)/2.0                              ! center string to x value
         call xy_rasters(ilabel_w)                              ! width of lines to draw software text
         call xy_juprint(xx,xlabely,axisl(iaxis),1)   ! print x-axis label
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call xy_rasters(idwid)
end subroutine priv_drawx
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine priv_labelx(alabel,x,y,xtext,ytext,xxx,icount,ilogx,fmt,iaxis)
use M_journal, only : journal
use M_draw
implicit none

! ident_70="@(#)M_xyplot::priv_labelx(3fp): print a logarithmic label or linear label on x axis on bottom side"

!     alabel is numeric value to print
!     y is height of baseline to print at
!     x is where tic is at
!     xtext,ytext is text size
!     xxx
!     icount
!     ilogx
character(len=80)  :: form
character(len=255) :: fmt
real       :: alabel
integer    :: iaxis
integer    :: icount
integer    :: ierr
integer    :: ilen
integer    :: ilogx
real       :: x
real       :: xover
real       :: xtext
real       :: xx
real       :: xxx
real       :: y
real       :: yprint
real       :: yprint2
real       :: ytext
   call xy_toa(alabel,form,ilen,ierr,xxx,fmt,icount) ! convert alabel to string form, return number of chars in ilen
   xx=xy_ustrlen(form(:ilen))
   if(ilogx.eq.-1)then                           ! do linear label
      !call xy_juprints(x-xx/2.0,y,form)              ! print string centered to given x value
      call xy_juprint(x-xx/2.0,y,form,2)              ! print string centered to given x value
   elseif(iaxis.eq.1)then                             ! do logarithmic label
      yprint=y-.7*ytext
      yprint2=yprint+0.9*ytext                    ! go up for superscript
      call textsize(0.7*xtext,0.7*ytext)          ! set to a smaller character size for superscript
      !call xy_juprints(x,yprint2,form)
      call xy_juprint(x,yprint2,form,2)
      xover=xy_ustrlen('10')                         ! get string length so can position to print the base number
      call textsize(xtext,ytext)                  ! restore text size
      !call xy_juprints(x-xover,yprint,'10')
      call xy_juprint(x-xover,yprint,'10',2)
   elseif(iaxis.eq.3)then
      yprint=y+.7*ytext
      yprint2=yprint+0.9*ytext                    ! go up for superscript
      call textsize(0.7*xtext,0.7*ytext)          ! set to a smaller character size for superscript
      !call xy_juprints(x,yprint2,form)
   call xy_juprint(x,yprint2,form,2)
      xover=xy_ustrlen('10')                         ! get string length so can position to print the base number
      call textsize(xtext,ytext)                  ! restore text size
   !call xy_juprints(x-xover,yprint,'10')
      call xy_juprint(x-xover,yprint,'10',2)
   else
      call journal('sc','*priv_labelx* unknown iaxis value=',iaxis)
   endif
end subroutine priv_labelx
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine priv_labely(iside,alabel,x,y,xtext,ytext,ilong,xxx,icount,ilogy,fmt)
use M_draw
implicit none

! ident_71="@(#)M_xyplot::priv_labely(3fp): print logarithmic or linear y axis label on left or right"

!     iside=1 for left side label, iside=2 for right side label
!     alabel is numeric value to print
!     y is height of baseline to print at
!     x is right edge of printed text string
character*23 form
character*(*)fmt
real          :: alabel
integer       :: icount
integer       :: ierr
integer       :: ilen
integer       :: ilogy
integer       :: ilong
integer       :: iside
real          :: x
real          :: xpos
real          :: xprint
real          :: xprint2
real          :: xtext
real          :: xtext2
real          :: xx
real          :: xxx
real          :: y
real          :: yprint2
real          :: ytext

      call xy_toa(alabel,form,ilen,ierr,xxx,fmt,icount)  ! convert alabel to string, return number of chars in ilen

      if(ilogy.eq.-1)then                              ! do linear label
         xx=xy_ustrlen(form(:ilen))
         if(iside.eq.1)then
            xpos=x-xx
         else
            xpos=x
         endif
         call xy_juprint(xpos,y,form,2)
      else                                           ! do logarithmic label
         xtext2=xy_ustrlen('0')
         xx=ilen*xtext2
         if(iside.eq.1)then                          ! number is on left axis
            xprint2=x-max(xx*.7,ilong*xtext2*.7)     ! if hardware text, xtext might not be correct, so make sure it fits
            xprint2=xprint2+0.6*xtext
         else                                        ! number is on right axis
            xprint2=x+xtext2                         ! if hardware text, xtext might not be correct, so make sure it fits
         endif
         yprint2=y+0.9*ytext                         ! go up for superscript
         call textsize(0.7*xtext,0.7*ytext)          ! set to a smaller character size for superscript
         call xy_juprint(xprint2,yprint2,form,2)
         xprint=xprint2-1.3*xtext                    ! position to print the base number
         call textsize(xtext,ytext)                  ! restore text size
         call xy_juprint(xprint,y,'10',2)
      endif
end subroutine priv_labely
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine xy_toa(val32,form,ilen,ierr,xxx,fmt,icount)
use M_journal, only : journal
use M_strings, only : value_to_string, crop
use M_time,    only : fmtdate, unix_to_date
implicit none

! ident_72="@(#)M_xyplot::xy_toa(3f): convert number to a nice string (1 to 20 characters) or explicit string for an axis label"

real,intent(in)                 :: val32   ! number to convert to a string
character(len=*),intent(out)    :: form    ! return string to hold ASCII representation of number
integer,intent(out)             :: ilen    ! number of characters in output string or 1 (for a blank string)
integer                         :: ierr    ! flag if error occurred while creating string representation of number
real,intent(in)                 :: xxx     ! xxx    is the "step" along the axis. This is so very small numbers
                                           !        close to zero are labeled as zero instead of some very small
                                           !        e-format number. Set it to zero if it does not apply.
character(len=*),intent(in)     :: fmt     !
                                           ! 'x'    use string from $x xy_array in calculator
                                           ! 'y'    use string from $y xy_array in calculator
                                           ! ' '    use value_to_string subroutine
                                           ! *%*    assume value is a Unix Epoch time value and use fmtdate to print the value
                                           ! other  user-specified 20 character format statement to write number with

integer                         :: itime(8)
integer,intent(in)              :: icount  ! if using $x and $y array, this is the subscript
integer                         :: ifail
integer,parameter               :: ixyc=50
character(len=255)              :: xc(ixyc), yc(ixyc), nc(ixyc),form2
real                            :: alabel
integer                         :: ios
integer                         :: isub
common/zzrayc/xc,yc,nc
save /zzrayc/

   form=' '
   if(fmt.eq.'x'.or.fmt.eq.'y')then   ! use $x string xy_array values
      if(icount.le.0)then
         isub=1
         call journal('sc','*xy_toa* string xy_array index too low=',icount)
         form2=' '
      elseif(icount.gt.ixyc)then
         isub=ixyc
         call journal('sc','*xy_toa* string xy_array index too high=',icount)
         form2=' '
      else
         isub=icount
         if(fmt.eq.'x')then
            form2=xc(isub)
         elseif(fmt.eq.'y')then
            form2=yc(isub)
         else
            form2=nc(isub)
         endif
      endif
      form=crop(form2)
      ilen=len_trim(form)
      ilen=max(1,ilen)                                ! prevent problems from blank strings
   elseif(index(fmt,'%').ne.0)then                    ! assume value is a Unix Epoch Time value
      call unix_to_date(dble(val32),itime,ifail)
      form=fmtdate(itime,fmt)
      ilen=len_trim(form)
   elseif(fmt.ne.' ')then                             ! user-specified format up to 20 characters
      write(form,fmt=fmt,iostat=ios)val32
      if(ios.ne.0)then
         call journal('*xy_toa* cannot use user-specified format'//trim(fmt))
      endif
      form2=form
      form=crop(form2)
      ilen=len_trim(form)
   else
      alabel=val32
      if(abs(val32).le.abs(xxx/1000.0))then
         form='0'
         ilen=1
         ierr=0
      else
         call value_to_string(alabel,form,ilen,ierr,fmt='(g20.7)',trimz=.true.)
         if(form(ilen:ilen).eq.'.')then
            form(ilen:ilen)=' '
            ilen=max(1,ilen-1)
         endif
      endif
   endif
end subroutine xy_toa
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine priv_findlongest(ilog,idivi,valsi,step,fmt,ilong,biggest)
use M_draw
implicit none

!     find longest numeric label along this axis and return number of characters and approximate length

character fmt*(*)
character cvalue*23
real          :: biggest
real          :: dumhh
integer       :: i45
integer       :: idivi
integer       :: ierr
integer       :: ilen
integer       :: ilog
integer       :: ilong
real          :: step
real          :: valsi
real          :: value
real          :: widthnum
real          :: xlong
real          :: ydown
real          :: ytall
real          :: yup

         ! ilong:   longest number of characters
         ! biggest: longest string using window units
         !
         ! assumes correctly set to proportional or not, font size, and textsize
             !  call fixedwidth(ifixednum)                        ! text is fixed space or not for xmin labels
             !  call font(fontN)
             !  call textsize(TXTWDM,TXTHIM)

      biggest=0
      ilong=1
      if(ilog.eq.-1)then
         do i45=0,idivi
            value = valsi + step * i45
            call xy_toa(value,cvalue,ilen,ierr,step,fmt,i45+1)
            ilong=max(ilong,ilen)
            !---------------------------------------------
            ! CONSIDER:
            !  faster to just count characters and multiply by text width to get approximate answer;
            !  or multiply number of characters by size of a '0' character for good guess
            !  instead of calling priv_justrlen.
            call priv_justrlen(cvalue(:ilen),xlong,ytall,ydown,yup)
            biggest=max(biggest,xlong)
            !---------------------------------------------
         enddo
         ilong=ilong+1                         ! try to stabilize position of axis, set using calculator LABELPLACES in set command
      else
         call getcharsize('0',widthnum,dumhh)  ! assuming numbers are all the same width
         biggest=3.0*widthnum
         ilong=3
      endif
end subroutine priv_findlongest
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine plot_page(xsmall,xlarge,ysmall,ylarge)
use M_draw
implicit none

! ident_73="@(#)M_xyplot::plot_page(3f): initialize plot page and set up common page-related values"

real,intent(in)    :: xsmall
real,intent(in)    :: xlarge
real,intent(in)    :: ysmall
real,intent(in)    :: ylarge

      call page(xsmall,xlarge,ysmall,ylarge)
      XMIN0Q2=xsmall  ! window area for entire display, leave alone
      XMAX0Q2=xlarge
      YMIN0Q2=ysmall
      YMAX0Q2=ylarge
      XMINQ2=xsmall  ! window area of current area drawing in
      XMAXQ2=xlarge
      YMINQ2=ysmall
      YMAXQ2=ylarge
end subroutine plot_page
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine priv_getpage(xsmall,xlarge,ysmall,ylarge)
implicit none

! ident_74="@(#)M_xyplot::priv_getpage(3fp):  return plot page size"

real,intent(out)    :: xsmall
real,intent(out)    :: xlarge
real,intent(out)    :: ysmall
real,intent(out)    :: ylarge

      xsmall=XMIN0Q2
      xlarge=XMAX0Q2
      ysmall=YMIN0Q2
      ylarge=YMAX0Q2
end subroutine priv_getpage
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine xy_setdatarange(x8)
implicit none

! ident_75="@(#)M_xyplot::xy_setdatarange(3f): set plot window extremes, set plot window extremes for dual axis"

real       :: x8(8)
integer    :: i10

      do i10=1,8
         valsq2(i10+3)=x8(i10)
      enddo
end subroutine xy_setdatarange
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_getdatarange(3f) - [M_xyplot] get plot window extremes, set plot window extremes for dual axis
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine plot_getdatarange(X8)
implicit none

! ident_76="@(#)M_xyplot::plot_getdatarange(3f):  get plot window extremes, set plot window extremes for dual axis"

real         :: x8(8)
integer      :: i10

      do i10=1,8
         x8(i10)=valsq2(i10+3)
      enddo
end subroutine plot_getdatarange
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine plot_setticper(TICPRX0,TICPRY0,TICPRX20,TICPRY20,TYPE)
implicit none

! ident_77="@(#)M_xyplot::plot_setticper(3f): set tic size"

character(len=*)        :: type
real                    :: ticprx0
real                    :: ticpry0
real                    :: ticprx20
real                    :: ticpry20
real                    :: ticprx
real                    :: ticprx2
real                    :: ticpry
real                    :: ticpry2
real                    :: tlong
real                    :: xmaxt
real                    :: xmint
real                    :: ymaxt
real                    :: ymint

!     scale tic as a percent of longest window side
!     TICPRX  major x tic length
!     TICPRY  major y tic length
!     TICPRX2 minor x tic length
!     TICPRY2 minor y tic length
!     so that no matter what the window size, a good attempt is made to select properly scaled values
!     should be called after plotting area is defined
      call plot_get_plot_area(xmint,xmaxt,ymint,ymaxt)
      tlong=max(xmaxt-xmint,ymaxt-ymint)
      if(type(1:3).eq.'set')then
         TICPRX=TICPRX0
         TICPRY=TICPRY0
         TICPRX2=TICPRX20
         TICPRY2=TICPRY20
         TICPRXQ2=TICPRX   ! last value set, used for resetting, initialized via common
         TICPRYQ2=TICPRY   ! last value set, used for resetting, initialized via common
         TICPRX2Q2=TICPRX2  ! last value set, used for resetting, initialized via common
         TICPRY2Q2=TICPRY2  ! last value set, used for resetting, initialized via common
      else  ! set to current percentages and recalculate tic lengths
         TICPRX=TICPRXQ2
         TICPRY=TICPRYQ2
         TICPRX2=TICPRX2Q2
         TICPRY2=TICPRY2Q2
      endif
      TICLNXQ2=TICPRX/100.0*tlong     ! linelen is length of tic
      TICLNYQ2=TICPRY/100.0*tlong     ! linelen is length of tic
      TICLNX2Q2=TICPRX2/100.0*tlong     ! linelen is length of tic for minor tic
      TICLNY2Q2=TICPRY2/100.0*tlong     ! linelen is length of tic for minor tic

      TEXTTICXQ2=max(TICLNXQ2,-TICLNX2Q2) ! distance of text from axis
      TEXTTICXQ2=max(TEXTTICXQ2,0.33/100.0*tlong) ! distance of text from axis

      TEXTTICYQ2=max(TICLNYQ2,-TICLNY2Q2) ! distance of text from axis
      TEXTTICYQ2=max(TEXTTICYQ2,0.33/100.0*tlong) ! distance of text from axis

      valsq2(2)=0.020*tlong  ! marker size

end subroutine plot_setticper
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine xy_getscale(iadd,xslope,xinter,yslope,yinter)
use M_draw
implicit none

! ident_78="@(#)M_xyplot::xy_getscale(3f): set up scaling between graphing area and user-scaled axis"

! iadd=0 uses left and bottom axis
real           :: left,right,bottom,top
integer        :: iadd
real           :: xslope
real           :: xinter
real           :: yslope
real           :: yinter
real           :: xbug
real           :: xmax0t
real           :: xmaxt
real           :: xmin0t
real           :: xmint
real           :: ybug
real           :: ymax0t
real           :: ymaxt
real           :: ymin0t
real           :: ymint
!-----------------------------------------------------------------------------------------------------------------------------------
   call getviewport(left, right, bottom, top)
   call plot_get_plot_area(xmint,xmaxt,ymint,ymaxt)
   call priv_getpage(xmin0t,xmax0t,ymin0t,ymax0t)
   xslope=(xmaxt-xmint)/(valsq2(iadd+5)-valsq2(iadd+4)) ! oldx=xslope*newx+xinter
   xinter=xmaxt-xslope*valsq2(iadd+5)
   yslope=(ymaxt-ymint)/(valsq2(iadd+7)-valsq2(iadd+6)) ! oldy=yslope*newy+yinter
   yinter=ymaxt-yslope*valsq2(iadd+7)
!-----------------------------------------------------------------------------------------------------------------------------------
!  call a window and viewport just so do not have to do own clipping
   call viewport(                                            &
  & left  +(xmint-xmin0t)/(xmax0t-xmin0t)*(right-left),      &
  & right -(xmax0t-xmaxt)/(xmax0t-xmin0t)*(right-left),      &
  & bottom+(ymint-ymin0t)/(ymax0t-ymin0t)*(top-bottom),      &
  & top -  (ymax0t-ymaxt)/(ymax0t-ymin0t)*(top-bottom))
!  to prevent clipping lines that are right on edge of window fudge a bit
   XBUG=(xmaxt-xmint)*.0003
   YBUG=(ymaxt-ymint)*.0003
   call ortho2(xmint-XBUG,xmaxt+XBUG,ymint-YBUG,ymaxt+YBUG)
end subroutine xy_getscale
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine plot_line(ipen,ivals,where,xar,yar)
implicit none

! ident_79="@(#)M_xyplot::plot_line(3f): buffer curve data and call xy_line"

integer,intent(in) :: ipen
integer,intent(in) :: ivals
real,intent(in)    :: xar(:),yar(:)
real               :: xbuf(size(xar)),ybuf(size(yar))
character*(*) where
   xbuf=xar
   ybuf=yar
   call xy_line(ipen,ivals,where,xbuf,ybuf)
end subroutine plot_line
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine plot_label(iwhich,label)
use M_journal, only : journal
implicit none

! ident_80="@(#)M_xyplot::plot_label(3f): set plot axis labels"

integer,intent(in)          :: iwhich
character(len=*),intent(in) :: label
   if(iwhich.ge.1.and.iwhich.le.4)then
     axlq2(iwhich)=label
   else
     call journal('sc','*plot_label* has bad subscript=',iwhich)
   endif
end subroutine plot_label
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine plot_resetplot()
implicit none

! ident_81="@(#)M_xyplot::plot_resetplot(3f): reset plot-related common values"

   call xy_resetpens()

   xlabel_fmtq2=' '
   xlabel2_fmtq2=' '
   ylabel_fmtq2=' '
   ylabel2_fmtq2=' '
   fontNq2='futura.l' ! font for numbers
   fontLq2='futura.m' ! font for axis labels

   plot_axis%grid_style(1)=0 !major x line  on
   plot_axis%grid_style(2)=0 !major y line  on
   plot_axis%grid_style(3)=1 !minor x tic   off
   plot_axis%grid_style(4)=1 !minor y tic   off

   orientlq2=90.0  ! angle to draw left y-axis label at; 90 and 270 are only reasonable values
   orientrq2=90.0  ! angle to draw right y-axis label at; 90 and 270 are only reasonable values

   plot_axis%xlogmode= -1     ! linear x-axis; other values are logarithmic
   plot_axis%ylogmode= -1     ! linear y-axis; other values are logarithmic

   lq2(logleq2)=.false.  ! flag to ignore leading values .le. 0 during plotting of logarithmic scales

! give some valid initial values to the plot range values or an initial interactive draw with create will not know what area to use
   valsq2(4)=0.0
   valsq2(5)=1000.0
   valsq2(6)=0.0
   valsq2(7)=1000.0
   valsq2(8)=0.0
   valsq2(9)=1000.0
   valsq2(10)=0.0
   valsq2(11)=1000.0

   aymulq2=0.0
   axmulq2=0.0

   ayconq2=0.0
   axconq2=0.0

   ixuniq2=0
   iyuniq2=0

   ! call plot_set_nice_range(xlow,xhigh,ylow,yhigh)
   call plot_set_nice_range(0.0,1000.0,0.0,1000.0)

   call plot_setticper(1.0,1.0,0.33,0.33,'set')

   axlq2(1)=' '    ! x-axis label
   axlq2(2)=' '    ! y-axis label
   axlq2(3)=' '    ! alternate y-axis label
   axlq2(4)=' '    ! alternate y-axis label

   ixdivq2(1)=5   ! number of major x tics
   iydivq2(2)=5   ! number of minor x tics

   ixdivq2(1)=5   ! number of major y tics
   iydivq2(2)=5   ! number of minor y tics

   ilabel_wq2=1    ! line thickness to draw software label text at
   imin_wq2=1      ! line thickness to draw software numeric values at

   ifixedlabq2=.false.    ! fixed-space label text or proportional
   ifixednumq2=.false.    ! fixed-space numeric text or proportional

   icrvsq2=1          ! number of curves specified on last plot command
   icrvs2q2=1         ! number of curves that go to left axis

   ixlabel_chq2=40
   ixmin_chq2=40

   plot_axis%background=-1          ! color of middle ground area

   deviceq2=' '        ! which device you are using

end subroutine plot_resetplot
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine plot_set_nice_range(xlow,xhigh,ylow,yhigh) ! USER ROUTINE
use M_draw
use M_math, only : scale1
implicit none

! ident_82="@(#)M_xyplot::plot_set_nice_range(3f): determine nice range for creating an axis"

real        :: X8(8)
real        :: xlow
real        :: xhigh
real        :: ylow
real        :: yhigh
real        :: xxdelta
real        :: xxdelta2
real        :: yydelta
real        :: yydelta2

   call scale1(xlow,xhigh,5,X8(1),X8(2),XXDELTA)
   call scale1(ylow,yhigh,5,X8(3),X8(4),YYDELTA)

   call scale1(xlow,xhigh,5,X8(5),X8(6),XXDELTA2)
   call scale1(ylow,yhigh,5,X8(7),X8(8),YYDELTA2)

   call xy_setdatarange(x8)

   ixdivq2(1)=(X8(2)-X8(1))/XXdelta+0.5   ! number of major x tics
   ixdivq2(2)=(X8(6)-X8(5))/XXdelta+0.5   ! number of minor x tics

   iydivq2(1)=(X8(4)-X8(3))/YYdelta+0.5   ! number of major y tics
   iydivq2(2)=(X8(8)-X8(7))/YYdelta+0.5   ! number of minor y tics

end subroutine plot_set_nice_range
!-----------------------------------------------------------------------------------------------------------------------------------
![][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine xy_line(ipen,ivals,ctype,xy_arrayx,xy_arrayy)
use M_draw
implicit none

! ident_83="@(#)M_xyplot::xy_line(3f): draw polyline using data in xy_arrayx xy_arrayy, using pen style ipen"

!===================================================================================================================================
integer                       :: iadd
integer,intent(in)            :: ipen   ! ipen=pen style (color, marker select, dashcode)
integer,intent(in)            :: ivals  ! ivals=number of points in the curve
character(len=*),intent(in)   :: ctype  ! toleft or toright = draw scaled to axis, toframe = draw scaled to overall frame
!     line is either drawn to overall page coordinates or to scaled axis area
!     data can be draw to left y axis scale or right y axis scale
!        CTYPE='toleft'
!        CTYPE='toright'
!        CTYPE='page' or 'toframe'
!        CTYPE='dash' ! just for drawing dash code samples from plot_marks(3f)

!     FLINT BUG: HAD TO CHANGE VARIABLE NAME FROM TYPE TO CTYPE OR FLINT WAS CONFUSED
real                          :: xy_arrayx(*)
real                          :: xy_arrayy(*)
!     CONSIDER DATA IN xy_arrayX and xy_arrayY DESTROYED !!!!
!===================================================================================================================================
! conversion xy_arrays used to distinguish between toframe and priv_toscale lines
real                :: con(7)
real,save           :: con2(7) = (/1.0,0.0,1.0,0.0,-1.0,-1.0,0.0/)
!===================================================================================================================================
   ipenq2=ipen
   call push()                                        ! save current color, font, ...
   call font(plot_ids(ipenq2)%marker_font)                 ! get font
   if(ctype.eq.'dash')then
      call color(plot_ids(0)%color)                          ! set color
      call xy_rasters(plot_ids(0)%width)                     ! set curve width
   else
      call color(plot_ids(ipen)%color)                       ! set color
      call xy_rasters(plot_ids(ipen)%width)                  ! set curve width
   endif
   if(ctype.eq.'toleft'.or.ctype.eq.'toright')then
      if(ctype.eq.'toleft')then
         iadd=0
      else
         iadd=4
      endif
      call xy_getscale(iadd,con(1),con(2),con(3),con(4))
      con(5)=plot_axis%xlogmode
      con(6)=plot_axis%ylogmode
      con(7)=iadd
      call priv_toscale(xy_arrayx,xy_arrayy,ivals,con,'graph')   ! draw the graph
   else
      call priv_toscale(xy_arrayx,xy_arrayy,ivals,con2,ctype)    ! draw the legend line
      call color(plot_ids(ipen)%color)                       ! set color. POP() should be doing this. Check M_DRAW routine
   endif
   call pop()
   call xy_rasters(1)                                    ! reset curve width, not yet in push/pop in M_DRAW 1.3
end subroutine xy_line
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_drawseg_using_pen(3fp) - [M_xyplot] draw a line using pen style ipen NOT to axis scale NOT setting color
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_drawseg_using_pen(ipen,x1,y1,x2,y2)
implicit none

! ident_84="@(#)M_xyplot::priv_drawseg_using_pen(3fp):  draw a line using pen style ipen NOT to axis scale NOT setting color"

!     conversion arrays used to distinguish between toframe and priv_toscale lines

integer         :: ipen
integer         :: ivals
real            :: x1
real            :: x2
real            :: y1
real            :: y2
real,save       :: con2(7)=[1.0,0.0,1.0,0.0,-1.0,-1.0,0.0]
real            :: x(2), y(2)
   x(1)=x1
   x(2)=x2
   y(1)=y1
   y(2)=y2
   ipenq2=ipen                            ! pen style (color,width,dash code)
   ivals=2                                ! number of points in the curve
   call priv_toscale(x,y,ivals,con2,'no') ! draw the line (push and pop color, font,...)
end subroutine priv_drawseg_using_pen
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot_setdash(3f) - [M_xyplot] allow user to display|alter|retrieve  dash codes by style number
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!   parms can be:
!!       o null: display current patterns
!!       o fetch: retrieve a stored pattern
!!       o scale: resize and retrieve patterns for drawing grid lines
!!                Make pattern a nice size so it repeats in intervals that
!!                make nice intersections when used as a grid
!!       o reset: set back to default values
!!       o something else: store a pattern
!!##EXAMPLE
!!
subroutine plot_setdash(parms,iwhich,patout,isize,ipatout)

!     1992 John S. Urban
use M_journal, only : journal
use M_strings, only : listout
implicit none

! ident_85="@(#)M_xyplot::plot_setdash(3f):  allow user to display|alter|retrieve  dash codes by style number"

character(len=*),intent(in) :: parms   ! action keyword

integer,intent(in)  :: iwhich         ! which pattern number to work on. If iwhich is 0, a solid line pattern is returned
integer,intent(in)  :: isize          ! size of patout
real,intent(out)    :: patout(isize)  ! xy_array to return a pattern, of size isize (assumed big enough to hold a pattern!!!)
integer,intent(out) :: ipatout        ! number of elements in pattern
integer          :: i10, i20, i30, i33, i40, i44, i444, i50, i70, i72, i90, i95, i97
integer          :: iend
integer          :: ier
integer          :: igot
integer          :: ii
integer          :: in
integer          :: iwhich2
integer          :: jj
integer          :: lennum
integer          :: lnum
real             :: overall
real             :: ratio
real             :: realx
real             :: realy
real             :: rleft
real             :: sum
real             :: xmax0t
real             :: xmaxt
real             :: xmin0t
real             :: xmint
real             :: ymax0dum
real             :: ymaxt
real             :: ymin0dum
real             :: ymint
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=255)          :: temp1
integer,parameter           :: IDSHS=20 ! number of normal pen styles
integer,parameter           :: IPATS=12 ! maximum number of codes in a pattern
real,save                   :: pat(-4:IDSHS,0:IPATS)
real,save                   :: patdef(-4:IDSHS,0:IPATS)
real                        :: rcurv1(IDSHS), rpat(IPATS)
integer                     :: icurv(IDSHS)
integer,save                :: icalls=0
integer                     :: ierr, ierr2
integer                     :: ipens(4)
data (patdef(-4,jj),jj=0,IPATS)/1.0,IPATS*0.0/
data (patdef(-3,jj),jj=0,IPATS)/1.0,IPATS*0.0/
data (patdef(-2,jj),jj=0,IPATS)/1.0,IPATS*0.0/
data (patdef(-1,jj),jj=0,IPATS)/1.0,IPATS*0.0/
data (patdef( 0,jj),jj=0,IPATS)/1.0,IPATS*0.0/
data (patdef( 1,jj),jj=0,IPATS)/2.0,1.2,1.2,10*0.0/
data (patdef( 2,jj),jj=0,IPATS)/2.0,0.6,0.6,10*0.0/
data (patdef( 3,jj),jj=0,IPATS)/4.0,3.6,0.6,0.6,0.6,8*0.0/
data (patdef( 4,jj),jj=0,IPATS)/4.0,2.4,1.2,1.2,1.2,8*0.0/
data (patdef( 5,jj),jj=0,IPATS)/2.0,2.4,1.2,10*0.0/
data (patdef( 6,jj),jj=0,IPATS)/4.0,2.4,0.6,0.0,0.6,8*0.0/
data (patdef( 7,jj),jj=0,IPATS)/4.0,1.2,0.6,0.0,0.6,8*0.0/
data (patdef( 8,jj),jj=0,IPATS)/4.0,0.6,1.2,0.0,1.2,8*0.0/
data (patdef( 9,jj),jj=0,IPATS)/2.0,0.0,1.2,10*0.0/
data (patdef(10,jj),jj=0,IPATS)/2.0,1.0,1.0,10*0.0/
data (patdef(11,jj),jj=0,IPATS)/2.0,1.1,1.0,10*0.0/
data (patdef(12,jj),jj=0,IPATS)/2.0,1.2,2.0,10*0.0/
data (patdef(13,jj),jj=0,IPATS)/2.0,1.3,1.0,10*0.0/
data (patdef(14,jj),jj=0,IPATS)/2.0,1.4,1.0,10*0.0/
data (patdef(15,jj),jj=0,IPATS)/2.0,1.5,1.0,10*0.0/
data (patdef(16,jj),jj=0,IPATS)/2.0,1.6,1.0,10*0.0/
data (patdef(17,jj),jj=0,IPATS)/2.0,1.7,1.0,10*0.0/
data (patdef(18,jj),jj=0,IPATS)/2.0,1.8,1.0,10*0.0/
data (patdef(19,jj),jj=0,IPATS)/2.0,1.9,1.0,10*0.0/
data (patdef(20,jj),jj=0,IPATS)/2.0,2.0,1.0,10*0.0/
!-----------------------------------------------------------------------------------------------------------------------------------
   if(parms.eq.'reset')then
      icalls=0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(icalls.eq.0)then ! initialize with default values
      do i33=-4,IDSHS
         do i44=0,IPATS
            pat(i33,i44)=patdef(i33,i44)
         enddo
      enddo
      icalls=1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call priv_getpage(xmin0t,xmax0t,ymin0dum,ymax0dum)
   !list non-default legend labels
!-----------------------------------------------------------------------------------------------------------------------------------
   if(parms.eq.' ')then ! if no parameters are present or filename present
      call journal('s','#-------------------------------------------------')
      do i10=1,IDSHS
         if(int(pat(i10,0)).le.0)then
            write(temp1,'("setdash ",i2," -p ",a,f6.1,1x," # solid")')i10,pat(i10,1)
         else
            in=max(1,int(pat(i10,0)))
            write(temp1,'("setdash ",i2," -p ",20f6.1:,1x)')i10,(pat(i10,ii),ii=1,in)
         endif
         call journal(temp1)
      enddo
      call journal('s','#-------------------------------------------------')
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(parms.eq.'fetch')then      ! fetch a stored pattern
      iwhich2=iwhich
      if(iwhich2.gt.0)then           ! a normal line
         iwhich2=mod(iwhich,IDSHS+1)
      elseif(iwhich2.eq.0)then
         patout(1)=0.0               ! solid line pattern
         ipatout=1                   ! number of values in pattern
         return
      elseif(iwhich2.gt.-4)then      ! scaled grid dash patterns
      elseif(iwhich2.lt.-4)then      ! user error
         iwhich2=abs(iwhich2)
         iwhich2=mod(iwhich,IDSHS+1)
      endif
      ipatout=int(pat(iwhich2,0)) ! number of values in pattern
      if(ipatout.le.0)then
         call journal('*setdash* no numbers in pattern -internal')
         return
      endif
      do i20=1,ipatout
         if(pat(iwhich2,i20).ge.0)then ! convert lengths from percents to real lengths
            patout(i20)=pat(iwhich2,i20)*(xmax0t-xmin0t)/100.0
         else          ! leave negative numbers alone, they are a flag to ask for a marker
            patout(i20)=pat(iwhich2,i20)
         endif
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
!  make nice grid patterns that fit multiple times across a plot (must be redone for every plot)
   elseif(parms.eq.'scale')then      ! define pens -1,-2,-3,-4 for drawing grid with
      call plot_get_plot_area(xmint,xmaxt,ymint,ymaxt)
      ipens(1)=plot_ids(-1)%dashcode  ! minor x pattern pen number
      ipens(2)=plot_ids(-2)%dashcode  ! major x pattern pen number
      ipens(3)=plot_ids(-1)%dashcode  ! major y pattern pen number
      ipens(4)=plot_ids(-2)%dashcode  ! major y pattern pen number
      realx=xmaxt-xmint
      realy=ymaxt-ymint
!     assume that when call with scale, patout has number of major and minor divisions in it
!     probably should put those values into an xy_array in common
      do i90=1,4
         sum=0.0  ! calculate length of pattern

         if(ipens(i90).gt.IDSHS)then
            ipens(i90)=mod(iwhich,IDSHS+1)
         elseif(ipens(i90).lt.-4)then      ! user error
            ipens(i90)=abs(ipens(i90))
            ipens(i90)=mod(iwhich,IDSHS+1)
         endif

         do i70=1, max(1,int(pat(ipens(i90),0)))
            sum=sum+max(0.0,pat(ipens(i90),i70)) ! make sure to ignore -int values, which denote a marker
         enddo
         if(sum.gt.0.and.patout(i90).gt.0)then
            sum=sum/100.00*(xmax0t-xmin0t) ! turn into length that fetch will produce (convert to percentage of x range)
            if(i90.eq.3)then            ! divide overall length by number of major x divisions
               overall=realy/patout(3)
            elseif(i90.eq.4)then        ! divide overall length by number of minor x divisions
               overall=realy/patout(4)/patout(3)
            elseif(i90.eq.1)then        ! divide overall length by number of major y divisions
               overall=realx/patout(1)
            elseif(i90.eq.2)then        ! divide overall length by number of minor y divisions
               overall=realx/patout(2)/patout(1)
            endif
            if(overall.lt.sum)then  ! scale it down to force it to fit at least once
              ratio=overall/sum
            else                    ! scale it up to fit a whole number of times
              rleft=int(overall/sum)*sum
              ratio=overall/rleft
            endif
            pat(-i90,0)=pat(ipens(i90),0)
            do i72=1,IPATS  ! copy the adjusted pattern into the grid pattern holding space
               pat(-i90,i72)=pat(ipens(i90),i72)*ratio
            enddo
         else  ! make a solid line pattern
            pat(-i90,0)=1.0
            pat(-i90,1)=0.0
         endif
      enddo
      if(ipens(1).eq.ipens(2))then ! if same pattern on major and minor grid lines, synchronize them
         do i95=1,IPATS
            pat(-1,i95)=pat(-2,i95)
         enddo
      endif
      if(ipens(3).eq.ipens(4))then ! if same pattern on major and minor grid lines, synchronize them
         do i97=1,IPATS
            pat(-3,i97)=pat(-4,i97)
         enddo
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
   else
! set a pattern
      call xy_retrv2('setdash_oo',temp1,lennum,ier) ! get list of pattern numbers to change
      if(lennum.eq.0)then
         call journal('*setdash* changing all curve numbers')
         iend=IDSHS
         do i40=1,IDSHS
            icurv(i40)=i40
         enddo
      else
        ierr=0
        call strgar2(fetch('setdash_oo'),IDSHS,rcurv1,igot,' ',ierr)
        if(ierr.ne.0.or.igot.le.0)then
         call journal('*setdash* stopped: undecipherable id number(s)')
         return
        endif
        call listout(int(rcurv1(:igot)),icurv(:IDSHS),igot,ierr2) ! get up to IDSHS curves
        iend=igot
      endif

      rpat(1)=0.0 ! in case no numbers in pattern
      call strgar2(fetch('setdash_p'),IPATS,rpat,igot,' ',ierr)

      do i30=1,iend
         ! get pattern number
         lnum=min(max(1,icurv(i30)),IDSHS) ! get a pattern number to replace, should probably yell about out-of-range numbers
         if(igot.eq.0)then
            !call journal('sc',"*setdash* restore pen ",lnum)
            do i444=0,IPATS                       ! restore default dashcode
               pat(lnum,i444)=patdef(lnum,i444)
            enddo
         else
            pat(lnum,0)=min(igot,IPATS)           ! store number of numbers in this pattern
            do i50=1,min(igot,IPATS)
               pat(lnum,i50)=rpat(i50)            ! store pattern numbers
            enddo
         endif
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine plot_setdash
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_resetpens(3f) - [M_xyplot] reset pens
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_resetpens()
implicit none

real         :: dumout(1)
integer      :: i10, i15
integer      :: idumin1
integer      :: idumout2
   do i10=1,ICS2Q

      ! set colors to basic primaries except skip yellow and background
      plot_ids(i10)%color=mod(i10-1,6)+1                ! line colors 1 to 6
      plot_ids(i10)%fill_style=0                        ! line fill style
      if(plot_ids(i10)%color.eq.3)plot_ids(i10)%color=7 ! if color is 3(yellow) switch to 7
      plot_ids(i10)%width=4                             ! line width
      !plot_ids(i10)%marker=mod(i10-1,20)+1             ! line marker (all geometric by default)
      plot_ids(i10)%marker=i10                          ! line marker (all geometric by default)
      plot_ids(i10)%dashcode=i10-1                      ! flag for dash codes
      plot_ids(i10)%legend=0                            ! turn legend line on
      plot_ids(i10)%marker_frequency=0                  ! frequency of marker
      plot_ids(i10)%marker_size=2.0                     ! marker size
   enddo
   do i15=-2,0
      plot_ids(i15)%color=7                             ! line color
      plot_ids(i15)%fill_style=0                        ! fill style
      plot_ids(i15)%marker=1                            ! line marker
      plot_ids(i15)%dashcode=9                          ! flag for a dashed line
      plot_ids(i15)%legend=0              ! turn legend line on (not applicable to these pens, but used for special dual axis use
      plot_ids(i15)%marker_frequency=0                  ! frequency of marker
      plot_ids(i15)%marker_size=2.0                     ! marker size
   enddo

   plot_ids( 0)%width=5                                 ! plot outline line width
   plot_ids(-1)%width=4                                 ! major grid line width
   plot_ids(-2)%width=1                                 ! minor grid line width
   plot_ids( 0)%dashcode=0                              ! box around plot
   plot_ids(-2)%dashcode=9                              ! box around plot (SOME HP-UX BUG? NEEDS SET HERE)
   plot_ids( 0)%marker_size=10                          ! geometric marker size for plot_setmark
   plot_ids(-1)%marker_size=1.5                         ! marker size for markers in dash codes
   plot_ids(-2)%marker_size=2.0                         !

   plot_ids(:)%marker_font='futura.l'

   xlabel_fmtq2=' '
   xlabel2_fmtq2=' '
   ylabel_fmtq2=' '
   ylabel2_fmtq2=' '

   call plot_setdash('reset',idumin1,dumout,1,idumout2)
end subroutine xy_resetpens
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_jurang(3f) - [M_xyplot] return nice max and min values for the axis, and nice axis grid spacings
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!    adjust the scaling according to user limits if specified
!!
!!    caution: changes xlow,ylow,xhigh,yhigh
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_jurang(iside,xlow,ylow,xhigh,yhigh,      &
& xtreme,ytreme,ideltax,ideltay,RANGE,ilogx,ilogy,     &
& lllogx,lllogy,lllxact,                               &
& ixdiv,ixtic,iydiv,iytic,                             &
& xlog_type,ylog_type)

use M_journal, only : journal
use M_math, only : scale1
implicit none

! ident_86="@(#)M_xyplot::xy_jurang(3f): return nice max and min values for the axis, and nice axis grid spacings"


integer,intent(in)           :: iside
real,intent(inout)           :: xlow, ylow, xhigh, yhigh
real,intent(inout)           :: xtreme(2),ytreme(2)
integer,intent(inout)        :: ideltax(2),ideltay(2)
logical,intent(in)           :: lllogx,lllogy
logical,intent(in)           :: lllxact
real,intent(in)              :: RANGE(*)
integer,intent(inout)        :: ilogx, ilogy
integer,intent(in)           :: ixdiv, ixtic, iydiv, iytic
character(len=*),intent(in)  :: xlog_type, ylog_type

integer                      :: ichange(4)
real                         :: dum1
real                         :: dum2
integer                      :: ilog
real                         :: xdist
real                         :: xminor
real                         :: ydist
real                         :: yminor
!-----------------------------------------------------------------------------------------------------------------------------------
      call priv_setrng(RANGE(1),xlow,ichange(1)) ! adjust the scaling according to user limits if specified
      call priv_setrng(RANGE(2),xhigh,ichange(3))
      if(iside.eq.1)then
         call priv_setrng(RANGE(3),ylow,ichange(2))
         call priv_setrng(RANGE(4),yhigh,ichange(4))
      else
         call priv_setrng(RANGE(8),yhigh,ichange(4))
         call priv_setrng(RANGE(7),ylow,ichange(2))
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     avoid bug where plot fails if zero range in abscissa or ordinate
      call priv_hilow(xlow,xhigh)
      call priv_hilow(ylow,yhigh)
!-----------------------------------------------------------------------------------------------------------------------------------
!     decide on nice values for axis ranges
!-----------------------------------------------------------------------------------------------------------------------------------
!     get nice values for x values
      if (lllogx.and.xlow.gt.0)then     ! if desire logarithmic x and no values .le. 0
       call priv_logrng(xlog_type,ixtic,ilog,xlow,xhigh,ixdiv,xtreme,ideltax)
       ilogx=ilog
      else                              ! find nice round values for linear scales
         ilogx=-1                       ! flag that linear is default
         if(lllogx)then
            call journal('*plot* xlog ignored: minimum x <= 0')
         endif
         call scale1(xlow,xhigh,ixdiv,   xtreme(1),xtreme(2),xdist)
         ideltax(1)=(xtreme(2)-xtreme(1))/xdist+0.5
!        find a nice number of minor tics
         call scale1(0.0,abs(xdist),ixtic,dum1,dum2,xminor)
         ideltax(2)=xdist/xminor+0.5
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!     get nice values for y values
!     CHECK IF RETURNED VALUES CAN HAVE LOG TAKEN OF THEM
      if (lllogy.and.ylow.gt.0)then
       call priv_logrng(ylog_type,iytic,ilog,ylow,yhigh,iydiv,ytreme,ideltay)
          ilogy=max(ilog,ilogy) ! assuming ilogy initialized to -1; other values are 0 and 1; then makes sure
          ! ilogy is not set back to a log scale for iside=2 if it was not permitted for the left scale
      else                              ! find nice round values for linear scales
         ilogy=-1                      ! flag that linear is default
         if(lllogy)then
            call journal('*plot* ylog ignored: minimum y <= 0')
         endif
         call scale1(ylow,yhigh,iydiv,ytreme(1),ytreme(2),ydist) ! find a nice number of major tics
         ideltay(1)=(ytreme(2)-ytreme(1))/ydist+0.5                ! a nice number of major tics
         call scale1(0.0,abs(ydist),iytic,dum1,dum2,yminor)           ! find a nice number of minor tics
         ideltay(2)=ydist/yminor+0.5                               ! a nice number of minor tics
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(lllxact)then ! exact mode is on, so ignore most of these calculations if user specified some specific values

         if(ichange(1).eq.1)then
            xtreme(1)=xlow                             ! if a user-specified extreme was specified, use it exactly
            if(ilogx.ne.-1)xtreme(1)=log10(xtreme(1))  !
         endif
         if(ichange(2).eq.1)then
            ytreme(1)=ylow
            if(ilogy.ne.-1)ytreme(1)=log10(ytreme(1))
         endif
         if(ichange(3).eq.1)then
            xtreme(2)=xhigh
            if(ilogx.ne.-1)xtreme(2)=log10(xtreme(2))
         endif
         if(ichange(4).eq.1)then
            ytreme(2)=yhigh
            if(ilogy.ne.-1)ytreme(2)=log10(ytreme(2))
         endif

         if(ichange(1).eq.1.or.ichange(3).eq.1)then   ! if user specified either end of range, use tic spacings from grid command
            ideltax(1)=ixdiv                          ! use number of divisions off of grid command
            if(ilogx.eq.-1)ideltax(2)=ixtic           ! if linear scales, use number of minor tics from grid command
         endif

         if(ichange(2).eq.1.or.ichange(4).eq.1)then   ! if user specified either end of range, use tic spacings from grid command
            ideltay(1)=iydiv                          ! use number of divisions off of grid command
            if(ilogy.eq.-1)ideltay(2)=iytic           ! if linear scales, use number of minor tics from grid command
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine xy_jurang
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_con_x(3f) - [M_xyplot] scale an axis value to the overall window/viewport
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
function xy_con_x(xvalue0) result(xvalue)
use M_journal, only : journal
implicit none

! ident_87="@(#)M_xyplot::xy_con_x(3f): scale an axis value to the overall window/viewport"

real     :: xvalue0
real     :: xvalue
   xvalue=xvalue0
   if(CONQ(5).ge.0)then
      if(xvalue.le.0)then
         call journal('sc','*xy_con_x* ERROR: cannot take log of ',xvalue)
         xvalue=0.0
      else
         xvalue=log10(xvalue)             ! take the log of the user curve values
      endif
   endif
   xvalue=CONQ(1)*(xvalue)+CONQ(2)        ! convert the values to the coordinate system of the graph area
end function xy_con_x
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_con_y(3f) - [M_xyplot] scale an axis value to the overall window/viewport
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
function xy_con_y(yvalue0) result(yvalue)
use M_journal, only : journal
implicit none

! ident_88="@(#)M_xyplot::xy_con_y(3f): scale an axis value to the overall window/viewport"

real     :: yvalue0
real     :: yvalue
   yvalue=yvalue0
   if(CONQ(6).ge.0)then
      if(yvalue.le.0)then
         call journal('sc','*xy_con_y* ERROR: cannot take log of ',yvalue)
         yvalue=0.0
      else
         yvalue=log10(yvalue)  ! take the log of the user curve values
      endif
   endif
   yvalue=CONQ(3)*(yvalue)+CONQ(4)
end function xy_con_y
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_setcnv(3f) - [M_xyplot] set up conversion table for xy_con_x and xy_con_y functions
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_setcnv(ilogx,ilogy)
use M_draw, only : pop, push
implicit none

! ident_89="@(#)M_xyplot::xy_setcnv(3f): set up conversion table for xy_con_x and xy_con_y functions"

integer,intent(in) :: ilogx
integer,intent(in) :: ilogy
integer            :: iadd

   iadd=0 ! draw to left scale
   call push()
   call xy_getscale(iadd,CONQ(1),CONQ(2),CONQ(3),CONQ(4))
   call pop()
   CONQ(5)=ilogx
   CONQ(6)=ilogy
end subroutine xy_setcnv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_jugetwn(3f) - [M_xyplot] query current window ( appropriate for 2-d only)
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_jugetwn(xsmall,xlarge,ysmall,ylarge)
!-----------------------------------------------------------------------------------------------------------------------------------
use M_draw
implicit none

! ident_90="@(#)M_xyplot::xy_jugetwn(3f): query current window ( appropriate for 2-d only)"

real,intent(out) :: xsmall
real,intent(out) :: xlarge
real,intent(out) :: ysmall
real,intent(out) :: ylarge
real             :: xhold, yhold, zhold
real             :: rleft, right, bottom, top
!-----------------------------------------------------------------------------------------------------------------------------------
!     anything you can set you should be able to query.
!     BUG: current position not updated by a smove2 unless draw something

      call getgp(xhold,yhold,zhold)              ! store current world position

      call getviewport(rleft,right,bottom,top)   ! get current viewport
      call smove2(rleft,bottom)                  ! move to lower left of viewport using returned screen coordinates
      call sdraw2(rleft,bottom)                  ! bug
      call getgp2(xsmall,ysmall)                 ! query position in world coordinates
      call smove2(right,top)                     ! move to upper right of viewport using screen coordinates
      call sdraw2(right,top)                     ! bug
      call getgp2(xlarge,ylarge)                 ! query position in world coordinates

      call smove2(rleft,bottom)
      call sdraw2(rleft,top)
      call sdraw2(right,top)
      call sdraw2(right,bottom)
      call sdraw2(rleft,bottom)

      call move(xhold,yhold,zhold)               ! restore original current position

!      write(*,*)'rleft,bottom=',rleft,bottom
!      write(*,*)'xsmall,ysmall=',xsmall,ysmall
!      write(*,*)'right,top=',right,top
!      write(*,*)'xlarge,ylarge=',xlarge,ylarge
end subroutine xy_jugetwn
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_zoom(3f) - [M_xyplot] given current four numbers defining a box; alter them by indicated selections of an old and new box
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_zoom(xmin,ymin,xmax,ymax,iend)
use M_journal,  only : journal
use M_draw
use M_drawplus, only : rdbox
use M_drawplus, only : plain_rect
implicit none

! ident_91="@(#)M_xyplot::xy_zoom(3f):  given current four numbers defining a box"

real             :: high1
real             :: high2
integer          :: iend
integer          :: ikey
real             :: rangenew
real             :: width1
real             :: width2
real             :: x1
real             :: x2
real             :: x3
real             :: x4
real             :: xback
real             :: xdel
real             :: xecho
real             :: xm1
real             :: xm2
real             :: xmax
real             :: xmax2
real             :: xmax3
real             :: xmin
real             :: xmin2
real             :: xmin3
real             :: xratio
real             :: y1
real             :: y2
real             :: y3
real             :: y4
real             :: yback
real             :: ydel
real             :: yecho
real             :: ym1
real             :: ym2
real             :: ymax
real             :: ymax2
real             :: ymax3
real             :: ymin
real             :: ymin2
real             :: ymin3
real             :: yrangenew
real             :: yratio
!     given a box defined by window xmin,ymin,xmax,ymax
!     select two new boxes x1,y1,x2,y2 and x3,y3,x4,y4
!     now if box 1,2 is zoomed and translated to fit into box 3,4 what are the
!     coordinates of the min,max corner points now?
!     if mouse 1 is used to select the first box assume the second box is min,max
!     if mouse 2 is used to select the first box select a second box
!     if mouse 3 is used quit zoom loop
!     iend=0 OK
!     iend=1 quit zoom loop
!     iend=3 request to reset xmin,ymin,xmax,ymax
      iend=1                                   ! initialize return status code to quit zoom loop
      call rdbox(x1,y1,x2,y2,ikey)             ! choose first box on press and release
      if(x1.eq.x2.and.y1.eq.y2.and.ikey.le.2)then
         goto 999         ! requesting to end loop
      endif
      call plain_rect(x1,y1,x2,y2)
      call vflush()
      if(ikey.eq.1)then                         ! if used key one, assume just zooming in
         x3=xmin
         y3=ymin
         y4=ymax
         x4=xmax
      elseif(ikey.gt.2)then                     ! reset back to original window
         iend=3                                 ! flag that want reset
         goto 999
      else
         call rdbox(x3,y3,x4,y4,ikey)              ! choose second box on press and release; note ignoring key
         if(x3.eq.x4.and.y3.eq.y4)goto 999         ! requesting to end loop
      endif
      call plain_rect(x3,y3,x4,y4)
      call vflush()
      xecho=x4
      yecho=y4
!-----------------------------------------------------------------------------------------------------------------------------------
!     simplistic approach to deciding when to quit. better method should be developed
      width1=abs(x2-x1)
      high1=abs(y2-y1)
      width2=abs(x4-x3)
      high2=abs(y4-y3)
      if(min(width1,width2,high1,high2).le.1.0/500.0)then  ! avoid division by zero
           call journal('*zoom* boxes must have area')
           goto 999
      endif
      xratio=width2/width1
      yratio=high2/high1
!-----------------------------------------------------------------------------------------------------------------------------------
!     find coordinates of middle point of box 1 and box 2
      xm1=(x1+x2)/2.0
      ym1=(y1+y2)/2.0
      xm2=(x3+x4)/2.0
      ym2=(y3+y4)/2.0
!-----------------------------------------------------------------------------------------------------------------------------------
!     move box1 so its center is in center of box2
      xdel=xm2-xm1
      xmin2=xmin-xdel
      xmax2=xmax-xdel
      ydel=ym2-ym1
      ymin2=ymin-ydel
      ymax2=ymax-ydel
!-----------------------------------------------------------------------------------------------------------------------------------
!     the new xmin,xmax,ymin,ymax represent a new translated window where
!     the middle of the old box is now where the middle of the new box was.
!     since no scaling has occurred yet, the x-range and y-range are still
!     the same.
!-----------------------------------------------------------------------------------------------------------------------------------
!     Now, to keep the center point where it is but stretch the old box
!     to the size of the new box.
!-----------------------------------------------------------------------------------------------------------------------------------
!     The coordinates of point <xm1,ym1> stay the same,
!     The overall distance of the stretched xscale should be
      rangenew=(xmax2-xmin2)/xratio
!     Since the old middle point was placed where the new middle point
!     was, it is the same percentage from the left edge. That means
      xback=rangenew*(xm2-xmin)/(xmax-xmin)
      xmin3=xm1-xback
      xmax3=xmin3+rangenew
!-----------------------------------------------------------------------------------------------------------------------------------
      yrangenew=(ymax2-ymin2)/yratio    ! same thing for the y scaling
      yback=yrangenew*(ym2-ymin)/(ymax-ymin)
      ymin3=ym1-yback
      ymax3=ymin3+yrangenew
!-----------------------------------------------------------------------------------------------------------------------------------
      xmax=xmax3   ! store the new plot range to use and flag they are usable
      xmin=xmin3
      ymax=ymax3
      ymin=ymin3
      iend=0
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue
      call vflush()
end subroutine xy_zoom
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_convert(3f) - [M_xyplot] do linear conversion for XY plot routines
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!   xy_convert(slope,intercept,x,n) # A linear conversion
!!
!!    real       :: slope
!!    real       :: intercept
!!    real       :: x(*)
!!    integer    :: n
!!
!!##DESCRIPTION
!!    used to do a fast conversion of an entire set of data
!!    instead of reparsing the text over and over with the
!!    math command. A linear conversion is very common for
!!    such operations as unit conversion.
!!
!!    Allows +-*/  operations on a dataset after
!!    operations have been performed. Does a conversion of the form
!!
!!           y=mx+b
!!
!!        where
!!
!!           the slope is m
!!           the y-intercept is b
!!
!!        if the intercept is left off, it defaults to a value of 0.
!!
!!##OPTIONS
!!##EXAMPLE
!!
!!  Showing usage from ush(1):
!!
!!    math c[4] -o xy_convert(1,40) xy_convert(9/5,-40)
!!    # equivalent to but faster than
!!    # math  (c[4]+40)*9/5-40 # xy_convert Centigrade to Fahrenheit
subroutine xy_convert(a,b,x,n)
!     1993 John S. Urban
use M_journal, only : journal
implicit none

! ident_92="@(#)M_xyplot::xy_convert(3f): do a linear conversion on an xy_array of the form y=ax+b"

real       :: x(*)
real       :: a
real       :: b
integer    :: n
integer    :: i10, i20
   if(n.le.0)then
      call journal('*xy_convert* number of points .le. 0')
      return
   endif
   if(a.ne.1)then
      do i10=1,n
         x(i10)=x(i10)*a
      enddo
   endif
   if(b.ne.0)then
      do i20=1,n
         x(i20)=x(i20)+b
      enddo
   endif
end subroutine xy_convert
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_hilow(3fp) - [M_xyplot] avoid bug where plot fails if zero range in abscissa or ordinate
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_hilow(rmin,rmax)
implicit none

! ident_93="@(#)M_xyplot::priv_hilow(3fp): avoid bug where plot fails if zero range in abscissa or ordinate"

real :: rmin
real :: rmax
real :: rdiff
!     might want to center curve, don't want to force a negative or zero value if in logarithmic mode, though
      if(rmin.eq.rmax)then
        rdiff=ceiling(max(1.0,rmin/10**6)) ! sometimes a-1 = a on hp if number is big enough, because of roundoff to n digits
        if(rmin.le.0)then      ! could not be logarithmic
           rmin=rmin-rdiff
           rmax=rmax+rdiff
        elseif(rmin.gt.1)then  ! won't put 0 or negative in range so ok to take log of resulting value
           rmin=rmin-rdiff
           rmax=rmax+rdiff
        else                   ! in range of 1 to 0, so half it
           rmin=rmin/2.0
           rmax=rmax+rmax/2.0
        endif
      endif
end subroutine priv_hilow
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_jubox(3fp) - [M_xyplot] draw a filled box around current scaled plot area
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_jubox(imidl,XMIN,YMIN,XMAX,YMAX)
use M_draw
use M_drawplus, only : plain_rect
implicit none

! ident_94="@(#)M_xyplot::priv_jubox(3fp): draw a filled box around current scaled plot area"

integer,intent(in) :: imidl
real,intent(in) :: XMIN, YMIN, XMAX, YMAX
   if(imidl.ge.0)then    ! flag to not draw the filled box
      call color(imidl)
      call polyfill(.true.)
      call makepoly()
      call plain_rect(XMIN,YMIN,XMAX,YMAX)
      call closepoly()
   endif
end subroutine priv_jubox
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_jugrid(3fp) - [M_xyplot] draw linear and logarithmic grids after actual axis have been drawn
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_jugrid(ixdiv,iydiv,igrid,vals,XMIN,XMAX,YMIN,YMAX,     &
     &idcol,idwid,ilogx,ilogy,TICLNY,TICLNX,TICLNY2,TICLNX2,            &
     &aymul,aycon,icrvs2,icrvs)

use M_draw
use M_drawplus, only : plain_rect
implicit none

! ident_95="@(#)M_xyplot::priv_jugrid(3fp): draw linear and logarithmic grids after actual axis have been drawn"

!     extracted this so can put colored box behind grid and come up with
!     nice tic plot_marks so tic lines cross with an x at grid points
!     ixdiv and iydiv are number of major and minor axis divisions; should make sure ixdiv and iydiv values are reasonable
!     igrid is an integer xy_array of flags designating styles: 0 - grid, 1 - tic, 2 - plain, for major x, major y, minor x, minor y

!     since tic length can now be controlled have to redraw major tics that might be inside
!     maybe need to lengths for each tic, in and out !!!!!!!!!!
integer ixdiv(4), iydiv(4)
!
!     igrid is an integer xy_array of flags designating styles:
!     0 - grid, 1 - tic, 2 - plain
!     igrid(1) -major x
!     igrid(2) -major y
!     igrid(3) -minor x
!     igrid(4) -minor y
real pat(4)
real vals(*)
integer igrid(*)
integer idcol(-2:*)
integer idwid(-2:*)
real            :: aa
real            :: actor
real            :: aleft
real            :: aleft10
real            :: anow
real            :: aright
real            :: astep
real            :: aycon
real            :: aymul
real            :: bb
real            :: conx
real            :: cony
integer         :: i10
integer         :: i12
integer         :: i15
integer         :: i20
integer         :: i22
integer         :: i25
integer         :: i30
integer         :: i35
integer         :: i50
integer         :: i52
integer         :: i55
integer         :: i60
integer         :: icrvs
integer         :: icrvs2
integer         :: idum
integer         :: ilogx
integer         :: ilogy
integer         :: ix1
integer         :: ix2
integer         :: iy1
integer         :: iy2
real            :: range1
real            :: range2
real            :: rnow
real            :: shift
real            :: sx
real            :: sx2
real            :: sy
real            :: sy2
real            :: ticlnx
real            :: ticlnx2
real            :: ticlny
real            :: ticlny2
real            :: x
real            :: xmax
real            :: xmaxend1
real            :: xmaxend2
real            :: xmin
real            :: xstep
real            :: xstep2
real            :: y
real            :: yleft
real            :: ymax
real            :: ymaxend1
real            :: ymaxend2
real            :: ymin
real            :: ystep
real            :: ystep2
real            :: yy
real            :: yystep
real            :: yystep2
!-----------------------------------------------------------------------------------------------------------------------------------
!     rescale some dash patterns to be used for making nicely spaced grid patterns
      pat(1)=ixdiv(1)
      pat(2)=ixdiv(2)
      pat(3)=iydiv(1)
      pat(4)=iydiv(2)
      call plot_setdash('scale',0,pat,4,idum)
!-----------------------------------------------------------------------------------------------------------------------------------
      y=vals(7)-vals(6)              ! scale height in axis units
      yy=vals(11)-vals(10)           ! scale height in axis units for right y-axis
      x=vals(5)-vals(4)              ! scale width in axis units
      !call clipping(.false.)        ! turn clipping off to give hardware text a chance when window is very small
      ystep=y/max(iydiv(1),1)        ! delta between major tic plot_marks to axis scale
      ystep2=ystep/max(iydiv(2),1)   ! delta between minor tic plot_marks to axis scale
      yystep=yy/max(iydiv(3),1)      ! delta between major tic plot_marks to axis scale
      yystep2=yystep/max(iydiv(4),1) ! delta between minor tic plot_marks to axis scale
      xstep=x/max(ixdiv(1),1)        ! delta between major tic plot_marks to axis scale
      xstep2=xstep/max(ixdiv(2),1)   ! delta between minor tic plot_marks to axis scale
!-----------------------------------------------------------------------------------------------------------------------------------
!     determine length of major and minor grid lines
      ix1=-3
      ix2=-4
      iy1=-5
      iy2=-6
      if(ticlny.ge.0.and.igrid(2).eq.1)then  ! a tic pointing out already drawn
         call priv_endgrid(igrid(2),xmaxend1,XMAX,XMIN,TICLNY,ix1) ! major y-axis grid style
      else
         call priv_endgrid(igrid(2),xmaxend1,XMAX,XMIN,0.0,ix2) ! major y-axis grid style stopping at same place
      endif

      call priv_endgrid(igrid(4),xmaxend2,XMAX,XMIN,TICLNY2,ix2) ! minor y-axis grid style

      if(ticlnx.ge.0.and.igrid(1).eq.1)then  ! a tic pointing out already drawn
         call priv_endgrid(igrid(1),ymaxend1,YMAX,YMIN,TICLNX,iy1) ! major y-axis grid style
      else
         call priv_endgrid(igrid(2),ymaxend1,YMAX,YMIN,0.0,iy2) ! major y-axis grid style stopping at same place
      endif

      call priv_endgrid(igrid(3),ymaxend2,YMAX,YMIN,TICLNX2,iy2) ! minor x-axis grid style
      ! iflag  <- 0 = full grid, 1= tic makrs, 2= plain
      ! endnew -> where to end line
      ! rmin   <- value of starting point of tic or grid line
      ! rmax   <- value of far edge line must end at before going out of plot
      ! ticlen <- length of tic mark for axis
      ! istyle -> set to zero for solid lines for drawing tics
!-----------------------------------------------------------------------------------------------------------------------------------
      cony= (YMAX - YMIN) / y * ystep
      if(XMIN.ne.xmaxend1)then          ! draw major grid lines
         call color(idcol(-1))          ! set color
         call xy_rasters(idwid(-1))        ! set curve width
         do i20=0,iydiv(1)
            sy = i20 * cony + YMIN
            call priv_drawseg_using_pen(ix1,XMIN,sy,xmaxend1,sy) ! major grid y-axis grid lines
         enddo
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
       if(XMIN.ne.xmaxend2)then                             ! draw minor grid lines
          call color(idcol(-2))
          call xy_rasters(idwid(-2))
          do i22=0,abs(iydiv(1)-1)
            sy = i22 * cony + YMIN
             if(ilogy.eq.1)then                             ! use decade-style minor logarithmic grids
                aleft=vals(6)+i22*ystep                     ! some of these are constants and can be moved
                aright=vals(6)+(i22+1)*ystep                ! some of these are constants and can be moved
                astep=(10**aright-10**aleft)/iydiv(2)
                aleft10=10**aleft
                do i25=1,iydiv(2)-1
                   anow=aleft10+i25*astep
                   rnow=log10(anow)
                   sy2=(rnow-vals(6))/y*(YMAX-YMIN)+YMIN
                   call priv_drawseg_using_pen(ix2,XMIN,sy2,xmaxend2,sy2)    ! minor y-axis grid lines for decade-style log scale
                enddo
            else
                do i30=1,abs(iydiv(2)-1)
                   sy2=sy+i30*(YMAX-YMIN)*ystep2/y
                   call priv_drawseg_using_pen(ix2,XMIN,sy2,xmaxend2,sy2)    ! minor y-axis grid lines
                enddo
             endif
          enddo
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      conx = (XMAX-XMIN)/x * xstep
      if(YMIN.ne.ymaxend1)then                   ! draw major grid lines
         call color(idcol(-1))                   ! set color
         call xy_rasters(idwid(-1))                 ! set curve width
         do i10=0,ixdiv(1)
            sx = i10 * conx + XMIN
            call priv_drawseg_using_pen(iy1,sx,YMIN,sx,ymaxend1)  ! major x-axis grid lines
         enddo
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(YMIN.ne.ymaxend2)then                 ! draw minor grid lines
         call color(idcol(-2))                 ! set color
         call xy_rasters(idwid(-2))               ! set curve width
           do i12=0,ixdiv(1)-1
             sx = i12 * conx + XMIN
             if(ilogx.eq.1)then                ! use decade-style minor grids
                aleft=vals(4)+i12*xstep           ! some of these are constants and can be moved
                aright=vals(4)+(i12+1)*xstep      ! some of these are constants and can be moved
                astep=(10**aright-10**aleft)/ixdiv(2)
                aleft10=10**aleft
                do i35=1,ixdiv(2)-1
                   anow=aleft10+i35*astep
                   rnow=log10(anow)
                   sx2=(rnow-vals(4))/x*(XMAX-XMIN)+XMIN
                   call priv_drawseg_using_pen(iy2,sx2,YMIN,sx2,ymaxend2)   ! minor decade-style logarithmic x-axis grid lines
                enddo
             else
                do i15=1,ixdiv(2)-1
                   sx2=sx+i15*(XMAX-XMIN)*xstep2/x
                   call priv_drawseg_using_pen(iy2,sx2,YMIN,sx2,ymaxend2)   ! minor x-axis grid lines
                enddo
             endif
          enddo
      endif
      !-----------------------------------------------------------------------------------------------------------------------------------
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !-----------------------------------------------------------------------------------------------------------------------------------
      if(icrvs2.ne.icrvs.or.aymul.ne.0.0)then  ! put tics pointing out on right axis
         if(aymul.ne.0)then
            ! force right step to be correct size
            bb=vals(7)*aymul+aycon
            aa=vals(6)*aymul+aycon
            range1=bb-aa   ! how many units should be along right edge
            yy=vals(11)-vals(10)            ! right scale height in axis units
            range2=yy      ! how many units are along right edge
            actor=range1/range2
            ! find point that should be at bottom right corner
            ! new axis will be different length
            !! yright=vals(6)*aymul+aycon ! point to put at YMIN on right is same value as on other side
            yleft=(vals(10)-aycon)/aymul ! bottom right converted back to left
            SHIFT=(vals(6)-yleft)/(vals(7)-vals(6))*(YMAX-YMIN)
         else
            actor=1.0
            shift=0.0
         endif
         cony= (YMAX - YMIN)/actor / yy * yystep
         call color(idcol(-1))          ! set color
         call xy_rasters(idwid(-1))        ! set curve width
         do i50=0,iydiv(3)            ! draw major grid tics
            sy = i50 * cony + YMIN
            sy = sy-SHIFT
            if(sy.le.YMAX.and.sy.ge.YMIN)then
               call priv_drawseg_using_pen(-1,XMAX,sy,XMAX+TICLNY,sy)   ! right-axis major tic lines
            endif
         enddo
!-----------------------------------------------------------------------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------------------------------------------------------------------
         if(XMIN.ne.xmaxend2)then !        draw minor grid lines
            call color(idcol(-2))
            call xy_rasters(idwid(-2))
            do i52=0,iydiv(3)-1
               sy = i52 * cony+ YMIN
               sy = sy-SHIFT
               if(ilogy.eq.1)then ! use decade-style minor logarithmic grids
                  aleft=vals(10)+i52*yystep         !some of these are constants and can be moved
                  aright=vals(10)+(i52+1)*yystep         !some of these are constants and can be moved
                  astep=(10**aright-10**aleft)/iydiv(4)
                  aleft10=10**aleft
                  do i55=1,iydiv(4)-1
                     anow=aleft10+i55*astep
                     rnow=log10(anow)
                     sy2=(rnow-vals(10))/yy*(YMAX-YMIN)+YMIN
                     if(sy2.le.YMAX.and.sy2.ge.YMIN)then
                        call priv_drawseg_using_pen(-2,XMAX,sy2,XMAX+TICLNY2/2,sy2) ! right-axis minor decade-style logarithmic tics
                     endif
                  enddo
               else         ! linear divisions
                  do i60=1,iydiv(4)-1
                     sy2=sy+i60*(YMAX-YMIN)/actor*yystep2/yy
                     if(sy2.le.YMAX.and.sy2.ge.YMIN)then
                        call priv_drawseg_using_pen(-2,XMAX,sy2,XMAX+TICLNY2/2,sy2) ! right axis minor tics
                     endif
                 enddo
               endif
            enddo
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
!-----------------------------------------------------------------------------------------------------------------------------------
!     draw thickened border
      call color(idcol(0))
      call xy_rasters(idwid(0))
      if(idwid(0).ne.0)then
         call plain_rect(XMIN,YMIN,XMAX,YMAX)  ! thickened axis border
      endif
      call xy_rasters(idwid(-1))
!-----------------------------------------------------------------------------------------------------------------------------------
      call stuff('GRID_XMIN',XMIN,'')  ! store corners of gridded area in page coordinates for Ted Rodack
      call stuff('GRID_YMIN',YMIN,'')  ! who wants to put on his own ylabels (multiple line)
      call stuff('GRID_XMAX',XMAX,'')
      call stuff('GRID_YMAX',YMAX,'')
!-----------------------------------------------------------------------------------------------------------------------------------
      !call clipping(.true.)        ! turn clipping on
end subroutine priv_jugrid
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_logrng(3fp) - [M_xyplot] return nice ranges for logarithmic scales
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_logrng(logtype,itic,ilog,rmin,rmax,idiv,treme,idel)
use M_math, only : scale1
use M_math, only : scale3
implicit none

! ident_96="@(#)M_xyplot::priv_logrng(3fp): return nice ranges for logarithmic scales"

character(len=*) :: logtype
integer          :: itic
integer          :: ilog
real             :: rmin
real             :: rmax
integer          :: idiv
real             :: treme(2)
integer          :: idel(2)
real             :: dum1
real             :: dum2
real             :: rdist
real             :: rminor
   call scale3(rmin,rmax,idiv,treme(1),treme(2),rdist) ! given suggested range and number of divisions, find something close
   treme(1)=log10(treme(1))
   treme(2)=log10(treme(2))
   if(logtype.eq.'decade')then                         ! conventional logarithmic axis
      ilog=1                                           ! logarithmic decade is being used
      treme(1)=floor(treme(1))
      treme(2)=ceiling(treme(2))
      if(treme(2).eq.treme(1))treme(2)=treme(2)+1
      idel(1)=treme(2)-treme(1)                        !this could be too close, use scale routine to come up with better method
      idel(2)=9
   else                                                ! linear logarithmic axis
      ilog=0                                           ! flag that linear style log axis are to be drawn
      idel(1)=(treme(2)-treme(1))/rdist+0.5
      call scale1(0.0,rdist,itic,dum1,dum2,rminor)     ! find a nice number of minor tics
      idel(2)=rdist/rminor+0.5
   endif
end subroutine priv_logrng
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    xy_obj12345(3f) - [M_xyplot] create or close M_plot object 12345
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_obj12345(string)
! create or close M_plot object 12345
! for use in defining displayed pages as an object so they can be easily copied
! using the hcopy command
!
use M_draw
implicit none

! ident_97="@(#)M_xyplot::xy_obj12345(3f): create or close M_plot object 12345"

logical                     :: ldummy
character(len=*),intent(in) :: string
integer                     :: idum
   if(string.eq.'before')then
      call plot_clear('all')! clear display
      call plot_storage     ! if on a tektronix 4014 that is not an xterm
      call push()      ! save M_plot environment so can restore it
      idum=backbuffer()!
      ldummy=xy_ifdrawn() ! let other routines know something has been drawn
      if(isobj(12345))then ! get rid of any previous object 12345
         call delobj(12345)
      endif
      call makeobj(12345)  ! start the object
   else
      call closeobj()      ! close the object
      call pop()           ! restore environment, assumed the one saved above
      call callobj(12345)  ! invoke the object
      call swapbuffers()   ! display it to front buffer
      call stuff('PLTOBJECT',12345.0d0,'')
      call vflush          ! flush the graphics display
   endif
end subroutine xy_obj12345
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_setrng(3fp) - [M_xyplot] adjust the scaling according to user limits if specified
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_setrng(rin,value,ichange)
implicit none

! ident_98="@(#)M_xyplot::priv_setrng(3fp): adjust the scaling according to user limits if specified"

real                ::  rin
real,intent(out)    :: value
integer,intent(out) :: ichange

   if(rin.ne.123.456)then
      value=rin  ! a value of 123.456 is a flag to ignore value
      ichange=1  ! flag that the value was changed
   else
      ichange=0
   endif
end subroutine priv_setrng
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_solidline(3fp) - [M_xyplot] override dash code and symbol drawing and draw solid line for error bars
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_solidline(mode)  ! override dash code and symbol drawing and draw solid line for error bars
implicit none

! ident_99="@(#)M_xyplot::priv_solidline(3fp): override dash code and symbol drawing and draw solid line for error bars"

logical,intent(in)  :: mode
      isolidq2=mode
end subroutine priv_solidline
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_toscale(3f) - [M_xyplot] draw line using current pen style either to axis or current window
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!   subroutine priv_toscale(x, y, n,con,keyword)
!!
!!    real                        :: x(*), y(*)    ! warning: changes curve values x,y if log plots made -consider data destroyed !!
!!    integer,intent(in)          :: n             ! number of points in X() and Y() to consider for plotting
!!    real,intent(in)             :: con(7)        ! xslope,xinter,yslope,yinter,xlogflag,ylogflag
!!    character(len=*),intent(in) :: keyword       ! flag whether to draw to axis or current plot window
!!##DESCRIPTION
!!    Draw a line using the current pen style (line width, color, dash code,
!!    symbols, under/over fill style) either to an axis or the current plot
!!    window.
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_toscale(x, y, n,con,keyword)
use M_journal, only : journal
use M_draw
implicit none

! ident_100="@(#)M_xyplot::priv_toscale(3fp): display 2-d vector described by x and y to axis or plot scale"

!     graph is in x,y xy_arrays; use points 1 to n

real                        :: x(*), y(*)    ! warning: changes curve values x,y if log plots made -consider data destroyed !!
integer,intent(in)          :: n             ! number of points in X() and Y() to consider for plotting
real,intent(in)             :: con(7)        ! xslope,xinter,yslope,yinter,xlogflag,ylogflag
character(len=*),intent(in) :: keyword       ! flag whether to draw to axis or current plot window

real                        :: pat(12)
integer      :: i
integer      :: i123
integer      :: i345
integer      :: iadd
integer      :: ibx
integer      :: iby
integer      :: idash
integer      :: ifill
integer      :: im
integer      :: ipatn
integer      :: ipen
integer      :: istart
integer      :: istep
integer      :: n2
real         :: offset
!===================================================================================================================================
      iadd=int(con(7)+0.5)
!===================================================================================================================================
!     if supposed to ignore leading values .le. 0 for logarithmic mode, do so.
      istart=1
      if(lq2(logleq2).and.con(5).ge.0)then
         do i123=1,n                               ! scan X values until a value > 0 is found
            if(x(i123).gt.0)exit
         enddo
         istart=i123                               ! first value can take a LOG() of
      endif
      if(lq2(logleq2).and.con(6).ge.0)then         ! scan Y values until a value > 0 is found
         do i345=1,n
            if(y(i345).gt.0)exit
         enddo
         istart=max(i345,istart)                   ! find starting index of value to try using logarithmically
      endif
!===================================================================================================================================
      if(n-istart+1.le.0)then
         call journal('*priv_toscale* no points in curve')
         return
      endif
!===================================================================================================================================
!     take the log of the user curve values
      ibx=0
      if(con(5).ge.0)then   ! logx
         do i=istart,n
            if(x(i).gt.0)then
               x(i)=log10(x(i))
            else
               x(i)=valsq2(4+iadd)
               ! could get fancier and find intercept on valsq2(6) horizontal line if adjoining points cross into plotted region
               ibx=ibx+1
            endif
         enddo
      endif
      iby=0
      if(con(6).ge.0)then   ! logy
         do i=istart,n
            if(y(i).gt.0)then
               y(i)=log10(y(i))
            else
               y(i)=valsq2(6+iadd)
               ! could get fancier and find intercept on valsq2(6) horizontal line if adjoining points cross into plotted region
               iby=iby+1
            endif
         enddo
      endif
!===================================================================================================================================
!     cannot take log of values <= 0; so changed the 'bad' values to 0. This will cause all lines drawn to the
!     zero points to be inaccurate. Could make more accurate by calculating intercept from last good point to
!     first bad point and vice versa across the horizontal line defined by valsq2(iadd+6)
      if(ibx.ne.0)then
         call journal('sc','*priv_toscale* WARNING:found x <= 0, occurrences=',ibx)
      endif
      if(iby.ne.0)then
         call journal('sc','*priv_toscale* WARNING:found y <= 0, occurrences=',iby)
      endif
!===================================================================================================================================
!     convert the values to the coordinate system of the graph area
      n2=n-istart+1 !istart is not 1 if ignoring leading values with a zero in them
      call xy_convert(con(1),con(2),x(istart),n2)
      call xy_convert(con(3),con(4),y(istart),n2)
!===================================================================================================================================
      ipen=ipenq2
      idash=ipen+2                                            ! load special dash patterns for grid pens
      if(ipen.ge.-2)then
        idash=plot_ids(ipen)%dashcode                                   ! a normal line, so use user-specified values
      elseif(ipen.lt.-6)then                                  ! user error
         ipen=-ipen
         idash=plot_ids(ipen)%dashcode                                  ! a normal line, so use user-specified values
      elseif(ipen.eq.-3)then                                  ! major x grid line draw
         ipen=-1
      elseif(ipen.eq.-4)then                                  ! minor x grid line draw
         ipen=-2
      elseif(ipen.eq.-5)then                                  ! major y grid line draw
         ipen=-1
      elseif(ipen.eq.-6)then                                  ! minor x grid line draw
         ipen=-2
      endif
      ipenq2=ipen
!===================================================================================================================================
      if(keyword.eq.'dash')then ! KLUDGE JUST FOR DRAWING DASH EXAMPLES
         call plot_setdash('fetch',ipen,pat,12,ipatn)
         call move2(x(istart),y(istart))                      ! priv_judash draws from current position
         call priv_judash(x(istart),y(istart),n2,pat,ipatn,0.0)
         return
      elseif(plot_ids(ipen)%dashcode.ge.0.or.isolidq2)then              ! not a scatter plot or error bar so draw connecting lines
         call plot_setdash('fetch',idash,pat,12,ipatn)
         call move2(x(istart),y(istart))                      ! priv_judash draws from current position
         if(idash.lt.0)then                                   ! for dashed grid lines, start in middle of first dash
            offset=pat(1)/2.0
         else
            offset=0.0
         endif
         if(isolidq2)then
            call priv_judash(x(istart),y(istart),n2,pat,0,offset) ! ignore dash pattern and draw solid line for error bars
            return
         else
            call priv_judash(x(istart),y(istart),n2,pat,ipatn,offset) ! experimental dash code test
         endif
      endif
!===================================================================================================================================
      if(plot_ids(ipen)%marker_frequency.ge.1.or.plot_ids(ipen)%dashcode.lt.0)then        ! put on markers at data points
         istep=plot_ids(ipen)%marker_frequency                ! how frequently to put on marker at data points
         im=max(1,abs(plot_ids(ipen)%marker))                 ! which marker to use
         if(istep.eq.0)istep=abs(plot_ids(ipen)%dashcode)     ! a scatter plot is implied if a null line style was set
         call priv_jusym(x(istart),y(istart),n2,istep,im,ipen)  ! draw symbol
      endif
!===================================================================================================================================
      ifill=plot_ids(ipen)%fill_style                         ! fill or impulse line mode
      if(keyword.eq.'graph'.and.ifill.ge.1)then               ! fill or impulse lines
         istep=plot_ids(ipen)%marker_frequency                ! how frequently to put impulse lines for some styles
         if(istep.eq.0)istep=abs(plot_ids(ipen)%dashcode)     ! a scatter plot is implied if a null line style was set
         call priv_jufill(x(istart),y(istart),n2,istep,ifill)   ! draw polygon
         call move2(x(n2),y(n2))                              ! position at last point of input data
      endif
!===================================================================================================================================
end subroutine priv_toscale
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_endgrid(3fp) - [M_xyplot] determine length of major and minor grid lines
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_endgrid(iflag,endnew,rmax,rmin,ticlen,istyle)
!
use M_journal, only : journal
implicit none

! ident_101="@(#)M_xyplot::priv_endgrid(3fp): determine length of major and minor grid lines"

integer,intent(in)    :: iflag  !-> 0 = full grid, 1= tic makrs, 2= plain
real,intent(out)      :: endnew !-> where to end line
real,intent(in)       :: rmin   !-> value of starting point of tic or grid line
real,intent(in)       :: rmax   !-> value of far edge line must end at before going out of plot
real,intent(in)       :: ticlen !-> length of tic mark for axis
integer,intent(inout) :: istyle !-> set to zero for solid lines for drawing tics
!
      select case (iflag)
      case(0)                             ! full grid
         endnew=rmax
      case(1)                             ! tic plot_marks
         endnew=min(rmin + ticlen,rmax)
         istyle=0                         ! use a solid line for ticks
      case(2)                             ! plain
         endnew=rmin
      case default
         call journal('*priv_endgrid* unknown axis grid style')
         endnew=rmax
      end select

end subroutine priv_endgrid
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!
!!    priv_judash(3fp) - [M_xyplot] draw dashed line
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_judash(x,y,n,pat,ipat,offset)
use M_draw
use ISO_C_BINDING
implicit none

! ident_102="@(#)M_xyplot::priv_judash(3fp): draw dashed line"

!  primitive dashed code generator. assumes a simple cartesian
!  coordinate system where x-units and y-units are the same length.
integer          :: n
real             :: x(n)
real             :: y(n)
integer          :: ipat
real             :: pat(ipat)
real,save        :: seglen
integer,save     :: index
real,save        :: lastx=-123.4567, lasty=-123.4567
real             :: curx
real             :: cury
real             :: diag
real             :: dist
real             :: dx
real             :: dy
real             :: f
real             :: fromx
real             :: fromy
integer          :: i10
integer          :: i20
integer          :: i30
integer          :: imark
real             :: offset
real             :: offtemp
real             :: sumlen
!***********************************************************************
! A line goes from fromx,fromy to tox,toy. We are following a pattern
! defined by a series of lengths. We might have a left-over fraction
! from a previous length, OR might already be pointing into the pattern with an offset(not implemented).
! draws from CURRENT POSITION thru the point(s) in x,y
! pat    = stores the pattern lengths
! ipat   = how many lengths are stored
! offset = for a "new" pattern, how far into the pattern to start (as a length)
! imark  = marker number used to select marker
!***********************************************************************
! seglen = length of current segment to draw (might be a remainder from previous call).
!          if length is negative, use marker of abs(seglen); then set seglen to 0
! index  = index pointing to segment length seglen came from
! offset
!***********************************************************************
! to do:
!         support hardware dash codes when possible
!         have a fast section to do n patterns without an if when dist is long enough
!         the way new is defined might not be restarting the pattern under some unlikely circumstances
!         consider always putting a point at the data points themselves
!         put out a point for a 0-length dash
!***********************************************************************
!-----------------------------------------------------------------------------------------------------------------------------------
      sumlen=0
      if(ipat.gt.1)then                         ! avoid patterns with a combined length of zero
         do i30=1,ipat
            sumlen=sumlen+max(0.0,pat(i30))     ! negative numbers are a zero-length segment with a symbol
         enddo
      endif
      if(ipat.le.1.or.sumlen.eq.0)then          ! if a zero-length pattern draw a solid line and return
         do i20=1,n
            call draw2(x(i20),y(i20))
         enddo
         return
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call getgp2(fromx,fromy)                  ! get coordinates of point to start from
      if(lastx.eq.fromx.and.lasty.eq.fromy)then
      else                                      ! we are starting a new line, not continuing a continuous line
         index = 0                              ! point to beginning of length patterns
         seglen=0
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      do i10=1,n
         curx = fromx                              ! point to start from
         cury = fromy                              !
         dx = x(i10) - fromx
         dy = y(i10) - fromy
         diag = sqrt(dx*dx + dy*dy)                ! length of total section from fromx,fromy to tox,toy to cover with the pattern
         dist = diag                               ! remaining length of section not covered with the pattern yet
         offtemp=0.0
         if(seglen.gt.0)goto 47
!-----------------------------------------------------------------------------------------------------------------------------------
         offtemp=offset
45       continue                                  ! this is the tight drawing loop- cycle thru the line lengths in pat
         index = mod(index, ipat) + 1
         if(offtemp.le.0)then
            seglen = pat(index)
         else
            seglen=offtemp
            offtemp=0.0
         endif
         if(seglen.lt.0.0)then
           imark=abs(seglen)
           seglen=0
         else
           imark=0
         endif
47       continue
         if (seglen .le. dist.and.dist.ne.0) then
            f = seglen/diag
            curx = curx + dx*f
            cury = cury + dy*f
            if(imark.ne.0)then                  ! a zero-length segment with a flag to draw a marker
               call priv_jusym([curx],[cury],1,1,imark,-1)  ! draw specified symbol HOW TO SPECIFY SIZE?
               call move2(curx,cury)
            elseif(mod(index,2).eq.0)then       ! even so make a gap
               call move2(curx,cury)
            else                                ! odd so make a dash
               call draw2(curx,cury)
            endif
            dist=dist-seglen
            goto 45
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         if (dist .ne. 0.) then            ! the distance to end is shorter than next segment. If dash mode, draw short segment
            if(mod(index,2).eq.0)then
                call move2(x(i10),y(i10))
            else
                call draw2(x(i10),y(i10))
            endif
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         seglen = seglen - dist            ! save our last values so we can pick up next call if necessary
         fromx=x(i10)
         fromy=y(i10)
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      lastx=x(n)
      lasty=y(n)
end subroutine priv_judash
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_jufill(3fp) - [M_xyplot] fill or draw impulse line under curve
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_jufill(x,y,n,istep,ifill)
use M_draw
use ISO_C_BINDING
implicit none

! ident_103="@(#)M_xyplot::priv_jufill(3fp): fill or draw impulse line under curve"

!     given a curve of x(n),y(n) put a line every istep points or fill
!     depending on clipping for now. Should let top/bottom be set
!     EXPERIMENTAL ROUTINE:
!     1 fill below curve
!     2 fill above curve
!     3 line from points down
!     4 line from points up
!     5 line from point down every istep points
!     6 line from point up every istep points

integer         :: n
real            :: x(n),y(n)
real            :: bottom
integer         :: i10
integer         :: i20
integer         :: i30
integer         :: i40
integer         :: i50
integer         :: i60
integer         :: i70
integer         :: i80
integer         :: ifill
integer         :: istep
real            :: radius
real            :: top

      TOP=100.0
      BOTTOM=0.0
      if(ifill.eq.0)then
      elseif(ifill.eq.1.and.n.ge.2)then  ! solid fill to every point
         call pushattributes()

         call polyfill(.true.)
         ! BUG: On IRIX M_DRAW does not fill or even draw the polygons if
         !      I use the same order of vertices that I use in ifill=3
         do i10=2,n
            call makepoly()
            call move2(x(i10-1),y(i10-1))
            call draw2(x(i10-1),BOTTOM )
            call draw2(x(i10  ),BOTTOM )
            call draw2(x(i10  ),y(i10  ))
            call draw2(x(i10-1),y(i10-1))
            call closepoly()
         enddo
         call polyfill(.false.)
         call popattributes()

      elseif(ifill.eq.2)then  ! solid fill up
         call pushattributes()

         call polyfill(.true.)
         do i20=2,n
            call makepoly()
            call move2(x(i20-1),y(i20-1))
            call draw2(x(i20  ),y(i20  ))
            call draw2(x(i20  ),TOP )
            call draw2(x(i20-1),TOP )
            call draw2(x(i20-1),y(i20-1))
            call closepoly()
         enddo
         call polyfill(.false.)
         call popattributes()

      elseif((ifill.eq.1).or.(ifill.eq.3))then  ! line from point down
         do i30=1,n
            call move2(x(i30),y(i30))
            call draw2(x(i30),BOTTOM)
         enddo

      elseif(ifill.eq.4)then  ! line from point up
         do i40=1,n
            call move2(x(i40),y(i40))
            call draw2(x(i40),TOP)
         enddo

      elseif(ifill.eq.5)then  ! line from points down every istep points
         if(istep.ne.0)then
            ! Compaq Tru64 1 F77 cannot handle istep=0 with certain options Wed Jan  5 09:55:26 EST 2000
            do i50=1,n,istep
               call move2(x(i50),y(i50))
               call draw2(x(i50),BOTTOM)
            enddo
         endif
      elseif(ifill.eq.6)then  ! line from points up every istep points
         if(istep.ne.0)then
            ! Compaq Tru64 1 F77 cannot handle istep=0 with certain options Wed Jan  5 09:55:26 EST 2000
            do i60=1,n,istep
               call move2(x(i60),y(i60))
               call draw2(x(i60),TOP)
            enddo
         endif

      elseif(ifill.eq.7)then  ! playing around. Don't document
         ! unsupported fill value
         ! if distance between points is a measure of the certainty
         ! of the value, the envelope of the circles is a measure
         ! of trust. Think about this ...

         ! home much does circle to before and circle to after
         ! change the plot?
         do i70=2,n
            radius=sqrt((x(i70)-x(i70-1))**2+(y(i70)-y(i70-1))**2)
            call circle(x(i70),y(i70),radius)
         enddo
      else

         ! home much does circle to before and circle to after
         ! change the plot?
         do i80=1,n-1
            radius=sqrt((x(i80)-x(i80+1))**2+(y(i80)-y(i80+1))**2)
            call circle(x(i80),y(i80),radius)
         enddo

      endif
end subroutine priv_jufill
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    priv_jusym(3f) - [M_xyplot] put symbols at points on a polyline
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine priv_jusym(x,y,n,istep,im,ipennum)
use M_journal, only : journal
use M_strings, only : change, crop
use M_draw
use M_drawplus, only : spirograph
use M_drawplus, only : arrowhead
implicit none

! ident_104="@(#)M_xyplot::priv_jusym(3fp): put symbols at points on a polyline"

!     given a curve of X(N),Y(N) put a symbol every ISTEP points, using symbol number IM
!     caution: when letters are used, centermode is off after call; and current font is used.
!BETTER     might be a good option to not put a mark out unless it is at least 1 diameter from previous mark
!BETTER     Make a coordinate mode that trims numeric strings and puts out numbers relative to axis coordinates
integer,intent(in)    :: n
real,intent(in)       :: x(n),y(n)
integer,intent(in)    :: istep
integer,intent(in)    :: im
integer,intent(in)    :: ipennum

real                  :: s(7)
real                  :: con(4)
character(len=1)      :: letter
character(len=132)    :: coord, coord2
real                  :: xhold, yhold
integer               :: i, i40, ii, i50, i60, i80
integer               :: i1000
integer               :: i70
integer               :: ier
integer               :: ilen
real                  :: xp
real                  :: yp

   call xy_getmark(s,im,ipennum) ! get numbers needed to draw a symbol
   call getfontsize(xhold,yhold)
!-----------------------------------------------------------------------------------------------------------------------------------
   if(im.gt.20.and.im.le.127)then  ! use a letter as a symbol
      if(im.lt.33)then         ! use some marker font symbols
         letter=char(im+47)
         call font('markers')  ! assume font reset by calling routine
         s(6)=s(6)*3.0
      else
         letter=char(im)
      endif
      call textsize(s(6),s(6))
      call centertext(.true.)
      do i=1,n,istep
         call move2(x(i),y(i))
         call drawchar(letter)
      enddo
      call centertext(.false.)
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(im.eq.128)then  ! arrow heads pointing back along vectors
      do i=2,n,istep
         call arrowhead(x(i-1),y(i-1),x(i),y(i),s(6),1)
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(im.eq.129)then  ! arrow heads pointing forward along vectors
      do i40=2,n,istep
         call arrowhead(x(i40),y(i40),x(i40-1),y(i40-1),s(6),1)
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(im.eq.130)then  ! rough draft for a possible marker that puts the coordinates at each point
      call textsize(s(6),s(6))
      do i50=1,n,istep
         write(coord,'(''('',g20.13,'','',g20.13,'')'')')x(i50),y(i50)
         call xy_juprints(x(i50),y(i50),coord)
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(im.eq.131)then  ! rough draft for a possible marker that puts the counter at each point
      call textsize(s(6),s(6))
      call centertext(.true.)
      !m=log10(real(n))+1
      do i60=1,n,istep
         write(coord,'(i8)')i60
         coord2=crop(coord)
         ilen=len_trim(coord2)
         !call circle(x(i60),y(i60),s(6)*m/2.0)
         call xy_juprints(x(i60),y(i60),coord2(:ilen))
      enddo
      call centertext(.false.)
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(im.eq.132)then  ! print index at point, and index and location to screen
         call push()
         call xy_getscale(0,con(1),con(2),con(3),con(4))      ! get scaling information between window and "axis scale window"
         call pop()
         call textsize(s(6),s(6))
         do i80=1,n,istep
            xp=(x(i80)-con(2))/con(1)  ! convert to scaled numbers for printing
            yp=(y(i80)-con(4))/con(3)
            write(coord,'(i8)')i80
            coord2=crop(coord)
            ilen=len_trim(coord2)
            call xy_juprints(x(i80),y(i80),coord2(:ilen))
            write(coord,'(i6,''('',g13.7,'','',g13.7,'')'')')i80,xp,yp
            call change(coord,'c/ //',ier) ! remove blanks from string
            call journal(coord)
         enddo
!-----------------------------------------------------------------------------------------------------------------------------------
!     specify format string
      elseif(im.ge.133.and.im.le.140)then  ! rough draft for a possible marker that puts the counter at each point scaled to axis
         ! specify angle or angle perpendicular to line
         ! 133 0
         ! 134 45
         ! 135 90
         ! 136 135
         ! 137 180
         ! 138 225
         ! 139 270
         ! 140 315
         call push()
         call xy_getscale(0,con(1),con(2),con(3),con(4))      ! get scaling information between window and "axis scale window"
         call pop()
         call textsize(s(6),s(6))
         call textang((im-133)*45.0)
         do i70=1,n,istep
            xp=(x(i70)-con(2))/con(1)  ! convert to scaled numbers for printing
            yp=(y(i70)-con(4))/con(3)
            write(coord,'(''('',g13.7,'','',g13.7,'')'')')xp,yp
            call change(coord,'c/ //',ier) ! remove blanks from string
            coord2=crop(coord)
            ilen=len_trim(coord2)
            call xy_juprints(x(i70),y(i70),coord2(:ilen))
            call circle(x(i70),y(i70),s(6)/4.0)
         enddo
         call textang(0.0)
!-----------------------------------------------------------------------------------------------------------------------------------
      elseif(im.gt.140)then  ! use an object
!     leave room for other things from 131 up, so documenting objects start at 1000
         if(isobj(im))then
          call pushattributes()
          do i1000=1,n,istep
             call invokeobj(x(i1000),y(i1000),0.0,s(6),s(6),1.0,0.0,0.0,0.0,im)
          enddo
          call popattributes()
         else
            call journal('sc','*priv_jusym* object number does not exist ==>',im)
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
   else ! use a geometric pattern for a symbol and handle bad marker numbers
      ii=nint(s(4))
      do i=1,n,istep
         !(xcenter,ycenter,sunrin,planin,offin,radius,ilines,ang,angs,fill)
         call spirograph(x(i),y(i),s(1),s(2),s(3),s(6)/2.0,ii,0.0,s(5),int(s(7)))
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call textsize(xhold,yhold)
end subroutine priv_jusym
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    xy_getmark(3f) - [M_xyplot] get marker geometries
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##EXAMPLE
!!
subroutine xy_getmark(pat,imarker,ipennum)
use M_draw
implicit none

!@(#) M_xyplot::xy_getmark(3f):get marker geometries

! 1992 John S. Urban
!
! now that supporting alphanumeric marker sizes, should remove size from
! geometric xy_array SHQ2 and just use plot_ids(:)%marker_size.
!
real,intent(out)   :: pat(ipq2)
integer,intent(in) :: imarker
integer,intent(in) :: ipennum

real               :: xmin0t,xmax0t,ymin0dum,ymax0dum
integer            :: j
integer            :: i20
integer            :: jj
! plot_ids(:)%marker_size is marker size set with ID command
! ICS2Q    is number of markers set with ID
! ipensq2 is number of geometric markers set with plot_setmark
!-----------------------------------------------------------------------------------------------------------------------------------
   call priv_getpage(xmin0t,xmax0t,ymin0dum,ymax0dum)
   if(imarker.le.ipensq2)then                              ! geometric markers
      j=max(1,imarker)
      do i20=1,ipq2
         pat(i20)=shq2(j,i20)
      enddo
      pat(5)=2*piq2*shq2(j,5)                              ! convert from fraction of a circle to radians
      jj=min(max(-2,ipennum),ICS2Q)                         ! size from ID command
      pat(6)=plot_ids(jj)%marker_size*shq2(j,6)*(xmax0t-xmin0t)/100.0   ! convert diameter from percent of full x window to a number
!-----------------------------------------------------------------------------------------------------------------------------------
   else                                                    ! alphameric markers
      j=min(max(-2,ipennum),ICS2Q)
      pat(6)=plot_ids(j)%marker_size*(xmax0t-xmin0t)/100.0              ! convert diameter from percent of full x window to a number
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine xy_getmark
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!   illusion(3f) - [M_xyplot:banner] draw a banner page with short labels at the compass points
!!   (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!   subroutine illusion(top,bottom,left,right)
!!
!!    character(len=*),intent(in) :: top
!!    character(len=*),intent(in) :: bottom
!!    character(len=*),intent(in) :: left
!!    character(len=*),intent(in) :: right
!!
!!##DESCRIPTION
!!    Draw a simple geometric illusion with short labels at the four compass points as a banner page
!!
!!##OPTIONS
!!    TOP      short top banner label
!!    BOTTOM   short bottom banner label
!!    LEFT     short left banner label
!!    RIGHT    short right banner label
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_illusion
!!    use M_draw,      only : vinit, color, clear, backbuffer, getkey, vexit, page
!!    use M_xyplot,    only : illusion
!!    call vinit(' ')
!!    call page(0.0,4800.0,0.0,4800.0)
!!    call color(6)
!!    call clear()
!!    call color(5)
!!    idum=backbuffer()
!!    call illusion('TOP','BOTTOM','LEFT','RIGHT')
!!    idum=getkey()
!!    call vexit()
!!    end program demo_illusion
subroutine illusion(top,bottom,left,right) !@(#) draw a simple geometric illusion
use M_draw
!use M_xyplot, only : xy_obj12345
implicit none

character(len=*),intent(in) :: top
character(len=*),intent(in) :: bottom
character(len=*),intent(in) :: left
character(len=*),intent(in) :: right

integer,save                :: iarx(9,16)  ! fill color, number of points
integer,save                :: iary(9,16)  ! x,y values for making polygons
integer                     :: i10, i20, i30
integer                     :: i
real                        :: x1, y1
real                        :: x, y
real                        :: smid
real                        :: smax
real                        :: smin
real                        :: xx
!-----------------------------------------------------------------------------------------------------------------------------------
data(iarx(i,1), iary(i,1),i=1,9)/  1, 9, 1950,1350, 4350,1350, 4350,3150, 3450,3150, 3450,2850, 4050,2850, 4050,1650, 1950,1650/
data(iarx(i,2), iary(i,2),i=1,5)/  1, 5, 450,1650,  750,1350,  1350,1350, 1350,1650/
data(iarx(i,3), iary(i,3),i=1,5)/  1, 5, 750,3150,  1050,2850, 2850,2850, 2850,3150/
data(iarx(i,4), iary(i,4),i=1,5)/  1, 5, 750,3150,  750,1950,  1050,1950, 1050,2850/
data(iarx(i,5), iary(i,5),i=1,5)/  2, 5, 3750,1950, 4050,1650, 4050,2850, 3750,2850/
data(iarx(i,6), iary(i,6),i=1,5)/  2, 5, 1950,1950, 1950,1650, 4050,1650, 3750,1950/
data(iarx(i,7), iary(i,7),i=1,9)/  2, 9, 1350,1650, 1350,1950, 750,1950,  750,3150,  2850,3150, 2850,3450, 450,3450,  450,1650/
data(iarx(i,8), iary(i,8),i=1,5)/  2, 5, 4350,3150, 4050,3450, 3450,3450, 3450,3150/
data(iarx(i,9), iary(i,9),i=1,9)/  3, 9, 3450,1950, 3450,4350, 1650,4350, 1650,3450, 1950,3450, 1950,4050, 3150,4050, 3150,1950/
data(iarx(i,10),iary(i,10),i=1,5)/ 3, 5, 3150,450,  3450,750,  3450,1350, 3150,1350/
data(iarx(i,11),iary(i,11),i=1,5)/ 3, 5, 1650,750,  1950,1050, 1950,2850, 1650,2850/
data(iarx(i,12),iary(i,12),i=1,5)/ 3, 5, 1650,750,  2850,750,  2850,1050, 1950,1050/
data(iarx(i,13),iary(i,13),i=1,5)/ 4, 5, 2850,1950, 3150,1950, 3150,4050, 2850,3750/
data(iarx(i,14),iary(i,14),i=1,9)/ 4, 9, 3150,1350, 2850,1350, 2850,750,  1650,750,  1650,2850, 1350,2850, 1350,450,  3150,450/
data(iarx(i,15),iary(i,15),i=1,5)/ 4, 5, 2850,3750, 3150,4050, 1950,4050, 1950,3750/
data(iarx(i,16),iary(i,16),i=1,5)/ 4, 5, 1650,4350, 1350,4050, 1350,3450, 1650,3450/
!-----------------------------------------------------------------------------------------------------------------------------------
   call xy_obj12345('before')   ! begin M_DRAW object 12345

   call linewidth(30)
   do i10=1,16               ! draw each polygon
      call polyfill(.true.)
      call color(iarx(1,i10))
      do i30=1,2             ! make filled polygon on first pass, outlined on second
         call makepoly()
         x1=real(iarx(2,i10))
         y1=real(iary(2,i10))
         call move2(x1,y1)
         do i20=3,iary(1,i10)
            x=real(iarx(i20,i10))
            y=real(iary(i20,i10))
            call draw2(x,y)
         enddo
         call draw2(x1,y1)
         call closepoly()
         call color(7)
         call polyfill(.false.)
      enddo
   enddo

   call linewidth(75)
   call textsize(410.0,400.0)
   call xcentertext()
   call color(7)
   smid=(450.0+4350.0)/2.0
   smax=4350.0
   smin=450.0
   xx=130.0

   call textang(0.0)
   call move2(smid+xx,smax)
   call drawstr(top)

   call textang(180.0)
   call move2(smid-xx,smin)
   call drawstr(bottom)

   call textang(90.0)
   call move2(smin,smid+xx)
   call drawstr(left)

   call textang(270.0)
   call move2(smax,smid-xx)
   call drawstr(right)

   call textang(0.0)
   call xy_obj12345('after')

end subroutine illusion
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_xyplot()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

! setup
   call test_plot_axes()
   call test_plot_clear()
   call test_plot_drawplot()
   call test_plot_get_plot_area()
   call test_plot_getdatarange()
   call test_plot_hcopy()
   call test_plot_id()
   call test_plot_init()
   call test_plot_init_globals()
   call test_plot_jupage()
   call test_plot_juparea()
   call test_plot_jut()
   call test_plot_label()
   call test_plot_line()
   call test_plot_marks()
   call test_plot_page()
   call test_plot_page_aspect()
   call test_plot_resetplot()
   call test_plot_set_nice_range()
   call test_plot_set_plot_area()
   call test_plot_set_xmin()
   call test_plot_setdash()
   call test_plot_setfill()
   call test_plot_setmark()
   call test_plot_setticper()
   call test_plot_storage()
   call test_plot_sz()
   call test_plot_title()
   call test_plot_toggle()
   call test_plot_z()
   call test_plot_zmode()
   call test_xy_aspct()
   call test_xy_con_x()
   call test_xy_con_y()
   call test_xy_getdat()
   call test_xy_getmark()
   call test_xy_getrel()
   call test_xy_getscale()
   call test_xy_ifdrawn()
   call test_xy_jucurv()
   call test_xy_judraw()
   call test_xy_jugetwn()
   call test_xy_jumapc()
   call test_xy_juprint()
   call test_xy_juput()
   call test_xy_jurang()
   call test_xy_jutitlx()
   call test_xy_line()
   call test_xy_obj12345()
   call test_xy_pickpnt()
   call test_xy_resetpens()
   call test_xy_setcnv()
   call test_xy_setdatarange()
   call test_xy_setsize()
   call test_xy_toa()
   call test_xy_units()
   call test_illusion()
! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_illusion()
implicit none
   call unit_check_start('illusion',msg='')
   !!call unit_check('illusion', 0.eq.0, 'checking',100)
   call unit_check_done('illusion',msg='')
end subroutine test_illusion
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_axes()
   call unit_check_start('plot_axes',msg='')
   !!call unit_check('plot_axes', 0.eq.0,'checking',100)
   call unit_check_done('plot_axes',msg='')
end subroutine test_plot_axes
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_clear()
   call unit_check_start('plot_clear',msg='')
   !!call unit_check('plot_clear', 0.eq.0,'checking',100)
   call unit_check_done('plot_clear',msg='')
end subroutine test_plot_clear
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_drawplot()
   call unit_check_start('plot_drawplot',msg='')
   !!call unit_check('plot_drawplot', 0.eq.0,'checking',100)
   call unit_check_done('plot_drawplot',msg='')
end subroutine test_plot_drawplot
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_get_plot_area()
   call unit_check_start('plot_get_plot_area',msg='')
   !!call unit_check('plot_get_plot_area', 0.eq.0,'checking',100)
   call unit_check_done('plot_get_plot_area',msg='')
end subroutine test_plot_get_plot_area
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_getdatarange()
   call unit_check_start('plot_getdatarange',msg='')
   !!call unit_check('plot_getdatarange', 0.eq.0,'checking',100)
   call unit_check_done('plot_getdatarange',msg='')
end subroutine test_plot_getdatarange
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_hcopy()
   call unit_check_start('plot_hcopy',msg='')
   !!call unit_check('plot_hcopy', 0.eq.0,'checking',100)
   call unit_check_done('plot_hcopy',msg='')
end subroutine test_plot_hcopy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_id()
   call unit_check_start('plot_id',msg='')
   !!call unit_check('plot_id', 0.eq.0,'checking',100)
   call unit_check_done('plot_id',msg='')
end subroutine test_plot_id
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_init()
   call unit_check_start('plot_init',msg='')
   !!call unit_check('plot_init', 0.eq.0,'checking',100)
   call unit_check_done('plot_init',msg='')
end subroutine test_plot_init
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_init_globals()
   call unit_check_start('plot_init_globals',msg='')
   !!call unit_check('plot_init_globals', 0.eq.0,'checking',100)
   call unit_check_done('plot_init_globals',msg='')
end subroutine test_plot_init_globals
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_jupage()
   call unit_check_start('plot_jupage',msg='')
   !!call unit_check('plot_jupage', 0.eq.0,'checking',100)
   call unit_check_done('plot_jupage',msg='')
end subroutine test_plot_jupage
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_juparea()
   call unit_check_start('plot_juparea',msg='')
   !!call unit_check('plot_juparea', 0.eq.0,'checking',100)
   call unit_check_done('plot_juparea',msg='')
end subroutine test_plot_juparea
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_jut()
   call unit_check_start('plot_jut',msg='')
   !!call unit_check('plot_jut', 0.eq.0,'checking',100)
   call unit_check_done('plot_jut',msg='')
end subroutine test_plot_jut
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_label()
   call unit_check_start('plot_label',msg='')
   !!call unit_check('plot_label', 0.eq.0,'checking',100)
   call unit_check_done('plot_label',msg='')
end subroutine test_plot_label
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_line()
   call unit_check_start('plot_line',msg='')
   !!call unit_check('plot_line', 0.eq.0,'checking',100)
   call unit_check_done('plot_line',msg='')
end subroutine test_plot_line
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_marks()
   call unit_check_start('plot_marks',msg='')
   !!call unit_check('plot_marks', 0.eq.0,'checking',100)
   call unit_check_done('plot_marks',msg='')
end subroutine test_plot_marks
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_page()
   call unit_check_start('plot_page',msg='')
   !!call unit_check('plot_page', 0.eq.0,'checking',100)
   call unit_check_done('plot_page',msg='')
end subroutine test_plot_page
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_page_aspect()
   call unit_check_start('plot_page_aspect',msg='')
   !!call unit_check('plot_page_aspect', 0.eq.0,'checking',100)
   call unit_check_done('plot_page_aspect',msg='')
end subroutine test_plot_page_aspect
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_resetplot()
   call unit_check_start('plot_resetplot',msg='')
   !!call unit_check('plot_resetplot', 0.eq.0,'checking',100)
   call unit_check_done('plot_resetplot',msg='')
end subroutine test_plot_resetplot
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_set_nice_range()
   call unit_check_start('plot_set_nice_range',msg='')
   !!call unit_check('plot_set_nice_range', 0.eq.0,'checking',100)
   call unit_check_done('plot_set_nice_range',msg='')
end subroutine test_plot_set_nice_range
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_set_plot_area()
   call unit_check_start('plot_set_plot_area',msg='')
   !!call unit_check('plot_set_plot_area', 0.eq.0,'checking',100)
   call unit_check_done('plot_set_plot_area',msg='')
end subroutine test_plot_set_plot_area
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_set_xmin()
   call unit_check_start('plot_set_xmin',msg='')
   !!call unit_check('plot_set_xmin', 0.eq.0,'checking',100)
   call unit_check_done('plot_set_xmin',msg='')
end subroutine test_plot_set_xmin
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_setdash()
   call unit_check_start('plot_setdash',msg='')
   !!call unit_check('plot_setdash', 0.eq.0,'checking',100)
   call unit_check_done('plot_setdash',msg='')
end subroutine test_plot_setdash
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_setfill()
   call unit_check_start('plot_setfill',msg='')
   !!call unit_check('plot_setfill', 0.eq.0,'checking',100)
   call unit_check_done('plot_setfill',msg='')
end subroutine test_plot_setfill
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_setmark()
   call unit_check_start('plot_setmark',msg='')
   !!call unit_check('plot_setmark', 0.eq.0,'checking',100)
   call unit_check_done('plot_setmark',msg='')
end subroutine test_plot_setmark
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_setticper()
   call unit_check_start('plot_setticper',msg='')
   !!call unit_check('plot_setticper', 0.eq.0,'checking',100)
   call unit_check_done('plot_setticper',msg='')
end subroutine test_plot_setticper
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_storage()
   call unit_check_start('plot_storage',msg='')
   !!call unit_check('plot_storage', 0.eq.0,'checking',100)
   call unit_check_done('plot_storage',msg='')
end subroutine test_plot_storage
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_sz()
   call unit_check_start('plot_sz',msg='')
   !!call unit_check('plot_sz', 0.eq.0,'checking',100)
   call unit_check_done('plot_sz',msg='')
end subroutine test_plot_sz
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_title()
   call unit_check_start('plot_title',msg='')
   !!call unit_check('plot_title', 0.eq.0,'checking',100)
   call unit_check_done('plot_title',msg='')
end subroutine test_plot_title
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_toggle()
   call unit_check_start('plot_toggle',msg='')
   !!call unit_check('plot_toggle', 0.eq.0,'checking',100)
   call unit_check_done('plot_toggle',msg='')
end subroutine test_plot_toggle
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_z()
   call unit_check_start('plot_z',msg='')
   !!call unit_check('plot_z', 0.eq.0,'checking',100)
   call unit_check_done('plot_z',msg='')
end subroutine test_plot_z
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_plot_zmode()
   call unit_check_start('plot_zmode',msg='')
   !!call unit_check('plot_zmode', 0.eq.0,'checking',100)
   call unit_check_done('plot_zmode',msg='')
end subroutine test_plot_zmode
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_aspct()
   call unit_check_start('xy_aspct',msg='')
   !!call unit_check('xy_aspct', 0.eq.0,'checking',100)
   call unit_check_done('xy_aspct',msg='')
end subroutine test_xy_aspct
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_con_x()
   call unit_check_start('xy_con_x',msg='')
   !!call unit_check('xy_con_x', 0.eq.0,'checking',100)
   call unit_check_done('xy_con_x',msg='')
end subroutine test_xy_con_x
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_con_y()
   call unit_check_start('xy_con_y',msg='')
   !!call unit_check('xy_con_y', 0.eq.0,'checking',100)
   call unit_check_done('xy_con_y',msg='')
end subroutine test_xy_con_y
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_getdat()
   call unit_check_start('xy_getdat',msg='')
   !!call unit_check('xy_getdat', 0.eq.0,'checking',100)
   call unit_check_done('xy_getdat',msg='')
end subroutine test_xy_getdat
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_getmark()
   call unit_check_start('xy_getmark',msg='')
   !!call unit_check('xy_getmark', 0.eq.0,'checking',100)
   call unit_check_done('xy_getmark',msg='')
end subroutine test_xy_getmark
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_getrel()
   call unit_check_start('xy_getrel',msg='')
   !!call unit_check('xy_getrel', 0.eq.0,'checking',100)
   call unit_check_done('xy_getrel',msg='')
end subroutine test_xy_getrel
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_getscale()
   call unit_check_start('xy_getscale',msg='')
   !!call unit_check('xy_getscale', 0.eq.0,'checking',100)
   call unit_check_done('xy_getscale',msg='')
end subroutine test_xy_getscale
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_ifdrawn()
   call unit_check_start('xy_ifdrawn',msg='')
   !!call unit_check('xy_ifdrawn', 0.eq.0,'checking',100)
   call unit_check_done('xy_ifdrawn',msg='')
end subroutine test_xy_ifdrawn
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_jucurv()
   call unit_check_start('xy_jucurv',msg='')
   !!call unit_check('xy_jucurv', 0.eq.0,'checking',100)
   call unit_check_done('xy_jucurv',msg='')
end subroutine test_xy_jucurv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_judraw()
   call unit_check_start('xy_judraw',msg='')
   !!call unit_check('xy_judraw', 0.eq.0,'checking',100)
   call unit_check_done('xy_judraw',msg='')
end subroutine test_xy_judraw
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_jugetwn()
   call unit_check_start('xy_jugetwn',msg='')
   !!call unit_check('xy_jugetwn', 0.eq.0,'checking',100)
   call unit_check_done('xy_jugetwn',msg='')
end subroutine test_xy_jugetwn
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_jumapc()
   call unit_check_start('xy_jumapc',msg='')
   !!call unit_check('xy_jumapc', 0.eq.0,'checking',100)
   call unit_check_done('xy_jumapc',msg='')
end subroutine test_xy_jumapc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_juprint()
   call unit_check_start('xy_juprint',msg='')
   !!call unit_check('xy_juprint', 0.eq.0,'checking',100)
   call unit_check_done('xy_juprint',msg='')
end subroutine test_xy_juprint
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_juput()
   call unit_check_start('xy_juput',msg='')
   !!call unit_check('xy_juput', 0.eq.0,'checking',100)
   call unit_check_done('xy_juput',msg='')
end subroutine test_xy_juput
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_jurang()
   call unit_check_start('xy_jurang',msg='')
   !!call unit_check('xy_jurang', 0.eq.0,'checking',100)
   call unit_check_done('xy_jurang',msg='')
end subroutine test_xy_jurang
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_jutitlx()
   call unit_check_start('xy_jutitlx',msg='')
   !!call unit_check('xy_jutitlx', 0.eq.0,'checking',100)
   call unit_check_done('xy_jutitlx',msg='')
end subroutine test_xy_jutitlx
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_line()
   call unit_check_start('xy_line',msg='')
   !!call unit_check('xy_line', 0.eq.0,'checking',100)
   call unit_check_done('xy_line',msg='')
end subroutine test_xy_line
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_obj12345()
   call unit_check_start('xy_obj12345',msg='')
   !!call unit_check('xy_obj12345', 0.eq.0,'checking',100)
   call unit_check_done('xy_obj12345',msg='')
end subroutine test_xy_obj12345
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_pickpnt()
   call unit_check_start('xy_pickpnt',msg='')
   !!call unit_check('xy_pickpnt', 0.eq.0,'checking',100)
   call unit_check_done('xy_pickpnt',msg='')
end subroutine test_xy_pickpnt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_resetpens()
   call unit_check_start('xy_resetpens',msg='')
   !!call unit_check('xy_resetpens', 0.eq.0,'checking',100)
   call unit_check_done('xy_resetpens',msg='')
end subroutine test_xy_resetpens
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_setcnv()
   call unit_check_start('xy_setcnv',msg='')
   !!call unit_check('xy_setcnv', 0.eq.0,'checking',100)
   call unit_check_done('xy_setcnv',msg='')
end subroutine test_xy_setcnv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_setdatarange()
   call unit_check_start('xy_setdatarange',msg='')
   !!call unit_check('xy_setdatarange', 0.eq.0,'checking',100)
   call unit_check_done('xy_setdatarange',msg='')
end subroutine test_xy_setdatarange
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_setsize()
   call unit_check_start('xy_setsize',msg='')
   !!call unit_check('xy_setsize', 0.eq.0,'checking',100)
   call unit_check_done('xy_setsize',msg='')
end subroutine test_xy_setsize
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_toa()
   call unit_check_start('xy_toa',msg='')
   !!call unit_check('xy_toa', 0.eq.0,'checking',100)
   call unit_check_done('xy_toa',msg='')
end subroutine test_xy_toa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xy_units()
   call unit_check_start('xy_units',msg='')
   !!call unit_check('xy_units', 0.eq.0,'checking',100)
   call unit_check_done('xy_units',msg='')
end subroutine test_xy_units
!===================================================================================================================================
end subroutine test_suite_M_xyplot
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_xyplot
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!-----------------------------------------------------------------------------------------------------------------------------------
