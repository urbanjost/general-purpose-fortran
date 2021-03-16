!-------------------------------------------------------------------------------
! @(#) Fortran interface to the ncurses(3c) library -John S.Urban 20141205
!
! The ncurses.f90 and macros.c files contain a  Fortran interface to
! the ncurses(3c) library.  The interface file is free and unencumbered software
! released into the public domain.
!
! For further details see the file UNLICENSE.txt. For more information,
! please refer to <http;//unlicense.org/>
! BUILD: 20150312

module ncurses_types
   use,intrinsic ::  iso_c_binding
   implicit none
 
   type,BIND(C) :: MEVENT
      integer(C_SHORT) :: id
      integer(C_INT)   :: x,y,z
      integer(C_LONG)  :: bstate
   end type MEVENT
 
!typedef unsigned long mmask_t
!!Fortran does not have unsigned types. if just do bit operations is this OK?
 integer, parameter :: mmask_t = kind(0_C_LONG)
 integer, parameter :: chtype = kind(0_C_LONG)
 integer, parameter :: attr_t = kind(0_C_LONG)
! ---------------------------------------------
! integer, parameter :: WINDOW = kind(0_C_PTR)
! integer, parameter :: WINDOW = kind(C_PTR(0))
!  type,BIND(C) :: WINDOW
!     type(C_PTR) :: win
!  end type WINDOW
! ---------------------------------------------
 
end module ncurses_types
module M_ncurses
use iso_c_binding
use ncurses_types
implicit none
!-------------------------------------------------------------------------------
! no mouse support yet
! limited wprint
! no wscan
! unfortunately, a lot depends on which ncurses options are used at build time
! and which terminal or terminal emulator is being used and X11 clients and Xresources set.
!-------------------------------------------------------------------------------
! In addition to the interface definitions, ....
!
! Created some pseudo-generic interfaces for some missing procedures such as
! the printw group of routines. Call them with one variable of type integer,real, or string.
! so generally have to break prints of a line up into multiple calls.
! Note that escaped strings like \n will not work as they do from C.
!
! Added the KEY_F() function, and getmaxyx(), getbegyx(), getparyx(), getyx()
! which would not work as calls as they are macros in the ncurses library I
! am working with.
!
! Most additions use a C wrapper (see wrapper.c) because the reason they were
! not supported by the Fortran/C interface definition was because they could not be directly called via the
! ISO_C_INTERFACE (varying argument lists, functions are actually macros created
! with cpp(1) directives, ...).
!
! Note that "External Variables" like LINES, COLS, stdscr, curscr, sp, COLORS, COLOR_PAIRS,
!            and TABSIZE do not appear to actually access the C variables; at least in
!            my programming environment.
!
! Did this as worked thru the samples found in the basics/ directory released with
! the ncurses(3c) package. These additions allowed a minimal amount of changes to
! be made while creating equivalents in Fortran.
!
! since global variables are not just global variables made getcolor(3f)
! have to get LINES and COLS with a call to getmaxyx(3c).
!
! John S. Urban
!-------------------------------------------------------------------------------
! constants from ncurses.h ...
! External Variables that are global in C
  logical(C_BOOL), parameter :: TRUE  = .true.
  logical(C_BOOL), parameter :: FALSE = .false.
  integer(C_INT), parameter  :: ERR   = -1
  integer(C_INT), parameter  :: OK    =  0
! screen
  type    (C_PTR), bind(C,name='sp')         :: sp
! ncurses extensions to curses
  integer(C_INT), bind(C,name='ESCDELAY')    :: ESCDELAY
  integer(C_INT), bind(C,name='TABSIZE')     :: TABSIZE
!----------------------------------------------------------
! in ncurses these are not simple variables (probably macros)
! so it will not work to treat them as global variables even
! though they might appear so to C users.
! need to call a subroutine to initialize them. So they are just
! global to ncurses module users, and do NOT reflect the ncurses
! variables ...
!!  integer(C_INT), bind(C,name='LINES')       :: LINES
!!  integer(C_INT), bind(C,name='COLS')        :: COLS
  integer(C_INT)  :: LINES
  integer(C_INT)  :: COLS
! windows
!!  type    (C_PTR), bind(C,name='stdscr')     :: stdscr
!!  type    (C_PTR), bind(C,name='curscr')     :: curscr
!!  type    (C_PTR) :: stdscr
!!  type    (WINDOW) :: stdscr
!!  type    (WINDOW) :: curscr
  type    (C_PTR) :: stdscr
  type    (C_PTR) :: curscr
!!  integer(C_INT), bind(C,name='COLORS')      :: COLORS
!!  integer(C_INT), bind(C,name='COLOR_PAIRS') :: COLOR_PAIRS
  integer(C_INT) :: COLORS
  integer(C_INT) :: COLOR_PAIRS
! integer(C_LONG) :: acs_map
!-------------------------------------------------------------------------------
! colors from ncurses.h ...
  integer(C_SHORT), parameter :: COLOR_BLACK     = 0_C_SHORT
  integer(C_SHORT), parameter :: COLOR_RED       = 1_C_SHORT
  integer(C_SHORT), parameter :: COLOR_GREEN     = 2_C_SHORT
  integer(C_SHORT), parameter :: COLOR_YELLOW    = 3_C_SHORT
  integer(C_SHORT), parameter :: COLOR_BLUE      = 4_C_SHORT
  integer(C_SHORT), parameter :: COLOR_MAGENTA   = 5_C_SHORT
  integer(C_SHORT), parameter :: COLOR_CYAN      = 6_C_SHORT
  integer(C_SHORT), parameter :: COLOR_WHITE     = 7_C_SHORT
!-------------------------------------------------------------------------------
  integer(C_INT), parameter :: button_released         =  0_C_INT
  integer(C_INT), parameter :: button_pressed          =  1_C_INT
  integer(C_INT), parameter :: button_clicked          =  2_C_INT
  integer(C_INT), parameter :: button_double_clicked   =  3_C_INT
  integer(C_INT), parameter :: button_triple_clicked   =  4_C_INT
  integer(C_INT), parameter :: button_moved            =  5_C_INT
  integer(C_INT), parameter :: wheel_scrolled          =  6_C_INT
  integer(C_INT), parameter :: button_action_mask      =  7_C_INT
  integer(C_INT), parameter :: pdc_button_shift        =  8_C_INT
  integer(C_INT), parameter :: pdc_button_control      = 16_C_INT
  integer(C_INT), parameter :: pdc_button_alt          = 32_C_INT
  integer(C_INT), parameter :: button_modifier_mask    = 56_C_INT
!-------------------------------------------------------------------------------
!
!  Pseudo-character tokens outside ASCII range.  The curses wgetch() function
!  will return any given one of these only if the corresponding k- capability
!  is defined in your terminal's terminfo entry.
!
!  Some keys (KEY_A1, etc) are arranged like this:
!       a1     up    a3
!       left   b2    right
!       c1     down  c3
!
!  A few key codes do not depend upon the terminfo entry.
!
integer(C_INT),parameter ::  KEY_CODE_YES  = INT(O'400',C_INT) ! A wchar_t contains a key code
integer(C_INT),parameter ::  KEY_MIN       = INT(O'401',C_INT) ! Minimum curses key
integer(C_INT),parameter ::  KEY_BREAK     = INT(O'401',C_INT) ! Break key (unreliable)
integer(C_INT),parameter ::  KEY_SRESET    = INT(O'530',C_INT) ! Soft (partial) reset (unreliable)
integer(C_INT),parameter ::  KEY_RESET     = INT(O'531',C_INT) ! Reset or hard reset (unreliable)
!
!  These definitions were generated by ./MKkey_defs.sh ./Caps
!
integer(C_INT),parameter ::  KEY_DOWN      = INT(O'402',C_INT) ! down-arrow key
integer(C_INT),parameter ::  KEY_UP        = INT(O'403',C_INT) ! up-arrow key
integer(C_INT),parameter ::  KEY_LEFT      = INT(O'404',C_INT) ! left-arrow key
integer(C_INT),parameter ::  KEY_RIGHT     = INT(O'405',C_INT) ! right-arrow key
integer(C_INT),parameter ::  KEY_HOME      = INT(O'406',C_INT) ! home key
integer(C_INT),parameter ::  KEY_BACKSPACE = INT(O'407',C_INT) ! backspace key
integer(C_INT),parameter ::  KEY_F0        = INT(O'410',C_INT) ! Function keys.  Space for 64
!integer(C_INT),parameter ::  KEY_F(n)      = (KEY_F0+(n))',C_INT) ! Value of function key n
integer(C_INT),parameter ::  KEY_DL        = INT(O'510',C_INT) ! delete-line key
integer(C_INT),parameter ::  KEY_IL        = INT(O'511',C_INT) ! insert-line key
integer(C_INT),parameter ::  KEY_DC        = INT(O'512',C_INT) ! delete-character key
integer(C_INT),parameter ::  KEY_IC        = INT(O'513',C_INT) ! insert-character key
integer(C_INT),parameter ::  KEY_EIC       = INT(O'514',C_INT) ! sent by rmir or smir in insert mode
integer(C_INT),parameter ::  KEY_CLEAR     = INT(O'515',C_INT) ! clear-screen or erase key
integer(C_INT),parameter ::  KEY_EOS       = INT(O'516',C_INT) ! clear-to-end-of-screen key
integer(C_INT),parameter ::  KEY_EOL       = INT(O'517',C_INT) ! clear-to-end-of-line key
integer(C_INT),parameter ::  KEY_SF        = INT(O'520',C_INT) ! scroll-forward key
integer(C_INT),parameter ::  KEY_SR        = INT(O'521',C_INT) ! scroll-backward key
integer(C_INT),parameter ::  KEY_NPAGE     = INT(O'522',C_INT) ! next-page key
integer(C_INT),parameter ::  KEY_PPAGE     = INT(O'523',C_INT) ! previous-page key
integer(C_INT),parameter ::  KEY_STAB      = INT(O'524',C_INT) ! set-tab key
integer(C_INT),parameter ::  KEY_CTAB      = INT(O'525',C_INT) ! clear-tab key
integer(C_INT),parameter ::  KEY_CATAB     = INT(O'526',C_INT) ! clear-all-tabs key
integer(C_INT),parameter ::  KEY_ENTER     = INT(O'527',C_INT) ! enter/send key
integer(C_INT),parameter ::  KEY_PRINT     = INT(O'532',C_INT) ! print key
integer(C_INT),parameter ::  KEY_LL        = INT(O'533',C_INT) ! lower-left key (home down)
integer(C_INT),parameter ::  KEY_A1        = INT(O'534',C_INT) ! upper left of keypad
integer(C_INT),parameter ::  KEY_A3        = INT(O'535',C_INT) ! upper right of keypad
integer(C_INT),parameter ::  KEY_B2        = INT(O'536',C_INT) ! center of keypad
integer(C_INT),parameter ::  KEY_C1        = INT(O'537',C_INT) ! lower left of keypad
integer(C_INT),parameter ::  KEY_C3        = INT(O'540',C_INT) ! lower right of keypad
integer(C_INT),parameter ::  KEY_BTAB      = INT(O'541',C_INT) ! back-tab key
integer(C_INT),parameter ::  KEY_BEG       = INT(O'542',C_INT) ! begin key
integer(C_INT),parameter ::  KEY_CANCEL    = INT(O'543',C_INT) ! cancel key
integer(C_INT),parameter ::  KEY_CLOSE     = INT(O'544',C_INT) ! close key
integer(C_INT),parameter ::  KEY_COMMAND   = INT(O'545',C_INT) ! command key
integer(C_INT),parameter ::  KEY_COPY      = INT(O'546',C_INT) ! copy key
integer(C_INT),parameter ::  KEY_CREATE    = INT(O'547',C_INT) ! create key
integer(C_INT),parameter ::  KEY_END       = INT(O'550',C_INT) ! end key
integer(C_INT),parameter ::  KEY_EXIT      = INT(O'551',C_INT) ! exit key
integer(C_INT),parameter ::  KEY_FIND      = INT(O'552',C_INT) ! find key
integer(C_INT),parameter ::  KEY_HELP      = INT(O'553',C_INT) ! help key
integer(C_INT),parameter ::  KEY_MARK      = INT(O'554',C_INT) ! mark key
integer(C_INT),parameter ::  KEY_MESSAGE   = INT(O'555',C_INT) ! message key
integer(C_INT),parameter ::  KEY_MOVE      = INT(O'556',C_INT) ! move key
integer(C_INT),parameter ::  KEY_NEXT      = INT(O'557',C_INT) ! next key
integer(C_INT),parameter ::  KEY_OPEN      = INT(O'560',C_INT) ! open key
integer(C_INT),parameter ::  KEY_OPTIONS   = INT(O'561',C_INT) ! options key
integer(C_INT),parameter ::  KEY_PREVIOUS  = INT(O'562',C_INT) ! previous key
integer(C_INT),parameter ::  KEY_REDO      = INT(O'563',C_INT) ! redo key
integer(C_INT),parameter ::  KEY_REFERENCE = INT(O'564',C_INT) ! reference key
integer(C_INT),parameter ::  KEY_REFRESH   = INT(O'565',C_INT) ! refresh key
integer(C_INT),parameter ::  KEY_REPLACE   = INT(O'566',C_INT) ! replace key
integer(C_INT),parameter ::  KEY_RESTART   = INT(O'567',C_INT) ! restart key
integer(C_INT),parameter ::  KEY_RESUME    = INT(O'570',C_INT) ! resume key
integer(C_INT),parameter ::  KEY_SAVE      = INT(O'571',C_INT) ! save key
integer(C_INT),parameter ::  KEY_SBEG      = INT(O'572',C_INT) ! shifted begin key
integer(C_INT),parameter ::  KEY_SCANCEL   = INT(O'573',C_INT) ! shifted cancel key
integer(C_INT),parameter ::  KEY_SCOMMAND  = INT(O'574',C_INT) ! shifted command key
integer(C_INT),parameter ::  KEY_SCOPY     = INT(O'575',C_INT) ! shifted copy key
integer(C_INT),parameter ::  KEY_SCREATE   = INT(O'576',C_INT) ! shifted create key
integer(C_INT),parameter ::  KEY_SDC       = INT(O'577',C_INT) ! shifted delete-character key
integer(C_INT),parameter ::  KEY_SDL       = INT(O'600',C_INT) ! shifted delete-line key
integer(C_INT),parameter ::  KEY_SELECT    = INT(O'601',C_INT) ! select key
integer(C_INT),parameter ::  KEY_SEND      = INT(O'602',C_INT) ! shifted end key
integer(C_INT),parameter ::  KEY_SEOL      = INT(O'603',C_INT) ! shifted clear-to-end-of-line key
integer(C_INT),parameter ::  KEY_SEXIT     = INT(O'604',C_INT) ! shifted exit key
integer(C_INT),parameter ::  KEY_SFIND     = INT(O'605',C_INT) ! shifted find key
integer(C_INT),parameter ::  KEY_SHELP     = INT(O'606',C_INT) ! shifted help key
integer(C_INT),parameter ::  KEY_SHOME     = INT(O'607',C_INT) ! shifted home key
integer(C_INT),parameter ::  KEY_SIC       = INT(O'610',C_INT) ! shifted insert-character key
integer(C_INT),parameter ::  KEY_SLEFT     = INT(O'611',C_INT) ! shifted left-arrow key
integer(C_INT),parameter ::  KEY_SMESSAGE  = INT(O'612',C_INT) ! shifted message key
integer(C_INT),parameter ::  KEY_SMOVE     = INT(O'613',C_INT) ! shifted move key
integer(C_INT),parameter ::  KEY_SNEXT     = INT(O'614',C_INT) ! shifted next key
integer(C_INT),parameter ::  KEY_SOPTIONS  = INT(O'615',C_INT) ! shifted options key
integer(C_INT),parameter ::  KEY_SPREVIOUS = INT(O'616',C_INT) ! shifted previous key
integer(C_INT),parameter ::  KEY_SPRINT    = INT(O'617',C_INT) ! shifted print key
integer(C_INT),parameter ::  KEY_SREDO     = INT(O'620',C_INT) ! shifted redo key
integer(C_INT),parameter ::  KEY_SREPLACE  = INT(O'621',C_INT) ! shifted replace key
integer(C_INT),parameter ::  KEY_SRIGHT    = INT(O'622',C_INT) ! shifted right-arrow key
integer(C_INT),parameter ::  KEY_SRSUME    = INT(O'623',C_INT) ! shifted resume key
integer(C_INT),parameter ::  KEY_SSAVE     = INT(O'624',C_INT) ! shifted save key
integer(C_INT),parameter ::  KEY_SSUSPEND  = INT(O'625',C_INT) ! shifted suspend key
integer(C_INT),parameter ::  KEY_SUNDO     = INT(O'626',C_INT) ! shifted undo key
integer(C_INT),parameter ::  KEY_SUSPEND   = INT(O'627',C_INT) ! suspend key
integer(C_INT),parameter ::  KEY_UNDO      = INT(O'630',C_INT) ! undo key
integer(C_INT),parameter ::  KEY_MOUSE     = INT(O'631',C_INT) ! Mouse event has occurred
integer(C_INT),parameter ::  KEY_RESIZE    = INT(O'632',C_INT) ! Terminal resize event
integer(C_INT),parameter ::  KEY_EVENT     = INT(O'633',C_INT) ! We were interrupted by an event

integer(C_INT),parameter ::  KEY_MAX       = INT(O'777',C_INT) ! Maximum key value is 0633
!-------------------------------------------------------------------------------
! ATTRIBUTES: Appear to be modules. Made a C program to print them, assuming
!             they do not change for other reasons? Got list from ncurses.h
! #include <ncurses.h>
! int main () {
!    initscr ();      /* Start curses mode              */
!    fprintf (stderr, "A_ALTCHARSET = %ld \n", A_ALTCHARSET); fprintf (stderr, "A_ATTRIBUTES = %ld \n", A_ATTRIBUTES);
!    fprintf (stderr, "A_BLINK = %ld \n", A_BLINK); fprintf (stderr, "A_BOLD = %ld \n", A_BOLD);
!    fprintf (stderr, "A_CHARTEXT = %ld \n", A_CHARTEXT); fprintf (stderr, "A_COLOR = %ld \n", A_COLOR);
!    fprintf (stderr, "A_DIM = %ld \n", A_DIM); fprintf (stderr, "A_HORIZONTAL = %ld \n", A_HORIZONTAL);
!    fprintf (stderr, "A_INVIS = %ld \n", A_INVIS); fprintf (stderr, "A_ITALIC = %ld \n", A_ITALIC);
!    fprintf (stderr, "A_LEFT = %ld \n", A_LEFT); fprintf (stderr, "A_LOW = %ld \n", A_LOW);
!    fprintf (stderr, "A_NORMAL = %ld \n", A_NORMAL); fprintf (stderr, "A_PROTECT = %ld \n", A_PROTECT);
!    fprintf (stderr, "A_REVERSE = %ld \n", A_REVERSE); fprintf (stderr, "A_RIGHT = %ld \n", A_RIGHT);
!    fprintf (stderr, "A_STANDOUT = %ld \n", A_STANDOUT); fprintf (stderr, "A_TOP = %ld \n", A_TOP);
!    fprintf (stderr, "A_UNDERLINE = %ld \n", A_UNDERLINE); fprintf (stderr, "A_VERTICAL = %ld \n", A_VERTICAL);
!    fprintf (stderr, "WA_ALTCHARSET = %ld \n", WA_ALTCHARSET); fprintf (stderr, "WA_ATTRIBUTES = %ld \n", WA_ATTRIBUTES);
!    fprintf (stderr, "WA_BLINK = %ld \n", WA_BLINK); fprintf (stderr, "WA_BOLD = %ld \n", WA_BOLD);
!    fprintf (stderr, "WA_DIM = %ld \n", WA_DIM); fprintf (stderr, "WA_HORIZONTAL = %ld \n", WA_HORIZONTAL);
!    fprintf (stderr, "WA_INVIS = %ld \n", WA_INVIS); fprintf (stderr, "WA_LEFT = %ld \n", WA_LEFT);
!    fprintf (stderr, "WA_LOW = %ld \n", WA_LOW); fprintf (stderr, "WA_NORMAL = %ld \n", WA_NORMAL);
!    fprintf (stderr, "WA_PROTECT = %ld \n", WA_PROTECT); fprintf (stderr, "WA_REVERSE = %ld \n", WA_REVERSE);
!    fprintf (stderr, "WA_RIGHT = %ld \n", WA_RIGHT); fprintf (stderr, "WA_STANDOUT = %ld \n", WA_STANDOUT);
!    fprintf (stderr, "WA_TOP = %ld \n", WA_TOP); fprintf (stderr, "WA_UNDERLINE = %ld \n", WA_UNDERLINE);
!    fprintf (stderr, "WA_VERTICAL = %ld \n", WA_VERTICAL);
!    endwin ();
! }
! The standard attributes
integer(C_LONG), parameter :: A_BLINK        =  524288_C_LONG       ! blink
integer(C_LONG), parameter :: A_BOLD         =  2097152_C_LONG      ! bold
integer(C_LONG), parameter :: A_INVIS        =  8388608_C_LONG      ! invisible
integer(C_LONG), parameter :: A_NORMAL       =  0_C_LONG            ! normal
integer(C_LONG), parameter :: A_PROTECT      =  16777216_C_LONG     ! protect
integer(C_LONG), parameter :: A_REVERSE      =  262144_C_LONG       ! reverse
integer(C_LONG), parameter :: A_UNDERLINE    =  131072_C_LONG       ! underline
integer(C_LONG), parameter :: A_DIM          =  1048576_C_LONG
integer(C_LONG), parameter :: A_HORIZONTAL   =  33554432_C_LONG
integer(C_LONG), parameter :: A_LEFT         =  67108864_C_LONG
integer(C_LONG), parameter :: A_LOW          =  134217728_C_LONG
integer(C_LONG), parameter :: A_RIGHT        =  268435456_C_LONG
integer(C_LONG), parameter :: A_STANDOUT     =  65536_C_LONG
integer(C_LONG), parameter :: A_TOP          =  536870912_C_LONG
integer(C_LONG), parameter :: A_VERTICAL     =  1073741824_C_LONG

integer(C_LONG), parameter :: WA_BLINK       =  524288_C_LONG
integer(C_LONG), parameter :: WA_BOLD        =  2097152_C_LONG
integer(C_LONG), parameter :: WA_DIM         =  1048576_C_LONG
integer(C_LONG), parameter :: WA_HORIZONTAL  =  33554432_C_LONG
integer(C_LONG), parameter :: WA_INVIS       =  8388608_C_LONG
integer(C_LONG), parameter :: WA_LEFT        =  67108864_C_LONG
integer(C_LONG), parameter :: WA_LOW         =  134217728_C_LONG
integer(C_LONG), parameter :: WA_NORMAL      =  0_C_LONG
integer(C_LONG), parameter :: WA_PROTECT     =  16777216_C_LONG
integer(C_LONG), parameter :: WA_REVERSE     =  262144_C_LONG
integer(C_LONG), parameter :: WA_RIGHT       =  268435456_C_LONG
integer(C_LONG), parameter :: WA_STANDOUT    =  65536_C_LONG
integer(C_LONG), parameter :: WA_TOP         =  536870912_C_LONG
integer(C_LONG), parameter :: WA_UNDERLINE   =  131072_C_LONG
integer(C_LONG), parameter :: WA_VERTICAL    =  1073741824_C_LONG
!-------------------------------------------------------------------------------
! addition ncurses attributes
integer(C_LONG), parameter :: A_COLOR        =  65280_C_LONG
integer(C_LONG), parameter :: A_ITALIC       =  2147483648_C_LONG
integer(C_LONG), parameter :: A_ALTCHARSET   =  4194304_C_LONG
integer(C_LONG), parameter :: A_ATTRIBUTES   =  4294967040_C_LONG
integer(C_LONG), parameter :: A_CHARTEXT     =  255_C_LONG
integer(C_LONG), parameter :: WA_ALTCHARSET  =  4194304_C_LONG
integer(C_LONG), parameter :: WA_ATTRIBUTES  =  4294967040_C_LONG
!-------------------------------------------------------------------------------
!  VT102-compatible symbols -- box chars
  integer(C_LONG), parameter :: ACS_ULCORNER  =  4194412_C_LONG ! NCURSES_ACS('l') /* upper left corner */
  integer(C_LONG), parameter :: ACS_LLCORNER  =  4194413_C_LONG ! NCURSES_ACS('m') /* lower left corner */
  integer(C_LONG), parameter :: ACS_URCORNER  =  4194411_C_LONG ! NCURSES_ACS('k') /* upper right corner */
  integer(C_LONG), parameter :: ACS_LRCORNER  =  4194410_C_LONG ! NCURSES_ACS('j') /* lower right corner */
  integer(C_LONG), parameter :: ACS_LTEE      =  4194420_C_LONG ! NCURSES_ACS('t') /* tee pointing right */
  integer(C_LONG), parameter :: ACS_RTEE      =  4194421_C_LONG ! NCURSES_ACS('u') /* tee pointing left */
  integer(C_LONG), parameter :: ACS_BTEE      =  4194422_C_LONG ! NCURSES_ACS('v') /* tee pointing up */
  integer(C_LONG), parameter :: ACS_TTEE      =  4194423_C_LONG ! NCURSES_ACS('w') /* tee pointing down */
  integer(C_LONG), parameter :: ACS_HLINE     =  4194417_C_LONG ! NCURSES_ACS('q') /* horizontal line */
  integer(C_LONG), parameter :: ACS_VLINE     =  4194424_C_LONG ! NCURSES_ACS('x') /* vertical line */
  integer(C_LONG), parameter :: ACS_PLUS      =  4194414_C_LONG ! NCURSES_ACS('n') /* large plus or crossover */
!  Line drawing ACS names are of the form ACS_trbl, where t is the top, r
!  is the right, b is the bottom, and l is the left.  t, r, b, and l might
!  be B (blank), S (single), D (double), or T (thick).  The subset defined
!  here only uses B and S.
   integer(C_LONG), parameter ::  ACS_BSSB=ACS_ULCORNER
   integer(C_LONG), parameter ::  ACS_SSBB=ACS_LLCORNER
   integer(C_LONG), parameter ::  ACS_BBSS=ACS_URCORNER
   integer(C_LONG), parameter ::  ACS_SBBS=ACS_LRCORNER
   integer(C_LONG), parameter ::  ACS_SBSS=ACS_RTEE
   integer(C_LONG), parameter ::  ACS_SSSB=ACS_LTEE
   integer(C_LONG), parameter ::  ACS_SSBS=ACS_BTEE
   integer(C_LONG), parameter ::  ACS_BSSS=ACS_TTEE
   integer(C_LONG), parameter ::  ACS_BSBS=ACS_HLINE
   integer(C_LONG), parameter ::  ACS_SBSB=ACS_VLINE
   integer(C_LONG), parameter ::  ACS_SSSS=ACS_PLUS
!-------------------------------------------------------------------------------
! additional VT102-compatible symbols
  integer(C_LONG), parameter :: ACS_S1        =  4194415_C_LONG ! NCURSES_ACS('o') /* scan line 1 */
  integer(C_LONG), parameter :: ACS_S9        =  4194419_C_LONG ! NCURSES_ACS('s') /* scan line 9 */
  integer(C_LONG), parameter :: ACS_DIAMOND   =  4194400_C_LONG ! NCURSES_ACS('`') /* diamond */
  integer(C_LONG), parameter :: ACS_CKBOARD   =  4194401_C_LONG ! NCURSES_ACS('a') /* checker board (stipple) */
  integer(C_LONG), parameter :: ACS_DEGREE    =  4194406_C_LONG ! NCURSES_ACS('f') /* degree symbol */
  integer(C_LONG), parameter :: ACS_PLMINUS   =  4194407_C_LONG ! NCURSES_ACS('g') /* plus/minus */
  integer(C_LONG), parameter :: ACS_BULLET    =  4194430_C_LONG ! NCURSES_ACS('~') /* bullet */
!-------------------------------------------------------------------------------
! Teletype 5410v1 symbols  --  not always portable
  integer(C_LONG), parameter :: ACS_LARROW    =  4194348_C_LONG ! NCURSES_ACS(',') /* arrow pointing left */
  integer(C_LONG), parameter :: ACS_RARROW    =  4194347_C_LONG ! NCURSES_ACS('+') /* arrow pointing right */
  integer(C_LONG), parameter :: ACS_DARROW    =  4194350_C_LONG ! NCURSES_ACS('.') /* arrow pointing down */
  integer(C_LONG), parameter :: ACS_UARROW    =  4194349_C_LONG ! NCURSES_ACS('-') /* arrow pointing up */
  integer(C_LONG), parameter :: ACS_BOARD     =  4194408_C_LONG ! NCURSES_ACS('h') /* board of squares */
  integer(C_LONG), parameter :: ACS_LANTERN   =  4194409_C_LONG ! NCURSES_ACS('i') /* lantern symbol */
  integer(C_LONG), parameter :: ACS_BLOCK     =  4194352_C_LONG ! NCURSES_ACS('0') /* solid square block */
!-------------------------------------------------------------------------------
! These aren't documented, but a lot of System Vs have them anyway
! (you can spot pprryyzz{{||}} in a lot of AT&T terminfo strings).
! The ACS_names may not match AT&T's, our source didn't know them.
  integer(C_LONG), parameter :: ACS_LEQUAL    =  4194425_C_LONG ! NCURSES_ACS('y') /* less/equal */
  integer(C_LONG), parameter :: ACS_GEQUAL    =  4194426_C_LONG ! NCURSES_ACS('z') /* greater/equal */
  integer(C_LONG), parameter :: ACS_PI        =  4194427_C_LONG ! NCURSES_ACS('{') /* Pi */
  integer(C_LONG), parameter :: ACS_NEQUAL    =  4194428_C_LONG ! NCURSES_ACS('|') /* not equal */
  integer(C_LONG), parameter :: ACS_STERLING  =  4194429_C_LONG ! NCURSES_ACS('}') /* UK pound sign */
  integer(C_LONG), parameter :: ACS_S3        =  4194416_C_LONG ! NCURSES_ACS('p') /* scan line 3 */
  integer(C_LONG), parameter :: ACS_S7        =  4194418_C_LONG ! NCURSES_ACS('r') /* scan line 7 */
!-------------------------------------------------------------------------------
! Appear to be modules. Made a C program to print them, assuming
! they do not change for other reasons? Got list from ncurses.h
! Here are the mouse event type masks which may be defined:

 !     Name                       Description
!-------------------------------------------------------------------------------
integer(C_LONG),parameter ::   BUTTON1_PRESSED        =         2_C_LONG  ! mouse button 1 down
integer(C_LONG),parameter ::   BUTTON1_RELEASED       =         1_C_LONG  ! mouse button 1 up
integer(C_LONG),parameter ::   BUTTON1_CLICKED        =         4_C_LONG  ! mouse button 1 clicked
integer(C_LONG),parameter ::   BUTTON1_DOUBLE_CLICKED =         8_C_LONG  ! mouse button 1 double clicked
integer(C_LONG),parameter ::   BUTTON1_TRIPLE_CLICKED =        16_C_LONG  ! mouse button 1 triple clicked
!-------------------------------------------------------------------------------
integer(C_LONG),parameter ::   BUTTON2_PRESSED        =        64_C_LONG  ! mouse button 2 down
integer(C_LONG),parameter ::   BUTTON2_RELEASED       =        32_C_LONG  ! mouse button 2 up
integer(C_LONG),parameter ::   BUTTON2_CLICKED        =       128_C_LONG  ! mouse button 2 clicked
integer(C_LONG),parameter ::   BUTTON2_DOUBLE_CLICKED =       256_C_LONG  ! mouse button 2 double clicked
integer(C_LONG),parameter ::   BUTTON2_TRIPLE_CLICKED =       512_C_LONG  ! mouse button 2 triple clicked
!-------------------------------------------------------------------------------
integer(C_LONG),parameter ::   BUTTON3_PRESSED        =      2048_C_LONG  ! mouse button 3 down
integer(C_LONG),parameter ::   BUTTON3_RELEASED       =      1024_C_LONG  ! mouse button 3 up
integer(C_LONG),parameter ::   BUTTON3_CLICKED        =      4096_C_LONG  ! mouse button 3 clicked
integer(C_LONG),parameter ::   BUTTON3_DOUBLE_CLICKED =      8192_C_LONG  ! mouse button 3 double clicked
integer(C_LONG),parameter ::   BUTTON3_TRIPLE_CLICKED =     16384_C_LONG  ! mouse button 3 triple clicked
!-------------------------------------------------------------------------------
integer(C_LONG),parameter ::   BUTTON4_PRESSED        =     65536_C_LONG  ! mouse button 4 down
integer(C_LONG),parameter ::   BUTTON4_RELEASED       =     32768_C_LONG  ! mouse button 4 up
integer(C_LONG),parameter ::   BUTTON4_CLICKED        =    131072_C_LONG  ! mouse button 4 clicked
integer(C_LONG),parameter ::   BUTTON4_DOUBLE_CLICKED =    262144_C_LONG  ! mouse button 4 double clicked
integer(C_LONG),parameter ::   BUTTON4_TRIPLE_CLICKED =    524288_C_LONG  ! mouse button 4 triple clicked
!-------------------------------------------------------------------------------
integer(C_LONG),parameter ::   BUTTON5_PRESSED        =   2097152_C_LONG  ! mouse button 5 down
integer(C_LONG),parameter ::   BUTTON5_RELEASED       =   1048576_C_LONG  ! mouse button 5 up
integer(C_LONG),parameter ::   BUTTON5_CLICKED        =   4194304_C_LONG  ! mouse button 5 clicked
integer(C_LONG),parameter ::   BUTTON5_DOUBLE_CLICKED =   8388608_C_LONG  ! mouse button 5 double clicked
integer(C_LONG),parameter ::   BUTTON5_TRIPLE_CLICKED =  16777216_C_LONG  ! mouse button 5 triple clicked
!-------------------------------------------------------------------------------
integer(C_LONG),parameter ::   BUTTON_SHIFT           =  67108864_C_LONG  ! shift was down during button state change
integer(C_LONG),parameter ::   BUTTON_CTRL            =  33554432_C_LONG  ! control was down during button state change
integer(C_LONG),parameter ::   BUTTON_ALT             = 134217728_C_LONG  ! alt was down during button state change
integer(C_LONG),parameter ::   ALL_MOUSE_EVENTS       = 268435455_C_LONG  ! report all button state changes
integer(C_LONG),parameter ::   REPORT_MOUSE_POSITION  = 268435456_C_LONG  ! report mouse movement
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!   Function Declarations
!-------------------------------------------------------------------------------
interface
!-------------------------------------------------------------------------------
! needs to be defined in a C routine (is macro)
!      void getyx(WINDOW *win, int y, int x);
!      void getparyx(WINDOW *win, int y, int x);
!      void getbegyx(WINDOW *win, int y, int x);
!      void getmaxyx(WINDOW *win, int y, int x);
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    subroutine getyx(win,y,x) bind(C, name='macro_getyx')
      use iso_c_binding
      use ncurses_types
      type (C_PTR), value :: win
      integer(C_INT) :: y,x
    end subroutine getyx
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    subroutine getparyx(win,y,x) bind(C, name='macro_getparyx')
      use iso_c_binding
      use ncurses_types
      type (C_PTR), value :: win
      integer(C_INT) :: y,x
    end subroutine getparyx
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    subroutine getbegyx(win,y,x) bind(C, name='macro_getbegyx')
      use iso_c_binding
      use ncurses_types
      type (C_PTR), value :: win
      integer(C_INT) :: y,x
    end subroutine getbegyx
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    subroutine getmaxyx(win,y,x) bind(C, name='macro_getmaxyx')
      use iso_c_binding
      use ncurses_types
      type (C_PTR), value :: win
      integer(C_INT) :: y,x
    end subroutine getmaxyx
!-------------------------------------------------------------------------------
    subroutine getcolor(colors,color_pairs) bind(C, name='macro_getcolor')
      use iso_c_binding
      integer(C_INT) :: colors
      integer(C_INT) :: color_pairs
    end subroutine getcolor
!-------------------------------------------------------------------------------
    subroutine getwindows(my_stdscr,my_curscr) bind(C, name='macro_getwindows')
    ! "variables" stdscr and curscr are (at least here) macros, so get the
    ! values the hard way by calling a C wrapper procedure in _macros.c
      use iso_c_binding
      type(C_PTR)         :: my_stdscr
      type(C_PTR)         :: my_curscr
    end subroutine getwindows
!-------------------------------------------------------------------------------
!  original:: WINDOW *getwin(FILE *filep); !  change from file pointer to filename and wrap
function getwin(filename) result (getwin__OUT) bind(C, name='macro_getwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR) :: getwin__OUT          ! WINDOW *getwin
   character(kind=c_char) ,intent(in):: filename             ! const char *filename
end function getwin
!-------------------------------------------------------------------------------
!  original:: int putwin(WINDOW *win, FILE *filep); !  change from file pointer to filename and wrap
function putwin(win,filename) result (putwin__OUT) bind(C, name='macro_putwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: putwin__OUT      ! int putwin
   type(C_PTR),value :: win           ! WINDOW *win
   character(kind=c_char) ,intent(in):: filename             ! const char *filename
end function putwin
!-------------------------------------------------------------------------------
!MANPAGE ERROR? int COLOR_PAIR(int ipair);
!               chtype COLOR_PAIR(int ipair);
    function COLOR_PAIR(ipair) result (attrs) bind(C, name='COLOR_PAIR')
      use iso_c_binding
      integer(C_INT),VALUE  :: ipair
      integer(C_LONG)       :: attrs
    end function COLOR_PAIR
!-------------------------------------------------------------------------------
!!  The following is the reverse of COLOR_PAIR(n):
!!     PAIR_NUMBER(attrs)
!!  Returns the pair number associated with the COLOR_PAIR(n) attribute.
    function PAIR_NUMBER(attrs) result (ipair) bind(C, name='PAIR_NUMBER')
      use iso_c_binding
      integer(C_LONG),VALUE :: attrs
      integer(C_INT)        :: ipair
    end function PAIR_NUMBER
!-----------------------------------------------------------------------------------------------------------------------------------
! CDEF: WINDOW *initscr ( void );
function f_initscr() result (initscr__OUT) bind(C, name='initscr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR):: initscr__OUT         ! WINDOW *initscr
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function f_initscr
!-----------------------------------------------------------------------------------------------------------------------------------
function ptr_curses_version() result (curses_version__OUT) bind(C, name='curses_version')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR) :: curses_version__OUT  ! const char *curses_version
end function ptr_curses_version
!-----------------------------------------------------------------------------------------------------------------------------------
function ptr_keyname(ich) result (keyname__OUT) bind(C, name='keyname')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR) :: keyname__OUT  ! const char *keyname
   integer(C_INT),intent(in),value :: ich
end function ptr_keyname
!-----------------------------------------------------------------------------------------------------------------------------------
function ptr_termname() result (termname__OUT) bind(C, name='termname')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR) :: termname__OUT  ! const char *termname
end function ptr_termname
!-----------------------------------------------------------------------------------------------------------------------------------
function ptr_longname() result (longname__OUT) bind(C, name='longname')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR) :: longname__OUT  ! const char *longname
end function ptr_longname
!-------------------------------------------------------------------------------
  end interface
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! requires a variable argument list
! kludge to allow fmt plus nothing, one int, one float, one character or one string
! multiple calls can accomplish most outputs; or do internal WRITE into a
! character variable and call addstr(3c)
!       int printw(const char *fmt, ...);
!       int wprintw(WINDOW *win, const char *fmt, ...);
!       int mvprintw(int y, int x, const char *fmt, ...);
!       int mvwprintw(WINDOW *win, int y, int x, const char *fmt, ...);
! make pseudo-generic procedures for C routines with variable argument lists
  INTERFACE MVPRINTW
     !PROCEDURE I_MVPRINTW, L_MVPRINTW, I2_MVPRINTW, N_MVPRINTW, R_MVPRINTW, S_MVPRINTW, II_MVPRINTW
!-------------------------------------------------------------------------------
     FUNCTION I_MVPRINTW (Y, X, FMT, I) RESULT(MyResult) BIND(C,NAME='i_mvprintw')
        USE ISO_C_BINDING
        integer(C_INT)        :: MyResult
        integer(C_INT), VALUE :: Y,X
        integer(C_INT), VALUE :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I_MVPRINTW
!-------------------------------------------------------------------------------
     FUNCTION I2_MVPRINTW (Y, X, FMT, I) RESULT(MyResult) BIND(C,NAME='i2_mvprintw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_INT),          VALUE :: Y,X
        integer(C_SHORT),        VALUE :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I2_MVPRINTW
!-------------------------------------------------------------------------------
     FUNCTION L_MVPRINTW (Y, X, FMT, I) RESULT(MyResult) BIND(C,NAME='l_mvprintw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_INT),          VALUE :: Y,X
        integer(C_LONG),         VALUE :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION L_MVPRINTW
!-------------------------------------------------------------------------------
     FUNCTION II_MVPRINTW (Y, X, FMT, I, J) RESULT(MyResult) BIND(C,NAME='ii_mvprintw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_INT), VALUE          :: Y,X
        integer(C_INT), VALUE          :: I
        integer(C_INT), VALUE          :: J
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION II_MVPRINTW
!-------------------------------------------------------------------------------
     FUNCTION R_MVPRINTW (Y, X, FMT, R) RESULT(MyResult) BIND(C,NAME='r_mvprintw')
        USE ISO_C_BINDING
        integer(C_INT)                :: MyResult
        integer(C_INT), VALUE         :: Y,X
        REAL (C_FLOAT), VALUE         :: R
        character(kind=c_char),INTENT(IN) :: FMT
     END FUNCTION R_MVPRINTW
!-------------------------------------------------------------------------------
     FUNCTION S_MVPRINTW (Y, X, FMT, STRING) RESULT(MyResult) BIND(C,NAME='s_mvprintw')
        USE ISO_C_BINDING
        integer(C_INT)                :: MyResult
        integer(C_INT), VALUE         :: Y,X
        character(kind=c_char),INTENT(IN) :: FMT
        character(kind=c_char),INTENT(IN) :: STRING
     END FUNCTION S_MVPRINTW
!-------------------------------------------------------------------------------
     FUNCTION N_MVPRINTW (Y, X, FMT) RESULT(MyResult) BIND(C,NAME='n_mvprintw')
        USE ISO_C_BINDING
        integer(C_INT)        :: MyResult
        integer(C_INT), VALUE :: Y,X
        ! THIS WORKS BUT ONLY DEFINES ONE CHARACTER LENGTH IS THIS A BUG OR A FEATURE?
        CHARACTER(KIND=C_CHAR),INTENT(IN) :: FMT
     END FUNCTION N_MVPRINTW
  END INTERFACE MVPRINTW
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  INTERFACE PRINTW
     !PROCEDURE I_PRINTW, L_PRINTW, I2_PRINTW, N_PRINTW, R_PRINTW, S_PRINTW, II_PRINTW
!-------------------------------------------------------------------------------
     FUNCTION I_PRINTW (FMT, I) RESULT(MyResult) BIND(C,NAME='i_printw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_INT), VALUE          :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I_PRINTW
!-------------------------------------------------------------------------------
     FUNCTION I2_PRINTW (FMT, I) RESULT(MyResult) BIND(C,NAME='i2_printw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_SHORT), VALUE        :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I2_PRINTW
!-------------------------------------------------------------------------------
     FUNCTION L_PRINTW (FMT, I) RESULT(MyResult) BIND(C,NAME='l_printw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_LONG), VALUE         :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION L_PRINTW
!-------------------------------------------------------------------------------
     FUNCTION II_PRINTW (FMT, I, J) RESULT(MyResult) BIND(C,NAME='ii_printw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_INT), VALUE          :: I
        integer(C_INT), VALUE          :: J
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION II_PRINTW
!-------------------------------------------------------------------------------
     FUNCTION R_PRINTW (FMT, R) RESULT(MyResult) BIND(C,NAME='r_printw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        REAL (C_FLOAT),VALUE           :: R
        character(kind=c_char),INTENT(IN)  :: FMT
     END FUNCTION R_PRINTW
!-------------------------------------------------------------------------------
     FUNCTION S_PRINTW (FMT, STRING) RESULT(MyResult) BIND(C,NAME='s_printw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        character(kind=c_char),INTENT(IN)  :: FMT
        character(kind=c_char),INTENT(IN)  :: STRING
     END FUNCTION S_PRINTW
!-------------------------------------------------------------------------------
     FUNCTION N_PRINTW (FMT) RESULT(MyResult) BIND(C,NAME='n_printw')
        USE ISO_C_BINDING
        integer(C_INT)                    :: MyResult
        CHARACTER(KIND=C_CHAR),INTENT(IN) :: FMT
     END FUNCTION N_PRINTW
  END INTERFACE PRINTW
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  INTERFACE mvwprintw
     !PROCEDURE I_mvwprintw, L_mvwprintw, I2_mvwprintw, N_mvwprintw, R_mvwprintw, S_mvwprintw, II_mvwprintw
!-------------------------------------------------------------------------------
     FUNCTION I_mvwprintw (WIN, Y, X, FMT, I) RESULT(MyResult) BIND(C,NAME='i_mvwprintw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)        :: MyResult
        integer(C_INT), VALUE :: Y,X
        integer(C_INT), VALUE :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I_mvwprintw
!-------------------------------------------------------------------------------
     FUNCTION I2_mvwprintw (WIN, Y, X, FMT, I) RESULT(MyResult) BIND(C,NAME='i2_mvwprintw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_INT),          VALUE :: Y,X
        integer(C_SHORT),        VALUE :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I2_mvwprintw
!-------------------------------------------------------------------------------
     FUNCTION L_mvwprintw (WIN, Y, X, FMT, I) RESULT(MyResult) BIND(C,NAME='l_mvwprintw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_INT),          VALUE :: Y,X
        integer(C_LONG),         VALUE :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION L_mvwprintw
!-------------------------------------------------------------------------------
     FUNCTION II_mvwprintw (WIN, Y, X, FMT, I, J) RESULT(MyResult) BIND(C,NAME='ii_mvwprintw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_INT), VALUE          :: Y,X
        integer(C_INT), VALUE          :: I
        integer(C_INT), VALUE          :: J
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION II_mvwprintw
!-------------------------------------------------------------------------------
     FUNCTION R_mvwprintw (WIN, Y, X, FMT, R) RESULT(MyResult) BIND(C,NAME='r_mvwprintw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                :: MyResult
        integer(C_INT), VALUE         :: Y,X
        REAL (C_FLOAT), VALUE         :: R
        character(kind=c_char),INTENT(IN) :: FMT
     END FUNCTION R_mvwprintw
!-------------------------------------------------------------------------------
     FUNCTION S_mvwprintw (WIN, Y, X, FMT, STRING) RESULT(MyResult) BIND(C,NAME='s_mvwprintw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                :: MyResult
        integer(C_INT), VALUE         :: Y,X
        character(kind=c_char),INTENT(IN) :: FMT
        character(kind=c_char),INTENT(IN) :: STRING
     END FUNCTION S_mvwprintw
!-------------------------------------------------------------------------------
     FUNCTION N_mvwprintw (WIN, Y, X, FMT) RESULT(MyResult) BIND(C,NAME='n_mvwprintw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)        :: MyResult
        integer(C_INT), VALUE :: Y,X
        ! THIS WORKS BUT ONLY DEFINES ONE CHARACTER LENGTH IS THIS A BUG OR A FEATURE?
        CHARACTER(KIND=C_CHAR),INTENT(IN) :: FMT
     END FUNCTION N_mvwprintw
  END INTERFACE mvwprintw
!-------------------------------------------------------------------------------
  INTERFACE wprintw
     !PROCEDURE I_wprintw, L_wprintw, I2_wprintw, N_wprintw, R_wprintw, S_wprintw, II_wprintw
!-------------------------------------------------------------------------------
     FUNCTION I_wprintw (WIN, FMT, I) RESULT(MyResult) BIND(C,NAME='i_wprintw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)        :: MyResult
        integer(C_INT), VALUE :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I_wprintw
!-------------------------------------------------------------------------------
     FUNCTION I2_wprintw (WIN, FMT, I) RESULT(MyResult) BIND(C,NAME='i2_wprintw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_SHORT),        VALUE :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I2_wprintw
!-------------------------------------------------------------------------------
     FUNCTION L_wprintw (WIN, FMT, I) RESULT(MyResult) BIND(C,NAME='l_wprintw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_LONG),         VALUE :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION L_wprintw
!-------------------------------------------------------------------------------
     FUNCTION II_wprintw (WIN, FMT, I, J) RESULT(MyResult) BIND(C,NAME='ii_wprintw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_INT), VALUE          :: I
        integer(C_INT), VALUE          :: J
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION II_wprintw
!-------------------------------------------------------------------------------
     FUNCTION R_wprintw (WIN, FMT, R) RESULT(MyResult) BIND(C,NAME='r_wprintw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                :: MyResult
        REAL (C_FLOAT), VALUE         :: R
        character(kind=c_char),INTENT(IN) :: FMT
     END FUNCTION R_wprintw
!-------------------------------------------------------------------------------
     FUNCTION S_wprintw (WIN, FMT, STRING) RESULT(MyResult) BIND(C,NAME='s_wprintw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                :: MyResult
        character(kind=c_char),INTENT(IN) :: FMT
        character(kind=c_char),INTENT(IN) :: STRING
     END FUNCTION S_wprintw
!-------------------------------------------------------------------------------
     FUNCTION N_wprintw (WIN, FMT) RESULT(MyResult) BIND(C,NAME='n_wprintw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)        :: MyResult
        ! THIS WORKS BUT ONLY DEFINES ONE CHARACTER LENGTH IS THIS A BUG OR A FEATURE?
        CHARACTER(KIND=C_CHAR),INTENT(IN) :: FMT
     END FUNCTION N_wprintw
  END INTERFACE wprintw
!-------------------------------------------------------------------------------
!------------------------------------------------------------------------------!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>!
!------------------------------------------------------------------------------!
  INTERFACE SCANW
!-------------------------------------------------------------------------------
     FUNCTION I_SCANW (FMT, I) RESULT(MyResult) BIND(C,NAME='i_scanw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_INT)                 :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I_SCANW
!-------------------------------------------------------------------------------
     FUNCTION I2_SCANW (FMT, I) RESULT(MyResult) BIND(C,NAME='i2_scanw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_SHORT)               :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I2_SCANW
!-------------------------------------------------------------------------------
     FUNCTION L_SCANW (FMT, I) RESULT(MyResult) BIND(C,NAME='l_scanw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_LONG)                :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION L_SCANW
!-------------------------------------------------------------------------------
     FUNCTION II_SCANW (FMT, I, J) RESULT(MyResult) BIND(C,NAME='ii_scanw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_INT)                 :: I
        integer(C_INT)                 :: J
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION II_SCANW
!-------------------------------------------------------------------------------
     FUNCTION R_SCANW (FMT, R) RESULT(MyResult) BIND(C,NAME='r_scanw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        REAL (C_FLOAT)                 :: R
        character(kind=c_char),INTENT(IN)  :: FMT
     END FUNCTION R_SCANW
!-------------------------------------------------------------------------------
     FUNCTION S_SCANW (FMT, STRING) RESULT(MyResult) BIND(C,NAME='s_scanw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        character(kind=c_char),INTENT(IN)  :: FMT
        character(kind=c_char),INTENT(IN)  :: STRING
     END FUNCTION S_SCANW
!-------------------------------------------------------------------------------
  END INTERFACE SCANW
!------------------------------------------------------------------------------!
  INTERFACE mvscanw
!-------------------------------------------------------------------------------
     FUNCTION I_mvscanw (Y,X,FMT, I) RESULT(MyResult) BIND(C,NAME='i_mvscanw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_INT)                 :: I
        integer(C_INT), VALUE          :: Y,X
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I_mvscanw
!-------------------------------------------------------------------------------
     FUNCTION I2_mvscanw (Y,X,FMT, I) RESULT(MyResult) BIND(C,NAME='i2_mvscanw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_SHORT)               :: I
        integer(C_INT), VALUE          :: Y,X
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I2_mvscanw
!-------------------------------------------------------------------------------
     FUNCTION L_mvscanw (Y,X,FMT, I) RESULT(MyResult) BIND(C,NAME='l_mvscanw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_LONG)                :: I
        integer(C_INT), VALUE          :: Y,X
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION L_mvscanw
!-------------------------------------------------------------------------------
     FUNCTION II_mvscanw (Y,X,FMT, I, J) RESULT(MyResult) BIND(C,NAME='ii_mvscanw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_INT)                 :: I
        integer(C_INT)                 :: J
        integer(C_INT), VALUE          :: Y,X
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION II_mvscanw
!-------------------------------------------------------------------------------
     FUNCTION R_mvscanw (Y,X,FMT, R) RESULT(MyResult) BIND(C,NAME='r_mvscanw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        REAL (C_FLOAT)                 :: R
        integer(C_INT), VALUE          :: Y,X
        character(kind=c_char),INTENT(IN)  :: FMT
     END FUNCTION R_mvscanw
!-------------------------------------------------------------------------------
     FUNCTION S_mvscanw (Y,X,FMT, STRING) RESULT(MyResult) BIND(C,NAME='s_mvscanw')
        USE ISO_C_BINDING
        integer(C_INT)                 :: MyResult
        integer(C_INT), VALUE          :: Y,X
        character(kind=c_char),INTENT(IN)  :: FMT
        character(kind=c_char),INTENT(IN)  :: STRING
     END FUNCTION S_mvscanw
!-------------------------------------------------------------------------------
  END INTERFACE mvscanw
!------------------------------------------------------------------------------!
  INTERFACE MVWSCANW
!-------------------------------------------------------------------------------
     FUNCTION I_mvwscanw (WIN,Y,X,FMT, I) RESULT(MyResult) BIND(C,NAME='i_mvwscanw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_INT)                 :: I
        integer(C_INT), VALUE          :: Y,X
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I_mvwscanw
!-------------------------------------------------------------------------------
     FUNCTION I2_mvwscanw (WIN,Y,X,FMT, I) RESULT(MyResult) BIND(C,NAME='i2_mvwscanw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_SHORT)               :: I
        integer(C_INT), VALUE          :: Y,X
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I2_mvwscanw
!-------------------------------------------------------------------------------
     FUNCTION L_mvwscanw (WIN,Y,X,FMT, I) RESULT(MyResult) BIND(C,NAME='l_mvwscanw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_LONG)                :: I
        integer(C_INT), VALUE          :: Y,X
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION L_mvwscanw
!-------------------------------------------------------------------------------
     FUNCTION II_mvwscanw (WIN,Y,X,FMT, I, J) RESULT(MyResult) BIND(C,NAME='ii_mvwscanw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_INT)                 :: I
        integer(C_INT)                 :: J
        integer(C_INT), VALUE          :: Y,X
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION II_mvwscanw
!-------------------------------------------------------------------------------
     FUNCTION R_mvwscanw (WIN,Y,X,FMT, R) RESULT(MyResult) BIND(C,NAME='r_mvwscanw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        REAL (C_FLOAT)                 :: R
        integer(C_INT), VALUE          :: Y,X
        character(kind=c_char),INTENT(IN)  :: FMT
     END FUNCTION R_mvwscanw
!-------------------------------------------------------------------------------
     FUNCTION S_mvwscanw (WIN,Y,X,FMT, STRING) RESULT(MyResult) BIND(C,NAME='s_mvwscanw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_INT), VALUE          :: Y,X
        character(kind=c_char),INTENT(IN)  :: FMT
        character(kind=c_char),INTENT(IN)  :: STRING
     END FUNCTION S_mvwscanw
!-------------------------------------------------------------------------------
  END INTERFACE MVWSCANW
!------------------------------------------------------------------------------!
  INTERFACE wscanw
!-------------------------------------------------------------------------------
     FUNCTION I_wscanw (WIN,FMT, I) RESULT(MyResult) BIND(C,NAME='i_wscanw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_INT)                 :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I_wscanw
!-------------------------------------------------------------------------------
     FUNCTION I2_wscanw (WIN,FMT, I) RESULT(MyResult) BIND(C,NAME='i2_wscanw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_SHORT)               :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION I2_wscanw
!-------------------------------------------------------------------------------
     FUNCTION L_wscanw (WIN,FMT, I) RESULT(MyResult) BIND(C,NAME='l_wscanw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_LONG)                :: I
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION L_wscanw
!-------------------------------------------------------------------------------
     FUNCTION II_wscanw (WIN,FMT, I, J) RESULT(MyResult) BIND(C,NAME='ii_wscanw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        integer(C_INT)                 :: I
        integer(C_INT)                 :: J
        character(kind=c_char), INTENT(IN) :: FMT
     END FUNCTION II_wscanw
!-------------------------------------------------------------------------------
     FUNCTION R_wscanw (WIN,FMT, R) RESULT(MyResult) BIND(C,NAME='r_wscanw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        REAL (C_FLOAT)                 :: R
        character(kind=c_char),INTENT(IN)  :: FMT
     END FUNCTION R_wscanw
!-------------------------------------------------------------------------------
     FUNCTION S_wscanw (WIN,FMT, STRING) RESULT(MyResult) BIND(C,NAME='s_wscanw')
        USE ISO_C_BINDING
        type(C_PTR) ,value:: win                  ! const WINDOW *win
        integer(C_INT)                 :: MyResult
        character(kind=c_char),INTENT(IN)  :: FMT
        character(kind=c_char),INTENT(IN)  :: STRING
     END FUNCTION S_wscanw
!-------------------------------------------------------------------------------
  END INTERFACE wscanw
!------------------------------------------------------------------------------!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>!
!------------------------------------------------------------------------------!
!-------------------------------------------------------------------------------
interface
!-------------------------------------------------------------------------------
! COMMENT: # NOTE: had to change (almost?) all CDEFs of "WINDOW *win" to "const WINDOW *win" for my rules to work
!-------------------------------------------------------------------------------
! COMMENT: # NOTE: had to change (almost?) all CDEFs of "void *" to "const void *" for my rules to work
!-------------------------------------------------------------------------------
! COMMENT: #-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! COMMENT: # getwin() and putwin() take a filename instead of a C FILE pointer.
!-------------------------------------------------------------------------------
! COMMENT: #-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! COMMENT: # make an initscr that sets up the standard global variables that are not
!-------------------------------------------------------------------------------
! COMMENT: # really set because they are macros
!-------------------------------------------------------------------------------
! CDEF: WINDOW *returnstd ( void );
function returnstd() result (returnstd__OUT) bind(C, name='returnstd')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR):: returnstd__OUT       ! WINDOW *returnstd
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function returnstd
!-------------------------------------------------------------------------------
! CDEF: WINDOW *returncur ( void );
function returncur() result (returncur__OUT) bind(C, name='returncur')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR):: returncur__OUT       ! WINDOW *returncur
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function returncur
!-------------------------------------------------------------------------------
! COMMENT: #WINDOW *initscr ( void );
!-------------------------------------------------------------------------------
! COMMENT: #-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! COMMENT: ################################################################################
!-------------------------------------------------------------------------------
! COMMENT: #curs_util(3X)
!-------------------------------------------------------------------------------
! CDEF: char *unctrl(chtype c);
function unctrl(c) result (unctrl__OUT) bind(C, name='unctrl')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   character(kind=c_char):: unctrl__OUT          ! char *unctrl
   integer(kind=chtype) ,value:: c                    ! chtype c
end function unctrl
!-------------------------------------------------------------------------------
! CDEF: void filter(void);
subroutine filter() bind(C, name='filter')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end subroutine filter
!-------------------------------------------------------------------------------
! CDEF: void nofilter(void);
subroutine nofilter() bind(C, name='nofilter')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end subroutine nofilter
!-------------------------------------------------------------------------------
! CDEF: void use_env(bool f);
subroutine use_env(f) bind(C, name='use_env')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) ,value:: f                    ! bool f
end subroutine use_env
!-------------------------------------------------------------------------------
! CDEF: void use_tioctl(bool f);
subroutine use_tioctl(f) bind(C, name='use_tioctl')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) ,value:: f                    ! bool f
end subroutine use_tioctl
!-------------------------------------------------------------------------------
! CDEF: int  delay_output(int ms);
function delay_output(ms) result (delay_output__OUT) bind(C, name='delay_output')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: delay_output__OUT    ! int  delay_output
   INTEGER(C_INT) ,value:: ms                   ! int ms
end function delay_output
!-------------------------------------------------------------------------------
! CDEF: int  flushinp(void);
function flushinp() result (flushinp__OUT) bind(C, name='flushinp')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: flushinp__OUT        ! int  flushinp
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function flushinp
!-------------------------------------------------------------------------------
! COMMENT: ################################################################################
!-------------------------------------------------------------------------------
! CDEF: int getmouse(MEVENT *event);
function getmouse(event) result (getmouse__OUT) bind(C, name='getmouse')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getmouse__OUT        ! int getmouse
   type(MEVENT):: event                ! MEVENT *event
end function getmouse
!-------------------------------------------------------------------------------
! CDEF: int ungetmouse(MEVENT *event);
function ungetmouse(event) result (ungetmouse__OUT) bind(C, name='ungetmouse')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: ungetmouse__OUT      ! int ungetmouse
   type(MEVENT):: event                ! MEVENT *event
end function ungetmouse
!-------------------------------------------------------------------------------
! CDEF: mmask_t mousemask(mmask_t newmask, mmask_t *oldmask);
function mousemask(newmask,oldmask) result (mousemask__OUT) bind(C, name='mousemask')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   integer(kind=mmask_t) :: mousemask__OUT       ! mmask_t mousemask
   integer(kind=mmask_t) ,value:: newmask              ! mmask_t newmask
   type(C_PTR):: oldmask              ! mmask_t *oldmask
end function mousemask
!-------------------------------------------------------------------------------
! COMMENT: #### change int* NN to int *NN to simplify things
!-------------------------------------------------------------------------------
! CDEF: bool has_mouse(void);
function has_mouse() result (has_mouse__OUT) bind(C, name='has_mouse')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: has_mouse__OUT       ! bool has_mouse
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function has_mouse
!-------------------------------------------------------------------------------
! CDEF: bool wenclose(const WINDOW *win, int y, int x);
function wenclose(win,y,x) result (wenclose__OUT) bind(C, name='wenclose')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: wenclose__OUT        ! bool wenclose
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
end function wenclose
!-------------------------------------------------------------------------------
! CDEF: bool mouse_trafo(int *pY, int *pX, bool to_screen);
function mouse_trafo(pY,pX,to_screen) result (mouse_trafo__OUT) bind(C, name='mouse_trafo')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: mouse_trafo__OUT     ! bool mouse_trafo
   integer(C_INT):: pY                   ! int *pY
   integer(C_INT):: pX                   ! int *pX
   logical(C_BOOL) ,value:: to_screen            ! bool to_screen
end function mouse_trafo
!-------------------------------------------------------------------------------
! CDEF: bool wmouse_trafo(const WINDOW *win, int *pY, int *pX, bool to_screen);
function wmouse_trafo(win,pY,pX,to_screen) result (wmouse_trafo__OUT) bind(C, name='wmouse_trafo')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: wmouse_trafo__OUT    ! bool wmouse_trafo
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(C_INT):: pY                   ! int *pY
   integer(C_INT):: pX                   ! int *pX
   logical(C_BOOL) ,value:: to_screen            ! bool to_screen
end function wmouse_trafo
!-------------------------------------------------------------------------------
! CDEF: int mouseinterval(int erval);
function mouseinterval(erval) result (mouseinterval__OUT) bind(C, name='mouseinterval')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mouseinterval__OUT   ! int mouseinterval
   INTEGER(C_INT) ,value:: erval                ! int erval
end function mouseinterval
!-------------------------------------------------------------------------------
! COMMENT: ################################################################################
!-------------------------------------------------------------------------------
! COMMENT: # fix:
!-------------------------------------------------------------------------------
! COMMENT: #WRAPPED: const char *curses_version(void);
!-------------------------------------------------------------------------------
! COMMENT: #WRAPPED: char *longname ( void );
!-------------------------------------------------------------------------------
! COMMENT: #WRAPPED: char *termname ( void );
!-------------------------------------------------------------------------------
! COMMENT: #WRAPPED: char *keyname ( int c );
!-------------------------------------------------------------------------------
! COMMENT: #int ripoffline ( int line, int (*init )(WINDOW *win, int));
!-------------------------------------------------------------------------------
! COMMENT: ################################################################################
!-------------------------------------------------------------------------------
! COMMENT: #WRAPPED: int printw    (                                  const char *fmt, ... );
!-------------------------------------------------------------------------------
! COMMENT: #WRAPPED: int mvprintw  (                    int y, int x, const char *fmt, ... );
!-------------------------------------------------------------------------------
! COMMENT: #WRAPPED: int wprintw   ( const WINDOW *win,               const char *fmt, ... );
!-------------------------------------------------------------------------------
! COMMENT: #WRAPPED: int mvwprintw ( const WINDOW *win, int y, int x, const char *fmt, ... );
!-------------------------------------------------------------------------------
! COMMENT: #int vw_printw ( const WINDOW *win,               const char *fmt, va_list varglist );
!-------------------------------------------------------------------------------
! COMMENT: #int vwprintw  ( const WINDOW *win,               const char *fmt, va_list varglist );
!-------------------------------------------------------------------------------
! COMMENT: ################################################################################
!-------------------------------------------------------------------------------
! COMMENT: #WRAPPED: #int scanw    (                                  char *fmt, ... );
!-------------------------------------------------------------------------------
! COMMENT: #WRAPPED: #int mvscanw  (                    int y, int x, char *fmt, ... );
!-------------------------------------------------------------------------------
! COMMENT: #WRAPPED: #int mvwscanw ( const WINDOW *win, int y, int x, char *fmt, ... );
!-------------------------------------------------------------------------------
! COMMENT: #WRAPPED: #int wscanw   ( const WINDOW *win,               char *fmt, ... );
!-------------------------------------------------------------------------------
! COMMENT: #int vw_scanw ( const WINDOW *win,               char *fmt, va_list varglist );
!-------------------------------------------------------------------------------
! COMMENT: #int vwscanw  ( const WINDOW *win,               char *fmt, va_list varglist );
!-------------------------------------------------------------------------------
! COMMENT: ################################################################################
!-------------------------------------------------------------------------------
! COMMENT: #char *key_name(wchar_t w);
!-------------------------------------------------------------------------------
! COMMENT: #wchar_t *wunctrl(cchar_t *c);
!-------------------------------------------------------------------------------
! COMMENT: #int getcchar ( const cchar_t *wcval, wchar_t *wch, attr_t *attrs, short *color_pair, const void *opts );
!-------------------------------------------------------------------------------
! COMMENT: #int setcchar ( cchar_t *wcval, const wchar_t *wch, const attr_t attrs, short color_pair, const void *opts );
!-------------------------------------------------------------------------------
! COMMENT: #int addwstr ( const wchar_t *wstr );
!-------------------------------------------------------------------------------
! COMMENT: #int echo_wchar ( const cchar_t *wch );
!-------------------------------------------------------------------------------
! COMMENT: #int erasewchar ( wchar_t *ch );
!-------------------------------------------------------------------------------
! COMMENT: #int innwstr ( wchar_t *str, int n );
!-------------------------------------------------------------------------------
! COMMENT: #int ins_nwstr ( const wchar_t *wstr, int n );
!-------------------------------------------------------------------------------
! COMMENT: #int ins_wstr ( const wchar_t *wstr );
!-------------------------------------------------------------------------------
! COMMENT: #int inwstr ( wchar_t *str );
!-------------------------------------------------------------------------------
! COMMENT: #int killwchar ( wchar_t *ch );
!-------------------------------------------------------------------------------
! COMMENT: #int mvaddnwstr ( int y, int x, const wchar_t *wstr, int n );
!-------------------------------------------------------------------------------
! COMMENT: #int mvaddwstr ( int y, int x, const wchar_t *wstr );
!-------------------------------------------------------------------------------
! COMMENT: #int mvinnwstr ( int y, int x, wchar_t *str, int n );
!-------------------------------------------------------------------------------
! COMMENT: #int mvins_nwstr ( int y, int x, const wchar_t *wstr, int n );
!-------------------------------------------------------------------------------
! COMMENT: #int mvins_wstr ( int y, int x, const wchar_t *wstr );
!-------------------------------------------------------------------------------
! COMMENT: #int mvinwstr ( int y, int x, wchar_t *str );
!-------------------------------------------------------------------------------
! COMMENT: #int mvwaddnwstr ( const WINDOW *win, int y, int x, const wchar_t *wstr, int n );
!-------------------------------------------------------------------------------
! COMMENT: #int mvwaddwstr ( const WINDOW *win, int y, int x, const wchar_t *wstr );
!-------------------------------------------------------------------------------
! COMMENT: #int mvwinnwstr ( const WINDOW *win, int y, int x, wchar_t *str, int n );
!-------------------------------------------------------------------------------
! COMMENT: #int mvwins_nwstr ( const WINDOW *win, int y, int x, const wchar_t *wstr, int n );
!-------------------------------------------------------------------------------
! COMMENT: #int mvwins_wstr ( const WINDOW *win, int y, int x, const wchar_t *wstr );
!-------------------------------------------------------------------------------
! COMMENT: #int mvwinwstr ( const WINDOW *win, int y, int x, wchar_t *str );
!-------------------------------------------------------------------------------
! COMMENT: #int pecho_wchar ( const WINDOW *pad, const cchar_t *wch );
!-------------------------------------------------------------------------------
! COMMENT: #int slk_wset ( int labnum, const wchar_t *label, int fmt );
!-------------------------------------------------------------------------------
! COMMENT: #int unget_wch ( const wchar_t wch );
!-------------------------------------------------------------------------------
! COMMENT: #int waddnwstr ( const WINDOW *win, const wchar_t *wstr, int n );
!-------------------------------------------------------------------------------
! COMMENT: #int waddwstr ( const WINDOW *win, const wchar_t *wstr );
!-------------------------------------------------------------------------------
! COMMENT: #int wecho_wchar ( const WINDOW *win, const cchar_t *wch );
!-------------------------------------------------------------------------------
! COMMENT: #int winnwstr ( const WINDOW *win, wchar_t *str, int n );
!-------------------------------------------------------------------------------
! COMMENT: #int wins_nwstr ( const WINDOW *win, const wchar_t *wstr, int n );
!-------------------------------------------------------------------------------
! COMMENT: #int wins_wstr ( const WINDOW *win, const wchar_t *wstr );
!-------------------------------------------------------------------------------
! COMMENT: #int winwstr ( const WINDOW *win, wchar_t *str );
!-------------------------------------------------------------------------------
! COMMENT: #wchar_t *wunctrl ( cchar_t *c );
!-------------------------------------------------------------------------------
! COMMENT: #int addnwstr ( const wchar_t *wstr, int n );
!-------------------------------------------------------------------------------
! COMMENT: ################################################################################
!-------------------------------------------------------------------------------
! CDEF: int box ( const WINDOW *win, chtype verch, chtype horch );
function box(win,verch,horch) result (box__OUT) bind(C, name='box')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: box__OUT             ! int box
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=chtype) ,value:: verch                ! chtype verch
   integer(kind=chtype) ,value:: horch                ! chtype horch
end function box
!-------------------------------------------------------------------------------
! CDEF: int wrefresh ( const WINDOW *win );
function wrefresh(win) result (wrefresh__OUT) bind(C, name='wrefresh')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wrefresh__OUT        ! int wrefresh
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function wrefresh
!-------------------------------------------------------------------------------
! CDEF: int keypad ( const WINDOW *win, bool bf );
function keypad(win,bf) result (keypad__OUT) bind(C, name='keypad')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: keypad__OUT          ! int keypad
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   logical(C_BOOL) ,value:: bf                   ! bool bf
end function keypad
!-------------------------------------------------------------------------------
! CDEF: int mvaddstr ( int y, int x, const char *str );
function mvaddstr(y,x,str) result (mvaddstr__OUT) bind(C, name='mvaddstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvaddstr__OUT        ! int mvaddstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char) ,intent(in):: str                  ! const char *str
end function mvaddstr
!-------------------------------------------------------------------------------
! CDEF: attr_t slk_attr ( void );
function slk_attr() result (slk_attr__OUT) bind(C, name='slk_attr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   integer(kind=attr_t) :: slk_attr__OUT        ! attr_t slk_attr
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function slk_attr
!-------------------------------------------------------------------------------
! CDEF: attr_t term_attrs ( void );
function term_attrs() result (term_attrs__OUT) bind(C, name='term_attrs')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   integer(kind=attr_t) :: term_attrs__OUT      ! attr_t term_attrs
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function term_attrs
!-------------------------------------------------------------------------------
! CDEF: int attr_get ( attr_t *attrs, short *pair, const void *opts )
function attr_get(attrs,pair,opts) result (attr_get__OUT) bind(C, name='attr_get')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: attr_get__OUT        ! int attr_get
   type(C_PTR):: attrs                ! attr_t *attrs
   integer(C_SHORT):: pair                 ! short *pair
   type(C_PTR) ,value:: opts                 ! const void *opts
end function attr_get
!-------------------------------------------------------------------------------
! CDEF: int attr_off ( attr_t attrs, const void *opts );
function attr_off(attrs,opts) result (attr_off__OUT) bind(C, name='attr_off')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: attr_off__OUT        ! int attr_off
   integer(kind=attr_t) ,value:: attrs                ! attr_t attrs
   type(C_PTR) ,value:: opts                 ! const void *opts
end function attr_off
!-------------------------------------------------------------------------------
! CDEF: int attr_on ( attr_t attrs, const void *opts );
function attr_on(attrs,opts) result (attr_on__OUT) bind(C, name='attr_on')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: attr_on__OUT         ! int attr_on
   integer(kind=attr_t) ,value:: attrs                ! attr_t attrs
   type(C_PTR) ,value:: opts                 ! const void *opts
end function attr_on
!-------------------------------------------------------------------------------
! CDEF: int attr_set ( attr_t attrs, short pair, const void *opts )
function attr_set(attrs,pair,opts) result (attr_set__OUT) bind(C, name='attr_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: attr_set__OUT        ! int attr_set
   integer(kind=attr_t) ,value:: attrs                ! attr_t attrs
   integer(C_SHORT) ,value:: pair                 ! short pair
   type(C_PTR) ,value:: opts                 ! const void *opts
end function attr_set
!-------------------------------------------------------------------------------
! CDEF: int chgat ( int n, attr_t attr, short color, const void *opts )
function chgat(n,attr,color,opts) result (chgat__OUT) bind(C, name='chgat')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: chgat__OUT           ! int chgat
   INTEGER(C_INT) ,value:: n                    ! int n
   integer(kind=attr_t) ,value:: attr                 ! attr_t attr
   integer(C_SHORT) ,value:: color                ! short color
   type(C_PTR) ,value:: opts                 ! const void *opts
end function chgat
!-------------------------------------------------------------------------------
! CDEF: int mvchgat ( int y, int x, int n, attr_t attr, short color, const void *opts )
function mvchgat(y,x,n,attr,color,opts) result (mvchgat__OUT) bind(C, name='mvchgat')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvchgat__OUT         ! int mvchgat
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   INTEGER(C_INT) ,value:: n                    ! int n
   integer(kind=attr_t) ,value:: attr                 ! attr_t attr
   integer(C_SHORT) ,value:: color                ! short color
   type(C_PTR) ,value:: opts                 ! const void *opts
end function mvchgat
!-------------------------------------------------------------------------------
! CDEF: int mvwchgat ( const WINDOW *win, int y, int x, int n, attr_t attr, short color, const void *opts )
function mvwchgat(win,y,x,n,attr,color,opts) result (mvwchgat__OUT) bind(C, name='mvwchgat')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwchgat__OUT        ! int mvwchgat
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   INTEGER(C_INT) ,value:: n                    ! int n
   integer(kind=attr_t) ,value:: attr                 ! attr_t attr
   integer(C_SHORT) ,value:: color                ! short color
   type(C_PTR) ,value:: opts                 ! const void *opts
end function mvwchgat
!-------------------------------------------------------------------------------
! CDEF: int slk_attr_off ( const attr_t attrs, const void *opts );
function slk_attr_off(attrs,opts) result (slk_attr_off__OUT) bind(C, name='slk_attr_off')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: slk_attr_off__OUT    ! int slk_attr_off
   integer(kind=attr_t) , value, intent(in):: attrs                ! const attr_t attrs
   type(C_PTR) ,value:: opts                 ! const void *opts
end function slk_attr_off
!-------------------------------------------------------------------------------
! CDEF: int slk_attr_on ( attr_t attrs, const void *opts );
function slk_attr_on(attrs,opts) result (slk_attr_on__OUT) bind(C, name='slk_attr_on')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: slk_attr_on__OUT     ! int slk_attr_on
   integer(kind=attr_t) ,value:: attrs                ! attr_t attrs
   type(C_PTR) ,value:: opts                 ! const void *opts
end function slk_attr_on
!-------------------------------------------------------------------------------
! CDEF: int slk_attr_set ( const attr_t attrs, short color_pair, const void *opts );
function slk_attr_set(attrs,color_pair,opts) result (slk_attr_set__OUT) bind(C, name='slk_attr_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: slk_attr_set__OUT    ! int slk_attr_set
   integer(kind=attr_t) , value, intent(in):: attrs                ! const attr_t attrs
   integer(C_SHORT) ,value:: color_pair           ! short color_pair
   type(C_PTR) ,value:: opts                 ! const void *opts
end function slk_attr_set
!-------------------------------------------------------------------------------
! CDEF: int wattr_get ( const WINDOW *win, attr_t *attrs, short *pair, const void *opts );
function wattr_get(win,attrs,pair,opts) result (wattr_get__OUT) bind(C, name='wattr_get')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wattr_get__OUT       ! int wattr_get
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR):: attrs                ! attr_t *attrs
   integer(C_SHORT):: pair                 ! short *pair
   type(C_PTR) ,value:: opts                 ! const void *opts
end function wattr_get
!-------------------------------------------------------------------------------
! CDEF: int wattr_off ( const WINDOW *win, attr_t attrs, const void *opts );
function wattr_off(win,attrs,opts) result (wattr_off__OUT) bind(C, name='wattr_off')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wattr_off__OUT       ! int wattr_off
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=attr_t) ,value:: attrs                ! attr_t attrs
   type(C_PTR) ,value:: opts                 ! const void *opts
end function wattr_off
!-------------------------------------------------------------------------------
! CDEF: int wattr_on ( const WINDOW *win, attr_t attrs, const void *opts );
function wattr_on(win,attrs,opts) result (wattr_on__OUT) bind(C, name='wattr_on')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wattr_on__OUT        ! int wattr_on
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=attr_t) ,value:: attrs                ! attr_t attrs
   type(C_PTR) ,value:: opts                 ! const void *opts
end function wattr_on
!-------------------------------------------------------------------------------
! CDEF: int wattr_set ( const WINDOW *win, attr_t attrs, short pair, const void *opts );
function wattr_set(win,attrs,pair,opts) result (wattr_set__OUT) bind(C, name='wattr_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wattr_set__OUT       ! int wattr_set
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=attr_t) ,value:: attrs                ! attr_t attrs
   integer(C_SHORT) ,value:: pair                 ! short pair
   type(C_PTR) ,value:: opts                 ! const void *opts
end function wattr_set
!-------------------------------------------------------------------------------
! CDEF: int wchgat ( const WINDOW *win, int n, attr_t attr, short color, const void *opts )
function wchgat(win,n,attr,color,opts) result (wchgat__OUT) bind(C, name='wchgat')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wchgat__OUT          ! int wchgat
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: n                    ! int n
   integer(kind=attr_t) ,value:: attr                 ! attr_t attr
   integer(C_SHORT) ,value:: color                ! short color
   type(C_PTR) ,value:: opts                 ! const void *opts
end function wchgat
!-------------------------------------------------------------------------------
! COMMENT: #######################################################
!-------------------------------------------------------------------------------
! CDEF: int scr_dump ( const char *filename );
function scr_dump(filename) result (scr_dump__OUT) bind(C, name='scr_dump')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: scr_dump__OUT        ! int scr_dump
   character(kind=c_char) ,intent(in):: filename             ! const char *filename
end function scr_dump
!-------------------------------------------------------------------------------
! CDEF: int scr_init ( const char *filename );
function scr_init(filename) result (scr_init__OUT) bind(C, name='scr_init')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: scr_init__OUT        ! int scr_init
   character(kind=c_char) ,intent(in):: filename             ! const char *filename
end function scr_init
!-------------------------------------------------------------------------------
! CDEF: int scr_restore ( const char *filename );
function scr_restore(filename) result (scr_restore__OUT) bind(C, name='scr_restore')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: scr_restore__OUT     ! int scr_restore
   character(kind=c_char) ,intent(in):: filename             ! const char *filename
end function scr_restore
!-------------------------------------------------------------------------------
! CDEF: int scr_set ( const char *filename );
function scr_set(filename) result (scr_set__OUT) bind(C, name='scr_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: scr_set__OUT         ! int scr_set
   character(kind=c_char) ,intent(in):: filename             ! const char *filename
end function scr_set
!-------------------------------------------------------------------------------
! COMMENT: #######################################################
!-------------------------------------------------------------------------------
! CDEF: bool can_change_color ( void );
function can_change_color() result (can_change_color__OUT) bind(C, name='can_change_color')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: can_change_color__OUT ! bool can_change_color
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function can_change_color
!-------------------------------------------------------------------------------
! CDEF: bool has_colors ( void );
function has_colors() result (has_colors__OUT) bind(C, name='has_colors')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: has_colors__OUT      ! bool has_colors
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function has_colors
!-------------------------------------------------------------------------------
! CDEF: bool has_ic ( void );
function has_ic() result (has_ic__OUT) bind(C, name='has_ic')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: has_ic__OUT          ! bool has_ic
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function has_ic
!-------------------------------------------------------------------------------
! CDEF: bool has_il ( void );
function has_il() result (has_il__OUT) bind(C, name='has_il')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: has_il__OUT          ! bool has_il
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function has_il
!-------------------------------------------------------------------------------
! CDEF: bool is_cleared ( const WINDOW *win );
function is_cleared(win) result (is_cleared__OUT) bind(C, name='is_cleared')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_cleared__OUT      ! bool is_cleared
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function is_cleared
!-------------------------------------------------------------------------------
! CDEF: bool is_idcok ( const WINDOW *win );
function is_idcok(win) result (is_idcok__OUT) bind(C, name='is_idcok')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_idcok__OUT        ! bool is_idcok
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function is_idcok
!-------------------------------------------------------------------------------
! CDEF: bool is_idlok ( const WINDOW *win );
function is_idlok(win) result (is_idlok__OUT) bind(C, name='is_idlok')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_idlok__OUT        ! bool is_idlok
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function is_idlok
!-------------------------------------------------------------------------------
! CDEF: bool is_immedok ( const WINDOW *win );
function is_immedok(win) result (is_immedok__OUT) bind(C, name='is_immedok')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_immedok__OUT      ! bool is_immedok
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function is_immedok
!-------------------------------------------------------------------------------
! CDEF: bool is_keypad ( const WINDOW *win );
function is_keypad(win) result (is_keypad__OUT) bind(C, name='is_keypad')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_keypad__OUT       ! bool is_keypad
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function is_keypad
!-------------------------------------------------------------------------------
! CDEF: bool is_leaveok ( const WINDOW *win );
function is_leaveok(win) result (is_leaveok__OUT) bind(C, name='is_leaveok')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_leaveok__OUT      ! bool is_leaveok
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function is_leaveok
!-------------------------------------------------------------------------------
! CDEF: bool is_nodelay ( const WINDOW *win );
function is_nodelay(win) result (is_nodelay__OUT) bind(C, name='is_nodelay')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_nodelay__OUT      ! bool is_nodelay
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function is_nodelay
!-------------------------------------------------------------------------------
! CDEF: bool is_notimeout ( const WINDOW *win );
function is_notimeout(win) result (is_notimeout__OUT) bind(C, name='is_notimeout')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_notimeout__OUT    ! bool is_notimeout
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function is_notimeout
!-------------------------------------------------------------------------------
! CDEF: bool is_pad ( const WINDOW *win );
function is_pad(win) result (is_pad__OUT) bind(C, name='is_pad')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_pad__OUT          ! bool is_pad
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function is_pad
!-------------------------------------------------------------------------------
! CDEF: bool is_scrollok ( const WINDOW *win );
function is_scrollok(win) result (is_scrollok__OUT) bind(C, name='is_scrollok')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_scrollok__OUT     ! bool is_scrollok
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function is_scrollok
!-------------------------------------------------------------------------------
! CDEF: bool is_subwin ( const WINDOW *win );
function is_subwin(win) result (is_subwin__OUT) bind(C, name='is_subwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_subwin__OUT       ! bool is_subwin
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function is_subwin
!-------------------------------------------------------------------------------
! CDEF: bool is_syncok ( const WINDOW *win );
function is_syncok(win) result (is_syncok__OUT) bind(C, name='is_syncok')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_syncok__OUT       ! bool is_syncok
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function is_syncok
!-------------------------------------------------------------------------------
! CDEF: bool is_term_resized ( int lines, int columns );
function is_term_resized(lines,columns) result (is_term_resized__OUT) bind(C, name='is_term_resized')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_term_resized__OUT ! bool is_term_resized
   INTEGER(C_INT) ,value:: lines                ! int lines
   INTEGER(C_INT) ,value:: columns              ! int columns
end function is_term_resized
!-------------------------------------------------------------------------------
! CDEF: bool is_linetouched ( const WINDOW *win, int line );
function is_linetouched(win,line) result (is_linetouched__OUT) bind(C, name='is_linetouched')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_linetouched__OUT  ! bool is_linetouched
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: line                 ! int line
end function is_linetouched
!-------------------------------------------------------------------------------
! CDEF: bool is_wintouched ( const WINDOW *win );
function is_wintouched(win) result (is_wintouched__OUT) bind(C, name='is_wintouched')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: is_wintouched__OUT   ! bool is_wintouched
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function is_wintouched
!-------------------------------------------------------------------------------
! CDEF: bool isendwin ( void );
function isendwin() result (isendwin__OUT) bind(C, name='isendwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   logical(C_BOOL) :: isendwin__OUT        ! bool isendwin
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function isendwin
!-------------------------------------------------------------------------------
! CDEF: char *keybound ( int keycode, int count );
function keybound(keycode,count) result (keybound__OUT) bind(C, name='keybound')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   character(kind=c_char):: keybound__OUT        ! char *keybound
   INTEGER(C_INT) ,value:: keycode              ! int keycode
   INTEGER(C_INT) ,value:: count                ! int count
end function keybound
!-------------------------------------------------------------------------------
! CDEF: char *slk_label ( int labnum );
function slk_label(labnum) result (slk_label__OUT) bind(C, name='slk_label')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   character(kind=c_char):: slk_label__OUT       ! char *slk_label
   INTEGER(C_INT) ,value:: labnum               ! int labnum
end function slk_label
!-------------------------------------------------------------------------------
! CDEF: char erasechar ( void );
function erasechar() result (erasechar__OUT) bind(C, name='erasechar')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   character(kind=c_char) :: erasechar__OUT       ! char erasechar
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function erasechar
!-------------------------------------------------------------------------------
! CDEF: char killchar ( void );
function killchar() result (killchar__OUT) bind(C, name='killchar')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   character(kind=c_char) :: killchar__OUT        ! char killchar
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function killchar
!-------------------------------------------------------------------------------
! CDEF: chtype getbkgd ( const WINDOW *win );
function getbkgd(win) result (getbkgd__OUT) bind(C, name='getbkgd')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   integer(kind=chtype) :: getbkgd__OUT         ! chtype getbkgd
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function getbkgd
!-------------------------------------------------------------------------------
! COMMENT: #######################################################
!-------------------------------------------------------------------------------
! CDEF: chtype winch ( const WINDOW *win );
function winch(win) result (winch__OUT) bind(C, name='winch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   integer(kind=chtype) :: winch__OUT           ! chtype winch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function winch
!-------------------------------------------------------------------------------
! CDEF: chtype mvwinch ( const WINDOW *win, int y, int x );
function mvwinch(win,y,x) result (mvwinch__OUT) bind(C, name='mvwinch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   integer(kind=chtype) :: mvwinch__OUT         ! chtype mvwinch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
end function mvwinch
!-------------------------------------------------------------------------------
! CDEF: chtype inch ( void );
function inch() result (inch__OUT) bind(C, name='inch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   integer(kind=chtype) :: inch__OUT            ! chtype inch
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function inch
!-------------------------------------------------------------------------------
! CDEF: chtype termattrs ( void );
function termattrs() result (termattrs__OUT) bind(C, name='termattrs')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   integer(kind=chtype) :: termattrs__OUT       ! chtype termattrs
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function termattrs
!-------------------------------------------------------------------------------
! CDEF: chtype mvinch ( int y, int x );
function mvinch(y,x) result (mvinch__OUT) bind(C, name='mvinch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   integer(kind=chtype) :: mvinch__OUT          ! chtype mvinch
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
end function mvinch
!-------------------------------------------------------------------------------
! CDEF: int assume_default_colors ( int fg, int bg );
function assume_default_colors(fg,bg) result (assume_default_colors__OUT) bind(C, name='assume_default_colors')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: assume_default_colors__OUT ! int assume_default_colors
   INTEGER(C_INT) ,value:: fg                   ! int fg
   INTEGER(C_INT) ,value:: bg                   ! int bg
end function assume_default_colors
!-------------------------------------------------------------------------------
! CDEF: int add_wch ( const cchar_t *wch );
function add_wch(wch) result (add_wch__OUT) bind(C, name='add_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: add_wch__OUT            ! int add_wch
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
end function add_wch
!-------------------------------------------------------------------------------
! CDEF: int add_wchstr ( const cchar_t *wchstr );
function add_wchstr(wchstr) result (add_wchstr__OUT) bind(C, name='add_wchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: add_wchstr__OUT         ! int add_wchstr
   type(C_PTR) ,value:: wchstr               ! const cchar_t *wchstr
end function add_wchstr
!-------------------------------------------------------------------------------
! CDEF: int addch ( const chtype ch );
function addch(ch) result (addch__OUT) bind(C, name='addch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: addch__OUT              ! int addch
   integer(kind=chtype) , value, intent(in):: ch                   ! const chtype ch
end function addch
!-------------------------------------------------------------------------------
! CDEF: int addchstr ( const chtype *chstr );
function addchstr(chstr) result (addchstr__OUT) bind(C, name='addchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: addchstr__OUT           ! int addchstr
   type(C_PTR) ,value:: chstr                ! const chtype *chstr
end function addchstr
!-------------------------------------------------------------------------------
! CDEF: int addstr ( const char *str );
function addstr(str) result (addstr__OUT) bind(C, name='addstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: addstr__OUT             ! int addstr
   character(kind=c_char) ,intent(in):: str       ! const char *str
end function addstr
!-------------------------------------------------------------------------------
! CDEF: int addnstr ( const char *str, int n );
function addnstr(str,n) result (addnstr__OUT) bind(C, name='addnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: addnstr__OUT            ! int addnstr
   character(kind=c_char) ,intent(in):: str       ! const char *str
   INTEGER(C_INT) ,value:: n                 ! int n
end function addnstr
!-------------------------------------------------------------------------------
! CDEF: int addchnstr ( const chtype *chstr, int n );
function addchnstr(chstr,n) result (addchnstr__OUT) bind(C, name='addchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: addchnstr__OUT          ! int addchnstr
   type(C_PTR) ,value:: chstr                ! const chtype *chstr
   INTEGER(C_INT) ,value:: n                 ! int n
end function addchnstr
!-------------------------------------------------------------------------------
! CDEF: int add_wchnstr ( const cchar_t *wchstr, int n );
function add_wchnstr(wchstr,n) result (add_wchnstr__OUT) bind(C, name='add_wchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: add_wchnstr__OUT        ! int add_wchnstr
   type(C_PTR) ,value:: wchstr               ! const cchar_t *wchstr
   INTEGER(C_INT) ,value:: n                 ! int n
end function add_wchnstr
!-------------------------------------------------------------------------------
! COMMENT: ################################################################################
!-------------------------------------------------------------------------------
! COMMENT: # man(1) pages shows 'int attrs' which does not work; others show 'chtype attrs' and 'attr_t attrs'
!-------------------------------------------------------------------------------
! COMMENT: # may depend on build options ??? or just a typo?
!-------------------------------------------------------------------------------
! COMMENT: #MANPAGE ERROR? int attroff ( int attrs );
!-------------------------------------------------------------------------------
! COMMENT: #MANPAGE ERROR? int attron ( int attrs );
!-------------------------------------------------------------------------------
! COMMENT: #MANPAGE ERROR? int wattron ( const WINDOW *win, int attrs );
!-------------------------------------------------------------------------------
! COMMENT: #MANPAGE ERROR? int wattroff ( const WINDOW *win, int attrs );
!-------------------------------------------------------------------------------
! COMMENT: #MANPAGE ERROR? int wattrset ( const WINDOW *win, int attrs );
!-------------------------------------------------------------------------------
! COMMENT: #MANPAGE ERROR? int attrset ( int attrs );
!-------------------------------------------------------------------------------
! CDEF: int attron ( chtype attrs );
function attron(attrs) result (attron__OUT) bind(C, name='attron')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: attron__OUT          ! int attron
   integer(kind=chtype) ,value:: attrs                ! chtype attrs
end function attron
!-------------------------------------------------------------------------------
! CDEF: int attroff ( chtype attrs );
function attroff(attrs) result (attroff__OUT) bind(C, name='attroff')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: attroff__OUT         ! int attroff
   integer(kind=chtype) ,value:: attrs                ! chtype attrs
end function attroff
!-------------------------------------------------------------------------------
! CDEF: int wattron ( const WINDOW *win, chtype attrs );
function wattron(win,attrs) result (wattron__OUT) bind(C, name='wattron')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wattron__OUT         ! int wattron
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=chtype) ,value:: attrs                ! chtype attrs
end function wattron
!-------------------------------------------------------------------------------
! CDEF: int wattroff ( const WINDOW *win, chtype attrs );
function wattroff(win,attrs) result (wattroff__OUT) bind(C, name='wattroff')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wattroff__OUT        ! int wattroff
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=chtype) ,value:: attrs                ! chtype attrs
end function wattroff
!-------------------------------------------------------------------------------
! CDEF: int wattrset ( const WINDOW *win, chtype attrs );
function wattrset(win,attrs) result (wattrset__OUT) bind(C, name='wattrset')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wattrset__OUT        ! int wattrset
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=chtype) ,value:: attrs                ! chtype attrs
end function wattrset
!-------------------------------------------------------------------------------
! CDEF: int attrset ( chtype attrs );
function attrset(attrs) result (attrset__OUT) bind(C, name='attrset')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: attrset__OUT         ! int attrset
   integer(kind=chtype) ,value:: attrs                ! chtype attrs
end function attrset
!-------------------------------------------------------------------------------
! COMMENT: ################################################################################
!-------------------------------------------------------------------------------
! CDEF: int slk_attrset ( const chtype attrs );
function slk_attrset(attrs) result (slk_attrset__OUT) bind(C, name='slk_attrset')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: slk_attrset__OUT     ! int slk_attrset
   integer(kind=chtype) , value, intent(in):: attrs                ! const chtype attrs
end function slk_attrset
!-------------------------------------------------------------------------------
! CDEF: int baudrate ( void );
function baudrate() result (baudrate__OUT) bind(C, name='baudrate')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: baudrate__OUT        ! int baudrate
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function baudrate
!-------------------------------------------------------------------------------
! CDEF: int beep ( void );
function beep() result (beep__OUT) bind(C, name='beep')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: beep__OUT            ! int beep
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function beep
!-------------------------------------------------------------------------------
! CDEF: int bkgd ( chtype ch );
function bkgd(ch) result (bkgd__OUT) bind(C, name='bkgd')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: bkgd__OUT            ! int bkgd
   integer(kind=chtype) ,value:: ch                   ! chtype ch
end function bkgd
!-------------------------------------------------------------------------------
! CDEF: int bkgrnd ( const cchar_t *wch );
function bkgrnd(wch) result (bkgrnd__OUT) bind(C, name='bkgrnd')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: bkgrnd__OUT          ! int bkgrnd
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
end function bkgrnd
!-------------------------------------------------------------------------------
! CDEF: int border ( chtype ls, chtype rs, chtype ts, chtype bs, chtype tl, chtype tr, chtype bl, chtype br );
function border(ls,rs,ts,bs,tl,tr,bl,br) result (border__OUT) bind(C, name='border')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: border__OUT          ! int border
   integer(kind=chtype) ,value:: ls                   ! chtype ls
   integer(kind=chtype) ,value:: rs                   ! chtype rs
   integer(kind=chtype) ,value:: ts                   ! chtype ts
   integer(kind=chtype) ,value:: bs                   ! chtype bs
   integer(kind=chtype) ,value:: tl                   ! chtype tl
   integer(kind=chtype) ,value:: tr                   ! chtype tr
   integer(kind=chtype) ,value:: bl                   ! chtype bl
   integer(kind=chtype) ,value:: br                   ! chtype br
end function border
!-------------------------------------------------------------------------------
! CDEF: int border_set ( const cchar_t *ls, const cchar_t *rs, const cchar_t *ts, const cchar_t *bs, const cchar_t *tl, const cchar_t *tr, const cchar_t *bl, const cchar_t *br );
function border_set(ls,rs,ts,bs,tl,tr,bl,br) result (border_set__OUT) bind(C, name='border_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: border_set__OUT         ! int border_set
   type(C_PTR) ,value:: ls                   ! const cchar_t *ls
   type(C_PTR) ,value:: rs                   ! const cchar_t *rs
   type(C_PTR) ,value:: ts                   ! const cchar_t *ts
   type(C_PTR) ,value:: bs                   ! const cchar_t *bs
   type(C_PTR) ,value:: tl                   ! const cchar_t *tl
   type(C_PTR) ,value:: tr                   ! const cchar_t *tr
   type(C_PTR) ,value:: bl                   ! const cchar_t *bl
   type(C_PTR) ,value:: br                   ! const cchar_t *br
end function border_set
!-------------------------------------------------------------------------------
! CDEF: int box_set ( const WINDOW *win, const cchar_t *verch, const cchar_t *horch );
function box_set(win,verch,horch) result (box_set__OUT) bind(C, name='box_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: box_set__OUT            ! int box_set
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR) ,value:: verch                ! const cchar_t *verch
   type(C_PTR) ,value:: horch                ! const cchar_t *horch
end function box_set
!-------------------------------------------------------------------------------
! CDEF: int cbreak ( void );
function cbreak() result (cbreak__OUT) bind(C, name='cbreak')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: cbreak__OUT             ! int cbreak
   !!!!!!!!!!!!!!!!!!!!!:: void              ! void
end function cbreak
!-------------------------------------------------------------------------------
! CDEF: int clear ( void );
function clear() result (clear__OUT) bind(C, name='clear')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: clear__OUT              ! int clear
   !!!!!!!!!!!!!!!!!!!!!:: void              ! void
end function clear
!-------------------------------------------------------------------------------
! CDEF: int clearok ( const WINDOW *win, bool bf );
function clearok(win,bf) result (clearok__OUT) bind(C, name='clearok')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: clearok__OUT            ! int clearok
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   logical(C_BOOL) ,value:: bf               ! bool bf
end function clearok
!-------------------------------------------------------------------------------
! CDEF: int clrtobot ( void );
function clrtobot() result (clrtobot__OUT) bind(C, name='clrtobot')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: clrtobot__OUT           ! int clrtobot
   !!!!!!!!!!!!!!!!!!!!!:: void              ! void
end function clrtobot
!-------------------------------------------------------------------------------
! CDEF: int clrtoeol ( void );
function clrtoeol() result (clrtoeol__OUT) bind(C, name='clrtoeol')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: clrtoeol__OUT           ! int clrtoeol
   !!!!!!!!!!!!!!!!!!!!!:: void              ! void
end function clrtoeol
!-------------------------------------------------------------------------------
! CDEF: int color_content ( short color, short *r, short *g, short *b );
function color_content(color,r,g,b) result (color_content__OUT) bind(C, name='color_content')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: color_content__OUT      ! int color_content
   integer(C_SHORT) ,value:: color           ! short color
   integer(C_SHORT):: r                      ! short *r
   integer(C_SHORT):: g                      ! short *g
   integer(C_SHORT):: b                      ! short *b
end function color_content
!-------------------------------------------------------------------------------
! CDEF: int color_set ( short color_pair_number, const void *opts );
function color_set(color_pair_number,opts) result (color_set__OUT) bind(C, name='color_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: color_set__OUT               ! int color_set
   integer(C_SHORT) ,value:: color_pair_number    ! short color_pair_number
   type(C_PTR) ,value:: opts                      ! const void *opts
end function color_set
!-------------------------------------------------------------------------------
! COMMENT: #int copywin ( const WINDOW *srcwin, WINDOW *dstwin, int sminrow, int smincol, int dminrow, int dmincol, int dmaxrow, int dmaxcol, int overlay );
!-------------------------------------------------------------------------------
! CDEF: int copywin ( const WINDOW *srcwin, WINDOW *dstwin, int sminr, int sminc, int dminr, int dminc, int dmaxr, int dmaxc, int overlay );
function copywin(srcwin,dstwin,sminr,sminc,dminr,dminc,dmaxr,dmaxc,overlay) result (copywin__OUT) bind(C, name='copywin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: copywin__OUT               ! int copywin
   type(C_PTR) ,value:: srcwin                  ! const WINDOW *srcwin
   type(C_PTR):: dstwin                         ! WINDOW *dstwin
   INTEGER(C_INT) ,value:: sminr                ! int sminr
   INTEGER(C_INT) ,value:: sminc                ! int sminc
   INTEGER(C_INT) ,value:: dminr                ! int dminr
   INTEGER(C_INT) ,value:: dminc                ! int dminc
   INTEGER(C_INT) ,value:: dmaxr                ! int dmaxr
   INTEGER(C_INT) ,value:: dmaxc                ! int dmaxc
   INTEGER(C_INT) ,value:: overlay              ! int overlay
end function copywin
!-------------------------------------------------------------------------------
! CDEF: int curs_set ( int visibility );
function curs_set(visibility) result (curs_set__OUT) bind(C, name='curs_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: curs_set__OUT              ! int curs_set
   INTEGER(C_INT) ,value:: visibility           ! int visibility
end function curs_set
!-------------------------------------------------------------------------------
! CDEF: int def_prog_mode ( void );
function def_prog_mode() result (def_prog_mode__OUT) bind(C, name='def_prog_mode')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: def_prog_mode__OUT         ! int def_prog_mode
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function def_prog_mode
!-------------------------------------------------------------------------------
! CDEF: int def_shell_mode ( void );
function def_shell_mode() result (def_shell_mode__OUT) bind(C, name='def_shell_mode')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: def_shell_mode__OUT        ! int def_shell_mode
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function def_shell_mode
!-------------------------------------------------------------------------------
! CDEF: int define_key ( const char *definition, int keycode );
function define_key(definition,keycode) result (define_key__OUT) bind(C, name='define_key')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: define_key__OUT            ! int define_key
   character(kind=c_char) ,intent(in):: definition   ! const char *definition
   INTEGER(C_INT) ,value:: keycode              ! int keycode
end function define_key
!-------------------------------------------------------------------------------
! CDEF: int delch ( void );
function delch() result (delch__OUT) bind(C, name='delch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: delch__OUT                 ! int delch
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function delch
!-------------------------------------------------------------------------------
! CDEF: int deleteln ( void );
function deleteln() result (deleteln__OUT) bind(C, name='deleteln')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: deleteln__OUT              ! int deleteln
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function deleteln
!-------------------------------------------------------------------------------
! CDEF: int delwin ( const WINDOW *win );
function delwin(win) result (delwin__OUT) bind(C, name='delwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: delwin__OUT                ! int delwin
   type(C_PTR) ,value:: win                     ! const WINDOW *win
end function delwin
!-------------------------------------------------------------------------------
! CDEF: int doupdate ( void );
function doupdate() result (doupdate__OUT) bind(C, name='doupdate')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: doupdate__OUT        ! int doupdate
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function doupdate
!-------------------------------------------------------------------------------
! CDEF: int echo ( void );
function echo() result (echo__OUT) bind(C, name='echo')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: echo__OUT            ! int echo
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function echo
!-------------------------------------------------------------------------------
! CDEF: int echochar ( const chtype ch );
function echochar(ch) result (echochar__OUT) bind(C, name='echochar')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: echochar__OUT        ! int echochar
   integer(kind=chtype) , value, intent(in):: ch                   ! const chtype ch
end function echochar
!-------------------------------------------------------------------------------
! CDEF: int endwin ( void );
function endwin() result (endwin__OUT) bind(C, name='endwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: endwin__OUT          ! int endwin
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function endwin
!-------------------------------------------------------------------------------
! CDEF: int erase ( void );
function erase() result (erase__OUT) bind(C, name='erase')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: erase__OUT           ! int erase
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function erase
!-------------------------------------------------------------------------------
! CDEF: int flash ( void );
function flash() result (flash__OUT) bind(C, name='flash')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: flash__OUT           ! int flash
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function flash
!-------------------------------------------------------------------------------
! CDEF: int get_wch ( wint_t *wch );
function get_wch(wch) result (get_wch__OUT) bind(C, name='get_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: get_wch__OUT         ! int get_wch
   type(C_PTR):: wch                  ! wint_t *wch
end function get_wch
!-------------------------------------------------------------------------------
! CDEF: int get_wstr ( wint_t *wstr );
function get_wstr(wstr) result (get_wstr__OUT) bind(C, name='get_wstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: get_wstr__OUT        ! int get_wstr
   type(C_PTR):: wstr                 ! wint_t *wstr
end function get_wstr
!-------------------------------------------------------------------------------
! CDEF: int getattrs ( const WINDOW *win );
function getattrs(win) result (getattrs__OUT) bind(C, name='getattrs')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getattrs__OUT        ! int getattrs
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function getattrs
!-------------------------------------------------------------------------------
! CDEF: int getbegx ( const WINDOW *win );
function getbegx(win) result (getbegx__OUT) bind(C, name='getbegx')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getbegx__OUT         ! int getbegx
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function getbegx
!-------------------------------------------------------------------------------
! CDEF: int getbegy ( const WINDOW *win );
function getbegy(win) result (getbegy__OUT) bind(C, name='getbegy')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getbegy__OUT         ! int getbegy
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function getbegy
!-------------------------------------------------------------------------------
! CDEF: int getbkgrnd ( cchar_t *wch );
function getbkgrnd(wch) result (getbkgrnd__OUT) bind(C, name='getbkgrnd')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getbkgrnd__OUT       ! int getbkgrnd
   type(C_PTR):: wch                  ! cchar_t *wch
end function getbkgrnd
!-------------------------------------------------------------------------------
! CDEF: int getch ( void );
function getch() result (getch__OUT) bind(C, name='getch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getch__OUT           ! int getch
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function getch
!-------------------------------------------------------------------------------
! CDEF: int getcurx ( const WINDOW *win );
function getcurx(win) result (getcurx__OUT) bind(C, name='getcurx')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getcurx__OUT         ! int getcurx
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function getcurx
!-------------------------------------------------------------------------------
! CDEF: int getcury ( const WINDOW *win );
function getcury(win) result (getcury__OUT) bind(C, name='getcury')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getcury__OUT         ! int getcury
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function getcury
!-------------------------------------------------------------------------------
! CDEF: int getmaxx ( const WINDOW *win );
function getmaxx(win) result (getmaxx__OUT) bind(C, name='getmaxx')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getmaxx__OUT         ! int getmaxx
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function getmaxx
!-------------------------------------------------------------------------------
! CDEF: int getmaxy ( const WINDOW *win );
function getmaxy(win) result (getmaxy__OUT) bind(C, name='getmaxy')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getmaxy__OUT         ! int getmaxy
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function getmaxy
!-------------------------------------------------------------------------------
! CDEF: int getn_wstr ( wint_t *wstr, int n );
function getn_wstr(wstr,n) result (getn_wstr__OUT) bind(C, name='getn_wstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getn_wstr__OUT       ! int getn_wstr
   type(C_PTR):: wstr                 ! wint_t *wstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function getn_wstr
!-------------------------------------------------------------------------------
! CDEF: int getnstr ( char *str, int n );
function getnstr(str,n) result (getnstr__OUT) bind(C, name='getnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getnstr__OUT         ! int getnstr
   character(kind=c_char):: str                  ! char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function getnstr
!-------------------------------------------------------------------------------
! CDEF: int getparx ( const WINDOW *win );
function getparx(win) result (getparx__OUT) bind(C, name='getparx')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getparx__OUT         ! int getparx
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function getparx
!-------------------------------------------------------------------------------
! CDEF: int getpary ( const WINDOW *win );
function getpary(win) result (getpary__OUT) bind(C, name='getpary')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getpary__OUT         ! int getpary
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function getpary
!-------------------------------------------------------------------------------
! CDEF: int getstr ( char *str );
function getstr(str) result (getstr__OUT) bind(C, name='getstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: getstr__OUT          ! int getstr
   character(kind=c_char):: str                  ! char *str
end function getstr
!-------------------------------------------------------------------------------
! CDEF: int halfdelay ( int tenths );
function halfdelay(tenths) result (halfdelay__OUT) bind(C, name='halfdelay')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: halfdelay__OUT       ! int halfdelay
   INTEGER(C_INT) ,value:: tenths               ! int tenths
end function halfdelay
!-------------------------------------------------------------------------------
! CDEF: int has_key ( int ch );
function has_key(ch) result (has_key__OUT) bind(C, name='has_key')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: has_key__OUT         ! int has_key
   INTEGER(C_INT) ,value:: ch                   ! int ch
end function has_key
!-------------------------------------------------------------------------------
! CDEF: int hline ( chtype ch, int n );
function hline(ch,n) result (hline__OUT) bind(C, name='hline')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: hline__OUT           ! int hline
   integer(kind=chtype) ,value:: ch                   ! chtype ch
   INTEGER(C_INT) ,value:: n                    ! int n
end function hline
!-------------------------------------------------------------------------------
! CDEF: int hline_set ( const cchar_t *wch, int n );
function hline_set(wch,n) result (hline_set__OUT) bind(C, name='hline_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: hline_set__OUT       ! int hline_set
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
   INTEGER(C_INT) ,value:: n                    ! int n
end function hline_set
!-------------------------------------------------------------------------------
! CDEF: int idlok ( const WINDOW *win, bool bf );
function idlok(win,bf) result (idlok__OUT) bind(C, name='idlok')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: idlok__OUT           ! int idlok
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   logical(C_BOOL) ,value:: bf                   ! bool bf
end function idlok
!-------------------------------------------------------------------------------
! CDEF: int in_wch ( cchar_t *wcval );
function in_wch(wcval) result (in_wch__OUT) bind(C, name='in_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: in_wch__OUT          ! int in_wch
   type(C_PTR):: wcval                ! cchar_t *wcval
end function in_wch
!-------------------------------------------------------------------------------
! CDEF: int in_wchnstr ( cchar_t *wchstr, int n );
function in_wchnstr(wchstr,n) result (in_wchnstr__OUT) bind(C, name='in_wchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: in_wchnstr__OUT      ! int in_wchnstr
   type(C_PTR):: wchstr               ! cchar_t *wchstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function in_wchnstr
!-------------------------------------------------------------------------------
! CDEF: int in_wchstr ( cchar_t *wchstr );
function in_wchstr(wchstr) result (in_wchstr__OUT) bind(C, name='in_wchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: in_wchstr__OUT       ! int in_wchstr
   type(C_PTR):: wchstr               ! cchar_t *wchstr
end function in_wchstr
!-------------------------------------------------------------------------------
! CDEF: int inchnstr ( chtype *chstr, int n );
function inchnstr(chstr,n) result (inchnstr__OUT) bind(C, name='inchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: inchnstr__OUT        ! int inchnstr
   type(C_PTR):: chstr                ! chtype *chstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function inchnstr
!-------------------------------------------------------------------------------
! CDEF: int inchstr ( chtype *chstr );
function inchstr(chstr) result (inchstr__OUT) bind(C, name='inchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: inchstr__OUT         ! int inchstr
   type(C_PTR):: chstr                ! chtype *chstr
end function inchstr
!-------------------------------------------------------------------------------
! CDEF: int init_color ( short color, short r, short g, short b );
function init_color(color,r,g,b) result (init_color__OUT) bind(C, name='init_color')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: init_color__OUT      ! int init_color
   integer(C_SHORT) ,value:: color                ! short color
   integer(C_SHORT) ,value:: r                    ! short r
   integer(C_SHORT) ,value:: g                    ! short g
   integer(C_SHORT) ,value:: b                    ! short b
end function init_color
!-------------------------------------------------------------------------------
! CDEF: int init_pair ( short pair, short f, short b );
function init_pair(pair,f,b) result (init_pair__OUT) bind(C, name='init_pair')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: init_pair__OUT       ! int init_pair
   integer(C_SHORT) ,value:: pair                 ! short pair
   integer(C_SHORT) ,value:: f                    ! short f
   integer(C_SHORT) ,value:: b                    ! short b
end function init_pair
!-------------------------------------------------------------------------------
! CDEF: int innstr ( char *str, int n );
function innstr(str,n) result (innstr__OUT) bind(C, name='innstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: innstr__OUT          ! int innstr
   character(kind=c_char):: str                  ! char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function innstr
!-------------------------------------------------------------------------------
! CDEF: int ins_wch ( const cchar_t *wch );
function ins_wch(wch) result (ins_wch__OUT) bind(C, name='ins_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: ins_wch__OUT         ! int ins_wch
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
end function ins_wch
!-------------------------------------------------------------------------------
! CDEF: int insch ( chtype ch );
function insch(ch) result (insch__OUT) bind(C, name='insch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: insch__OUT           ! int insch
   integer(kind=chtype) ,value:: ch                   ! chtype ch
end function insch
!-------------------------------------------------------------------------------
! CDEF: int insdelln ( int n );
function insdelln(n) result (insdelln__OUT) bind(C, name='insdelln')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: insdelln__OUT        ! int insdelln
   INTEGER(C_INT) ,value:: n                    ! int n
end function insdelln
!-------------------------------------------------------------------------------
! CDEF: int insertln ( void );
function insertln() result (insertln__OUT) bind(C, name='insertln')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: insertln__OUT        ! int insertln
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function insertln
!-------------------------------------------------------------------------------
! CDEF: int insnstr ( const char *str, int n );
function insnstr(str,n) result (insnstr__OUT) bind(C, name='insnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: insnstr__OUT         ! int insnstr
   character(kind=c_char) ,intent(in):: str                  ! const char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function insnstr
!-------------------------------------------------------------------------------
! CDEF: int insstr ( const char *str );
function insstr(str) result (insstr__OUT) bind(C, name='insstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: insstr__OUT          ! int insstr
   character(kind=c_char) ,intent(in):: str                  ! const char *str
end function insstr
!-------------------------------------------------------------------------------
! CDEF: int instr ( char *str );
function instr(str) result (instr__OUT) bind(C, name='instr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: instr__OUT           ! int instr
   character(kind=c_char):: str                  ! char *str
end function instr
!-------------------------------------------------------------------------------
! CDEF: int intrflush ( const WINDOW *win, bool bf );
function intrflush(win,bf) result (intrflush__OUT) bind(C, name='intrflush')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: intrflush__OUT       ! int intrflush
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   logical(C_BOOL) ,value:: bf                   ! bool bf
end function intrflush
!-------------------------------------------------------------------------------
! CDEF: int key_defined ( const char *definition );
function key_defined(definition) result (key_defined__OUT) bind(C, name='key_defined')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: key_defined__OUT     ! int key_defined
   character(kind=c_char) ,intent(in):: definition           ! const char *definition
end function key_defined
!-------------------------------------------------------------------------------
! CDEF: int keyok ( int keycode, bool enable );
function keyok(keycode,enable) result (keyok__OUT) bind(C, name='keyok')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: keyok__OUT           ! int keyok
   INTEGER(C_INT) ,value:: keycode              ! int keycode
   logical(C_BOOL) ,value:: enable               ! bool enable
end function keyok
!-------------------------------------------------------------------------------
! CDEF: int leaveok ( const WINDOW *win, bool bf );
function leaveok(win,bf) result (leaveok__OUT) bind(C, name='leaveok')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: leaveok__OUT         ! int leaveok
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   logical(C_BOOL) ,value:: bf                   ! bool bf
end function leaveok
!-------------------------------------------------------------------------------
! CDEF: int mcprint ( char *data, int len );
function mcprint(data,len) result (mcprint__OUT) bind(C, name='mcprint')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mcprint__OUT         ! int mcprint
   character(kind=c_char):: data                 ! char *data
   INTEGER(C_INT) ,value:: len                  ! int len
end function mcprint
!-------------------------------------------------------------------------------
! CDEF: int meta ( const WINDOW *win, bool bf );
function meta(win,bf) result (meta__OUT) bind(C, name='meta')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: meta__OUT            ! int meta
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   logical(C_BOOL) ,value:: bf                   ! bool bf
end function meta
!-------------------------------------------------------------------------------
! CDEF: int move ( int y, int x );
function move(y,x) result (move__OUT) bind(C, name='move')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: move__OUT            ! int move
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
end function move
!-------------------------------------------------------------------------------
! CDEF: int mvadd_wch ( int y, int x, const cchar_t *wch );
function mvadd_wch(y,x,wch) result (mvadd_wch__OUT) bind(C, name='mvadd_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvadd_wch__OUT       ! int mvadd_wch
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
end function mvadd_wch
!-------------------------------------------------------------------------------
! CDEF: int mvadd_wchnstr ( int y, int x, const cchar_t *wchstr, int n );
function mvadd_wchnstr(y,x,wchstr,n) result (mvadd_wchnstr__OUT) bind(C, name='mvadd_wchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvadd_wchnstr__OUT   ! int mvadd_wchnstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: wchstr               ! const cchar_t *wchstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvadd_wchnstr
!-------------------------------------------------------------------------------
! CDEF: int mvadd_wchstr ( int y, int x, const cchar_t *wchstr );
function mvadd_wchstr(y,x,wchstr) result (mvadd_wchstr__OUT) bind(C, name='mvadd_wchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvadd_wchstr__OUT    ! int mvadd_wchstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: wchstr               ! const cchar_t *wchstr
end function mvadd_wchstr
!-------------------------------------------------------------------------------
! CDEF: int mvaddch ( int y, int x, const chtype ch );
function mvaddch(y,x,ch) result (mvaddch__OUT) bind(C, name='mvaddch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvaddch__OUT         ! int mvaddch
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   integer(kind=chtype) , value, intent(in):: ch                   ! const chtype ch
end function mvaddch
!-------------------------------------------------------------------------------
! CDEF: int mvaddchnstr ( int y, int x, const chtype *chstr, int n );
function mvaddchnstr(y,x,chstr,n) result (mvaddchnstr__OUT) bind(C, name='mvaddchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvaddchnstr__OUT     ! int mvaddchnstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: chstr                ! const chtype *chstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvaddchnstr
!-------------------------------------------------------------------------------
! CDEF: int mvaddchstr ( int y, int x, const chtype *chstr );
function mvaddchstr(y,x,chstr) result (mvaddchstr__OUT) bind(C, name='mvaddchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvaddchstr__OUT      ! int mvaddchstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: chstr                ! const chtype *chstr
end function mvaddchstr
!-------------------------------------------------------------------------------
! CDEF: int mvaddnstr ( int y, int x, const char *str, int n );
function mvaddnstr(y,x,str,n) result (mvaddnstr__OUT) bind(C, name='mvaddnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvaddnstr__OUT       ! int mvaddnstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char) ,intent(in):: str                  ! const char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvaddnstr
!-------------------------------------------------------------------------------
! CDEF: int mvdelch ( int y, int x );
function mvdelch(y,x) result (mvdelch__OUT) bind(C, name='mvdelch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvdelch__OUT         ! int mvdelch
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
end function mvdelch
!-------------------------------------------------------------------------------
! CDEF: int mvderwin ( const WINDOW *win, int par_y, int par_x );
function mvderwin(win,par_y,par_x) result (mvderwin__OUT) bind(C, name='mvderwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvderwin__OUT        ! int mvderwin
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: par_y                ! int par_y
   INTEGER(C_INT) ,value:: par_x                ! int par_x
end function mvderwin
!-------------------------------------------------------------------------------
! CDEF: int mvget_wch ( int y, int x, wint_t *wch );
function mvget_wch(y,x,wch) result (mvget_wch__OUT) bind(C, name='mvget_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvget_wch__OUT       ! int mvget_wch
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: wch                  ! wint_t *wch
end function mvget_wch
!-------------------------------------------------------------------------------
! CDEF: int mvget_wstr ( int y, int x, wint_t *wstr );
function mvget_wstr(y,x,wstr) result (mvget_wstr__OUT) bind(C, name='mvget_wstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvget_wstr__OUT      ! int mvget_wstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: wstr                 ! wint_t *wstr
end function mvget_wstr
!-------------------------------------------------------------------------------
! CDEF: int mvgetch ( int y, int x );
function mvgetch(y,x) result (mvgetch__OUT) bind(C, name='mvgetch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvgetch__OUT         ! int mvgetch
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
end function mvgetch
!-------------------------------------------------------------------------------
! CDEF: int mvgetn_wstr ( int y, int x, wint_t *wstr, int n );
function mvgetn_wstr(y,x,wstr,n) result (mvgetn_wstr__OUT) bind(C, name='mvgetn_wstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvgetn_wstr__OUT     ! int mvgetn_wstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: wstr                 ! wint_t *wstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvgetn_wstr
!-------------------------------------------------------------------------------
! CDEF: int mvgetnstr ( int y, int x, char *str, int n );
function mvgetnstr(y,x,str,n) result (mvgetnstr__OUT) bind(C, name='mvgetnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvgetnstr__OUT       ! int mvgetnstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char):: str                  ! char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvgetnstr
!-------------------------------------------------------------------------------
! CDEF: int mvgetstr ( int y, int x, char *str );
function mvgetstr(y,x,str) result (mvgetstr__OUT) bind(C, name='mvgetstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvgetstr__OUT        ! int mvgetstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char):: str                  ! char *str
end function mvgetstr
!-------------------------------------------------------------------------------
! CDEF: int mvhline ( int y, int x, chtype ch, int n );
function mvhline(y,x,ch,n) result (mvhline__OUT) bind(C, name='mvhline')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvhline__OUT         ! int mvhline
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   integer(kind=chtype) ,value:: ch                   ! chtype ch
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvhline
!-------------------------------------------------------------------------------
! CDEF: int mvhline_set ( int y, int x, const cchar_t *wch, int n );
function mvhline_set(y,x,wch,n) result (mvhline_set__OUT) bind(C, name='mvhline_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvhline_set__OUT     ! int mvhline_set
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvhline_set
!-------------------------------------------------------------------------------
! CDEF: int mvin_wch ( int y, int x, cchar_t *wcval );
function mvin_wch(y,x,wcval) result (mvin_wch__OUT) bind(C, name='mvin_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvin_wch__OUT        ! int mvin_wch
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: wcval                ! cchar_t *wcval
end function mvin_wch
!-------------------------------------------------------------------------------
! CDEF: int mvin_wchnstr ( int y, int x, cchar_t *wchstr, int n );
function mvin_wchnstr(y,x,wchstr,n) result (mvin_wchnstr__OUT) bind(C, name='mvin_wchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvin_wchnstr__OUT    ! int mvin_wchnstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: wchstr               ! cchar_t *wchstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvin_wchnstr
!-------------------------------------------------------------------------------
! CDEF: int mvin_wchstr ( int y, int x, cchar_t *wchstr );
function mvin_wchstr(y,x,wchstr) result (mvin_wchstr__OUT) bind(C, name='mvin_wchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvin_wchstr__OUT     ! int mvin_wchstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: wchstr               ! cchar_t *wchstr
end function mvin_wchstr
!-------------------------------------------------------------------------------
! CDEF: int mvinchnstr ( int y, int x, chtype *chstr, int n );
function mvinchnstr(y,x,chstr,n) result (mvinchnstr__OUT) bind(C, name='mvinchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvinchnstr__OUT      ! int mvinchnstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: chstr                ! chtype *chstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvinchnstr
!-------------------------------------------------------------------------------
! CDEF: int mvinchstr ( int y, int x, chtype *chstr );
function mvinchstr(y,x,chstr) result (mvinchstr__OUT) bind(C, name='mvinchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvinchstr__OUT       ! int mvinchstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: chstr                ! chtype *chstr
end function mvinchstr
!-------------------------------------------------------------------------------
! CDEF: int mvinnstr ( int y, int x, char *str, int n );
function mvinnstr(y,x,str,n) result (mvinnstr__OUT) bind(C, name='mvinnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvinnstr__OUT        ! int mvinnstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char):: str                  ! char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvinnstr
!-------------------------------------------------------------------------------
! CDEF: int mvins_wch ( int y, int x, const cchar_t *wch );
function mvins_wch(y,x,wch) result (mvins_wch__OUT) bind(C, name='mvins_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvins_wch__OUT       ! int mvins_wch
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
end function mvins_wch
!-------------------------------------------------------------------------------
! CDEF: int mvinsch ( int y, int x, chtype ch );
function mvinsch(y,x,ch) result (mvinsch__OUT) bind(C, name='mvinsch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvinsch__OUT         ! int mvinsch
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   integer(kind=chtype) ,value:: ch                   ! chtype ch
end function mvinsch
!-------------------------------------------------------------------------------
! CDEF: int mvinsnstr ( int y, int x, const char *str, int n );
function mvinsnstr(y,x,str,n) result (mvinsnstr__OUT) bind(C, name='mvinsnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvinsnstr__OUT       ! int mvinsnstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char) ,intent(in):: str                  ! const char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvinsnstr
!-------------------------------------------------------------------------------
! CDEF: int mvinsstr ( int y, int x, const char *str );
function mvinsstr(y,x,str) result (mvinsstr__OUT) bind(C, name='mvinsstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvinsstr__OUT        ! int mvinsstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char) ,intent(in):: str                  ! const char *str
end function mvinsstr
!-------------------------------------------------------------------------------
! CDEF: int mvinstr ( int y, int x, char *str );
function mvinstr(y,x,str) result (mvinstr__OUT) bind(C, name='mvinstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvinstr__OUT         ! int mvinstr
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char):: str                  ! char *str
end function mvinstr
!-------------------------------------------------------------------------------
! CDEF: int mvvline ( int y, int x, chtype ch, int n );
function mvvline(y,x,ch,n) result (mvvline__OUT) bind(C, name='mvvline')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvvline__OUT         ! int mvvline
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   integer(kind=chtype) ,value:: ch                   ! chtype ch
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvvline
!-------------------------------------------------------------------------------
! CDEF: int mvvline_set ( int y, int x, const cchar_t *wch, int n );
function mvvline_set(y,x,wch,n) result (mvvline_set__OUT) bind(C, name='mvvline_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvvline_set__OUT     ! int mvvline_set
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvvline_set
!-------------------------------------------------------------------------------
! CDEF: int mvwadd_wch ( const WINDOW *win, int y, int x, const cchar_t *wch );
function mvwadd_wch(win,y,x,wch) result (mvwadd_wch__OUT) bind(C, name='mvwadd_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwadd_wch__OUT      ! int mvwadd_wch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
end function mvwadd_wch
!-------------------------------------------------------------------------------
! CDEF: int mvwadd_wchnstr ( const WINDOW *win, int y, int x, const cchar_t *wchstr, int n );
function mvwadd_wchnstr(win,y,x,wchstr,n) result (mvwadd_wchnstr__OUT) bind(C, name='mvwadd_wchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwadd_wchnstr__OUT  ! int mvwadd_wchnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: wchstr               ! const cchar_t *wchstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvwadd_wchnstr
!-------------------------------------------------------------------------------
! CDEF: int mvwadd_wchstr ( const WINDOW *win, int y, int x, const cchar_t *wchstr );
function mvwadd_wchstr(win,y,x,wchstr) result (mvwadd_wchstr__OUT) bind(C, name='mvwadd_wchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwadd_wchstr__OUT   ! int mvwadd_wchstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: wchstr               ! const cchar_t *wchstr
end function mvwadd_wchstr
!-------------------------------------------------------------------------------
! CDEF: int mvwaddch ( const WINDOW *win, int y, int x, const chtype ch );
function mvwaddch(win,y,x,ch) result (mvwaddch__OUT) bind(C, name='mvwaddch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwaddch__OUT        ! int mvwaddch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   integer(kind=chtype) , value, intent(in):: ch                   ! const chtype ch
end function mvwaddch
!-------------------------------------------------------------------------------
! CDEF: int mvwaddchnstr ( const WINDOW *win, int y, int x, const chtype *chstr, int n );
function mvwaddchnstr(win,y,x,chstr,n) result (mvwaddchnstr__OUT) bind(C, name='mvwaddchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwaddchnstr__OUT    ! int mvwaddchnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: chstr                ! const chtype *chstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvwaddchnstr
!-------------------------------------------------------------------------------
! CDEF: int mvwaddchstr ( const WINDOW *win, int y, int x, const chtype *chstr );
function mvwaddchstr(win,y,x,chstr) result (mvwaddchstr__OUT) bind(C, name='mvwaddchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwaddchstr__OUT     ! int mvwaddchstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: chstr                ! const chtype *chstr
end function mvwaddchstr
!-------------------------------------------------------------------------------
! CDEF: int mvwaddnstr ( const WINDOW *win, int y, int x, const char *str, int n );
function mvwaddnstr(win,y,x,str,n) result (mvwaddnstr__OUT) bind(C, name='mvwaddnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwaddnstr__OUT      ! int mvwaddnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char) ,intent(in):: str                  ! const char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvwaddnstr
!-------------------------------------------------------------------------------
! CDEF: int mvwaddstr ( const WINDOW *win, int y, int x, const char *str );
function mvwaddstr(win,y,x,str) result (mvwaddstr__OUT) bind(C, name='mvwaddstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwaddstr__OUT       ! int mvwaddstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char) ,intent(in):: str                  ! const char *str
end function mvwaddstr
!-------------------------------------------------------------------------------
! CDEF: int mvwdelch ( const WINDOW *win, int y, int x );
function mvwdelch(win,y,x) result (mvwdelch__OUT) bind(C, name='mvwdelch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwdelch__OUT        ! int mvwdelch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
end function mvwdelch
!-------------------------------------------------------------------------------
! CDEF: int mvwget_wch ( const WINDOW *win, int y, int x, wint_t *wch );
function mvwget_wch(win,y,x,wch) result (mvwget_wch__OUT) bind(C, name='mvwget_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwget_wch__OUT      ! int mvwget_wch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: wch                  ! wint_t *wch
end function mvwget_wch
!-------------------------------------------------------------------------------
! CDEF: int mvwget_wstr ( const WINDOW *win, int y, int x, wint_t *wstr );
function mvwget_wstr(win,y,x,wstr) result (mvwget_wstr__OUT) bind(C, name='mvwget_wstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwget_wstr__OUT     ! int mvwget_wstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: wstr                 ! wint_t *wstr
end function mvwget_wstr
!-------------------------------------------------------------------------------
! CDEF: int mvwgetch ( const WINDOW *win, int y, int x );
function mvwgetch(win,y,x) result (mvwgetch__OUT) bind(C, name='mvwgetch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwgetch__OUT        ! int mvwgetch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
end function mvwgetch
!-------------------------------------------------------------------------------
! CDEF: int mvwgetn_wstr ( const WINDOW *win, int y, int x, wint_t *wstr, int n );
function mvwgetn_wstr(win,y,x,wstr,n) result (mvwgetn_wstr__OUT) bind(C, name='mvwgetn_wstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwgetn_wstr__OUT    ! int mvwgetn_wstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: wstr                 ! wint_t *wstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvwgetn_wstr
!-------------------------------------------------------------------------------
! CDEF: int mvwgetnstr ( const WINDOW *win, int y, int x, char *str, int n );
function mvwgetnstr(win,y,x,str,n) result (mvwgetnstr__OUT) bind(C, name='mvwgetnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwgetnstr__OUT      ! int mvwgetnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char):: str                  ! char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvwgetnstr
!-------------------------------------------------------------------------------
! CDEF: int mvwgetstr ( const WINDOW *win, int y, int x, char *str );
function mvwgetstr(win,y,x,str) result (mvwgetstr__OUT) bind(C, name='mvwgetstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwgetstr__OUT       ! int mvwgetstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char):: str                  ! char *str
end function mvwgetstr
!-------------------------------------------------------------------------------
! CDEF: int mvwhline ( const WINDOW *win, int y, int x, chtype ch, int n );
function mvwhline(win,y,x,ch,n) result (mvwhline__OUT) bind(C, name='mvwhline')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwhline__OUT        ! int mvwhline
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   integer(kind=chtype) ,value:: ch                   ! chtype ch
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvwhline
!-------------------------------------------------------------------------------
! CDEF: int mvwhline_set ( const WINDOW *win, int y, int x, const cchar_t *wch, int n );
function mvwhline_set(win,y,x,wch,n) result (mvwhline_set__OUT) bind(C, name='mvwhline_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwhline_set__OUT    ! int mvwhline_set
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvwhline_set
!-------------------------------------------------------------------------------
! CDEF: int mvwin ( const WINDOW *win, int y, int x );
function mvwin(win,y,x) result (mvwin__OUT) bind(C, name='mvwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwin__OUT           ! int mvwin
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
end function mvwin
!-------------------------------------------------------------------------------
! CDEF: int mvwin_wch ( const WINDOW *win, int y, int x, cchar_t *wcval );
function mvwin_wch(win,y,x,wcval) result (mvwin_wch__OUT) bind(C, name='mvwin_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwin_wch__OUT       ! int mvwin_wch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: wcval                ! cchar_t *wcval
end function mvwin_wch
!-------------------------------------------------------------------------------
! CDEF: int mvwin_wchnstr ( const WINDOW *win, int y, int x, cchar_t *wchstr, int n );
function mvwin_wchnstr(win,y,x,wchstr,n) result (mvwin_wchnstr__OUT) bind(C, name='mvwin_wchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwin_wchnstr__OUT   ! int mvwin_wchnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: wchstr               ! cchar_t *wchstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvwin_wchnstr
!-------------------------------------------------------------------------------
! CDEF: int mvwin_wchstr ( const WINDOW *win, int y, int x, cchar_t *wchstr );
function mvwin_wchstr(win,y,x,wchstr) result (mvwin_wchstr__OUT) bind(C, name='mvwin_wchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwin_wchstr__OUT    ! int mvwin_wchstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: wchstr               ! cchar_t *wchstr
end function mvwin_wchstr
!-------------------------------------------------------------------------------
! CDEF: int mvwinchnstr ( const WINDOW *win, int y, int x, chtype *chstr, int n );
function mvwinchnstr(win,y,x,chstr,n) result (mvwinchnstr__OUT) bind(C, name='mvwinchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwinchnstr__OUT     ! int mvwinchnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: chstr                ! chtype *chstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvwinchnstr
!-------------------------------------------------------------------------------
! CDEF: int mvwinchstr ( const WINDOW *win, int y, int x, chtype *chstr );
function mvwinchstr(win,y,x,chstr) result (mvwinchstr__OUT) bind(C, name='mvwinchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwinchstr__OUT      ! int mvwinchstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR):: chstr                ! chtype *chstr
end function mvwinchstr
!-------------------------------------------------------------------------------
! CDEF: int mvwinnstr ( const WINDOW *win, int y, int x, char *str, int n );
function mvwinnstr(win,y,x,str,n) result (mvwinnstr__OUT) bind(C, name='mvwinnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwinnstr__OUT       ! int mvwinnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char):: str                  ! char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvwinnstr
!-------------------------------------------------------------------------------
! CDEF: int mvwins_wch ( const WINDOW *win, int y, int x, const cchar_t *wch );
function mvwins_wch(win,y,x,wch) result (mvwins_wch__OUT) bind(C, name='mvwins_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwins_wch__OUT      ! int mvwins_wch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
end function mvwins_wch
!-------------------------------------------------------------------------------
! CDEF: int mvwinsch ( const WINDOW *win, int y, int x, chtype ch );
function mvwinsch(win,y,x,ch) result (mvwinsch__OUT) bind(C, name='mvwinsch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwinsch__OUT        ! int mvwinsch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   integer(kind=chtype) ,value:: ch                   ! chtype ch
end function mvwinsch
!-------------------------------------------------------------------------------
! CDEF: int mvwinsnstr ( const WINDOW *win, int y, int x, const char *str, int n );
function mvwinsnstr(win,y,x,str,n) result (mvwinsnstr__OUT) bind(C, name='mvwinsnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwinsnstr__OUT      ! int mvwinsnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char) ,intent(in):: str                  ! const char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvwinsnstr
!-------------------------------------------------------------------------------
! CDEF: int mvwinsstr ( const WINDOW *win, int y, int x, const char *str );
function mvwinsstr(win,y,x,str) result (mvwinsstr__OUT) bind(C, name='mvwinsstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwinsstr__OUT       ! int mvwinsstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char) ,intent(in):: str                  ! const char *str
end function mvwinsstr
!-------------------------------------------------------------------------------
! CDEF: int mvwinstr ( const WINDOW *win, int y, int x, char *str );
function mvwinstr(win,y,x,str) result (mvwinstr__OUT) bind(C, name='mvwinstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwinstr__OUT        ! int mvwinstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   character(kind=c_char):: str                  ! char *str
end function mvwinstr
!-------------------------------------------------------------------------------
! CDEF: int mvwvline ( const WINDOW *win, int y, int x, chtype ch, int n );
function mvwvline(win,y,x,ch,n) result (mvwvline__OUT) bind(C, name='mvwvline')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwvline__OUT        ! int mvwvline
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   integer(kind=chtype) ,value:: ch                   ! chtype ch
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvwvline
!-------------------------------------------------------------------------------
! CDEF: int mvwvline_set ( const WINDOW *win, int y, int x, const cchar_t *wch, int n );
function mvwvline_set(win,y,x,wch,n) result (mvwvline_set__OUT) bind(C, name='mvwvline_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: mvwvline_set__OUT    ! int mvwvline_set
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
   INTEGER(C_INT) ,value:: n                    ! int n
end function mvwvline_set
!-------------------------------------------------------------------------------
! CDEF: int napms ( int ms );
function napms(ms) result (napms__OUT) bind(C, name='napms')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: napms__OUT           ! int napms
   INTEGER(C_INT) ,value:: ms                   ! int ms
end function napms
!-------------------------------------------------------------------------------
! CDEF: int nl ( void );
function nl() result (nl__OUT) bind(C, name='nl')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: nl__OUT              ! int nl
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function nl
!-------------------------------------------------------------------------------
! CDEF: int nocbreak ( void );
function nocbreak() result (nocbreak__OUT) bind(C, name='nocbreak')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: nocbreak__OUT        ! int nocbreak
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function nocbreak
!-------------------------------------------------------------------------------
! CDEF: int nodelay ( const WINDOW *win, bool bf );
function nodelay(win,bf) result (nodelay__OUT) bind(C, name='nodelay')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: nodelay__OUT         ! int nodelay
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   logical(C_BOOL) ,value:: bf                   ! bool bf
end function nodelay
!-------------------------------------------------------------------------------
! CDEF: int noecho ( void );
function noecho() result (noecho__OUT) bind(C, name='noecho')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: noecho__OUT          ! int noecho
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function noecho
!-------------------------------------------------------------------------------
! CDEF: int nonl ( void );
function nonl() result (nonl__OUT) bind(C, name='nonl')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: nonl__OUT            ! int nonl
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function nonl
!-------------------------------------------------------------------------------
! CDEF: int noraw ( void );
function noraw() result (noraw__OUT) bind(C, name='noraw')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: noraw__OUT           ! int noraw
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function noraw
!-------------------------------------------------------------------------------
! CDEF: int notimeout ( const WINDOW *win, bool bf );
function notimeout(win,bf) result (notimeout__OUT) bind(C, name='notimeout')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: notimeout__OUT       ! int notimeout
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   logical(C_BOOL) ,value:: bf                   ! bool bf
end function notimeout
!-------------------------------------------------------------------------------
! CDEF: int overlay ( const WINDOW *srcwin, const WINDOW *dstwin );
function overlay(srcwin,dstwin) result (overlay__OUT) bind(C, name='overlay')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: overlay__OUT         ! int overlay
   type(C_PTR) ,value:: srcwin               ! const WINDOW *srcwin
   type(C_PTR) ,value:: dstwin               ! const WINDOW *dstwin
end function overlay
!-------------------------------------------------------------------------------
! CDEF: int overwrite ( const WINDOW *srcwin, const WINDOW *dstwin );
function overwrite(srcwin,dstwin) result (overwrite__OUT) bind(C, name='overwrite')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: overwrite__OUT       ! int overwrite
   type(C_PTR) ,value:: srcwin               ! const WINDOW *srcwin
   type(C_PTR) ,value:: dstwin               ! const WINDOW *dstwin
end function overwrite
!-------------------------------------------------------------------------------
! CDEF: int pair_content ( short pair, short *f, short *b );
function pair_content(pair,f,b) result (pair_content__OUT) bind(C, name='pair_content')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: pair_content__OUT    ! int pair_content
   integer(C_SHORT) ,value:: pair                 ! short pair
   integer(C_SHORT):: f                    ! short *f
   integer(C_SHORT):: b                    ! short *b
end function pair_content
!-------------------------------------------------------------------------------
! CDEF: int pechochar ( const WINDOW *pad, chtype ch );
function pechochar(pad,ch) result (pechochar__OUT) bind(C, name='pechochar')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: pechochar__OUT       ! int pechochar
   type(C_PTR) ,value:: pad                  ! const WINDOW *pad
   integer(kind=chtype) ,value:: ch                   ! chtype ch
end function pechochar
!-------------------------------------------------------------------------------
! CDEF: int pnoutrefresh ( const WINDOW *pad, int pminrow, int pmincol, int sminrow, int smincol, int smaxrow, int smaxcol );
function pnoutrefresh(pad,pminrow,pmincol,sminrow,smincol,smaxrow,smaxcol) result (pnoutrefresh__OUT) bind(C, name='pnoutrefresh')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: pnoutrefresh__OUT    ! int pnoutrefresh
   type(C_PTR) ,value:: pad                  ! const WINDOW *pad
   INTEGER(C_INT) ,value:: pminrow              ! int pminrow
   INTEGER(C_INT) ,value:: pmincol              ! int pmincol
   INTEGER(C_INT) ,value:: sminrow              ! int sminrow
   INTEGER(C_INT) ,value:: smincol              ! int smincol
   INTEGER(C_INT) ,value:: smaxrow              ! int smaxrow
   INTEGER(C_INT) ,value:: smaxcol              ! int smaxcol
end function pnoutrefresh
!-------------------------------------------------------------------------------
! CDEF: int prefresh ( const WINDOW *pad, int pminrow, int pmincol, int sminrow, int smincol, int smaxrow, int smaxcol );
function prefresh(pad,pminrow,pmincol,sminrow,smincol,smaxrow,smaxcol) result (prefresh__OUT) bind(C, name='prefresh')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: prefresh__OUT        ! int prefresh
   type(C_PTR) ,value:: pad                  ! const WINDOW *pad
   INTEGER(C_INT) ,value:: pminrow              ! int pminrow
   INTEGER(C_INT) ,value:: pmincol              ! int pmincol
   INTEGER(C_INT) ,value:: sminrow              ! int sminrow
   INTEGER(C_INT) ,value:: smincol              ! int smincol
   INTEGER(C_INT) ,value:: smaxrow              ! int smaxrow
   INTEGER(C_INT) ,value:: smaxcol              ! int smaxcol
end function prefresh
!-------------------------------------------------------------------------------
! CDEF: int raw ( void );
function raw() result (raw__OUT) bind(C, name='raw')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: raw__OUT             ! int raw
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function raw
!-------------------------------------------------------------------------------
! CDEF: int redrawwin ( const WINDOW *win );
function redrawwin(win) result (redrawwin__OUT) bind(C, name='redrawwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: redrawwin__OUT       ! int redrawwin
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function redrawwin
!-------------------------------------------------------------------------------
! CDEF: int refresh ( void );
function refresh() result (refresh__OUT) bind(C, name='refresh')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: refresh__OUT         ! int refresh
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function refresh
!-------------------------------------------------------------------------------
! CDEF: int reset_prog_mode ( void );
function reset_prog_mode() result (reset_prog_mode__OUT) bind(C, name='reset_prog_mode')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: reset_prog_mode__OUT ! int reset_prog_mode
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function reset_prog_mode
!-------------------------------------------------------------------------------
! CDEF: int reset_shell_mode ( void );
function reset_shell_mode() result (reset_shell_mode__OUT) bind(C, name='reset_shell_mode')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: reset_shell_mode__OUT ! int reset_shell_mode
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function reset_shell_mode
!-------------------------------------------------------------------------------
! CDEF: int resetty ( void );
function resetty() result (resetty__OUT) bind(C, name='resetty')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: resetty__OUT         ! int resetty
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function resetty
!-------------------------------------------------------------------------------
! CDEF: int resize_term ( int lines, int columns );
function resize_term(lines,columns) result (resize_term__OUT) bind(C, name='resize_term')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: resize_term__OUT     ! int resize_term
   INTEGER(C_INT) ,value:: lines                ! int lines
   INTEGER(C_INT) ,value:: columns              ! int columns
end function resize_term
!-------------------------------------------------------------------------------
! CDEF: int resizeterm ( int lines, int columns );
function resizeterm(lines,columns) result (resizeterm__OUT) bind(C, name='resizeterm')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: resizeterm__OUT      ! int resizeterm
   INTEGER(C_INT) ,value:: lines                ! int lines
   INTEGER(C_INT) ,value:: columns              ! int columns
end function resizeterm
!-------------------------------------------------------------------------------
! CDEF: int savetty ( void );
function savetty() result (savetty__OUT) bind(C, name='savetty')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: savetty__OUT         ! int savetty
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function savetty
!-------------------------------------------------------------------------------
! CDEF: int scrl ( int n );
function scrl(n) result (scrl__OUT) bind(C, name='scrl')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: scrl__OUT            ! int scrl
   INTEGER(C_INT) ,value:: n                    ! int n
end function scrl
!-------------------------------------------------------------------------------
! CDEF: int scroll ( const WINDOW *win );
function scroll(win) result (scroll__OUT) bind(C, name='scroll')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: scroll__OUT          ! int scroll
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function scroll
!-------------------------------------------------------------------------------
! CDEF: int scrollok ( const WINDOW *win, bool bf );
function scrollok(win,bf) result (scrollok__OUT) bind(C, name='scrollok')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: scrollok__OUT        ! int scrollok
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   logical(C_BOOL) ,value:: bf                   ! bool bf
end function scrollok
!-------------------------------------------------------------------------------
! CDEF: int setscrreg ( int top, int bot );
function setscrreg(top,bot) result (setscrreg__OUT) bind(C, name='setscrreg')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: setscrreg__OUT       ! int setscrreg
   INTEGER(C_INT) ,value:: top                  ! int top
   INTEGER(C_INT) ,value:: bot                  ! int bot
end function setscrreg
!-------------------------------------------------------------------------------
! CDEF: int slk_attroff ( const chtype attrs );
function slk_attroff(attrs) result (slk_attroff__OUT) bind(C, name='slk_attroff')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: slk_attroff__OUT     ! int slk_attroff
   integer(kind=chtype) , value, intent(in):: attrs                ! const chtype attrs
end function slk_attroff
!-------------------------------------------------------------------------------
! CDEF: int slk_attron ( const chtype attrs );
function slk_attron(attrs) result (slk_attron__OUT) bind(C, name='slk_attron')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: slk_attron__OUT      ! int slk_attron
   integer(kind=chtype) , value, intent(in):: attrs                ! const chtype attrs
end function slk_attron
!-------------------------------------------------------------------------------
! CDEF: int slk_clear ( void );
function slk_clear() result (slk_clear__OUT) bind(C, name='slk_clear')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: slk_clear__OUT       ! int slk_clear
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function slk_clear
!-------------------------------------------------------------------------------
! CDEF: int slk_color ( short color_pair );
function slk_color(color_pair) result (slk_color__OUT) bind(C, name='slk_color')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: slk_color__OUT       ! int slk_color
   integer(C_SHORT) ,value:: color_pair           ! short color_pair
end function slk_color
!-------------------------------------------------------------------------------
! CDEF: int slk_init ( int fmt );
function slk_init(fmt) result (slk_init__OUT) bind(C, name='slk_init')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: slk_init__OUT        ! int slk_init
   INTEGER(C_INT) ,value:: fmt                  ! int fmt
end function slk_init
!-------------------------------------------------------------------------------
! CDEF: int slk_noutrefresh ( void );
function slk_noutrefresh() result (slk_noutrefresh__OUT) bind(C, name='slk_noutrefresh')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: slk_noutrefresh__OUT ! int slk_noutrefresh
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function slk_noutrefresh
!-------------------------------------------------------------------------------
! CDEF: int slk_refresh ( void );
function slk_refresh() result (slk_refresh__OUT) bind(C, name='slk_refresh')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: slk_refresh__OUT     ! int slk_refresh
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function slk_refresh
!-------------------------------------------------------------------------------
! CDEF: int slk_restore ( void );
function slk_restore() result (slk_restore__OUT) bind(C, name='slk_restore')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: slk_restore__OUT     ! int slk_restore
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function slk_restore
!-------------------------------------------------------------------------------
! CDEF: int slk_set ( int labnum, const char *label, int fmt );
function slk_set(labnum,label,fmt) result (slk_set__OUT) bind(C, name='slk_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: slk_set__OUT         ! int slk_set
   INTEGER(C_INT) ,value:: labnum               ! int labnum
   character(kind=c_char) ,intent(in):: label                ! const char *label
   INTEGER(C_INT) ,value:: fmt                  ! int fmt
end function slk_set
!-------------------------------------------------------------------------------
! CDEF: int slk_touch ( void );
function slk_touch() result (slk_touch__OUT) bind(C, name='slk_touch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: slk_touch__OUT       ! int slk_touch
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function slk_touch
!-------------------------------------------------------------------------------
! CDEF: int standend ( void );
function standend() result (standend__OUT) bind(C, name='standend')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: standend__OUT        ! int standend
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function standend
!-------------------------------------------------------------------------------
! CDEF: int standout ( void );
function standout() result (standout__OUT) bind(C, name='standout')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: standout__OUT        ! int standout
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function standout
!-------------------------------------------------------------------------------
! CDEF: int start_color ( void );
function start_color() result (start_color__OUT) bind(C, name='start_color')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: start_color__OUT     ! int start_color
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function start_color
!-------------------------------------------------------------------------------
! CDEF: int syncok ( const WINDOW *win, bool bf );
function syncok(win,bf) result (syncok__OUT) bind(C, name='syncok')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: syncok__OUT          ! int syncok
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   logical(C_BOOL) ,value:: bf                   ! bool bf
end function syncok
!-------------------------------------------------------------------------------
! CDEF: int touchline ( const WINDOW *win, int start, int count );
function touchline(win,start,count) result (touchline__OUT) bind(C, name='touchline')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: touchline__OUT       ! int touchline
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: start                ! int start
   INTEGER(C_INT) ,value:: count                ! int count
end function touchline
!-------------------------------------------------------------------------------
! CDEF: int touchwin ( const WINDOW *win );
function touchwin(win) result (touchwin__OUT) bind(C, name='touchwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: touchwin__OUT        ! int touchwin
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function touchwin
!-------------------------------------------------------------------------------
! CDEF: int typeahead ( int fd );
function typeahead(fd) result (typeahead__OUT) bind(C, name='typeahead')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: typeahead__OUT       ! int typeahead
   INTEGER(C_INT) ,value:: fd                   ! int fd
end function typeahead
!-------------------------------------------------------------------------------
! CDEF: int ungetch ( int ch );
function ungetch(ch) result (ungetch__OUT) bind(C, name='ungetch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: ungetch__OUT         ! int ungetch
   INTEGER(C_INT) ,value:: ch                   ! int ch
end function ungetch
!-------------------------------------------------------------------------------
! CDEF: int untouchwin ( const WINDOW *win );
function untouchwin(win) result (untouchwin__OUT) bind(C, name='untouchwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: untouchwin__OUT      ! int untouchwin
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function untouchwin
!-------------------------------------------------------------------------------
! CDEF: int use_default_colors ( void );
function use_default_colors() result (use_default_colors__OUT) bind(C, name='use_default_colors')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: use_default_colors__OUT ! int use_default_colors
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end function use_default_colors
!-------------------------------------------------------------------------------
! CDEF: int use_extended_names ( bool enable );
function use_extended_names(enable) result (use_extended_names__OUT) bind(C, name='use_extended_names')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: use_extended_names__OUT ! int use_extended_names
   logical(C_BOOL) ,value:: enable               ! bool enable
end function use_extended_names
!-------------------------------------------------------------------------------
! CDEF: int use_legacy_coding ( int level );
function use_legacy_coding(level) result (use_legacy_coding__OUT) bind(C, name='use_legacy_coding')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: use_legacy_coding__OUT ! int use_legacy_coding
   INTEGER(C_INT) ,value:: level                ! int level
end function use_legacy_coding
!-------------------------------------------------------------------------------
! CDEF: int vline ( chtype ch, int n );
function vline(ch,n) result (vline__OUT) bind(C, name='vline')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: vline__OUT           ! int vline
   integer(kind=chtype) ,value:: ch                   ! chtype ch
   INTEGER(C_INT) ,value:: n                    ! int n
end function vline
!-------------------------------------------------------------------------------
! CDEF: int vline_set ( const cchar_t *wch, int n );
function vline_set(wch,n) result (vline_set__OUT) bind(C, name='vline_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: vline_set__OUT       ! int vline_set
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
   INTEGER(C_INT) ,value:: n                    ! int n
end function vline_set
!-------------------------------------------------------------------------------
! CDEF: int wadd_wch ( const WINDOW *win, const cchar_t *wch );
function wadd_wch(win,wch) result (wadd_wch__OUT) bind(C, name='wadd_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wadd_wch__OUT        ! int wadd_wch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
end function wadd_wch
!-------------------------------------------------------------------------------
! CDEF: int wadd_wchnstr ( const WINDOW *win, const cchar_t *wchstr, int n );
function wadd_wchnstr(win,wchstr,n) result (wadd_wchnstr__OUT) bind(C, name='wadd_wchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wadd_wchnstr__OUT    ! int wadd_wchnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR) ,value:: wchstr               ! const cchar_t *wchstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function wadd_wchnstr
!-------------------------------------------------------------------------------
! CDEF: int wadd_wchstr ( const WINDOW *win, const cchar_t *wchstr );
function wadd_wchstr(win,wchstr) result (wadd_wchstr__OUT) bind(C, name='wadd_wchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wadd_wchstr__OUT     ! int wadd_wchstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR) ,value:: wchstr               ! const cchar_t *wchstr
end function wadd_wchstr
!-------------------------------------------------------------------------------
! CDEF: int waddch ( const WINDOW *win, const chtype ch );
function waddch(win,ch) result (waddch__OUT) bind(C, name='waddch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: waddch__OUT          ! int waddch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=chtype) , value, intent(in):: ch                   ! const chtype ch
end function waddch
!-------------------------------------------------------------------------------
! CDEF: int waddchnstr ( const WINDOW *win, const chtype *chstr, int n );
function waddchnstr(win,chstr,n) result (waddchnstr__OUT) bind(C, name='waddchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: waddchnstr__OUT      ! int waddchnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR) ,value:: chstr                ! const chtype *chstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function waddchnstr
!-------------------------------------------------------------------------------
! CDEF: int waddchstr ( const WINDOW *win, const chtype *chstr );
function waddchstr(win,chstr) result (waddchstr__OUT) bind(C, name='waddchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: waddchstr__OUT       ! int waddchstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR) ,value:: chstr                ! const chtype *chstr
end function waddchstr
!-------------------------------------------------------------------------------
! CDEF: int waddnstr ( const WINDOW *win, const char *str, int n );
function waddnstr(win,str,n) result (waddnstr__OUT) bind(C, name='waddnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: waddnstr__OUT        ! int waddnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   character(kind=c_char) ,intent(in):: str                  ! const char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function waddnstr
!-------------------------------------------------------------------------------
! CDEF: int waddstr ( const WINDOW *win, const char *str );
function waddstr(win,str) result (waddstr__OUT) bind(C, name='waddstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: waddstr__OUT         ! int waddstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   character(kind=c_char) ,intent(in):: str                  ! const char *str
end function waddstr
!-------------------------------------------------------------------------------
! CDEF: int wbkgd ( const WINDOW *win, chtype ch );
function wbkgd(win,ch) result (wbkgd__OUT) bind(C, name='wbkgd')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wbkgd__OUT           ! int wbkgd
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=chtype) ,value:: ch                   ! chtype ch
end function wbkgd
!-------------------------------------------------------------------------------
! CDEF: int wbkgrnd ( const WINDOW *win, const cchar_t *wch );
function wbkgrnd(win,wch) result (wbkgrnd__OUT) bind(C, name='wbkgrnd')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wbkgrnd__OUT         ! int wbkgrnd
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
end function wbkgrnd
!-------------------------------------------------------------------------------
! CDEF: int wborder ( const WINDOW *win, chtype ls, chtype rs, chtype ts, chtype bs, chtype tl, chtype tr, chtype bl, chtype br );
function wborder(win,ls,rs,ts,bs,tl,tr,bl,br) result (wborder__OUT) bind(C, name='wborder')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wborder__OUT         ! int wborder
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=chtype) ,value:: ls                   ! chtype ls
   integer(kind=chtype) ,value:: rs                   ! chtype rs
   integer(kind=chtype) ,value:: ts                   ! chtype ts
   integer(kind=chtype) ,value:: bs                   ! chtype bs
   integer(kind=chtype) ,value:: tl                   ! chtype tl
   integer(kind=chtype) ,value:: tr                   ! chtype tr
   integer(kind=chtype) ,value:: bl                   ! chtype bl
   integer(kind=chtype) ,value:: br                   ! chtype br
end function wborder
!-------------------------------------------------------------------------------
! CDEF: int wborder_set ( const WINDOW *win, const cchar_t *ls, const cchar_t *rs, const cchar_t *ts, const cchar_t *bs, const cchar_t *tl, const cchar_t *tr, const cchar_t *bl, const cchar_t *br );
function wborder_set(win,ls,rs,ts,bs,tl,tr,bl,br) result (wborder_set__OUT) bind(C, name='wborder_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wborder_set__OUT     ! int wborder_set
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR) ,value:: ls                   ! const cchar_t *ls
   type(C_PTR) ,value:: rs                   ! const cchar_t *rs
   type(C_PTR) ,value:: ts                   ! const cchar_t *ts
   type(C_PTR) ,value:: bs                   ! const cchar_t *bs
   type(C_PTR) ,value:: tl                   ! const cchar_t *tl
   type(C_PTR) ,value:: tr                   ! const cchar_t *tr
   type(C_PTR) ,value:: bl                   ! const cchar_t *bl
   type(C_PTR) ,value:: br                   ! const cchar_t *br
end function wborder_set
!-------------------------------------------------------------------------------
! CDEF: int wclear ( const WINDOW *win );
function wclear(win) result (wclear__OUT) bind(C, name='wclear')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wclear__OUT          ! int wclear
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function wclear
!-------------------------------------------------------------------------------
! CDEF: int wclrtobot ( const WINDOW *win );
function wclrtobot(win) result (wclrtobot__OUT) bind(C, name='wclrtobot')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wclrtobot__OUT       ! int wclrtobot
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function wclrtobot
!-------------------------------------------------------------------------------
! CDEF: int wclrtoeol ( const WINDOW *win );
function wclrtoeol(win) result (wclrtoeol__OUT) bind(C, name='wclrtoeol')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wclrtoeol__OUT       ! int wclrtoeol
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function wclrtoeol
!-------------------------------------------------------------------------------
! CDEF: int wcolor_set ( const WINDOW *win, short color_pair_number, const void *opts );
function wcolor_set(win,color_pair_number,opts) result (wcolor_set__OUT) bind(C, name='wcolor_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wcolor_set__OUT      ! int wcolor_set
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(C_SHORT) ,value:: color_pair_number    ! short color_pair_number
   type(C_PTR) ,value:: opts                 ! const void *opts
end function wcolor_set
!-------------------------------------------------------------------------------
! CDEF: int wdelch ( const WINDOW *win );
function wdelch(win) result (wdelch__OUT) bind(C, name='wdelch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wdelch__OUT          ! int wdelch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function wdelch
!-------------------------------------------------------------------------------
! CDEF: int wdeleteln ( const WINDOW *win );
function wdeleteln(win) result (wdeleteln__OUT) bind(C, name='wdeleteln')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wdeleteln__OUT       ! int wdeleteln
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function wdeleteln
!-------------------------------------------------------------------------------
! CDEF: int wechochar ( const WINDOW *win, const chtype ch );
function wechochar(win,ch) result (wechochar__OUT) bind(C, name='wechochar')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wechochar__OUT       ! int wechochar
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=chtype) , value, intent(in):: ch                   ! const chtype ch
end function wechochar
!-------------------------------------------------------------------------------
! CDEF: int werase ( const WINDOW *win );
function werase(win) result (werase__OUT) bind(C, name='werase')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: werase__OUT          ! int werase
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function werase
!-------------------------------------------------------------------------------
! CDEF: int wget_wch ( const WINDOW *win, wint_t *wch );
function wget_wch(win,wch) result (wget_wch__OUT) bind(C, name='wget_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wget_wch__OUT        ! int wget_wch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR):: wch                  ! wint_t *wch
end function wget_wch
!-------------------------------------------------------------------------------
! CDEF: int wget_wstr ( const WINDOW *win, wint_t *wstr );
function wget_wstr(win,wstr) result (wget_wstr__OUT) bind(C, name='wget_wstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wget_wstr__OUT       ! int wget_wstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR):: wstr                 ! wint_t *wstr
end function wget_wstr
!-------------------------------------------------------------------------------
! CDEF: int wgetbkgrnd ( const WINDOW *win, cchar_t *wch );
function wgetbkgrnd(win,wch) result (wgetbkgrnd__OUT) bind(C, name='wgetbkgrnd')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wgetbkgrnd__OUT      ! int wgetbkgrnd
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR):: wch                  ! cchar_t *wch
end function wgetbkgrnd
!-------------------------------------------------------------------------------
! CDEF: int wgetch ( const WINDOW *win );
function wgetch(win) result (wgetch__OUT) bind(C, name='wgetch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wgetch__OUT          ! int wgetch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function wgetch
!-------------------------------------------------------------------------------
! CDEF: int wgetdelay ( const WINDOW *win );
function wgetdelay(win) result (wgetdelay__OUT) bind(C, name='wgetdelay')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wgetdelay__OUT       ! int wgetdelay
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function wgetdelay
!-------------------------------------------------------------------------------
! CDEF: int wgetn_wstr ( const WINDOW *win, wint_t *wstr, int n );
function wgetn_wstr(win,wstr,n) result (wgetn_wstr__OUT) bind(C, name='wgetn_wstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wgetn_wstr__OUT      ! int wgetn_wstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR):: wstr                 ! wint_t *wstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function wgetn_wstr
!-------------------------------------------------------------------------------
! CDEF: int wgetnstr ( const WINDOW *win, char *str, int n );
function wgetnstr(win,str,n) result (wgetnstr__OUT) bind(C, name='wgetnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wgetnstr__OUT        ! int wgetnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   character(kind=c_char):: str                  ! char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function wgetnstr
!-------------------------------------------------------------------------------
! CDEF: int wgetscrreg ( const WINDOW *win, int *top, int *bottom );
function wgetscrreg(win,top,bottom) result (wgetscrreg__OUT) bind(C, name='wgetscrreg')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wgetscrreg__OUT      ! int wgetscrreg
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(C_INT):: top                  ! int *top
   integer(C_INT):: bottom               ! int *bottom
end function wgetscrreg
!-------------------------------------------------------------------------------
! CDEF: int wgetstr ( const WINDOW *win, char *str );
function wgetstr(win,str) result (wgetstr__OUT) bind(C, name='wgetstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wgetstr__OUT         ! int wgetstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   character(kind=c_char):: str                  ! char *str
end function wgetstr
!-------------------------------------------------------------------------------
! CDEF: int whline ( const WINDOW *win, chtype ch, int n );
function whline(win,ch,n) result (whline__OUT) bind(C, name='whline')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: whline__OUT          ! int whline
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=chtype) ,value:: ch                   ! chtype ch
   INTEGER(C_INT) ,value:: n                    ! int n
end function whline
!-------------------------------------------------------------------------------
! CDEF: int whline_set ( const WINDOW *win, const cchar_t *wch, int n );
function whline_set(win,wch,n) result (whline_set__OUT) bind(C, name='whline_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: whline_set__OUT      ! int whline_set
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
   INTEGER(C_INT) ,value:: n                    ! int n
end function whline_set
!-------------------------------------------------------------------------------
! CDEF: int win_wch ( const WINDOW *win, cchar_t *wcval );
function win_wch(win,wcval) result (win_wch__OUT) bind(C, name='win_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: win_wch__OUT         ! int win_wch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR):: wcval                ! cchar_t *wcval
end function win_wch
!-------------------------------------------------------------------------------
! CDEF: int win_wchnstr ( const WINDOW *win, cchar_t *wchstr, int n );
function win_wchnstr(win,wchstr,n) result (win_wchnstr__OUT) bind(C, name='win_wchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: win_wchnstr__OUT     ! int win_wchnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR):: wchstr               ! cchar_t *wchstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function win_wchnstr
!-------------------------------------------------------------------------------
! CDEF: int win_wchstr ( const WINDOW *win, cchar_t *wchstr );
function win_wchstr(win,wchstr) result (win_wchstr__OUT) bind(C, name='win_wchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: win_wchstr__OUT      ! int win_wchstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR):: wchstr               ! cchar_t *wchstr
end function win_wchstr
!-------------------------------------------------------------------------------
! CDEF: int winchnstr ( const WINDOW *win, chtype *chstr, int n );
function winchnstr(win,chstr,n) result (winchnstr__OUT) bind(C, name='winchnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: winchnstr__OUT       ! int winchnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR):: chstr                ! chtype *chstr
   INTEGER(C_INT) ,value:: n                    ! int n
end function winchnstr
!-------------------------------------------------------------------------------
! CDEF: int winchstr ( const WINDOW *win, chtype *chstr );
function winchstr(win,chstr) result (winchstr__OUT) bind(C, name='winchstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: winchstr__OUT        ! int winchstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR):: chstr                ! chtype *chstr
end function winchstr
!-------------------------------------------------------------------------------
! CDEF: int winnstr ( const WINDOW *win, char *str, int n );
function winnstr(win,str,n) result (winnstr__OUT) bind(C, name='winnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: winnstr__OUT         ! int winnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   character(kind=c_char):: str                  ! char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function winnstr
!-------------------------------------------------------------------------------
! CDEF: int wins_wch ( const WINDOW *win, const cchar_t *wch );
function wins_wch(win,wch) result (wins_wch__OUT) bind(C, name='wins_wch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wins_wch__OUT        ! int wins_wch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
end function wins_wch
!-------------------------------------------------------------------------------
! CDEF: int winsch ( const WINDOW *win, chtype ch );
function winsch(win,ch) result (winsch__OUT) bind(C, name='winsch')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: winsch__OUT          ! int winsch
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=chtype) ,value:: ch                   ! chtype ch
end function winsch
!-------------------------------------------------------------------------------
! CDEF: int winsdelln ( const WINDOW *win, int n );
function winsdelln(win,n) result (winsdelln__OUT) bind(C, name='winsdelln')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: winsdelln__OUT       ! int winsdelln
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: n                    ! int n
end function winsdelln
!-------------------------------------------------------------------------------
! CDEF: int winsertln ( const WINDOW *win );
function winsertln(win) result (winsertln__OUT) bind(C, name='winsertln')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: winsertln__OUT       ! int winsertln
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function winsertln
!-------------------------------------------------------------------------------
! CDEF: int winsnstr ( const WINDOW *win, const char *str, int n );
function winsnstr(win,str,n) result (winsnstr__OUT) bind(C, name='winsnstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: winsnstr__OUT        ! int winsnstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   character(kind=c_char) ,intent(in):: str                  ! const char *str
   INTEGER(C_INT) ,value:: n                    ! int n
end function winsnstr
!-------------------------------------------------------------------------------
! CDEF: int winsstr ( const WINDOW *win, const char *str );
function winsstr(win,str) result (winsstr__OUT) bind(C, name='winsstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: winsstr__OUT         ! int winsstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   character(kind=c_char) ,intent(in):: str                  ! const char *str
end function winsstr
!-------------------------------------------------------------------------------
! CDEF: int winstr ( const WINDOW *win, char *str );
function winstr(win,str) result (winstr__OUT) bind(C, name='winstr')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: winstr__OUT          ! int winstr
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   character(kind=c_char):: str                  ! char *str
end function winstr
!-------------------------------------------------------------------------------
! CDEF: int wmove ( const WINDOW *win, int y, int x );
function wmove(win,y,x) result (wmove__OUT) bind(C, name='wmove')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wmove__OUT           ! int wmove
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
end function wmove
!-------------------------------------------------------------------------------
! CDEF: int wnoutrefresh ( const WINDOW *win );
function wnoutrefresh(win) result (wnoutrefresh__OUT) bind(C, name='wnoutrefresh')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wnoutrefresh__OUT    ! int wnoutrefresh
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function wnoutrefresh
!-------------------------------------------------------------------------------
! CDEF: int wredrawln ( const WINDOW *win, int beg_line, int num_lines );
function wredrawln(win,beg_line,num_lines) result (wredrawln__OUT) bind(C, name='wredrawln')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wredrawln__OUT       ! int wredrawln
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: beg_line             ! int beg_line
   INTEGER(C_INT) ,value:: num_lines            ! int num_lines
end function wredrawln
!-------------------------------------------------------------------------------
! CDEF: int wresize ( const WINDOW *win, int lines, int columns );
function wresize(win,lines,columns) result (wresize__OUT) bind(C, name='wresize')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wresize__OUT         ! int wresize
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: lines                ! int lines
   INTEGER(C_INT) ,value:: columns              ! int columns
end function wresize
!-------------------------------------------------------------------------------
! CDEF: int wscrl ( const WINDOW *win, int n );
function wscrl(win,n) result (wscrl__OUT) bind(C, name='wscrl')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wscrl__OUT           ! int wscrl
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: n                    ! int n
end function wscrl
!-------------------------------------------------------------------------------
! CDEF: int wsetscrreg ( const WINDOW *win, int top, int bot );
function wsetscrreg(win,top,bot) result (wsetscrreg__OUT) bind(C, name='wsetscrreg')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wsetscrreg__OUT      ! int wsetscrreg
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: top                  ! int top
   INTEGER(C_INT) ,value:: bot                  ! int bot
end function wsetscrreg
!-------------------------------------------------------------------------------
! CDEF: int wstandend ( const WINDOW *win );
function wstandend(win) result (wstandend__OUT) bind(C, name='wstandend')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wstandend__OUT       ! int wstandend
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function wstandend
!-------------------------------------------------------------------------------
! CDEF: int wstandout ( const WINDOW *win );
function wstandout(win) result (wstandout__OUT) bind(C, name='wstandout')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wstandout__OUT       ! int wstandout
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function wstandout
!-------------------------------------------------------------------------------
! CDEF: int wtouchln ( const WINDOW *win, int y, int n, int changed );
function wtouchln(win,y,n,changed) result (wtouchln__OUT) bind(C, name='wtouchln')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wtouchln__OUT        ! int wtouchln
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: n                    ! int n
   INTEGER(C_INT) ,value:: changed              ! int changed
end function wtouchln
!-------------------------------------------------------------------------------
! CDEF: int wvline ( const WINDOW *win, chtype ch, int n );
function wvline(win,ch,n) result (wvline__OUT) bind(C, name='wvline')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wvline__OUT          ! int wvline
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=chtype) ,value:: ch                   ! chtype ch
   INTEGER(C_INT) ,value:: n                    ! int n
end function wvline
!-------------------------------------------------------------------------------
! CDEF: int wvline_set ( const WINDOW *win, const cchar_t *wch, int n );
function wvline_set(win,wch,n) result (wvline_set__OUT) bind(C, name='wvline_set')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) :: wvline_set__OUT      ! int wvline_set
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
   INTEGER(C_INT) ,value:: n                    ! int n
end function wvline_set
!-------------------------------------------------------------------------------
! CDEF: SCREEN *newterm ( char *type, FILE *outfd, FILE *infd );
function newterm(type,outfd,infd) result (newterm__OUT) bind(C, name='newterm')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR):: newterm__OUT         ! SCREEN *newterm
   character(kind=c_char):: type                 ! char *type
   type(C_PTR):: outfd                ! FILE *outfd
   type(C_PTR):: infd                 ! FILE *infd
end function newterm
!-------------------------------------------------------------------------------
! CDEF: SCREEN *set_term ( SCREEN *new );
function set_term(new) result (set_term__OUT) bind(C, name='set_term')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR):: set_term__OUT        ! SCREEN *set_term
   type(C_PTR):: new                  ! SCREEN *new
end function set_term
!-------------------------------------------------------------------------------
! CDEF: void bkgdset ( chtype ch );
subroutine bkgdset(ch) bind(C, name='bkgdset')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   integer(kind=chtype) ,value:: ch                   ! chtype ch
end subroutine bkgdset
!-------------------------------------------------------------------------------
! CDEF: void bkgrndset ( const cchar_t *wch );
subroutine bkgrndset(wch) bind(C, name='bkgrndset')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
end subroutine bkgrndset
!-------------------------------------------------------------------------------
! CDEF: void delscreen ( SCREEN *sp );
subroutine delscreen(sp) bind(C, name='delscreen')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR):: sp                   ! SCREEN *sp
end subroutine delscreen
!-------------------------------------------------------------------------------
! COMMENT: #MACRO: void getbegyx ( const WINDOW *win, int y, int x );
!-------------------------------------------------------------------------------
! COMMENT: #MACRO: void getmaxyx ( const WINDOW *win, int y, int x );
!-------------------------------------------------------------------------------
! COMMENT: #MACRO: void getparyx ( const WINDOW *win, int y, int x );
!-------------------------------------------------------------------------------
! CDEF: void getsyx ( int y, int x );
subroutine getsyx(y,x) bind(C, name='getsyx')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
end subroutine getsyx
!-------------------------------------------------------------------------------
! COMMENT: #MACRO: void getyx ( const WINDOW *win, int y, int x );
!-------------------------------------------------------------------------------
! CDEF: void idcok ( const WINDOW *win, bool bf );
subroutine idcok(win,bf) bind(C, name='idcok')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   logical(C_BOOL) ,value:: bf                   ! bool bf
end subroutine idcok
!-------------------------------------------------------------------------------
! CDEF: void immedok ( const WINDOW *win, bool bf );
subroutine immedok(win,bf) bind(C, name='immedok')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   logical(C_BOOL) ,value:: bf                   ! bool bf
end subroutine immedok
!-------------------------------------------------------------------------------
! CDEF: void noqiflush ( void );
subroutine noqiflush() bind(C, name='noqiflush')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end subroutine noqiflush
!-------------------------------------------------------------------------------
! CDEF: void qiflush ( void );
subroutine qiflush() bind(C, name='qiflush')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   !!!!!!!!!!!!!!!!!!!!!:: void                 ! void
end subroutine qiflush
!-------------------------------------------------------------------------------
! CDEF: void setsyx ( int y, int x );
subroutine setsyx(y,x) bind(C, name='setsyx')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) ,value:: y                    ! int y
   INTEGER(C_INT) ,value:: x                    ! int x
end subroutine setsyx
!-------------------------------------------------------------------------------
! CDEF: void timeout ( int delay );
subroutine timeout(delay) bind(C, name='timeout')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   INTEGER(C_INT) ,value:: delay                ! int delay
end subroutine timeout
!-------------------------------------------------------------------------------
! CDEF: void wbkgdset ( const WINDOW *win, chtype ch );
subroutine wbkgdset(win,ch) bind(C, name='wbkgdset')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   integer(kind=chtype) ,value:: ch                   ! chtype ch
end subroutine wbkgdset
!-------------------------------------------------------------------------------
! CDEF: void wbkgrndset ( const WINDOW *win, const cchar_t *wch );
subroutine wbkgrndset(win,wch) bind(C, name='wbkgrndset')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   type(C_PTR) ,value:: wch                  ! const cchar_t *wch
end subroutine wbkgrndset
!-------------------------------------------------------------------------------
! CDEF: void wcursyncup ( const WINDOW *win );
subroutine wcursyncup(win) bind(C, name='wcursyncup')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end subroutine wcursyncup
!-------------------------------------------------------------------------------
! CDEF: void wsyncdown ( const WINDOW *win );
subroutine wsyncdown(win) bind(C, name='wsyncdown')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end subroutine wsyncdown
!-------------------------------------------------------------------------------
! CDEF: void wsyncup ( const WINDOW *win );
subroutine wsyncup(win) bind(C, name='wsyncup')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end subroutine wsyncup
!-------------------------------------------------------------------------------
! CDEF: void wtimeout ( const WINDOW *win, int delay );
subroutine wtimeout(win,delay) bind(C, name='wtimeout')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR) ,value:: win                  ! const WINDOW *win
   INTEGER(C_INT) ,value:: delay                ! int delay
end subroutine wtimeout
!-------------------------------------------------------------------------------
! CDEF: WINDOW *wgetparent ( const WINDOW *win );
function wgetparent(win) result (wgetparent__OUT) bind(C, name='wgetparent')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR):: wgetparent__OUT      ! WINDOW *wgetparent
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function wgetparent
!-------------------------------------------------------------------------------
! CDEF: WINDOW *derwin ( const WINDOW *orig, int nlines, int ncols, int begin_y, int begin_x );
function derwin(orig,nlines,ncols,begin_y,begin_x) result (derwin__OUT) bind(C, name='derwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR):: derwin__OUT          ! WINDOW *derwin
   type(C_PTR) ,value:: orig                 ! const WINDOW *orig
   INTEGER(C_INT) ,value:: nlines               ! int nlines
   INTEGER(C_INT) ,value:: ncols                ! int ncols
   INTEGER(C_INT) ,value:: begin_y              ! int begin_y
   INTEGER(C_INT) ,value:: begin_x              ! int begin_x
end function derwin
!-------------------------------------------------------------------------------
! CDEF: WINDOW *dupwin ( const WINDOW *win );
function dupwin(win) result (dupwin__OUT) bind(C, name='dupwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR):: dupwin__OUT          ! WINDOW *dupwin
   type(C_PTR) ,value:: win                  ! const WINDOW *win
end function dupwin
!-------------------------------------------------------------------------------
! CDEF: WINDOW *newpad ( int nlines, int ncols );
function newpad(nlines,ncols) result (newpad__OUT) bind(C, name='newpad')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR):: newpad__OUT          ! WINDOW *newpad
   INTEGER(C_INT) ,value:: nlines               ! int nlines
   INTEGER(C_INT) ,value:: ncols                ! int ncols
end function newpad
!-------------------------------------------------------------------------------
! CDEF: WINDOW *newwin ( int nlines, int ncols, int begin_y, int begin_x );
function newwin(nlines,ncols,begin_y,begin_x) result (newwin__OUT) bind(C, name='newwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR):: newwin__OUT          ! WINDOW *newwin
   INTEGER(C_INT) ,value:: nlines               ! int nlines
   INTEGER(C_INT) ,value:: ncols                ! int ncols
   INTEGER(C_INT) ,value:: begin_y              ! int begin_y
   INTEGER(C_INT) ,value:: begin_x              ! int begin_x
end function newwin
!-------------------------------------------------------------------------------
! CDEF: WINDOW *subpad ( const WINDOW *orig, int nlines, int ncols, int begin_y, int begin_x );
function subpad(orig,nlines,ncols,begin_y,begin_x) result (subpad__OUT) bind(C, name='subpad')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR):: subpad__OUT          ! WINDOW *subpad
   type(C_PTR) ,value:: orig                 ! const WINDOW *orig
   INTEGER(C_INT) ,value:: nlines               ! int nlines
   INTEGER(C_INT) ,value:: ncols                ! int ncols
   INTEGER(C_INT) ,value:: begin_y              ! int begin_y
   INTEGER(C_INT) ,value:: begin_x              ! int begin_x
end function subpad
!-------------------------------------------------------------------------------
! CDEF: WINDOW *subwin ( const WINDOW *orig, int nlines, int ncols, int begin_y, int begin_x );
function subwin(orig,nlines,ncols,begin_y,begin_x) result (subwin__OUT) bind(C, name='subwin')
   use,intrinsic ::  iso_c_binding
   use ncurses_types
   implicit none
   type(C_PTR):: subwin__OUT          ! WINDOW *subwin
   type(C_PTR) ,value:: orig                 ! const WINDOW *orig
   INTEGER(C_INT) ,value:: nlines               ! int nlines
   INTEGER(C_INT) ,value:: ncols                ! int ncols
   INTEGER(C_INT) ,value:: begin_y              ! int begin_y
   INTEGER(C_INT) ,value:: begin_x              ! int begin_x
end function subwin
!-------------------------------------------------------------------------------
end interface
!-------------------------------------------------------------------------------
contains
!-------------------------------------------------------------------------------
function icharl(letter) ! @(#) wrapper around ICHAR(3f) that returns integer type C_LONG
   use iso_c_binding
   implicit none
   character(len=1),intent(in)  :: letter
   integer(C_LONG)              :: icharl
   icharl=ichar(letter)
end function icharl
!-------------------------------------------------------------------------------
function key_f(functionkeynumber) result (key_plus) ! duplicate the KEY_F(n) macro
   implicit none
   integer                      :: key_plus
   integer,intent(in)           :: functionkeynumber
   ! finds value of function key n
   key_plus=key_f0+functionkeynumber ! from ncurses.h:#define KEY_F(n)   (KEY_F0+(n))
end function key_f
!-------------------------------------------------------------------------------
subroutine newline(win)  ! do the equivalent of a newline
   implicit none
   !type(WINDOW), value  :: win
   type(C_PTR), value    :: win
   integer               :: current_y,current_x,ierr
   call getyx(win,current_y,current_x)
   ierr=move(current_y+1,0)
end subroutine newline
!-------------------------------------------------------------------------------
function initscr() result (stdscr__OUT) ! call initscr() but set global variables too
   implicit none
   type(C_PTR)           :: stdscr__OUT
   stdscr=f_initscr()
   !stdscr=returnstd()
   curscr=returncur()
   stdscr__OUT=stdscr
   call getmaxyx(stdscr,LINES,COLS)
end function initscr
!-------------------------------------------------------------------------------
function C_to_F_string(c_string_pointer) result(f_string)
! gets a C string (pointer), and returns the corresponding Fortran string;
! If the C string is null, it returns "NULL", similar to C's "(null)"
! printed in similar cases:
   use, intrinsic :: iso_c_binding, only: c_ptr,c_f_pointer,c_char,c_null_char
   type(c_ptr), intent(in) :: c_string_pointer
   character(len=:), allocatable :: f_string
   character(kind=c_char), dimension(:), pointer :: char_array_pointer => null()
   character(len=255) :: aux_string
   integer :: i,length=0
   call c_f_pointer(c_string_pointer,char_array_pointer,[255])
   if (.not.associated(char_array_pointer)) then
     allocate(character(len=4)::f_string); f_string="NULL"; return
   endif
   aux_string=" "
   do i=1,255
     if (char_array_pointer(i)==c_null_char) then
       length=i-1; exit
     endif
     aux_string(i:i)=char_array_pointer(i)
   enddo
   allocate(character(len=length)::f_string)
   f_string=aux_string(1:length)
end function C_to_F_string
!-------------------------------------------------------------------------------
function curses_version() result(string__OUT)
   use,intrinsic ::  iso_c_binding
   implicit none
   character(len=:), allocatable :: string__OUT
   string__OUT=c_to_f_string(ptr_curses_version())
end function curses_version
!-------------------------------------------------------------------------------
function keyname(ich) result(string__OUT)
   use,intrinsic ::  iso_c_binding
   implicit none
   character(len=:), allocatable :: string__OUT
   integer(C_INT),intent(in) :: ich
   string__OUT=c_to_f_string(ptr_keyname(ich))
end function keyname
!-------------------------------------------------------------------------------
function termname() result(string__OUT)
   use,intrinsic ::  iso_c_binding
   implicit none
   character(len=:), allocatable :: string__OUT
   string__OUT=c_to_f_string(ptr_termname())
end function termname
!-------------------------------------------------------------------------------
function longname() result(string__OUT)
   use,intrinsic ::  iso_c_binding
   implicit none
   character(len=:), allocatable :: string__OUT
   string__OUT=c_to_f_string(ptr_longname())
end function longname
!-------------------------------------------------------------------------------
end module M_ncurses
