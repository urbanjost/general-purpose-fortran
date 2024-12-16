










!>
!!##NAME
!!     M_fixedform(3fm) - [NCURSES] convert a text block into a TUI (screen interface)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  Usage:
!!
!!      use :: M_fixedform, only : icount_ptr,page_ptr,page_pd,icount_pd
!!      use :: M_fixedform, only : fixedform ,loaddata
!!
!!##DESCRIPTION
!!    M_fixedform(3fm) takes a text block as input, and converts it
!!    to a form-like screen interface using the ncurses(3f) library. An
!!    appropriate action is taken for each keypress.
!!
!!##EXAMPLES
!!
!! Sample program:
!!
!!    program demo_M_fixedform
!!    use M_fixedform, only : fixedform, loaddata, icount_ptr, page_ptr, page_pd, icount_pd
!!    use M_strings, only : split, s2v
!!    implicit none
!!    character(len=5),parameter :: &
!!    & names(10)=[character(len=5) :: "RED","WHITE","BLUE","NAME","DATE","VALUE","EAST","WEST","NORTH","SOUTH"]
!!    character(len=:),allocatable   :: tabs
!!    character(len=:),allocatable   :: answers(:) ! output array of tokens
!!    logical                        :: lanswer
!!    real                           :: ranswer
!!    integer                        :: i
!!    ! make a namelist for the form
!!    logical                        :: red, white, blue
!!    character(len=80)              :: name
!!    character(len=80)              :: date
!!    real                           :: value
!!    logical                        :: east, west, north, south
!!    namelist /form/ RED,WHITE,BLUE,NAME,DATE,VALUE,EAST,WEST,NORTH,SOUTH
!!    call make_data()
!!    page_ptr=>page_pd
!!    icount_ptr=>icount_pd
!!    call loaddata('test.dat')      ! fill the page(*) with user data
!!    call fixedform(tabs)           ! display the form converted to a TDU
!!
!!    write(*,*)'The field values are returned left to right, top to bottom'
!!    write(*,*)'as a tab-delimited string. The returned string is ...'
!!    write(*,*)tabs
!!    write(*,'(a)')repeat('=',80)
!!
!!    write(*,*)'The strings can be read from the string, reformatted as a '
!!    write(*,*)'NAMELIST string, or parsed.'
!!    write(*,'(a)')repeat('=',80)
!!
!!    write(*,*)'parse string into an array of strings using tab delimiter'
!!    call split(tabs,answers,char(9),nulls='return')
!!    write(*,*)'ARRAY IS'
!!    write(*,'(i0,t5,a)')(i,trim(answers(i)),i=1,size(answers))
!!    write(*,'(a)')repeat('=',80)
!!
!!    ! different ways of converting the strings to other types
!!
!!    write(*,*)' convert a T/F string to a logical'
!!    lanswer=answers(1).eq.'T'
!!    write(*,*)' RED is ',lanswer
!!    ! or use an internal read
!!    read(answers(2),*) lanswer
!!    write(*,*)' WHITE is ',lanswer
!!    write(*,'(a)')repeat('=',80)
!!
!!    write(*,*)' NAME is  ',trim(answers(5))
!!    write(*,'(a)')repeat('=',80)
!!
!!    write(*,*)' get a numeric value from a string'
!!    ranswer=real(s2v(answers(6)))
!!    write(*,*)' VALUE is ',ranswer
!!    ! or
!!    read(answers(6),'(g20.13)') ranswer
!!    write(*,*)' VALUE is ',ranswer
!!    write(*,'(a)')repeat('=',80)
!!
!!    write(*,*)'write out the data in the form of a namelist file'
!!    NAME='UNKNOWN'
!!    DATE=''
!!    VALUE=0.00
!!    !*!open(11,status='scratch')
!!    open(11,file='form.txt',action='readwrite')
!!    write(11,'("&FORM")')
!!    do i=1,size(answers)
!!       answers(i)=adjustl(answers(i))
!!       if(answers(i).eq.'')then
!!          write(11,'(*(a))')"! ",trim(names(i)),' was blank'
!!       elseif(index(trim(answers(i)),' ').ne.0)then
!!          write(11,'(*(a))')trim(names(i)),'="',trim(answers(i)),'",'
!!       else
!!          write(11,'(*(a))')trim(names(i)),'=',trim(answers(i)),','
!!       endif
!!    enddo
!!    write(11,'("/")')
!!    ! read the namelist file (an internal file could have been used instead of a file)
!!    write(*,*)'read it back in'
!!    rewind(11)
!!    read(11,form)
!!    write(*,*)'write out namelist using namelist output'
!!    write(*,form)
!!    write(*,'(a)')repeat('=',80)
!!    contains
!!    subroutine make_data()
!!    integer,parameter :: LUN=10
!!    integer           :: ios
!!    open(unit=LUN,file='test.dat')
!!    write(LUN,'(a)')[character(len=80) ::                                            &
!!       '@    The simplest use of FIXEDFORM is when a text file is used to define a     @', &
!!       '@    form to be generated much like it could be drawn on paper:                @', &
!!       '################################################################################', &
!!       '#                                                                              #', &
!!       '#  ~ A basic form definition:         ~  ^ RED                                 #', &
!!       '#  ~ o Underlines become input fields ~  ^ WHITE                               #', &
!!       '#  ~ o Up-carets become menu options  ~  ^ BLUE                                #', &
!!       '#  ~ o Pound characters define boxes  ~                                        #', &
!!       '#  ~ o Text otherwise displays as-is  ~  Connected by pound characters or      #', &
!!       '#  ~   for the most part.             ~  adjacent to one another, up-carets    #', &
!!       '#  Name:  ___________________            form a radio button.                  #', &
!!       '#  Date:  ___________________            #######################               #', &
!!       '#  Value: ___________________            ^      ^       ^      ^               #', &
!!       '#                                       EAST   WEST   NORTH  SOUTH             #', &
!!       '#                                                                              #', &
!!       '# When the cursor is over a menu item it is toggled by pressing the space bar. #', &
!!       '# A tab character moves to the next selectable item. Typing in an input value  #', &
!!       '# changes the value. When the form is complete use the ctrl-S keys to submit.  #', &
!!       '################################################################################' ]
!!    close(unit=LUN,iostat=ios)
!!    end subroutine make_data
!!    end program demo_M_fixedform
!!
!!  A sample of the output from filling out the screen
!!
!!    > The field values are returned left to right, top to bottom
!!    > as a tab-delimited string. The returned string is ...
!!    > F  F       T       John S. Urban           2018-09-20              13.97                   F       T       F       F
!!    >============================================================================
!!    > The strings can be read from the string, reformatted as a
!!    > NAMELIST string, or parsed.
!!    >============================================================================
!!    > parse string into an array of strings using tab delimiter
!!    > ARRAY IS
!!    >1   F
!!    >2   F
!!    >3   T
!!    >4   John S. Urban
!!    >5   2018-09-20
!!    >6   13.97
!!    >7   F
!!    >8   T
!!    >9   F
!!    >10  F
!!    >============================================================================
!!    >  convert a T/F string to a logical
!!    >  RED is  F
!!    >  WHITE is  F
!!    >============================================================================
!!    >  NAME is  2018-09-20
!!    >============================================================================
!!    >  get a numeric value from a string
!!    >  VALUE is    13.9700003
!!    >  VALUE is    13.9700003
!!    >============================================================================
!!    > write out the data in the form of a namelist file
!!    > read it back in
!!    > write out namelist using namelist output
!!    >&FORM
!!    > RED=F,
!!    > WHITE=F,
!!    > BLUE=T,
!!    > NAME="John S. Urban                                                                   ",
!!    > DATE="2018-09-20                                                                      ",
!!    > VALUE=  13.9700003    ,
!!    > EAST=F,
!!    > WEST=T,
!!    > NORTH=F,
!!    > SOUTH=F,
!!    > /
!!    >============================================================================
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
module M_fixedform
! global data
use,intrinsic :: iso_fortran_env
use,intrinsic :: ISO_C_BINDING
use M_ncurses
use M_ncurses, only : A_UNDERLINE
use M_ncurses, only : A_CHARTEXT
use M_ncurses, only : A_ATTRIBUTES
use M_ncurses, only : A_HORIZONTAL
use M_ncurses, only : A_ALTCHARSET
use M_ncurses, only : A_STANDOUT
use M_ncurses, only : ACS_BTEE, ACS_DEGREE, ACS_DIAMOND, ACS_HLINE
use M_ncurses, only : ACS_LLCORNER, ACS_LRCORNER, ACS_ULCORNER, ACS_URCORNER
use M_ncurses, only : ACS_LTEE, ACS_PLUS, ACS_RTEE, ACS_TTEE, ACS_VLINE
use M_ncurses, only : C_INT,C_LONG,C_SHORT
use M_ncurses, only : getmaxyx, getyx, wmove, mvwinch, refresh, wrefresh, move
use M_ncurses, only : LINES,COLS
use M_ncurses, only : PAIR_NUMBER  !  PAIR_NUMBER(attrs) is the reverse of COLOR_PAIR(n)
use M_ncurses, only : stdscr
use M_ncurses, only : mevent
use M_ncurses, only : chtype
use M_ncurses, only : ALL_MOUSE_EVENTS
use M_ncurses, only : BUTTON1_CLICKED, BUTTON1_DOUBLE_CLICKED, BUTTON1_PRESSED, BUTTON1_RELEASED
use M_ncurses, only : BUTTON2_CLICKED, BUTTON2_DOUBLE_CLICKED, BUTTON2_PRESSED, BUTTON2_RELEASED
use M_ncurses, only : BUTTON3_CLICKED, BUTTON3_DOUBLE_CLICKED, BUTTON3_PRESSED, BUTTON3_RELEASED
use M_ncurses, only : C_NULL_PTR
!write(19
use M_ncurses, only : key_a1,        key_a3,      key_b2,       key_backspace, key_beg,    key_break,    key_btab,      key_c1
use M_ncurses, only : key_c3,        key_cancel,  key_catab,    key_clear,     key_close,  key_command,  key_copy,      key_create
use M_ncurses, only : key_ctab,      key_dc,      key_dl,       key_down,      key_eic,    key_end,      key_enter,     key_eol
use M_ncurses, only : key_eos,       key_event,   key_exit,     key_f0,        key_find,   key_help,     key_home,      key_ic
use M_ncurses, only : key_il,        key_left,    key_ll,       key_mark,      key_max,    key_message,  key_mouse,     key_move
use M_ncurses, only : key_next,      key_npage,   key_open,     key_options,   key_ppage,  key_previous, key_print,     key_redo
use M_ncurses, only : key_reference, key_refresh, key_replace,  key_reset,     key_resize, key_restart,  key_resume,    key_right
use M_ncurses, only : key_save,      key_sbeg,    key_scancel,  key_scommand,  key_scopy,  key_screate,  key_sdc,       key_sdl
use M_ncurses, only : key_select,    key_send,    key_seol,     key_sexit,     key_sf,     key_sfind,    key_shelp,     key_shome
use M_ncurses, only : key_sic,       key_sleft,   key_smessage, key_smove,     key_snext,  key_soptions, key_sprevious, key_sprint
use M_ncurses, only : key_sr,        key_sredo,   key_sreplace, key_sreset,    key_sright, key_srsume,   key_ssave
use M_ncurses, only : key_ssuspend,  key_stab,    key_sundo,    key_suspend,   key_undo,   key_up
use M_framework__journal, only : journal
implicit none
private
integer,parameter :: pg_lines=2000, pg_columns=256          ! upper limits for user input file data
character(len=pg_columns)         :: mesg                    ! message to appear in status bar
type(C_PTR)                       :: big_pd
character(len=pg_columns),target  :: page_pd(pg_lines)=' '  ! array to hold user definition of form

integer(C_INT)                    :: pad_corner_x=0, pad_corner_y=0   ! upper left-hand corner of rectangle displayed in pad
integer                           :: displaywidth_pd=0      ! width of section of pad displayed on screen

integer,save                      :: button_lines=4         ! number of lines used for buttons and message field
integer,save                      :: button_boxes(5,4)
logical                           :: G_submitted
character(len=:),allocatable      :: G_answers

character(len=pg_columns),pointer :: page_ptr(:)            ! user data to convert from input to window
integer,pointer                   :: icount_ptr             ! pointer to line count of data to convert from input to window
integer,target                    :: icount_pd              ! number of last line of data used in page_pd(*)
integer                           :: longline_pd            ! number of longest line of data used in page_pd(*)

public icount_ptr
public page_ptr
public page_pd
public icount_pd
public longline_pd
public fixedform
public loaddata
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
public :: nc_errmessage
public :: nc_printescape
public :: nc_printhtml
public :: nc_printunicode
public :: nc_printplain

contains

!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine nc_errmessage(msg) ! exit screen mode to use WRITE(3f) to print a message and then read a line to create a pause
   character(len=*),intent(in) :: msg
   integer                     :: ierr    ! return value for most ncurses(3c) functions
   integer                     :: ios     ! status return of READ(3f)
   ierr=def_prog_mode()                   ! Save the tty modes
   ierr=refresh()
   ierr=endwin()                          ! End curses mode temporarily
   write(*,*)msg
   read(*,'(a)',iostat=ios)
   ierr=refresh() ! Back to curses. You can once again use the full capabilities of curses; and the screen resumes where it was
   ierr=flash()
end subroutine nc_errmessage
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine nc_printescape(win,filename)
! @(#) plain text print reading window from ncurses
!! write as HTML or ANSI escape codes per ansi2html(1) script
!! maybe look in original page data instead like loaddata(3f)
!! what about alternate characters like box characters ?
!! could newterm(3c) be used to set to a file instead of a terminal to capture escape sequences and ensure they are VT102 commands?
!-----------------------------------------------------------------------------------------------------------------------------------
   type(C_PTR),intent(in)       :: win          ! window to print
   character(len=*),intent(in)  :: filename     ! filename to print to
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: my,mx        ! size of the specified window
   integer(C_LONG)              :: ch           ! long character in cell (attributes, color pair, and character)
   character(len=1)             :: let          ! the character in the cell
   integer                      :: ilet         ! decimal value of character in the cell
   integer(C_LONG)              :: attr         ! attributes of the cell
   integer                      :: ios          ! status from open(3f)
   character(len=256)           :: msg          ! message from open(3f)
   integer,save                 :: iout=11      ! unit to open(3f) for writing
   integer                      :: i,j          ! loop counters
   integer                      :: ierr
   integer(C_SHORT)             :: pair         ! color pair used by the cell
   integer(C_SHORT)             :: rf,gf,bf     ! color components of cell foreground
   integer(C_SHORT)             :: rb,gb,bb     ! color components of cell background
   integer(C_SHORT)             :: fg,bg        ! foreground and background color numbers of cell
   logical                      :: alt, bold, reverse
!-----------------------------------------------------------------------------------------------------------------------------------
   OPEN(UNIT=iout,FILE=trim(filename),ACTION='write',ACCESS='stream',FORM='unformatted',IOSTAT=ios,IOMSG=msg,STATUS='unknown')
   if(ios.ne.0)then
      call nc_errmessage("failed to open print file "//trim(filename)//':'//trim(msg))
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call getmaxyx(win,my,mx)                     ! size window size as defined (all of it, even if subsection being displayed)
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=0,my-1
      do j=0,mx-1
         ch=mvwinch(win,i,j)                    ! retrieve cell value
         ilet=iand(ch,A_CHARTEXT)               ! get decimal letter from cell using bit-mask
         let=char(ilet)                         ! get character from decimal
         pair=int(PAIR_NUMBER(ch),C_INT)        ! the color pair used to draw the cell
         ierr=pair_content(pair,fg,bg)          ! find out how a given color-pair is currently defined
         ierr=color_content(fg,rf,gf,bf)        ! extract red, green, and blue components in an initialized color
         ierr=color_content(bg,rb,gb,bb)        ! extract red, green, and blue components in an initialized color
         attr=iand(ch,A_ATTRIBUTES)             !! the attributes of the cell
         alt=.false.
         bold=.false.
         reverse=.false.
         if(iand(attr,A_BLINK).eq. A_BLINK)then           !  blink
         endif
         if(iand(attr,A_INVIS).eq. A_INVIS)then           !  invisible
         endif
         if(iand(attr,A_NORMAL).eq. A_NORMAL)then
         endif
         if(iand(attr,A_PROTECT).eq. A_PROTECT)then
         endif
         if(iand(attr,A_UNDERLINE).eq. A_UNDERLINE)then
         endif
         if(iand(attr,A_DIM).eq.A_DIM)then
         endif
         if(iand(attr,A_HORIZONTAL).eq.A_HORIZONTAL)then
         endif
         if(iand(attr,A_LEFT).eq.A_LEFT)then
         endif
         if(iand(attr,A_LOW).eq.A_LOW)then
         endif
         if(iand(attr,A_RIGHT).eq.A_RIGHT)then
         endif
         if(iand(attr,A_STANDOUT).eq.A_STANDOUT)then
         endif
         if(iand(attr,A_TOP).eq.A_TOP)then
         endif
         if(iand(attr,A_VERTICAL).eq.A_VERTICAL)then
         endif
         if(iand(attr,A_ALTCHARSET).eq.A_ALTCHARSET)then
            alt=.true.
            select case(let)
            case('l') ! ACS_ULCORNER  upper left corner
               let='#'
            case('m') ! ACS_LLCORNER  lower left corner
               let='#'
            case('k') ! ACS_URCORNER  upper right corner
               let='#'
            case('j') ! ACS_LRCORNER  lower right corner
               let='#'
            case('t') ! ACS_LTEE      tee pointing right
               let='+'
            case('u') ! ACS_RTEE      tee pointing left
               let='+'
            case('v') ! ACS_BTEE      tee pointing up
               let='+'
            case('w') ! ACS_TTEE      tee pointing down
               let='+'
            case('q') ! ACS_HLINE     horizontal line
               let='-'
            case('x') ! ACS_VLINE     vertical line
               let='|'
            case('n') ! ACS_PLUS      large plus or crossover
               let='+'
            case('`') ! ACS_DIAMOND
               let='o'
            end select
         endif
         if(iand(attr,A_BOLD).eq.A_BOLD)then
            bold=.true.
         endif
         if(iand(attr,A_REVERSE).eq.A_REVERSE)then
            reverse=.true.
         endif
         write(iout)let                         !! use stream position or buffer lines and print to remove trailing white space
      enddo
      write(iout)NEW_LINE('a')
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine nc_printescape
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine nc_printhtml(win,filename) ! @(#) print ncurses(3c) window as HTML
!-----------------------------------------------------------------------------------------------------------------------------------
   type(C_PTR),intent(in)       :: win          ! window to print
   character(len=*),intent(in)  :: filename     ! filename to print to
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: my,mx        ! size of the specified window
   integer(C_LONG)              :: cell         ! long character in cell (attributes, color pair, and character)
   character(len=1)             :: let          ! the character in the cell
   character(len=128)           :: lets         ! the characters that need printed in HTML to represent the cell
   integer                      :: ilet         ! decimal value of character in the cell
   integer(C_LONG)              :: attr         ! attributes of the cell
   integer                      :: ios          ! status from open(3f)
   character(len=256)           :: msg          ! message from open(3f)
   integer                      :: iout         ! unit to open(3f) for writing
   integer                      :: i,j          ! loop counters
   integer                      :: ierr
   integer(C_SHORT)             :: pair         ! color pair used by the cell
   integer(C_SHORT)             :: rf,gf,bf     ! color components of cell foreground
   integer(C_SHORT)             :: rb,gb,bb     ! color components of cell background
   integer(C_SHORT)             :: fg,bg        ! foreground and background color numbers of cell
   logical                      :: span=.false. !! started a span region or not. Might actually use it later
   character(len=100)           :: colordef
   character(len=100)           :: class
   character(len=100)           :: lastclass
!-----------------------------------------------------------------------------------------------------------------------------------
   if(filename.ne.'')then
      iout=11
      OPEN(UNIT=iout,FILE=trim(filename),ACTION='write',ACCESS='stream',FORM='unformatted',IOSTAT=ios,IOMSG=msg,STATUS='unknown')
      if(ios.ne.0)then
         call nc_errmessage("failed to open print file "//trim(filename)//':'//trim(msg))
         return
      endif
   else       !! Actually an error for now:: So far, no way to write a stream to stdout using Fortran
      iout=6
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
call ln('<html>')
call ln('<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>')
call ln('<head>')
call ln('<title></title>')
!-----------------------------------------------------------------------------------------------------------------------------------
call ln('<style type="text/css">')
call ln('.BO { font-weight:     bold;         }')
call ln('.U  { text-decoration: underline;    }')
call ln('.S  { text-decoration: line-through; }')
call ln('.BL { text-decoration: blink;        }')
call ln('.I  { font-style:      italic;       }')
call ln('.N  { font-style:      normal;       }')
!-----------------------------------------------------------------------------------------------------------------------------------
call ln('body{ color: #FFFFFF; background-color: #000000; }')
! write out defined color pairs
! CSS does not seem to support the idea of reverse video, so make a "PAIRnn" and a "RPAIRnn" for each pair
!! might be worth it to scan window and only print color definitions that are used
! always define foreground and background

call ln('.PAIR0{ color: #FFFFFF; background-color: #000000; }')
call ln('.RPAIR0{ color: #000000; background-color: #FFFFFF; }')

call getcolor(COLORS,COLOR_PAIRS) !! extension to get these set
COLOR_PAIRS=min(COLOR_PAIRS,4096) !! after last upgrade this starts failing on large values

do pair=0_C_SHORT,COLOR_PAIRS-1_C_SHORT
   !! even when color not defined did not get an error value back
   ierr=pair_content(pair,fg,bg)            ! find out how a given color-pair is currently defined
   ierr=color_content(fg,rf,gf,bf)          ! extract red, green, and blue components in an initialized color
   ierr=color_content(bg,rb,gb,bb)          ! extract red, green, and blue components in an initialized color
   if(fg+bg+rf+gf+bf+bg+rb+gb+bb.ne.0_C_SHORT)then     !assumption! assume this means not defined
      rf=rf*255_C_SHORT/1000_C_SHORT   ! the colors are in the range 0 to 1000 inclusive, want a two-digit hex value
      gf=gf*255_C_SHORT/1000_C_SHORT
      bf=bf*255_C_SHORT/1000_C_SHORT
      rb=rb*255_C_SHORT/1000_C_SHORT
      gb=gb*255_C_SHORT/1000_C_SHORT
      bb=bb*255_C_SHORT/1000_C_SHORT
      write(colordef,'(".PAIR",i0,"{ color: #",z2.2,z2.2,z2.2,"; background-color: #",z2.2,z2.2,z2.2,"; }")')pair,rf,gf,bf,rb,gb,bb
      write(iout)colordef
      write(iout)new_line('a')
      write(colordef,'(".RPAIR",i0,"{ color: #",z2.2,z2.2,z2.2,"; background-color: #",z2.2,z2.2,z2.2,"; }")')pair,rb,gb,bb,rf,gf,bf
      write(iout)colordef
      write(iout)new_line('a')
   endif
enddo
call ln('</style>')
!-----------------------------------------------------------------------------------------------------------------------------------
call ln('</head>')
call ln('<body>')
call ln('<pre>')
!-----------------------------------------------------------------------------------------------------------------------------------
   call getmaxyx(win,my,mx)                     ! size window size as defined (all of it, even if subsection being displayed)
!-----------------------------------------------------------------------------------------------------------------------------------
   lastclass=''
   do i=0,my-1
      do j=0,mx-1
         class=''
         cell=mvwinch(win,i,j)                    ! retrieve cell value
         ilet=iand(cell,A_CHARTEXT)               ! get decimal letter from cell using bit-mask
         let=char(ilet)                           ! get character from decimal
         lets=let                                 ! what to print for the character
         pair=int(PAIR_NUMBER(cell),C_INT)        ! the color pair used to draw the cell
         attr=iand(cell,A_ATTRIBUTES)             !! the attributes of the cell
         if(iand(attr,A_BLINK).eq. A_BLINK)          class=trim(class)//' BL'
         if(iand(attr,A_INVIS).eq. A_INVIS)          class=trim(class)//' IN'
         if(iand(attr,A_NORMAL).eq. A_NORMAL)        class=class !class=' '
         if(iand(attr,A_PROTECT).eq. A_PROTECT)      class=trim(class)//' '
         if(iand(attr,A_UNDERLINE).eq. A_UNDERLINE)  class=trim(class)//' U'
         if(iand(attr,A_DIM).eq.A_DIM)               class=trim(class)//' '
         if(iand(attr,A_HORIZONTAL).eq.A_HORIZONTAL) class=trim(class)//' '
         if(iand(attr,A_LEFT).eq.A_LEFT)             class=trim(class)//' '
         if(iand(attr,A_LOW).eq.A_LOW)               class=trim(class)//' '
         if(iand(attr,A_RIGHT).eq.A_RIGHT)           class=trim(class)//' '
         if(iand(attr,A_STANDOUT).eq.A_STANDOUT)     class=trim(class)//' '
         if(iand(attr,A_TOP).eq.A_TOP)               class=trim(class)//' '
         if(iand(attr,A_VERTICAL).eq.A_VERTICAL)     class=trim(class)//' '
         if(iand(attr,A_BOLD).eq.A_BOLD)             class=trim(class)//' BO'
         if(iand(attr,A_ITALIC).eq.A_ITALIC)         class=trim(class)//' I'
!-----------------------------------------------------------------------------------------------------------------------------------
         ! The Alternate Character Set ( includes lines & boxes)
         if(iand(attr,A_ALTCHARSET).eq.A_ALTCHARSET)then
            select case(let)
            case('l'); lets="&#9484;"   ! ACS_ULCORNER upper left corner        ┌
            case('m'); lets="&#9492;"   ! ACS_LLCORNER lower left corner        └
            case('k'); lets="&#9488;"   ! ACS_URCORNER upper right corner       ┐
            case('j'); lets="&#9496;"   ! ACS_LRCORNER lower right corner       ┘
            case('t'); lets="&#9500;"   ! ACS_LTEE     tee pointing right       ├
            case('u'); lets="&#9508;"   ! ACS_RTEE     tee pointing left        ┤
            case('v'); lets="&#9524;"   ! ACS_BTEE     tee pointing up          ┴
            case('w'); lets="&#9516;"   ! ACS_TTEE     tee pointing down        ┬
            case('q'); lets="&#9472;"   ! ACS_HLINE    horizontal line          ─
            case('x'); lets="&#9474;"   ! ACS_VLINE    vertical line            !
            case('n'); lets="&#9532;"   ! ACS_PLUS     large plus or crossover  ┼
            case('o'); lets="&#9146;"   ! ACS_S1       macron, overline, scan line 1 (above) ⎺
               lets="&macr;"
               lets="&#9146;"
            case('s'); lets="&#9149;"   ! ACS_S9       scan line 9 (below)      ⎽
            case('`'); lets="&#9670;"   ! ACS_DIAMOND  diamond                  ◆
               lets="&loz;"
               lets="&diams;"
            case('a'); lets="&#2591;"   ! ACS_CKBOARD  checker board (stipple)  ▒
               lets="&#9618;"
            case('f'); lets="&deg;"     ! ACS_DEGREE   degree symbol            °
            case('g'); lets="&plusmn;"  ! ACS_PLMINUS  plus/minus               ±
            case('~'); lets="&bull;"    ! ACS_BULLET   bullet                   · •
            case(','); lets="&larr;"    ! ACS_LARROW   arrow pointing left      ←
            case('+'); lets="&rarr;"    ! ACS_RARROW   arrow pointing right     →
            case('.'); lets="&darr;"    ! ACS_DARROW   arrow pointing down      ↓
            case('-'); lets="&uarr;"    ! ACS_UARROW   arrow pointing up        ↑
            case('h'); lets="&#9626;"   ! ACS_BOARD    board of squares         ▚
            case('i'); lets="&#9227;"   ! ACS_LANTERN  lantern symbol           ␋
            case('0'); lets="&block;"   ! ACS_BLOCK    solid square block       █
            case('p'); lets="&#9147;"   ! ACS_S3       scan line 3(at top)      ⎻
            case('r'); lets="&#9148;"   ! ACS_S7       scan line 7 (at bottom)  ⎼
            case('y'); lets="&le;"      ! ACS_LEQUAL   less/equal               ≤
            case('z'); lets="&ge;"      ! ACS_GEQUAL   greater/equal            ≥
            case('{'); lets="&pi;"      ! ACS_PI       Pi                       π
            case('|'); lets="&ne;"      ! ACS_NEQUAL   not equal                ≠
            case('}'); lets="&pound;"   ! ACS_STERLING UK pound sign            £
            end select
         else                           ! Regular Characters that are special in HTML
            select case(let)
            case('&'); lets='&amp;'     !   &  &#38;  {ampersand}
            case('<'); lets='&lt;'      !   <  &#60;  {less than}
            case('>'); lets='&gt;'      !   >  &#62;  {greater than}
            case('"'); lets='&quot;'    !   "  &#34;  {quotation mark}
            !case(' '); lets='&nbsp;'   !      &#160; {Non-breaking space}
            end select
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         if(iand(attr,A_REVERSE).eq.A_REVERSE)then
            write(class,'(a," RPAIR",i0)') trim(class),pair
         else
            write(class,'(a," PAIR",i0)') trim(class),pair
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         if(iand(attr,A_STANDOUT).eq.A_STANDOUT)then
            write(class,'(a," RPAIR",i0)') trim(class),pair
         else
            write(class,'(a," PAIR",i0)') trim(class),pair
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         if(class.ne.lastclass)then
            if(span)then
               write(iout)'</span>'
               span=.false.
            endif
            if(class.eq.' ')then
               write(iout)'<span class="N">'
            else
               write(iout)'<span class="'//trim(class)//'">'
            endif
            span=.true.
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         write(iout)lets(:max(1,len_trim(lets))) ! print a space even if string is blank
         lastclass=class
      enddo
      write(iout)NEW_LINE('a')
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   if(span)call ln('</span>')
   call ln('</pre>')
   call ln('</body>')
   call ln('</html>')
   endfile(unit=iout,iostat=ios,iomsg=msg) ! make sure file is truncated or longer old files may leave data in file
contains

subroutine ln(string)
implicit none
character(len=*) :: string
   write(iout)string
   write(iout)new_line('a')
end subroutine ln

end subroutine nc_printhtml
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine nc_printunicode(win,filename)
! @(#) plain text print reading window from ncurses
!! write as HTML or ANSI escape codes per ansi2html(1) script
!! maybe look in original page data instead like loaddata(3f)
!! what about alternate characters like box characters ?
!! could newterm(3c) be used to set to a file instead of a terminal to capture escape sequences and ensure they are VT102 commands?
!-----------------------------------------------------------------------------------------------------------------------------------
   type(C_PTR),intent(in)       :: win          ! window to print
   character(len=*),intent(in)  :: filename     ! filename to print to
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: my,mx        ! size of the specified window
   integer(C_LONG)              :: ch           ! long character in cell (attributes, color pair, and character)
   character(len=1)             :: let          ! the character in the cell
   integer                      :: ilet         ! decimal value of character in the cell
   integer(C_LONG)              :: attr         ! attributes of the cell
   integer                      :: ios          ! status from open(3f)
   character(len=256)           :: msg          ! message from open(3f)
   integer,save                 :: iout=11      ! unit to open(3f) for writing
   integer                      :: i,j          ! loop counters
   integer                      :: ierr
   integer(C_SHORT)             :: pair         ! color pair used by the cell
   integer(C_SHORT)             :: rf,gf,bf     ! color components of cell foreground
   integer(C_SHORT)             :: rb,gb,bb     ! color components of cell background
   integer(C_SHORT)             :: fg,bg        ! foreground and background color numbers of cell
   logical                      :: alt, bold, reverse
!-----------------------------------------------------------------------------------------------------------------------------------
   OPEN(UNIT=iout,FILE=trim(filename),ACTION='write',ACCESS='stream',FORM='unformatted',IOSTAT=ios,IOMSG=msg,STATUS='unknown')
   if(ios.ne.0)then
      call nc_errmessage("failed to open print file "//trim(filename)//':'//trim(msg))
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call getmaxyx(win,my,mx)                     ! size window size as defined (all of it, even if subsection being displayed)
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=0,my-1
      do j=0,mx-1
         ch=mvwinch(win,i,j)                    ! retrieve cell value
         ilet=iand(ch,A_CHARTEXT)               ! get decimal letter from cell using bit-mask
         let=char(ilet)                         ! get character from decimal
         pair=int(PAIR_NUMBER(ch),C_INT)        ! the color pair used to draw the cell
         ierr=pair_content(pair,fg,bg)          ! find out how a given color-pair is currently defined
         ierr=color_content(fg,rf,gf,bf)        ! extract red, green, and blue components in an initialized color
         ierr=color_content(bg,rb,gb,bb)        ! extract red, green, and blue components in an initialized color
         attr=iand(ch,A_ATTRIBUTES)             !! the attributes of the cell
         alt=.false.
         bold=.false.
         reverse=.false.
         if(iand(attr,A_BLINK).eq. A_BLINK)then           !  blink
         endif
         if(iand(attr,A_INVIS).eq. A_INVIS)then           !  invisible
         endif
         if(iand(attr,A_NORMAL).eq. A_NORMAL)then
         endif
         if(iand(attr,A_PROTECT).eq. A_PROTECT)then
         endif
         if(iand(attr,A_UNDERLINE).eq. A_UNDERLINE)then
         endif
         if(iand(attr,A_DIM).eq.A_DIM)then
         endif
         if(iand(attr,A_HORIZONTAL).eq.A_HORIZONTAL)then
         endif
         if(iand(attr,A_LEFT).eq.A_LEFT)then
         endif
         if(iand(attr,A_LOW).eq.A_LOW)then
         endif
         if(iand(attr,A_RIGHT).eq.A_RIGHT)then
         endif
         if(iand(attr,A_STANDOUT).eq.A_STANDOUT)then
         endif
         if(iand(attr,A_TOP).eq.A_TOP)then
         endif
         if(iand(attr,A_VERTICAL).eq.A_VERTICAL)then
         endif
         if(iand(attr,A_ALTCHARSET).eq.A_ALTCHARSET)then
            alt=.true.
            select case(let)
            case('l') ! ACS_ULCORNER  upper left corner
               let='#'
            case('m') ! ACS_LLCORNER  lower left corner
               let='#'
            case('k') ! ACS_URCORNER  upper right corner
               let='#'
            case('j') ! ACS_LRCORNER  lower right corner
               let='#'
            case('t') ! ACS_LTEE      tee pointing right
               let='+'
            case('u') ! ACS_RTEE      tee pointing left
               let='+'
            case('v') ! ACS_BTEE      tee pointing up
               let='+'
            case('w') ! ACS_TTEE      tee pointing down
               let='+'
            case('q') ! ACS_HLINE     horizontal line
               let='-'
            case('x') ! ACS_VLINE     vertical line
               let='|'
            case('n') ! ACS_PLUS      large plus or crossover
               let='+'
            case('`') ! ACS_DIAMOND
               let='o'
            end select
         endif
         if(iand(attr,A_BOLD).eq.A_BOLD)then
            bold=.true.
         endif
         if(iand(attr,A_REVERSE).eq.A_REVERSE)then
            reverse=.true.
         endif
         write(iout)let                         !! use stream position or buffer lines and print to remove trailing white space
      enddo
      write(iout)NEW_LINE('a')
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine nc_printunicode
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine nc_printplain(win,filename) ! @(#) plain text print reading window from ncurses
   use M_ncurses
   implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
   type(C_PTR),intent(in)       :: win          ! window to print
   character(len=*),intent(in)  :: filename     ! filename to print to
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: my,mx        ! size of the specified window
   integer(C_LONG)              :: cell         ! cell information (attributes, color pair, and character)
   character(len=1)             :: let          ! the character in the cell
   integer(C_LONG)              :: attr         ! attributes of the cell
   integer                      :: ios          ! status from open(3f)
   character(len=256)           :: msg          ! message from open(3f)
   integer,save                 :: iout=11      ! unit to open(3f) for writing
   integer                      :: i,j          ! loop counters
!-----------------------------------------------------------------------------------------------------------------------------------
   OPEN(UNIT=iout,FILE=trim(filename),ACTION='write',ACCESS='stream',FORM='unformatted',IOSTAT=ios,IOMSG=msg,STATUS='unknown')
   if(ios.ne.0)then
      call nc_errmessage("failed to open print file "//trim(filename)//':'//trim(msg))
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call getmaxyx(win,my,mx)                     ! window size as defined (all of it, even if subsection being displayed)
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=0,my-1                                  ! scan window cell by cell
      do j=0,mx-1
         cell=mvwinch(win,i,j)                  ! retrieve cell value
         let=char(iand(cell,A_CHARTEXT))        ! get letter from cell using bit-mask
         attr=iand(cell,A_ATTRIBUTES)           ! the attributes of the cell
         if(iand(attr,A_ALTCHARSET).eq.A_ALTCHARSET)then  ! if an alternate character pick a standard character
            select case(let)
            case('l');let='#' ! ACS_ULCORNER  upper left corner
            case('m');let='#' ! ACS_LLCORNER  lower left corner
            case('k');let='#' ! ACS_URCORNER  upper right corner
            case('j');let='#' ! ACS_LRCORNER  lower right corner
            case('t');let='+' ! ACS_LTEE      tee pointing right
            case('u');let='+' ! ACS_RTEE      tee pointing left
            case('v');let='+' ! ACS_BTEE      tee pointing up
            case('w');let='+' ! ACS_TTEE      tee pointing down
            case('q');let='-' ! ACS_HLINE     horizontal line
            case('x');let='|' ! ACS_VLINE     vertical line
            case('n');let='+' ! ACS_PLUS      large plus or crossover
            case('`');let='o' ! ACS_DIAMOND
               ! for the fixedform(1) program change diamond if "selected" so can tell the difference in output
               if(iand(attr,A_STANDOUT).eq.A_STANDOUT)then  ! using reverse or standout on a diamond to select it as a menu item
                  let='X'
               endif
               if(iand(attr,A_REVERSE).eq.A_REVERSE)then
                  let='X'
               endif
            end select
         endif
         write(iout)let          !! use stream position or buffer lines and print to remove trailing white space
      enddo
      write(iout)NEW_LINE('a')   ! write system-appropriate end-of-line
   enddo
end subroutine nc_printplain

subroutine fixedform(tabs)
   character(len=:),allocatable,optional,intent(out) :: tabs
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_INT)     :: ierr
   integer(C_INT)     :: ic
   integer(C_INT)     :: previous
!-----------------------------------------------------------------------------------------------------------------------------------
   G_submitted=.false.                      ! flag to exit fixedform(3f)
   previous=0
   call start_screen()                      ! set up for using main window in ncurses(3c)
!-----------------------------------------------------------------------------------------------------------------------------------
   big_pd=newpad(icount_pd,longline_pd)     ! create window to hold complete form
   if (.not.c_associated(big_pd)) then
      call nc_errmessage('the area to draw the form in did not initialize')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call user_to_screen(big_pd)              ! use data in page(*) to build ncurses window
!-----------------------------------------------------------------------------------------------------------------------------------
   call redraw()
!-----------------------------------------------------------------------------------------------------------------------------------
   READKEY: do
      ic=getch()
      call process_keypress(ic,previous)
      previous=ic
      if(G_submitted)exit
   enddo READKEY
   ierr=endwin()
   if(present(tabs))then
      tabs=G_answers
   endif
end subroutine fixedform
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
recursive subroutine process_keypress(ch,pch) ! @(#) take an appropriate action for each keypress
! (although the actions taken are application-specific this is a good skeleton for processing keystrokes)
!  given integer "character" take action
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_INT),intent(in)            :: ch
   integer(C_INT),intent(in),optional   :: pch ! previous key pressed, ignored except in vi(1) mode
   integer(C_INT)                       :: previous ! copy of pch if present, else zero
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=256)        :: searchstring
   character(len=128)        :: mesg
   integer                   :: ierr
   integer                   :: r
   integer(C_INT)            :: ix,iy
   logical,save              :: vi_mode=.false.
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_INT)            :: py,px
   logical                   :: isunderline=.false.
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_LONG)           :: cell
   integer(C_SHORT)          :: pair
   integer(C_LONG)           :: attr,ch8
   integer(C_INT)            :: ich_dum
   character(LEN=1)          :: ch_dum
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_INT)            :: start_underline
   integer(C_INT)            :: end_underline
!-----------------------------------------------------------------------------------------------------------------------------------
   type(MEVENT)              :: eek
   integer(C_LONG),save      :: mouse_mask
   logical,save              :: firstcall=.true.
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                   :: ibox
!-----------------------------------------------------------------------------------------------------------------------------------
   ch8=ch
   if(firstcall)then
      mouse_mask=mousemask(BUTTON1_CLICKED,C_NULL_PTR)
      !mouse_mask=mousemask(ALL_MOUSE_EVENTS,C_NULL_PTR)
      firstcall=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(pch))then
      previous=pch
   else
      previous=0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   mesg=""
   call getyx(stdscr,iy,ix)
!-----------------------------------------------------------------------------------------------------------------------------------
   call cursor2pad(iy,ix,py,px,cell)                  ! convert screen cursor to pad position and get cell data
   call get_cell_components(cell,attr,pair,ich_dum,ch_dum)
   if(iand(attr,A_UNDERLINE).eq. A_UNDERLINE)then
      isunderline=.true.
   else
      isunderline=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call inbox(ix,iy,ibox)
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ch >= KEY_F(1).and. ch <= KEY_F(64) )then       ! Without keypad enabled this will not get to us either
      write(mesg,'(a,i0)')"Function Key ",ch-KEY_F0
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(ch>=33 .and. ch<=126)then   ! regular printable character
      write(mesg,'(a,a)')"regular character ",char(ch)
      if(vi_mode.or.(.not.isunderline))then           ! if in vi(1) mode or character underneath is not an underline field
         select case(char(ch))
         !=================================================================
         ! move cursor unless hit edge then scroll subwindow if not at edge
         case('j') ! V
            if(iy.ge.LINES-1)then
               call move_pd(1,0,'L')
            else
               ierr=move(iy+1,ix)
            endif
         case('k') ! ^
            if(iy.gt.0)then
               ierr=move(iy-1,ix)
            else
               call move_pd(-1,0,'L')
            endif
         case('l') ! >
            if(ix.lt.COLS-1)then
               ierr=move(iy,ix+1)
            else
               call move_pd(0,1,'L')
            endif
         case('h') ! <
            if(ix.gt.0)then
               ierr=move(iy,ix-1)
            else
               call move_pd(0,-1,'L')
            endif
         !=================================================================
         case('g') ; call home_pad();mesg=trim(mesg)//' : home form'         ! "home" user data
         case('i') ; call insert_in_underline()                            ! <
         case('n') ; call find_next()                                      ! next input field
         case('N') ; call space_bar(mesg);call find_next()                  ! change menu; then next input field
         case('p') ; call find_previous()                                  ! previous input field
         case('P') ; call space_bar(mesg);call find_previous()              ! change menu; then previous input field
         case('q') ; call my_exit()                                        ! quit with prompt
         case('r') ; call replace_in_underline()                           !
         case('R') ; call replace_in_underline()                           !
         case('x') ; if(isunderline)then
                        call delete_in_underline()
                     else
                        call space_bar(mesg)
                     endif
         case('z') ; call move_pd(iy,0,'L')                                ! current line to top of screen, like z<CR> or z+
         case('Z') ; call move_pd(iy-((LINES-1)-button_lines),0,'L')       ! current line to bottom of screen, like z-
         case('0')                                                         ! make first column of pad data be first column displayed
            pad_corner_x=0
            ierr=move(iy, max(0,(cols-longline_pd)/2-1))
            call refresh_pd()
         !=================================================================
         ! like vi(1) cursor movement but move selection that can be viewed
         case('J') ; call move_pd(1,0,'L')                                 ! V
         case('K') ; call move_pd(-1,0,'L')                                ! ^
         case('L') ; call move_pd(0,1,'L')                                 ! >
         case('H') ; call move_pd(0,-1,'L')                                ! <
         !=================================================================
         case('/')
            call promptfor(char(ch),searchstring)
            mesg=searchstring
            call searchfor(big_pd,searchstring)                           ! search for string in window starting at current position
         !=================================================================
         case(':')
            call promptfor(char(ch),searchstring)
            mesg=searchstring
         !=================================================================
         end select
      else
         ierr=mvwaddch(big_pd,py,px,ior(ch8,attr))
         call underline_ends(start_underline,end_underline) ! assuming current position is on an underline, find ends of underlining
         if(ix.eq.end_underline)then  ! if at end of underline do not advance cursor
            ierr=move(iy,ix)
         else
            ierr=move(iy,ix+1)
         endif
         call refresh_pd()                                                ! refresh so can see change in display
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
   else
      SELECT CASE (ch)
      case (key_break ) ; mesg="break"
      case (key_down ) ; mesg="down: Down arrow key"
            if(iy.ge.LINES-1)then
               call move_pd(1,0,'L')
            else
               ierr=move(iy+1,ix)
            endif
      case (key_up ) ; mesg="up: Up arrow key"
            if(iy.gt.0)then
               ierr=move(iy-1,ix)
            else
               call move_pd(-1,0,'L')
            endif
      case (key_left ) ; mesg="left: Left arrow key"
            if(ix.gt.0)then
               ierr=move(iy,ix-1)
            else
               call move_pd(0,-1,'L')
            endif
      case (key_right ) ; mesg="right: Right arrow key"
            if(ix.lt.COLS-1)then
               ierr=move(iy,ix+1)
            else
               call move_pd(0,1,'L')
            endif
      case (key_home ) ; mesg="home: home key"
         call home_pad()                                       ! "home" user data
      case (key_backspace )     ; mesg="backspace"  ! not on all PC keyboards
         if(ix.gt.0)then
            ierr=move(iy,ix-1)
         else
            call move_pd(0,-1,'L')
         endif
      case (key_f0 )            ; mesg="f0: function keys; 64 reserved"
      case (key_dl )            ; mesg="dl: delete line"
      case (key_il )            ; mesg="il: insert line"
      case (key_dc )            ; mesg="dc: delete character"
         if(isunderline) call delete_in_underline()
      case (key_ic )            ; mesg="ic: insert char or enter ins mode"
         if(isunderline) call insert_in_underline()
      case (key_eic )           ; mesg="eic: exit insert char mode"
      case (key_clear )         ; mesg="clear: clear screen"
      case (key_eos )           ; mesg="eos: clear to end of screen"
      case (key_eol )           ; mesg="eol: clear to end of line"
      case (key_sf )            ; mesg="sf: scroll 1 line forward"
         call move_pd(1,0,'L')
      case (key_sr )            ; mesg="sr: scroll 1 line back (reverse)"
         call move_pd(-1,0,'L')
      case (key_npage )         ; mesg="npage: next page"
         call move_pd(1,0,'P')
      case (key_ppage )         ; mesg="ppage: previous page"
         call move_pd(-1,0,'P')
      case (key_stab )          ; mesg="stab: set tab"
      case (key_ctab )          ; mesg="ctab: clear tab"
      case (key_catab )         ; mesg="catab: clear all tabs"
      case (key_enter )         ; mesg="enter: enter or send" ! unreliable
         if(ibox.eq.0)then
            call space_bar(mesg)
         else
            call button_action()
            if(ibox.eq.5)return
         endif
      case (key_sreset )        ; mesg="sreset: soft/reset" ! unreliable
      case (key_reset )         ; mesg="reset: reset/hard reset" ! unreliable
      case (key_print )         ; mesg="print: print/copy"
         call printit()
      case (key_ll )            ; mesg="ll: home down/bottom (lower left)"
      case (key_a1 )            ; mesg="a1:"
      case (key_a3 )            ; mesg="a3:"
      case (key_b2 )            ; mesg="b2:"
      case (key_c1 )            ; mesg="c1:"
      case (key_c3 )            ; mesg="c3:"
      case (key_btab )          ; mesg="btab: Back tab key"
               call find_previous()
      case (key_beg )           ; mesg="beg: beginning key"
      case (key_cancel )        ; mesg="cancel: cancel key"
      case (key_close )         ; mesg="close: close key"
      case (key_command )       ; mesg="command: command key"
      case (key_copy )          ; mesg="copy: copy key"
      case (key_create )        ; mesg="create: create key"
      case (key_end )           ; mesg="end: end key"
         call message(mesg)                  ! draw message line
         call extract_answers_tabs()
         ierr=flash()
         return
      case (key_exit )          ; mesg="exit: exit key"
      case (key_find )          ; mesg="find: find key"
      case (key_help )          ; mesg="help: help key"
      case (key_mark )          ; mesg="mark: mark key"
      case (key_message )       ; mesg="message: message key"
      case (key_move )          ; mesg="move: move key"
      case (key_next )          ; mesg="next: next object key"
      case (key_open )          ; mesg="open: open key"
      case (key_options )       ; mesg="options: options key"
      case (key_previous )      ; mesg="previous: previous object key"
      case (key_redo )          ; mesg="redo: redo key"
      case (key_reference )     ; mesg="reference: reference key"
      case (key_refresh )       ; mesg="refresh: refresh key"
         ierr=refresh()
      case (key_replace )       ; mesg="replace: replace key"
      case (key_restart )       ; mesg="restart: restart key"
      case (key_resume )        ; mesg="resume: resume key"
      case (key_save )          ; mesg="save: save key"
      case (key_sbeg )          ; mesg="sbeg: shifted beginning key"
      case (key_scancel )       ; mesg="scancel: shifted cancel key"
      case (key_scommand )      ; mesg="scommand: shifted command key"
      case (key_scopy )         ; mesg="scopy: shifted copy key"
      case (key_screate )       ; mesg="screate: shifted create key"
      case (key_sdc )           ; mesg="sdc: shifted delete char key"
      case (key_sdl )           ; mesg="sdl: shifted delete line key"
      case (key_select )        ; mesg="select: select key"
      case (key_send )          ; mesg="send: shifted end key"
      case (key_seol )          ; mesg="seol: shifted clear line key"
      case (key_sexit )         ; mesg="sexit: shifted exit key"
      case (key_sfind )         ; mesg="sfind: shifted find key"
      case (key_shelp )         ; mesg="shelp: shifted help key"
      case (key_shome )         ; mesg="shome: shifted home key"
      case (key_sic )           ; mesg="sic: shifted input key"
      case (key_sleft )         ; mesg="sleft: shifted left arrow key"
         call move_pd(0,-1,'L')
      case (key_smessage )      ; mesg="smessage: shifted message key"
      case (key_smove )         ; mesg="smove: shifted move key"
      case (key_snext )         ; mesg="snext: shifted next key"
      case (key_soptions )      ; mesg="soptions: shifted options key"
      case (key_sprevious )     ; mesg="sprevious: shifted prev key"
      case (key_sprint )        ; mesg="sprint: shifted print key"
      case (key_sredo )         ; mesg="sredo: shifted redo key"
      case (key_sreplace )      ; mesg="sreplace: shifted replace key"
      case (key_sright )        ; mesg="sright: shifted right arrow"
         call move_pd(0,1,'L')
      case (key_srsume )        ; mesg="srsume: shifted resume key"
      case (key_ssave )         ; mesg="ssave: shifted save key"
      case (key_ssuspend )      ; mesg="ssuspend: shifted suspend key"
      case (key_sundo )         ; mesg="sundo: shifted undo key"
      case (key_suspend )       ; mesg="suspend: suspend key"
      case (key_undo )          ; mesg="undo: undo key"
      case (key_mouse )         ; mesg="mouse: mouse key"
         ierr=getmouse(eek)
         !ierr=mvaddch(eek%y,eek%x,int(ichar('*'),C_LONG))
         !------------------------       !*! some of this is not needed
         ierr=wmove(stdscr,eek%y,eek%x)
         ierr=refresh()
         call getyx(stdscr,iy,ix)
         call cursor2pad(iy,ix,py,px,cell)                  ! convert screen cursor to pad position and get cell data
         call get_cell_components(cell,attr,pair,ich_dum,ch_dum)
         if(iand(attr,A_UNDERLINE).eq. A_UNDERLINE)then
            isunderline=.true.
         else
            isunderline=.false.
         endif
         !------------------------
         call inbox(ix,iy,ibox)
         if(ibox.eq.0)then
            !write(19,*)'EEK=',eek
            !write(19,*) 'MASK=', ALL_MOUSE_EVENTS
            !write(19,*)BUTTON1_PRESSED,BUTTON1_RELEASED,BUTTON1_CLICKED,BUTTON1_DOUBLE_CLICKED
            !write(19,*)BUTTON2_PRESSED,BUTTON2_RELEASED,BUTTON2_CLICKED,BUTTON2_DOUBLE_CLICKED
            !write(19,*)BUTTON3_PRESSED,BUTTON3_RELEASED,BUTTON3_CLICKED,BUTTON3_DOUBLE_CLICKED
            mesg="mouse: key"
            !if(eek%bstate.eq.BUTTON1_PRESSED)then
               call space_bar(mesg)      ! maybe clicked on a menu option
            !endif
         else
            call button_action(eek)
            if(ibox.eq.5)return
         endif
         !------------------------
         r = curs_set(0)          ! first, turn the cursor off
         ierr=refresh()
         ierr=napms(100)          ! pause for 1/10 of a second
         ierr = curs_set(r)       ! second, reset the cursor
         !------------------------
      case (key_resize )        ; mesg="resize: window resize"
          call redraw()
      case (key_event )         ; mesg="event: event key"
      case (key_max )           ; mesg="max: undo key"
      CASE DEFAULT
        mesg=""
      END SELECT
      if(mesg == "" )then
         SELECT CASE (ch)
         !------------------------------------------
         !WARNING: SOMETIMES ALTERNATES ARE USED
         !00 (NUL) ctrl @ NULL               ctrl-?
         !1C (FS)  ctrl \ FILE SEPARATOR     ctrl-@
         !1E (RS)  ctrl ^ RECORD SEPARATOR   ctrl-=
         !7F (DEL) ctrl ? DELETE
         !------------------------------------------
            CASE(0) ; mesg="NUL '\0' ctrl-@  NULL"
            CASE(1) ; mesg="SOH      ctrl-A  START OF HEADING"
            CASE(2) ; mesg="STX      ctrl-B  START OF TEXT"
               call move_pd(-1,0,'P')
            CASE(3) ; mesg="ETX      ctrl-C  END OF TEXT"
               !call my_exit()
               ierr=def_prog_mode()                                           ! Save the tty modes
               ierr=refresh()                                                 ! clear screen
               ierr=endwin()                                                  ! End curses mode
               write(*,*)'EXIT'
               stop
            CASE(4) ; mesg="EOT      ctrl-D  END OF TRANSMISSION"
               call move_pd(1,0,'H')
            CASE(5) ; mesg="ENQ      ctrl-E  ENQUIRY"
               call move_pd(1,0,'L')
            CASE(6) ; mesg="ACK      ctrl-F  ACKNOWLEDGE"
               call move_pd(1,0,'P')
            CASE(7) ; mesg="BEL '\a  ctrl-G  BELL'"
               ierr=beep()
            CASE(8) ; mesg="BS  '\b' ctrl-H  BACKSPACE"
               if(ix.gt.0)then
                  ierr=move(iy,ix-1)
               else
                  call move_pd(0,-1,'L')
               endif
            CASE(9) ; mesg="HT  '\t' ctrl-I  HORIZONTAL TABULATION"
               call find_next()
            CASE(10) ; mesg="LF '\n' ctrl-J  LINE FEED"
               call space_bar(mesg)
               call find_next()
            CASE(11) ; mesg="VT '\v' ctrl-K  VERTICAL TABULATION"
            CASE(12) ; mesg="FF '\f' ctrl-L  FORM FEED or NEW PAGE"
               call move_pd(0,-1,'L')
            CASE(13) ; mesg="CR '\r' ctrl-M  CARRIAGE RETURN"
               call message(mesg)                  ! draw message line
               call extract_answers_tabs()
               ierr=flash()
               return
            CASE(14) ; mesg="SO      ctrl-N  SHIFT OUT"
               call find_next()
            CASE(15) ; mesg="SI      ctrl-O  SHIFT IN"
               call printit()
            CASE(16) ; mesg="DLE     ctrl-P  DATA LINK ESCAPE"
               call find_previous()
            CASE(17) ; mesg="DC1     ctrl-Q  DEVICE CONTROL 1"
               call my_exit()                                        ! quit
            CASE(18) ; mesg="DC2     ctrl-R  DEVICE CONTROL 2"
               call move_pd(0,1,'L')
            CASE(19) ; mesg="DC3     ctrl-S  DEVICE CONTROL 3"
               call message(mesg)                  ! draw message line
               call extract_answers_tabs()
               ierr=flash()
               return
            CASE(20) ; mesg="DC4     ctrl-T  DEVICE CONTROL 4"
            CASE(21) ; mesg="NAK     ctrl-U  NEGATIVE ACKNOWLEDGE"
               call move_pd(-1,0,'H')
            CASE(22) ; mesg="SYN     ctrl-V  SYNCHRONOUS IDLE"
               vi_mode=.true.
               mesg=trim(mesg)// ' :: vi mode on'
            CASE(23) ; mesg="ETB     ctrl-W  END OF TRANSMISSION BLOCK"
            CASE(24) ; mesg="CAN     ctrl-X  CANCEL"
               if(isunderline) call delete_in_underline()
            CASE(25) ; mesg="EM      ctrl-Y  END OF MEDIUM"
               call move_pd(-1,0,'L')
            CASE(26) ; mesg="SUB     ctrl-Z  SUBSTITUTE"
               call nc_errmessage("TEST of nc_errmessage")
            CASE(27) ; mesg="ESC     ctrl-[  ESCAPE"
               !call my_exit()    !*! gets called too often by undefined function keys, which often send sequences starting with esc
               if(vi_mode)then
                  vi_mode=.false.
                  mesg=trim(mesg)// ' :: vi mode off'
               else
                  vi_mode=.true.
                  mesg=trim(mesg)// ' :: vi mode on'
               endif
            CASE(28) ; mesg="FS      ctrl-\  FILE SEPARATOR"
            CASE(29) ; mesg="GS      ctrl-]  GROUP SEPARATOR"
            CASE(30) ; mesg="RS      ctrl-^  RECORD SEPARATOR"
            CASE(31) ; mesg="US      ctrl-_  UNIT SEPARATOR"
            CASE(32) ; mesg="SPACE"
               if(isunderline)then
                  ierr=mvwaddch(big_pd,py,px,ior(ch8,attr))
                  ierr=move(iy,ix+1)
                  call refresh_pd()                                                 ! refresh so can see change in display
               else
                  call space_bar(mesg)
               endif
            CASE(127) ; mesg="DEL"
         CASE DEFAULT
            mesg=""
         END SELECT
         if(mesg == "" )then
            write(mesg,'(a,i0)') "UNKNOWN: The key value is ",ch
         endif
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call message(mesg)                  ! draw message line
!-----------------------------------------------------------------------------------------------------------------------------------
contains
subroutine button_action(eek)
type(MEVENT),optional     :: eek
   select case(ibox)
   case(1) ! help
   case(2) ! home
      mesg=trim(mesg)//' : home form'         ! "home" user data
      call home_pad()
   case(3) ! next page
      mesg="npage: next page"
      if(present(eek))then
         select case(eek%bstate)
            case (BUTTON1_PRESSED,BUTTON1_RELEASED,BUTTON1_CLICKED,BUTTON1_DOUBLE_CLICKED)
               mesg=trim(mesg)//" BUTTON 1"
               call move_pd(1,0,'P')
            case (BUTTON2_PRESSED,BUTTON2_RELEASED,BUTTON2_CLICKED,BUTTON2_DOUBLE_CLICKED)
               mesg=trim(mesg)//" BUTTON 2"
               call move_pd(1,0,'P')
            case (BUTTON3_PRESSED,BUTTON3_RELEASED,BUTTON3_CLICKED,BUTTON3_DOUBLE_CLICKED)
               mesg=trim(mesg)//" BUTTON 3"
               call move_pd(1,0,'P')
            case default
               call move_pd(1,0,'P')
         end select
      else
         call move_pd(1,0,'P')
      endif
   case(4) ! previous page
      mesg="ppage: previous page"
      if(present(eek))then
         select case(eek%bstate)
            case (BUTTON1_PRESSED,BUTTON1_RELEASED,BUTTON1_CLICKED,BUTTON1_DOUBLE_CLICKED)
               mesg=trim(mesg)//" BUTTON 1"
               call move_pd(-1,0,'P')
            case (BUTTON2_PRESSED,BUTTON2_RELEASED,BUTTON2_CLICKED,BUTTON2_DOUBLE_CLICKED)
               mesg=trim(mesg)//" BUTTON 2"
               call move_pd(-1,0,'P')
            case (BUTTON3_PRESSED,BUTTON3_RELEASED,BUTTON3_CLICKED,BUTTON3_DOUBLE_CLICKED)
               mesg=trim(mesg)//" BUTTON 3"
               call move_pd(-1,0,'P')
            case default
               call move_pd(-1,0,'P')
         end select
      else
         call move_pd(-1,0,'P')
      endif
   case(5) ! submit
      mesg="SUBMIT"
      call message(mesg)                  ! draw message line
      call extract_answers_tabs()
      ierr=flash()
   end select
end subroutine button_action
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine process_keypress
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine underline_ends(start_underline,end_underline) ! assuming current position is on an underline, find ends of underlining
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_INT),intent(out):: start_underline, end_underline
   integer(C_INT)            :: j, py, px, my, mx, hy, hx
   integer                   :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_LONG)           :: cell
   integer(C_SHORT)          :: pair_dum
   integer(C_LONG)           :: attr
   integer(C_INT)            :: ich_dum
   character(LEN=1)          :: ch_dum
!-----------------------------------------------------------------------------------------------------------------------------------
   call getyx(stdscr,hy,hx)               ! cache initial cursor position
   call getmaxyx(big_pd,my,mx)            ! size of the specified window as defined (all of it, even if subsection being displayed)
   call cursor2pad(hy,hx,py,px,cell)      ! convert screen cursor to pad position and get cell data
   end_underline=px-1
   do j=px,mx-1                                                   ! find end of underline section and delete last one
      cell=mvwinch(big_pd,py,j)                                   ! retrieve cell value
      call get_cell_components(cell,attr,pair_dum,ich_dum,ch_dum)
      if(iand(cell,A_UNDERLINE).ne.A_UNDERLINE)then               ! found end of underline section
         end_underline=j-1
         exit
      endif
   enddo
   start_underline=0
   do j=px,0,-1
      cell=mvwinch(big_pd,py,j)                                   ! retrieve cell value
      call get_cell_components(cell,attr,pair_dum,ich_dum,ch_dum)
      if(iand(cell,A_UNDERLINE).ne.A_UNDERLINE)then               ! found start of underline section
         start_underline=j+1
         exit
      endif
   enddo
   ierr=wmove(big_pd,py,px)                                       ! restore cursor position
   ierr=move(hy,hx)                                               ! restore cursor position
end subroutine underline_ends
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine delete_in_underline
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                   :: py, px, hy, hx, my_dum, mx
   integer                   :: j
   integer                   :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_LONG)           :: cell
   integer(C_SHORT)          :: pair, hpair
   integer(C_LONG)           :: attr, hattr
   integer(C_INT)            :: ich_dum
   character(LEN=1)          :: ch_dum, ch
   integer(C_INT)            :: start_underline,end_underline
   character(len=pg_columns,kind=C_CHAR) :: str
   integer(C_INT)            :: n
!-----------------------------------------------------------------------------------------------------------------------------------
!*! check logic if underscored region is clipped from screen
   call getyx(stdscr,hy,hx)                                          ! cache initial cursor position
   call cursor2pad(hy,hx,py,px,cell)                                 ! convert screen cursor to pad position and get cell data
   call get_cell_components(cell,hattr,hpair,ich_dum,ch_dum)
   ierr=wdelch(big_pd)                                               ! delete character at current position
   call getmaxyx(big_pd,my_dum,mx)        ! size of the specified window as defined (all of it, even if subsection being displayed)
   do j=px,mx-1                                                      ! find end of underline section and insert space
      cell=mvwinch(big_pd,py,j)                                      ! retrieve cell value
      call get_cell_components(cell,attr,pair,ich_dum,ch_dum)
      if(iand(attr,A_UNDERLINE).ne.A_UNDERLINE)then                  ! found end of underline section
         ierr=mvwinsch(big_pd,py,j,int(ior(ichar(' '),int(hattr)),C_LONG))! deleted a character so assume there is room to insert space
         exit
      endif
   enddo
   ierr=move(hy,hx)                                                  ! restore cursor position
   call refresh_pd()                                                 ! refresh so can see change in display
   !----- try this: move cursor one to left if now on a blank at end of string, more like vim(1) editor
   call underline_ends(start_underline,end_underline) ! assuming current position is on an underline, find ends of underlining
   if(hx.gt.start_underline)then          ! if not in first column of underlined region might move cursor to left if at string end
      ch=char(winch(big_pd))                                         ! retrieve character value at cursor position
      if(ch.eq.' ')then                                              ! if now on a blank character move one to left
         n=end_underline-start_underline+1
         str=' '
         ierr= mvwinnstr(big_pd,py,start_underline,str,n)
         if(len_trim(str(1:n))+start_underline.le.px)then            ! on blank past end of string
            ierr=move(hy,hx-1)                                       ! restore cursor position
            call refresh_pd()                                        ! refresh so can see change in display
         endif
      endif
   endif
   !-----
end subroutine delete_in_underline
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine replace_in_underline
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_INT)            :: py, px, hy, hx
   integer                   :: ierr
   integer(C_INT)            :: start_of_underline, end_of_underline
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_LONG)           :: cell, cellread
   integer(C_SHORT)          :: hpair
   integer(C_LONG)           :: hattr
   integer(C_INT)            :: ich_dum
   character(LEN=1)          :: ch_dum
!-----------------------------------------------------------------------------------------------------------------------------------
   call message(mesg)                      ! draw message set by process_key(3f) now because going to read more keys
   call getyx(stdscr,hy,hx)               ! cache initial cursor position
   call cursor2pad(hy,hx,py,px,cell)      ! convert screen cursor to pad position and get cell data
   call get_cell_components(cell,hattr,hpair,ich_dum,ch_dum)
   call underline_ends(start_of_underline,end_of_underline)             ! get start and end coordinates of underline in pad coords.
   !write(20,*)'START,END=',start_of_underline,end_of_underline
   INFINITE: do
      call getyx(stdscr,hy,hx)                                          ! get current cursor position on display screen
      call cursor2pad(hy,hx,py,px,cell)                                 ! convert screen cursor to pad position (and get cell data)
      !write(20,*)'HY,HX,PY,PX=',hy,hx,py,px
      if(px.lt.start_of_underline.or.px.gt.end_of_underline)then        ! if moved left or right out of underline region exit
         exit INFINITE
      endif
      if(hx.ge.pad_corner_x+displaywidth_pd-1)then                      ! filling out an underscore region that goes off screen
         call move_pd(0,1,'L')                                          ! move displayed area one to right
      endif
      cellread=getch()
      if(cellread>=32 .and. cellread<=126)then                          ! insert regular character and stay in replace mode
         write(mesg,'(a,a)')"REPLACE regular character ",char(cellread)  ! write message bar message
         call message(mesg)
         cellread=ior(cellread,hattr)                                   ! add original attributes to character read from keyboard
         ierr=mvwaddch(big_pd,py,px,cellread)                           ! replace character
         if(px>=end_of_underline)then
            ierr=wmove(big_pd,py,px)                                    ! hold cursor position
            ierr=move(hy,hx)                                            ! hold cursor position
         else
            ierr=wmove(big_pd,py,px+1)                                  ! advance cursor position
            ierr=move(hy,hx+1)                                          ! advance cursor position
         endif
         call refresh_pd()                                              ! refresh so can see change in display
      else
         SELECT CASE (cellread)
         case (key_backspace,key_left,8,key_right,key_dc )              ! do normal action but do not leave replace mode
            call process_keypress(int(cellread,C_INT))
         CASE(27)                                                       ! exit replace mode and do not do normal action
            call message("ESC     ctrl-[  ESCAPE: exit replace mode")
            exit INFINITE
         case default                                                   ! do normal action and exit replace mode
            call process_keypress(int(cellread,C_INT))
            exit INFINITE
         END SELECT
      endif
   enddo INFINITE
   call refresh_pd()                                                    ! refresh so can see change in display
end subroutine replace_in_underline
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine insert_in_underline
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                   :: py, px, hy, hx, my, mx
   integer                   :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_LONG)           :: cell
   integer(C_LONG)           :: cellread
   integer(C_SHORT)          :: hpair
   integer(C_LONG)           :: hattr
   integer(C_INT)            :: ich_dum
   character(LEN=1)          :: ch_dum
   integer(C_INT)            :: start_of_underline, end_of_underline
!-----------------------------------------------------------------------------------------------------------------------------------
   call message(mesg)                      ! draw message set by process_key(3f) now because going to read more keys
   call getyx(stdscr,hy,hx)               ! cache initial cursor position
   call cursor2pad(hy,hx,py,px,cell)      ! convert screen cursor to pad position and get cell data
   call getmaxyx(big_pd,my,mx)            ! size of the specified window as defined (all of it, even if subsection being displayed)
   call get_cell_components(cell,hattr,hpair,ich_dum,ch_dum)
   call underline_ends(start_of_underline,end_of_underline)
   INFINITE: do
      call getyx(stdscr,hy,hx)                                          ! cache current cursor position
      call cursor2pad(hy,hx,py,px,cell)                                 ! convert screen cursor to pad position (and get cell data)
      if(px.lt.start_of_underline.or.px.gt.end_of_underline)then        ! if moved left or right out of underline region exit
         exit INFINITE
      endif
      cellread=getch()
      if(cellread>=32 .and. cellread<=126)then                          ! insert regular printable character and stay in insert mode
         ierr=mvwdelch(big_pd,py,end_of_underline)                      ! delete last underline character
         ierr=wmove(big_pd,py,px)                                       ! restore cursor position after deleting end of underlines
         write(mesg,'(a,a)')"INSERT regular character ",char(cellread)   ! write message bar message
         call message(mesg)
         cellread=ior(cellread,hattr)                                   ! add original attributes to character read from keyboard
         ierr=winsch(big_pd,cellread)                                   ! insert character at current position
         if(px==end_of_underline)then
            ierr=wmove(big_pd,py,px)                                    ! hold cursor position
            ierr=move(hy,hx)                                            ! hold cursor position
         else
            ierr=wmove(big_pd,py,px+1)                                  ! advance cursor position
            ierr=move(hy,hx+1)                                          ! advance cursor position
         endif
         call refresh_pd()                                              ! refresh so can see change in display
      else
         SELECT CASE (cellread)
         case (key_backspace,key_left,8,key_right,key_dc )              ! do normal action but do not leave insert mode
            call process_keypress(int(cellread,C_INT))
         CASE(27)                                                       ! exit insert mode and do not do normal action
            mesg=("ESC     ctrl-[  : EXIT insert mode")
            exit INFINITE
         case (key_ic )                                                 ! exit insert mode if insert key pressed
            mesg=("ic: insert char or enter ins mode : EXIT insert mode")
            exit INFINITE
         case default                                                   ! do normal action and exit insert mode
            call process_keypress(int(cellread,C_INT))
            exit INFINITE
         END SELECT
      endif
   enddo INFINITE
   call refresh_pd()                                                    ! refresh so can see change in display
end subroutine insert_in_underline
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine home_pad()
   integer(C_INT)   :: py, px
   integer          :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   pad_corner_x=0
   pad_corner_y=0
   call getbegyx(big_pd,py,px)   ! return current beginning coordinates of specified window (always relative to stdscr?)
   ierr=move(py,px)              ! move cursor on stdscr to the origin corner of big_pd
   call refresh_pd()             ! will "home" user data now that pad_corner_y and pad_corner_x are zero
end subroutine home_pad
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine promptfor(prefix,mesg)  ! go to message bar and read string
   character(len=*),intent(in) :: prefix
   character(len=*)            :: mesg
   integer(C_INT)              :: iline                                   ! line number to print at
   integer(C_INT)              :: hy,hx                                   ! store cursor position at start of this routine
   integer(C_INT)              :: ierr
   integer(C_INT)              :: ii
!-----------------------------------------------------------------------------------------------------------------------------------
   call getmaxyx(stdscr,LINES,COLS)                                       ! get screen size
   call getyx(stdscr,hy,hx)                                               ! cache current position
   iline=LINES-button_lines                                               ! decide which line is the message bar
   ierr=move(iline,0_C_INT)                                               ! move to message line
   ierr=attron(COLOR_PAIR(11))                                            ! set color attributes for message line
   ierr=clrtoeol()                                                        ! clear to end of line
   ierr=printw("%s"//C_NULL_CHAR,trim(prefix)//C_NULL_CHAR)               ! print prefix across window
   ierr=refresh()
   ierr=echo()                                                            ! turn on echoing so can see string as it is entered
   mesg=' '
   ierr=getnstr(mesg,len(mesg)-1)                                           ! read the string
   ii=index(mesg,char(0))                                                  ! find null in C string
   if(ii.ne.0)mesg(ii:)=' '
   ierr=noecho()                                                          ! turn echoing back off
   ierr=attroff(COLOR_PAIR(11))                                           ! turn off message color attributes
   ierr=move(hy,hx)                                                       ! restore cursor position
   ierr=refresh()                                                         ! Print changes onto the real screen
end subroutine promptfor
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine searchfor(win,strin)    ! search for string in window starting at current position
   type(C_PTR),intent(in)           :: win                                     ! window to search
   character(len=*)                 :: strin                                   ! string to search for
   character(len=pg_columns),save   :: oldstr=' '                              ! saved string to search for
   character(len=pg_columns)        :: aline                                   ! line of characters
   integer(C_INT)                   :: hy,hx                                   ! store cursor position at start of this routine
   integer(C_INT)                   :: py,px
   integer(C_INT)                   :: ierr                                    ! return value for most functions
   integer(C_LONG)                  :: cell
   integer(C_INT)                   :: plines,pcols                            ! size of pad
   integer(C_INT)                   :: hy_dum
   integer(C_INT)                   :: ii                                      ! where search string occurs in line
   integer(C_INT)                   :: i                                       ! loop counter
   integer(C_INT)                   :: iwidth
!-----------------------------------------------------------------------------------------------------------------------------------
   aline=' '
   if(strin.ne.' ')oldstr=strin
   call getyx(stdscr,hy,hx)                                               ! cache current position
   call getmaxyx(win,plines,pcols)                                        ! get pad size
   call cursor2pad(hy,hx,py,px,cell)                                      ! convert screen cursor to pad position and get cell data
   iwidth=len(aline)-1                                                    ! read at most up to size of character variable
   iwidth=min(iwidth,pcols-px-1)                                          ! should not read past edge of window
   ierr=mvwinnstr(win, py, min(px+1,pcols-1), aline, iwidth)              ! read current line from current cursor position
   ii=index(aline,char(0))
   if(ii.ne.0)aline(ii:)=' '
   ii=index(aline,trim(oldstr))
   if(ii.ne.0)then
      call pad2cursor(py,ii,hy,hx)
      ierr=move(hy,hx)                                                    ! move cursor on current line
   else
      FIND: do i=py+1,plines-1
         aline=' '
         ierr=mvwinnstr(win, i, 0_C_INT, aline, pcols-1)                  ! read line from input window
         ii=index(aline,char(0))
         if(ii.ne.0)aline(ii:)=' '
         ii=index(aline,trim(oldstr))
         if(ii.ne.0)then
           call move_pd(i-py,0_C_INT,'L')                                 ! change the corner of the data being displayed
           call pad2cursor(i,ii,hy_dum,hx)
           ierr=move(hy,hx)
           exit FIND
         endif
      enddo FIND
   endif
   ierr=refresh()                                                         ! print changes onto the real screen
end subroutine searchfor
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine message(mesg)  ! a message is printed with every key stroke and by explicit calls to this routine
   character(len=*),intent(in) :: mesg
   character(len=256)          :: longmsg
   integer(C_INT)              :: iline ! line number to print at
   integer(C_INT)              :: sy,sx                                   ! store cursor position at start of this routine
   integer(C_INT)              :: ierr
   character(LEN=256)          :: where
   integer(C_INT)              :: py,px
   integer(C_INT)              :: py3,px3
   integer(C_LONG)             :: cell
   integer                     :: ibox
!-----------------------------------------------------------------------------------------------------------------------------------
   call getmaxyx(stdscr,LINES,COLS)                                       ! get screen size
   call getyx(stdscr,sy,sx)                                               ! cache current position
   call inbox(sx,sy,ibox)
!-----------------------------------------------------------------------------------------------------------------------------------
   longmsg=mesg
   iline=LINES-button_lines                                               ! decide which line is the message bar
   ierr=move(iline,0_C_INT)                                               ! move to message line
   ierr=attron(COLOR_PAIR(11))                                            ! set color attributes for message line
   ierr=clrtoeol()                                                        ! clear to end of line
   ierr=mvprintw(iline,0_C_INT," KEY:%s"//C_NULL_CHAR,longmsg(:COLS-button_lines)//C_NULL_CHAR)  ! print message across window
   ierr=move(LINES-1_C_INT,0_C_INT)                                       ! move to bottom line
   ierr=clrtoeol()                                                        ! clear to end of line
   write(where,'(" ROW: ",I0,"/",I0," COL: ",I0,"/",I0,"IBOX:",I0)')sy+1,LINES,sx+1,COLS,IBOX
   ierr=mvprintw(LINES-1_C_INT,COLS-1-len_trim(where),"%s"//C_NULL_CHAR,trim(where)//C_NULL_CHAR) ! print row information for cursor
!-----------------------------------------------------------------------------------------------------------------------------------
   call cursor2pad(sy,sx,py,px,cell)                                      ! convert screen cursor to pad position and get cell data
   call getmaxyx(big_pd,py3,px3)                                          ! the size of the specified window.
   ierr=mvprintw(LINES-1_C_INT,0_C_INT,"[%d/%d:"//C_NULL_CHAR,py+1,py3)   ! print row information for cursor
   ierr=printw("%d/%d]"//C_NULL_CHAR,px+1,px3)                            ! print column information for cursor
   ierr=printw("CH:[%c]"//C_NULL_CHAR,iand(cell,A_CHARTEXT))              ! print the letter
   ierr=printw("PAIR:[%d]"//C_NULL_CHAR,PAIR_NUMBER(cell))                ! print the color pair used to draw the cell
   ierr=printw("AT:[%d]"//C_NULL_CHAR,iand(cell,A_ATTRIBUTES))            ! print the attributes used to draw the cell
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=attroff(COLOR_PAIR(11))                                           ! turn off message color attributes
   ierr=move(sy,sx)                                                       ! restore cursor position
   ierr=refresh()                                                         ! Print changes onto the real screen
end subroutine message
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine errmessage(mesg) ! exit screen mode to use WRITE(3f) to print a message and then read a line to create a pause
   character(len=*),intent(in) :: mesg
   integer                     :: ierr    ! return value for most ncurses(3c) functions
   integer                     :: ios     ! status return of READ(3f)
   ierr=def_prog_mode()                   ! Save the tty modes
   ierr=refresh()
  ierr=endwin()                           ! End curses mode temporarily
   write(*,*)mesg
   read(*,'(a)',iostat=ios)
   ierr=refresh() ! Back to curses. You can once again use the full capabilities of curses; and the screen resumes where it was
   call refresh_pd()
   ierr=flash()
end subroutine errmessage
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine my_exit()                           ! temporarily exit to standard mode or terminate program
   integer          :: ierr                    ! return value for most ncurses(3c) functions
   character(len=1) :: answer                  ! read an answer in line mode to confirm exit
   integer          :: itries                  ! count how many tries at a valid answer
   integer          :: ios                     ! status return of READ(3f)
   character(len=256) :: command
   integer            :: i
   !-------------------------------------------- (POSSIBLY TEMPORARILY) REVERT TO LINE MODE:
   ierr=def_prog_mode()                        ! Save the tty modes
   ierr=refresh()
   ierr=endwin()                               ! End curses mode temporarily
   !--------------------------------------------
   itries=0                                    ! track number of tries at an answer to avoid potential infinite loop
   INFINITE: do                                ! start reading a confirmation or selection
      itries=itries+1                          ! increment count of number of tries at a response
      write(*,*)'You are about to exit your program.'
      write(*,'(1x,a)',advance='no')'Are you sure? ((y)es, (n)o, or s(ystem):'
      answer=' '
      read(*,'(a)',iostat=ios)answer
      select case(answer)
      case('a','A')
         write(*,'(a)')G_answers
      case('y','Y',' ')
         stop
      case('n','N')
         exit INFINITE
      case('v','V')
         do i=1,icount_ptr
            write(*,'(i4.4,1x,a)')i,trim(page_ptr(i))
         enddo
         write(*,*)'icount_ptr=',icount_ptr
      case('i','?')
         write(ERROR_UNIT,*)'COLORS=          ',colors
         write(ERROR_UNIT,*)'COLOR_PAIRS=     ',color_pairs
         write(ERROR_UNIT,*)'LINES=           ',lines
         write(ERROR_UNIT,*)'COLS=            ',cols
         write(ERROR_UNIT,*)'PAD_CORNER_Y=    ',pad_corner_y
         write(ERROR_UNIT,*)'PAD_CORNER_X=    ',pad_corner_x
         write(ERROR_UNIT,*)'ICOUNT_PD=       ',icount_pd
         write(ERROR_UNIT,*)'LONGLINE_PD=     ',longline_pd
         write(ERROR_UNIT,*)'displaywidth_pd= ',displaywidth_pd
         !*!write(ERROR_UNIT,*)"COMPILER VERSION=",COMPILER_VERSION()
         !*!write(ERROR_UNIT,*)"COMPILER OPTIONS=",COMPILER_OPTIONS()
         call get_command_argument(0,command,i,ierr)
         write(ERROR_UNIT,*)"get_command_argument(0) is "//trim(command)
         call get_command(command,i,ierr)
         write(ERROR_UNIT,*)"get_command is " //trim(command)
         do i=1,command_argument_count()
            call get_command_argument(i,command)
            write(ERROR_UNIT,*)i,"th argument="//trim(command)
         enddo
      case('s','S')
         write(*,*)'You have now started a system shell.'
         write(*,*)'You can enter regular commands. When done,'
         write(*,*)'enter "exit" to resume your program.'
         call execute_command_line("/bin/sh -i")     ! Do whatever you like in cooked mode
         !--------------------------------------------
         ierr=reset_prog_mode()                      ! Return to the previous tty mode stored by def_prog_mode()
         exit INFINITE                               !
         !--------------------------------------------
      end select
      if(itries.gt.10)then                           ! enough errors to assume something is wrong
         stop
      endif
   enddo INFINITE
   !--------------------------------------------
   ierr=refresh() ! Back to curses. You can once again use the full capabilities of curses; and the screen resumes where it was
end subroutine my_exit
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine buttons() ! create and show buttons on last five lines of display
   character(len=256),target :: buffer(4)
   integer,target            :: icount_buffer=size(buffer)
   integer(C_INT)            :: istart
   type(C_PTR),save          :: botwin                     ! WINDOW
   integer,target            :: ierr
   integer                   :: i
   integer                   :: ii
!-----------------------------------------------------------------------------------------------------------------------------------
   if (c_associated(botwin)) then
      ierr=delwin(botwin)
   endif
   !WINDOW *newwin( int nlines, int ncols, int begin_y, int begin_x);
   botwin=newwin(button_lines,COLS,LINES-button_lines,0)
   if (.not.c_associated(botwin)) then
      call nc_errmessage("failed to open bottom button window")
   endif
   page_ptr=>buffer
   icount_ptr=>icount_buffer
   page_ptr(:)(:)=' '
   if(COLS.ge.66)then
      istart=(COLS-66)/2+1
                             ! 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789
      page_ptr(icount_ptr-2)(istart:)="#~        ~#  #~        ~#  #~  NEXT  ~#  #~PREVIOUS~#  #~        ~# "
      page_ptr(icount_ptr-1)(istart:)="#~  HELP  ~#  #~  HOME  ~#  #~  PAGE  ~#  #~  PAGE  ~#  #~ SUBMIT ~# "
      button_boxes(:,4)= icount_ptr-2 ! top
      button_boxes(:,3)= icount_ptr-1 ! bottom
      ii=0
      do i=1,5
         button_boxes(i,1)= istart   +ii*14 ! left
         button_boxes(i,2)= istart+9 +ii*14 ! right
         ii=ii+1
      enddo
   elseif(COLS.ge.37)then
      istart=(COLS-37)/2+1
                             ! 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789
      page_ptr(icount_ptr-2)(istart:)="#~   ~# #~   ~# #~PG~# #~PG~# #~   ~#"
      page_ptr(icount_ptr-1)(istart:)="#~HLP~# #~HOM~# #~DN~# #~UP~# #~SUB~#"
      button_boxes(:,4)= icount_ptr-2 ! top
      button_boxes(:,3)= icount_ptr-1 ! bottom

      button_boxes(1,1)= istart    ! left
      button_boxes(1,2)= istart+4  ! right

      button_boxes(2,1)= istart+8  ! left
      button_boxes(2,2)= istart+12 ! right

      button_boxes(3,1)= istart+16 ! left
      button_boxes(3,2)= istart+19 ! right

      button_boxes(4,1)= istart+23 ! left
      button_boxes(4,2)= istart+26 ! right

      button_boxes(5,1)= istart+30 ! left
      button_boxes(5,2)= istart+34 ! right
   else
      istart=(COLS-26)/2+1
      istart=max(istart,1)
                             ! 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789
      page_ptr(icount_ptr-2)(istart:)="~   ~ ~   ~ ~PG~ ~PG~ ~   ~"
      page_ptr(icount_ptr-1)(istart:)="~HLP~ ~HOM~ ~DN~ ~UP~ ~SUB~"
      button_boxes(:,4)= icount_ptr-2 ! top
      button_boxes(:,3)= icount_ptr-1 ! bottom

      button_boxes(1,1)= istart    ! left
      button_boxes(1,2)= istart+4  ! right

      button_boxes(2,1)= istart+6  ! left
      button_boxes(2,2)= istart+10 ! right

      button_boxes(3,1)= istart+12 ! left
      button_boxes(3,2)= istart+13 ! right

      button_boxes(4,1)= istart+17 ! left
      button_boxes(4,2)= istart+20 ! right

      button_boxes(5,1)= istart+22 ! left
      button_boxes(5,2)= istart+26 ! right
   endif
   ierr=wattron(botwin,A_BOLD)
   call user_to_screen(botwin)  ! use data in page_ptr(*) to build ncurses window
   ierr=wattroff(botwin,A_BOLD)
   page_ptr(:)(:)=' '
   page_ptr=>page_pd
   icount_ptr=>icount_pd
   ierr=wrefresh(botwin)
end subroutine buttons
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine start_screen()                ! set up for using main screen window
   integer(kind=chtype) :: ch
   integer              :: ierr
   integer(C_LONG)      :: mouse_mask

   stdscr=initscr()
   if (.not.c_associated(stdscr)) then
      ierr=endwin()
      write(*,*)'could not open standard screen. input not a tty? ERRNO=',ierr
      stop
   endif
   ierr=flushinp()                       ! flush any typeahead input the user had provided
   call getmaxyx(stdscr,LINES,COLS)      !*! should not have to set these
   ierr=noecho()                         ! Note that without noecho() some ugly escape characters might have been printed  on screen
   ierr=cbreak()                         ! Line buffering disabled, Pass on everything to me
   ierr=raw()                            ! pass on everything, including interrupt and flow control (ctrl-C, ...)
   ierr=keypad(stdscr, TRUE)             ! Enable keypad mode
   mouse_mask=mousemask(ALL_MOUSE_EVENTS,C_NULL_PTR)
   if(has_colors())then
      ierr=start_color()
      call getcolor(COLORS,COLOR_PAIRS) !*! extension to get these set
                                !foreground      background
      ierr=init_pair(1_C_SHORT,  COLOR_GREEN,    COLOR_YELLOW );
      ierr=init_pair(2_C_SHORT,  COLOR_RED,      COLOR_BLACK  );
      ierr=init_pair(3_C_SHORT,  COLOR_GREEN,    COLOR_BLACK  );
      ierr=init_pair(4_C_SHORT,  COLOR_YELLOW,   COLOR_BLACK  );
      ierr=init_pair(5_C_SHORT,  COLOR_BLUE,     COLOR_BLACK  );
      ierr=init_pair(6_C_SHORT,  COLOR_MAGENTA,  COLOR_BLACK  );
      ierr=init_pair(7_C_SHORT,  COLOR_CYAN,     COLOR_BLACK  );
      ierr=init_pair(8_C_SHORT,  COLOR_WHITE,    COLOR_BLACK  );
      ierr=init_pair(9_C_SHORT,  COLOR_BLUE,     COLOR_WHITE  );
      ierr=init_pair(10_C_SHORT, COLOR_BLACK,    COLOR_RED    );
      ierr=init_pair(11_C_SHORT, COLOR_BLACK,    COLOR_GREEN  );
      ierr=init_pair(12_C_SHORT, COLOR_BLACK,    COLOR_YELLOW );
      ierr=init_pair(13_C_SHORT, COLOR_BLACK,    COLOR_BLUE   );
      ierr=init_pair(14_C_SHORT, COLOR_BLACK,    COLOR_MAGENTA);
      ierr=init_pair(15_C_SHORT, COLOR_BLACK,    COLOR_CYAN   );
      ierr=init_pair(16_C_SHORT, COLOR_BLACK,    COLOR_WHITE  );
      ierr=init_pair(17_C_SHORT, COLOR_WHITE,    COLOR_BLACK  );
      ierr=init_pair(18_C_SHORT, COLOR_WHITE,    COLOR_RED    );
      ierr=init_pair(19_C_SHORT, COLOR_WHITE,    COLOR_GREEN  );
      ierr=init_pair(20_C_SHORT, COLOR_WHITE,    COLOR_YELLOW );
      ierr=init_pair(21_C_SHORT, COLOR_WHITE,    COLOR_BLUE   );
      ierr=init_pair(22_C_SHORT, COLOR_WHITE,    COLOR_MAGENTA);
      ierr=init_pair(23_C_SHORT, COLOR_WHITE,    COLOR_CYAN   );
      ierr=init_pair(24_C_SHORT, COLOR_CYAN,     COLOR_BLUE   );
      call message("This terminal has colors")
   else
        call message("This terminal does not have colors")
   endif
   ch=A_REVERSE
   !*!call bkgdset(ch)
   !*!call wbkgdset(stdscr,ch)
   !*!ierr=bkgd(ch)
   !*!ierr=wbkgd(stdscr,ch)
end subroutine start_screen
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine loaddata(filename) ! populate the form array definition pointed to by page_ptr(:) by reading in a file defining the form
   !use M_fixedform, only : page_ptr, icount_ptr, longline_pd, pg_lines
   character(len=*),intent(in)     :: filename   ! name of file to open and read form definition from
   character(len=len(page_ptr(1))) :: buffer     ! a buffer for reading a line from the input file
   integer                         :: ios        ! I/O error status flag returned by READ(3f)
   integer                         :: ilen
   character(len=2046)             :: mesg
!-----------------------------------------------------------------------------------------------------------------------------------
   OPEN(UNIT=10,FILE=trim(filename),ACTION='read',ACCESS='sequential',FORM='formatted',IOSTAT=ios,IOMSG=mesg,STATUS='old')
   if(ios.ne.0)then
      write(*,*)'E-R-R-O-R: COULD NOT OPEN FILE '//trim(filename)
      write(*,'(" IOSTAT=",i0,1x,":",a)')ios,trim(mesg)
      stop
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   icount_ptr=0
   longline_pd=1
   INFINITE: do
      read(10,'(a)',iostat=ios,iomsg=mesg)buffer                       ! read line of user data
      if(ios.ne.0) exit INFINITE                                      ! end on any non-zero error from READ(3f)
      call nc_notabs(buffer,page_ptr(icount_ptr+1),ilen)              ! expand tabs and remove DOS line terminators
      longline_pd=max(longline_pd,ilen)                               ! keep track of longest line read in
      if(page_ptr(icount_ptr+1)(1:1).ne.'.')then                      ! lines with period in column 1 are special and are not stored
         icount_ptr=icount_ptr+1
         if(icount_ptr.gt.pg_lines)exit INFINITE                      ! buffer is full
      else
         !*! do something special with lines starting with period (".").
      endif
   enddo INFINITE
end subroutine loaddata
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine user_to_screen(win)  ! @(#) convert user data to an ncurses(3c) window
!-----------------------------------------------------------------------------------------------------------------------------------
   type(C_PTR)            :: win
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                :: i,j    ! position in user data
   integer(C_INT)         :: y,x    ! position in output window
   character(len=1)       :: curch  ! current character from user data
   integer(C_INT)         :: ierr
   logical                :: inverse=.false.  ! in a region to be inverse, delimited by ~
   logical                :: atcolor=.false.  ! in a region to have a green background, delimited by @
   logical                :: grcolor=.false.  ! in a region to have a color pair, delimited by `
   integer                :: gr=20            ! the color for delimited by `
   logical                :: next=.false.     ! flag to get color for data delimited by `
   integer                :: ipreviousch      ! previous character as an integer so can skip underscores in words
   integer                :: inextch          ! next character as an integer so can skip underscores in words
!-----------------------------------------------------------------------------------------------------------------------------------
   inverse=.false.                            ! flag that not in region delimited by ~
   atcolor=.false.                            ! flag not in region delimited by @
   next=.false.
   do i=1,icount_ptr
      do j=1,len_trim(page_ptr(i))                                   ! size_page assumed big enough to hold data plus one blank line
         y=i-1                                                       ! pad coordinates start at <0,0> not <1,1>
         x=j-1
         curch=page_ptr(i)(j:j)                                      ! current character being placed into window/pad
         if(next)then                                                ! get color to use for gr area
            gr=mod(ichar(curch),24)+1
            ierr=wattron(win,COLOR_PAIR(gr))
            ierr=mvwaddch(win,y,x,int(ichar(' '),C_LONG))
            next=.false.
            cycle
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         if(curch.eq.'#')then
            call acs(win,i,j)
!-----------------------------------------------------------------------------------------------------------------------------------
         elseif(curch.eq.'`')then ! start or end color pair
            if(grcolor)then
               ierr=mvwaddch(win,y,x,int(ichar(' '),C_LONG))
               ierr=wattroff(win,COLOR_PAIR(gr))
               grcolor=.false.
            else                                                  ! starting gr region
               next=.true.
               ierr=mvwaddch(win,y,x,int(ichar(' '),C_LONG))
               grcolor=.true.
            endif
!-----------------------------------------------------------------------------------------------------------------------------------
         elseif(curch.eq.'@')then ! start or end color pair 11
            if(atcolor)then
               ierr=mvwaddch(win,y,x,int(ichar(' '),C_LONG))
               ierr=wattroff(win,COLOR_PAIR(11))
               atcolor=.false.
            else
               ierr=wattron(win,COLOR_PAIR(11))
               ierr=mvwaddch(win,y,x,int(ichar(' '),C_LONG))
               atcolor=.true.
            endif
!-----------------------------------------------------------------------------------------------------------------------------------
         elseif(curch.eq.'\')then ! colored space
               ierr=wattron(win,COLOR_PAIR(12))
               ierr=mvwaddch(win,y,x,int(ichar(' '),C_LONG))
               ierr=wattroff(win,COLOR_PAIR(12))
!-----------------------------------------------------------------------------------------------------------------------------------
         elseif(curch.eq.'^')then
              ierr=mvwaddch(win,y,x,ACS_DIAMOND)
!-----------------------------------------------------------------------------------------------------------------------------------
         elseif(curch.eq.'_')then
            ! if character before and after are normal letters assume this is a single underscore in a word instead of input field
            if(j.ne.1)then
               ipreviousch=ichar(page_ptr(i)(j-1:j-1))
               if(ipreviousch.le.32.or.ipreviousch.gt.126)ipreviousch=0
               if(ipreviousch.eq.95)ipreviousch=0
            else
               ipreviousch=0
            endif
            if(j.ne.len_trim(page_ptr(i)))then
               inextch=ichar(page_ptr(i)(j+1:j+1))
               if(inextch.le.32.or.inextch.gt.126)inextch=0
               if(inextch.eq.95)inextch=0
            else
               inextch=0
            endif
            if( (inextch.ne.0).and.(ipreviousch.ne.0) )then
               ierr=mvwaddch(win,y,x,int(ichar(curch),C_LONG))    ! just a regular character
            else                                                  ! text input field
               ierr=wattron(win,A_UNDERLINE)
               ierr=mvwaddch(win,y,x,int(ichar(' '),C_LONG))
               ierr=wattroff(win,A_UNDERLINE)
            endif
!-----------------------------------------------------------------------------------------------------------------------------------
         elseif(curch.eq.'~')then  ! start or end inverse
            if(inverse)then
               ierr=mvwaddch(win,y,x,int(ichar(' '),C_LONG))
               ierr=wattroff(win,A_REVERSE)
               inverse=.false.
            else
               ierr=wattron(win,A_REVERSE)
               inverse=.true.
               ierr=mvwaddch(win,y,x,int(ichar(' '),C_LONG))
            endif
!-----------------------------------------------------------------------------------------------------------------------------------
         else
            ierr=mvwaddch(win,y,x,int(ichar(curch),C_LONG))
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
      enddo
   enddo
end subroutine user_to_screen
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine acs(win,i,j) ! figure out which box character to use
! assuming boxes do not touch that are not part of same structure
   type(C_PTR),intent(in)    :: win
   integer,intent(in)        :: i,j ! position in user data defining the page (starts at <1,1>)
   integer(C_INT)            :: y,x ! screen coordinates (starts at <0,0>)
   integer                   :: isum
   integer                   :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   !    #    2| 3| 5    character of interest is assumed at the center of a 3x3 grid
   !   ###   7|11|13    and the sum of selected primes produces unique numbers
   !    #   17|19|23    for patterns of interest (else use prime multiplication or binary)
   isum=11
   y=i-1  ! screen coordinates
   x=j-1
   if(j>=2)then                                        ! if not first column look to left for adjacent line-drawing characters
      if(page_ptr(i)(j-1:j-1).eq.'#') isum=isum+7
      if(page_ptr(i)(j-1:j-1).eq.'^') isum=isum+7
   endif
   if(j<len(page_ptr(1)))then                          ! if not last column look to right
      if(page_ptr(i)(j+1:j+1).eq.'#') isum=isum+13
      if(page_ptr(i)(j+1:j+1).eq.'^') isum=isum+13
   endif
   if(i<size(page_ptr)-1)then                          ! if know have at least one line below look one line below
      if(page_ptr(i+1)(j:j).eq.'#')   isum=isum+19
      if(page_ptr(i+1)(j:j).eq.'^')   isum=isum+19
   endif
   if(i>=2)then                                        ! if not top line (and know have 1000 lines) look at line above
      if(page_ptr(i-1)(j:j).eq.'#')   isum=isum+3
      if(page_ptr(i-1)(j:j).eq.'^')   isum=isum+3
   endif
   ierr=wmove(win,y,x)
   select case(isum)
      !case(11);                 ierr=waddch(win,ACS_DEGREE)
      case(11);                 ierr=waddch(win,int(ichar('#'),C_LONG))      ! a pound character in isolation
      case(3+11+7);             ierr=waddch(win,ACS_LRCORNER)
      case(11+3,11+19,11+3+19); ierr=waddch(win,ACS_VLINE)
      case(11+7,11+13,7+11+13); ierr=waddch(win,ACS_HLINE)
      case(13+11+19);           ierr=waddch(win,ACS_ULCORNER)
      case(3+11+13);            ierr=waddch(win,ACS_LLCORNER)
      case(3+11+13+19);         ierr=waddch(win,ACS_LTEE)       !Tee pointing right
      case(3+11+19+7);          ierr=waddch(win,ACS_RTEE)       !Tee pointing left
      case(7+11+13+3);          ierr=waddch(win,ACS_BTEE)       !Tee pointing up
      case(7+11+13+19);         ierr=waddch(win,ACS_TTEE)       !Tee pointing down
      case(7+11+19);            ierr=waddch(win,ACS_URCORNER)
      case(7+11+13+3+19);       ierr=waddch(win,ACS_PLUS)
   end select
end subroutine acs
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine refresh_pd() ! assuming form window big_pd exists redraw it
   integer                   :: ierr
   integer(C_INT)            :: ileft
   integer(C_INT)            :: iright
   integer(C_INT),save       :: itop=0
   call getmaxyx(stdscr,LINES,COLS)      !*! should not have to set these
   ileft=max(0,(cols-longline_pd)/2-1)
   iright=min(cols-1,ileft+longline_pd-1)
   displaywidth_pd=iright-ileft+1      ! width of section of pad displayed on screen
   !*!ierr=prefresh(big_pd, pad_corner_y,pad_corner_x, itop,ileft, LINES-button_lines-1,iright)
   ierr=wnoutrefresh(stdscr)
   ierr=pnoutrefresh(big_pd, pad_corner_y,pad_corner_x, itop,ileft, LINES-button_lines-1,iright)
   ierr=doupdate()
end subroutine refresh_pd
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine cursor2pad(sy,sx,y,x,cell)
! @(#) look "under" cursor into pad data and return coordinates and cell data from specified window
!-----------------------------------------------------------------------------------------------------------------------------------
   integer,intent(in)           :: sy,sx  ! screen coordinates to assume cursor is at
   integer,intent(out)          :: y,x    ! element in page_pd(y)(x:x) character was found at
   integer(C_LONG),intent(out)  :: cell   ! description of character under cursor (color pair, character, attributes)
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: my,mx  ! size of the specified window
   integer                      :: cy,cx  ! beginning coordinates of subsection being displayed
   integer                      :: hy,hx  ! cache cursor position in case it is not sy,sx
   integer                      :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   call getyx(stdscr,hy,hx)               ! cache current position
   call getmaxyx(big_pd,my,mx)            ! size of the specified window as defined (all of it, even if subsection being displayed)
   call getbegyx(big_pd,cy,cx)            ! return current beginning coordinates of specified window (always relative to stdscr?)
   y=sy+cy+pad_corner_y
   x=pad_corner_x+(sx-cx)
   if((y.ge.0.and.y.lt.my).and.(x.ge.0.and.x.lt.mx))then
      cell=mvwinch(big_pd,y,x)            ! retrieve cell value
   else
      cell=0
      cell=inch()                         ! cannot convert to a pad value to use screen value
   endif
   ierr=move(hy,hx)                       ! restore cursor position
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine cursor2pad
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine pad2cursor(pad_x,pad_y,screen_y,screen_x)
! @(#) if pad coordinates map to a screen area find screen coordinates
!-----------------------------------------------------------------------------------------------------------------------------------
   integer,intent(in)           :: pad_y,pad_x        ! element coordinates in page_pd(pad_y)(pad_x:pad_x)
   integer,intent(out)          :: screen_y,screen_x  ! screen coordinates to assume cursor is at
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: cy,cx              ! beginning coordinates of subsection being displayed
!-----------------------------------------------------------------------------------------------------------------------------------
   call getbegyx(big_pd,cy,cx)            ! return current beginning coordinates of specified window (always relative to stdscr?)
   screen_y=pad_y-(cy+pad_corner_y)
   screen_x=pad_x+cx-pad_corner_x
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine pad2cursor
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine printit() ! @(#) access program data here so nc_printplain(3f) is generic
   character(len=256)           :: ufilename    ! filename to print to
   integer                      :: ierr
!   ufilename=nc_uniqname("paper.txt",ierr)
!   if(ierr.eq.0)then
!      call nc_printplain(big_pd,ufilename)
!   endif
   ufilename=nc_uniqname("paper.html",ierr)
   if(ierr.eq.0)then
      call nc_printhtml(big_pd,ufilename)
   endif
end subroutine printit
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine extract_answers_tabs()
   integer                   :: i,j,imax,jmax
   logical                   :: inunderline
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_LONG)           :: cell
   integer(C_INT)            :: ich
   character(len=1)          :: ch
   integer(C_LONG)           :: attr
   integer(C_SHORT)          :: pair
!-----------------------------------------------------------------------------------------------------------------------------------
   call getmaxyx(big_pd,imax,jmax)                     ! size window size as defined (all of it, even if subsection being displayed)
   inunderline=.false.
   G_answers=''
   do i=0,imax-1                                                                ! print underlined regions and buttons
      do j=0,jmax-1
         cell=mvwinch(big_pd,i,j)
         call get_cell_components(cell,attr,pair,ich,ch)
         if(iand(attr,A_UNDERLINE).eq. A_UNDERLINE)then
            if(.not.inunderline)then                                            ! this is the beginning of an underline entry
            endif
            G_answers=G_answers//ch
            inunderline=.true.
         else                                                                   ! found end of underlined region
            if(inunderline.eqv..true.)then
               G_answers=G_answers//char(9)
            endif
            inunderline=.false.
         endif
         if((iand(attr,A_ALTCHARSET).eq.A_ALTCHARSET).and.(ch.eq.'`'))then      ! found a button
            if(iand(attr,A_STANDOUT).eq.A_STANDOUT)then                         ! this is a selected button
               G_answers=G_answers//'T'//char(9)                                ! write value for selected menu diamond bullet
            else
               G_answers=G_answers//'F'//char(9)                                ! write value for unselected menu diamond bullet
            endif
         endif
      enddo
   enddo
   G_submitted=.true.
end subroutine extract_answers_tabs
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine extract_answers_namelist()
   integer                   :: i,j,imax,jmax
   logical                   :: inunderline
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_LONG)           :: cell
   integer(C_INT)            :: ich
   character(len=1)          :: ch
   integer(C_LONG)           :: attr
   integer(C_SHORT)          :: pair
!-----------------------------------------------------------------------------------------------------------------------------------
   call getmaxyx(big_pd,imax,jmax)                     ! size window size as defined (all of it, even if subsection being displayed)
   inunderline=.false.
   do i=0,imax-1                                                                ! print underlined regions and buttons
      do j=0,jmax-1
         cell=mvwinch(big_pd,i,j)
         call get_cell_components(cell,attr,pair,ich,ch)
         if(iand(attr,A_UNDERLINE).eq. A_UNDERLINE)then
            if(.not.inunderline)then                                            ! this is the beginning of an underline entry
               write(22,'("TEXT_Y",i0,"_X",i0,"=""")',advance='no')i,j
            endif
            write(22,'(a)',advance='no')ch
            if(ch.eq.'"') write(22,'(a)',advance='no')ch
            inunderline=.true.
         else
            if(inunderline)write(22,'(a)')'"'                                   ! found end of underlined region
            inunderline=.false.
         endif
         if((iand(attr,A_ALTCHARSET).eq.A_ALTCHARSET).and.(ch.eq.'`'))then      ! found a button
            write(22,'("BUTTON_Y",i0,"_X",i0,"=",a)',advance='no')i,j
            if(iand(attr,A_STANDOUT).eq.A_STANDOUT)then                         ! this is a selected button
               write(22,'(a)')'"T"'                                             ! write value for selected menu diamond bullet
            else
               write(22,'(a)')'"F"'                                             ! write value for unselected menu diamond bullet
            endif
         endif
      enddo
   enddo
end subroutine extract_answers_namelist
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine space_bar(mesg) ! @(#) if on a menu button clear any menu buttons connected to it and toggle it
   character(len=128)        :: mesg
   integer                   :: i,j
   integer                   :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_LONG)           :: cell
   integer(C_LONG)           :: attr
   integer(C_SHORT)          :: pair
   integer(C_INT)            :: ich
   character(len=1)          :: ch
!-----------------------------------------------------------------------------------------------------------------------------------
   cell=winch(big_pd)
   call get_cell_components(cell,attr,pair,ich,ch)
   call getyx(big_pd,i,j)                             ! cache current position
   write(mesg,'("CHANGE:CH=",a," PAIR=",i0," ATTR=",i0," LOC=",i0,1x,i0)')ch,pair,attr,i,j
   if((iand(attr,A_ALTCHARSET).eq.A_ALTCHARSET).and.(ch.eq.'`'))then  ! if character is ` this is a menu diamond bullet
      if(iand(attr,A_STANDOUT).eq.A_STANDOUT)then
         attr=ieor(attr,A_STANDOUT)                                   ! attr=attr-A_STANDOUT; remove the attribute
         ierr=wchgat(big_pd, 1_C_INT, attr, pair , C_NULL_PTR)        ! remove A_STANDOUT from attributes
         mesg=trim(mesg)//' Turn OFF menu option'
      else
         call clear_radio(i,j)
         ierr=wmove(big_pd,i,j)
         attr=ior(attr,A_STANDOUT)                                    ! Add A_STANDOUT to attributes
         ierr=wchgat(big_pd, 1_C_INT, attr, pair, C_NULL_PTR)
         mesg=trim(mesg)//' Turn ON menu option'
      endif
      call refresh_pd()                                               ! refresh so can see change in menu button display
   endif
end subroutine space_bar
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine clear_radio(i,j)
   ! anyone attached by line drawing or diamonds is assumed to be part of a set that should be cleared
   ! simplistically assuming any character from the alternate character set (just using line drawing and diamonds for now)
   ! has the attribute to change. Use simple flood-fill algorithm to find all connected alternate characters and if they are
   ! diamonds remove the A_STANDOUT attribute so all menu buttons part of a group are turned off. That means you can "draw"
   ! a variety of shapes of menu buttons connected via menu buttons or line-drawing characters that will act as one radio button.
   !
   !*! maybe treat similar attributes or color as a structure too instead of just ACS (Alternate Character Set) characters??
   !
   integer(C_INT)            :: i,j
   integer(C_INT)            :: imax,jmax
   call getmaxyx(big_pd,imax,jmax)                    ! size window size as defined (all of it, even if subsection being displayed)
   call flood_fill(i,j,imax,jmax)
   call un_flood_fill(i,j,imax,jmax)
   !*! this works, but the code can probably be simplified. After function is proven, review.
end subroutine clear_radio
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
recursive subroutine flood_fill(y,x,ymax,xmax)
! Stack-based recursive flood-fill (Four-way)
!
! Flood fill, also called seed fill, is an algorithm that determines the
! area connected to a given node in a multi-dimensional array. It is used
! in pixel-based graphics to "bucket" fill connected, similarly-colored
! areas with a different color,
!
! The flood fill algorithm takes three parameters: a start node, a target
! color, and a replacement color. The algorithm looks for all nodes in the
! array which are connected to the start node by a path of the target color,
! and changes them to the replacement color.
!
! Depending on whether we consider nodes touching at the corners connected
! or not, we have two variations, Eight-way and Four-way, respectively.
!
! One implicitly stack-based (recursive) flood-fill implementation (for
! a two-dimensional array) for four-ways goes as follows:
!
! In this case, the old "color" is any character that has the A_ALTCHARSET attribute.
! the new "color" will be to change these (temporarily) to the unused A_HORIZONTAL attribute
! and at the same time remove A_STANDOUT from any node that is an A_DIAMOND character.
! then, change A_HORIZONTAL back to A_ALTCHARSET.
! the result is to turn off A_STANDOUT on any connected diamond, which is used to show selected
! diamonds (which are used as menu buttons).
! The array is the big_pd window.
!
   integer(C_INT)   :: y, x
   integer(C_INT)   :: ymax,xmax
   integer          :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_LONG)           :: cell
   integer(C_LONG)           :: attr
   integer(C_SHORT)          :: pair
   integer(C_INT)            :: ich
   character(len=1)          :: ch
!-----------------------------------------------------------------------------------------------------------------------------------
   cell=mvwinch(big_pd,y,x)
   call get_cell_components(cell,attr,pair,ich,ch)  ! extract cell components
!-----------------------------------------------------------------------------------------------------------------------------------
!  1. If current attribute is equal to new attribute, return.
      if(iand(attr,A_HORIZONTAL).eq.A_HORIZONTAL)return  !*! assume all A_HORIZONTAL were A_ALTCHARSET (menu or box characters)
!-----------------------------------------------------------------------------------------------------------------------------------
!  2. If the current attribute is not equal to the target that you want to change (A_ALTCHARSET), return.
      if(iand(attr,A_ALTCHARSET).ne.A_ALTCHARSET)return
!-----------------------------------------------------------------------------------------------------------------------------------
!  3. Set the color of node to replacement-color.
      attr=ior(attr,A_HORIZONTAL)                                           ! add the attribute to the attribute list
      attr=ieor(attr,A_ALTCHARSET)                                          ! remove the attribute
      if(ch.eq.'`')then                                                     ! turn off A_STANDOUT on any diamond found
         if(iand(attr,A_STANDOUT).eq.A_STANDOUT) attr=ieor(attr,A_STANDOUT) ! attr=attr-A_STANDOUT; remove this  attribute too
      endif
      ierr=wchgat(big_pd, 1_C_INT, attr, pair , C_NULL_PTR)                 ! update the cell
!-----------------------------------------------------------------------------------------------------------------------------------
!  4. Recursively call on the adjacent nodes. So perform flood-fill on:
      if(x.gt.0)      call flood_fill(y,x-1,ymax,xmax) ! one step to the west of node
      if(x.lt.xmax-1) call flood_fill(y,x+1,ymax,xmax) ! one step to the east of node
      if(y.gt.0)      call flood_fill(y-1,x,ymax,xmax) ! one step to the north of node
      if(y.lt.ymax-1) call flood_fill(y+1,x,ymax,xmax) ! one step to the south of node
!-----------------------------------------------------------------------------------------------------------------------------------
!  5. Return.
!-----------------------------------------------------------------------------------------------------------------------------------
! Though easy to understand, the implementation of the algorithm above
! is impractical in languages and environments where stack space is
! severely constrained and large arrays are involved. Many other algorithms are available if this were an issue.
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine flood_fill
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
recursive subroutine un_flood_fill(y,x,ymax,xmax) ! put back the A_ALTCHARSET ATTRIBUTE and remove A_HORIZONTAL attribute
   integer(C_INT)   :: y, x
   integer(C_INT)   :: ymax,xmax
   integer          :: ierr
   integer(C_LONG)           :: cell
   integer(C_LONG)           :: attr
   integer(C_SHORT)          :: pair
   integer(C_INT)            :: ich
   character(len=1)          :: ch
   cell=mvwinch(big_pd,y,x)
   call get_cell_components(cell,attr,pair,ich,ch)  ! extract cell components
      if(iand(attr,A_ALTCHARSET).eq.A_ALTCHARSET)return  !*! assume all A_HORIZONTAL were (menu or box characters)
      if(iand(attr,A_HORIZONTAL).ne.A_HORIZONTAL)return
      attr=ior(attr,A_ALTCHARSET)                         ! add the attribute to the attribute list
      attr=ieor(attr,A_HORIZONTAL)                               ! remove the attribute
      ierr=wchgat(big_pd, 1_C_INT, attr, pair , C_NULL_PTR)                 ! update the cell
      if(x.gt.0)      call un_flood_fill(y,x-1,ymax,xmax) ! one step to the west of node
      if(x.lt.xmax-1) call un_flood_fill(y,x+1,ymax,xmax) ! one step to the east of node
      if(y.gt.0)      call un_flood_fill(y-1,x,ymax,xmax) ! one step to the north of node
      if(y.lt.ymax-1) call un_flood_fill(y+1,x,ymax,xmax) ! one step to the south of node
end subroutine un_flood_fill
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine find_next() ! @(#) from current cursor position jump to next input field in pad
   integer(C_INT)            :: ys, xs
   integer(C_INT)            :: ydelta, xdelta
   integer(C_INT)            :: ymax, xmax
   integer(C_INT)            :: ystart, xstart
   integer                   :: xstartline
   integer(C_INT)            :: iy,ix
   integer                   :: i
   integer                   :: ierr
   logical                   :: skip_underscore
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_LONG)           :: cell
   character(len=1)          :: ch
   integer(C_INT)            :: ich
   integer(C_LONG)           :: attr
   integer(C_SHORT)          :: pair
   integer(C_LONG)           :: cell_before
   character(len=1)          :: ch_before
   integer(C_INT)            :: ich_before
   integer(C_LONG)           :: attr_before
   integer(C_SHORT)          :: pair_before
   integer(C_INT)            :: ymove,xmove
!-----------------------------------------------------------------------------------------------------------------------------------
   call getyx(stdscr,ys,xs)               ! real cursor position in terms of standard screen coordinates
   call getmaxyx(big_pd,ymax,xmax)        ! window size as defined (all of it, even if subsection being displayed)
   call getyx(big_pd,ystart,xstart)       !*! current position in columns and rows of pad DATA. Assume in pad for now
   skip_underscore=.false.                ! if this is not the beginning of an underscore region but an underscore go to next field
   PASSES: do i=1,2                       ! start from current position; but if get to end make one more pass from beginning
      xstartline=xstart+1                 ! on first pass this is current position on first line tested
      iy=ystart
      ix=xstartline
      NEXTLINE: do iy=ystart,ymax-1                          ! starting at current position look for an input field
         skip_underscore=.false.
         ONLINE: do ix=xstartline,xmax-1
            cell=mvwinch(big_pd,iy,ix)                       ! retrieve cell value
            call get_cell_components(cell,attr,pair,ich,ch)  ! extract cell constituents (attributes, color pair, letter)
            if(iand(attr,A_UNDERLINE).eq. A_UNDERLINE)then   ! if an underscore character
               if(ix.eq.0)then                      ! cannot be second underscore
                  exit PASSES                                ! exit and move to this position
               elseif(skip_underscore)then                   ! in process of skipping to end of underscore field
                  cycle ONLINE
               else                                          ! test if not the first character of an underscore field
                  cell_before=mvwinch(big_pd,iy,ix-1)        ! retrieve previous cell value
                  call get_cell_components(cell_before,attr_before,pair_before,ich_before,ch_before)
                  if(iand(attr_before,A_UNDERLINE).eq. A_UNDERLINE)then
                      skip_underscore=.true.
                      cycle ONLINE
                   else                                      ! firs underline
                      exit PASSES
                   endif
               endif
            elseif((iand(attr,A_ALTCHARSET).eq.A_ALTCHARSET).and.(ch.eq.'`'))then
               skip_underscore=.false.
               exit PASSES
            endif
            skip_underscore=.false.
         enddo ONLINE
         xstartline=0                                        ! after first line scan entire line
      enddo NEXTLINE
      ystart=0 ! if got to end of NEXTLINE loop start from top for one pass (no more in case no input fields).
      xstart=0
      !*! If nothing found go to original
   enddo PASSES
   !------------
   !ierr=wmove(big_pd,iy,ix)  !*! DOES NOT UPDATE CURSOR POSITION
   xdelta=xstart-ix
   ydelta=ystart-iy
   ierr=move(ys-ydelta,xs-xdelta)
   !------------
   !*! this is not quite it, especially left-right
   ! if field not visible move window forward. Try to put in a nice position (center if fits?)
   call getmaxyx(stdscr,LINES,COLS)                          ! window size as defined (all of it, not subsection being displayed)
   if(ys-ydelta.lt.0.or.ys-ydelta.gt.lines-button_lines)then ! find if off screen
      ymove=(LINES-button_lines)/2-(iy-pad_corner_y)
      call move_pd(-ymove,0,'L')                             ! change the corner of the data being displayed
      ierr=move((LINES-button_lines)/2,xs-xdelta)
   endif
   if(xs-xdelta.lt.0.or.xs-xdelta.gt.cols)then               ! find if off screen
      xmove=COLS/2-(ix-pad_corner_x)
      call move_pd(0,-xmove,'L')                             ! change the corner of the data being displayed
      ierr=move(ys-ydelta,COLS/2)
      !jsu
   endif
   !------------
   call refresh_pd()                                         ! refresh so can see change in menu button display
end subroutine find_next
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine find_previous() ! @(#) from current cursor position jump to previous input field in pad jsu
   integer(C_INT)            :: ys, xs
   integer(C_INT)            :: previous_y, previous_x
   integer(C_INT)            :: ydelta, xdelta
   integer(C_INT)            :: ymax, xmax             ! number or rows and columns in window
   integer(C_INT)            :: current_y, current_x   ! where started at
   integer(C_INT)            :: iy,ix                  ! pointer into page
   integer                   :: ierr
   integer                   :: icount_current
   integer                   :: icount
   integer(C_INT)            :: start_of_underline, end_of_underline
   integer(C_INT)            :: ymove,xmove
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_LONG)           :: cell
   character(len=1)          :: ch
   integer(C_INT)            :: ich
   integer(C_LONG)           :: attr
   integer(C_SHORT)          :: pair
!-----------------------------------------------------------------------------------------------------------------------------------
   call getyx(stdscr,ys,xs)                        ! real cursor position in terms of standard screen coordinates
   call getmaxyx(big_pd,ymax,xmax)                 ! size window size as defined (all of it, even if subsection being displayed)
   call getyx(big_pd,current_y,current_x)          !*! current position in columns and rows of pad DATA. Assume in pad for now
   previous_y=current_y
   previous_x=current_x
   icount_current=current_y*xmax+(current_x+1)     ! number of cells into array for current position
      NEXTLINE: do iy=0,ymax-1                     ! starting at top left corner look for an input field till pass original position
         ONLINE: do ix=0,xmax-1
            cell=mvwinch(big_pd,iy,ix)                       ! retrieve cell value
            call get_cell_components(cell,attr,pair,ich,ch)  ! extract cell constituents (attributes, color pair, letter)
            icount=iy*xmax+(ix+1)
            if(iand(attr,A_UNDERLINE).eq. A_UNDERLINE)then
               if(icount.ge.icount_current) exit NEXTLINE    ! up to or past initial position(which might not have been input field)
               previous_y=iy
               previous_x=ix                                 ! this could move back one underline at a time
            elseif((iand(attr,A_ALTCHARSET).eq.A_ALTCHARSET).and.(ch.eq.'`'))then
               if(icount.ge.icount_current) exit NEXTLINE    ! up to or past initial position(which might not have been input field)
               previous_y=iy
               previous_x=ix
            endif
         enddo ONLINE
      enddo NEXTLINE
   !*! if field not visible move window forward. Try to put in a nice position (center if fits?)
   !------------
   xdelta=current_x-previous_x
   ydelta=current_y-previous_y
   !ierr=wmove(big_pd,previous_y,previous_x)                  !*! NOTE: this does not update stdscr cursor position
   ierr=move(ys-ydelta,xs-xdelta)
   !------------
   cell=mvwinch(big_pd,previous_y,previous_x)                ! retrieve cell value of new location (previous input field)
   call get_cell_components(cell,attr,pair,ich,ch)           ! extract cell constituents (attributes, color pair, letter)
   if(iand(attr,A_UNDERLINE).eq. A_UNDERLINE)then            ! this jumps back to the beginning of the underlined section
      call underline_ends(start_of_underline,end_of_underline)
      xdelta=xdelta-(start_of_underline-previous_x)
      ierr=move(ys-ydelta,xs-xdelta)
   endif
   !------------
   !*! this is not quite it, especially left-right
   ! if field not visible move window forward. Try to put in a nice position (center if fits?)
   call getmaxyx(stdscr,LINES,COLS)        ! window size as defined (all of it, even if subsection being displayed)
   if(ys-ydelta.lt.0.or.ys-ydelta.gt.lines-button_lines)then !find if off screen
      ymove=(LINES-button_lines)/2-(previous_y-pad_corner_y)
      call move_pd(-ymove,0,'L')  ! change the corner of the data being displayed
      ierr=move((LINES-button_lines)/2,xs-xdelta)
      !jsu
   endif
   if(xs-xdelta.lt.0.or.xs-xdelta.gt.COLS)then ! find if off screen
      xmove=COLS/2-(previous_x-pad_corner_x)
      call move_pd(0,-xmove,'L')  ! change the corner of the data being displayed
      ierr=move(ys-ydelta,xs-xdelta-xmove)
      !jsu
   endif
   !------------
   call refresh_pd()                                         ! refresh so can see change in menu button display
end subroutine find_previous
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine move_pd(y,x,units)  ! relative change of the corner of the pad data being displayed
! more current corner <pad_corner_y,pad_corner_x> by <y,x> units
! units are L for lines, H for half-pages, P for pages
   ! pad_corner_[x,y] == current position in stdscr of the corner of data displayed, updated to be the new desired corner of data
   ! longline_pd      == number of longest line of data used in page_pd(*)
   ! displaywidth_pd  == width of section of pad displayed on screen
   integer,intent(in)            :: y,x
   character(len=1),intent(in)   :: units
   integer                       :: old_pad_corner_y,old_pad_corner_x
   integer                       :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   call getmaxyx(stdscr,LINES,COLS)                              !*! should not have to set LINES and COLS
   old_pad_corner_y=pad_corner_y
   old_pad_corner_x=pad_corner_x
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (units)                                            ! calculate how much to move given the units passed in
      case('L')                                                   ! move by lines
         pad_corner_x=pad_corner_x+x
         pad_corner_y=pad_corner_y+y
      case('H')                                                   ! move by 1/2 "pages" (half the area the pad section displays in)
         pad_corner_x=pad_corner_x+x*(displaywidth_pd-1)/2
         pad_corner_y=pad_corner_y+y*(LINES-button_lines)/2
      case('P')                                                   ! move by full "pages"
         pad_corner_x=pad_corner_x+x*(displaywidth_pd-1)
         pad_corner_y=pad_corner_y+y*(LINES-button_lines)
   end select
   pad_corner_x=max(0,pad_corner_x)                               ! make sure the move would not go past the edges of the data
   pad_corner_x=min(pad_corner_x,longline_pd-displaywidth_pd)
   pad_corner_y=max(0,pad_corner_y)
   pad_corner_y=min(pad_corner_y,icount_pd-(LINES-button_lines))
!-----------------------------------------------------------------------------------------------------------------------------------
   if(old_pad_corner_y.ne.pad_corner_y.or.old_pad_corner_x.ne.pad_corner_x)then
      call redraw()                                               ! redraw the pad showing the new subsection selected
   else
      ierr=refresh()
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine move_pd
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine redraw()                  ! clear and redraw after verifying (potentially new) page size
   integer :: ierr                   ! holds return value of ncurses routines
   integer :: iy,ix                  ! holds cursor position
   call getmaxyx(stdscr,LINES,COLS)  ! make sure screen size variables are updated because they are really C macros, not variables
   call getyx(stdscr,iy,ix)          ! save cursor position and restore it at the end of the procedure
   !ierr=erase()                      ! clear stdscr
   call refresh_pd()                 ! select region of form to display and draw it
   call buttons()                    ! create and draw buttons at bottom of page
   call message(' ')                 ! draw message line
   ierr=move(iy,ix)                  ! restore cursor position assuming redrawing moved it
end subroutine redraw
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine get_cell_components(cell,attr,pair,ich,ch)  ! given a cell value, find attributes, color pair, and character in cell
!-----------------------------------------------------------------------------------------------------------------------------------
                                    ! EXAMPLE:
                                    !    cell=mvwinch(stdscr,sy,sx)
                                    !    n=PAIR_NUMBER(iand(cell,A_ATTRIBUTES))
                                    !*!   works with cell as well as attributes. Just because way cell data is stored,or dependable?
!-----------------------------------------------------------------------------------------------------------------------------------
   integer(C_LONG),intent(in)    :: cell
   integer(C_LONG),intent(out)   :: attr
   integer(C_SHORT),intent(out)  :: pair
   integer(C_INT),intent(out)    :: ich
   character(len=1),intent(out)  :: ch
!-----------------------------------------------------------------------------------------------------------------------------------
   ich=iand(cell,A_CHARTEXT)               ! extract decimal letter from cell data
   ch=char(ich)                            ! convert decimal letter to ASCII letter
                                           !  Returns the pair number associated with the COLOR_PAIR(n) attribute.
   pair=PAIR_NUMBER(cell)                  ! extract color pair used to draw the cell
   attr=iand(cell,A_ATTRIBUTES)            ! extract the attributes used to draw the cell
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine get_cell_components
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
logical function in(x,y,left,right,bottom,top)
integer :: x,y
integer :: left, right, bottom, top
!  Tests whether a point is inside a particular box.
   if( x.ge.left .and. x.le.right .and. y.ge.bottom .and. y.le.top )then
      in=.true.
   else
      in=.false.
   endif
end function in
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine inbox(x,y,ii)
integer :: x
integer :: y
integer :: ii
integer :: i
integer :: aleft, arit, abot, atop
ii=0
call getmaxyx(stdscr,LINES,COLS)  ! get screen size
do i=1,5
   aleft=button_boxes(i,1)
   arit=button_boxes(i,2)
   abot=LINES-button_boxes(i,3)
   atop=LINES-button_boxes(i,4)
   if(in(x,y,aleft,arit,abot,atop))then
      ii=i
   endif
enddo
end subroutine inbox
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine nc_notabs(INSTR,OUTSTR,ILEN)
! @(#) convert tabs in input to spaces in output while maintaining columns, assuming a tab is set every 8 characters
! given input string INSTR
! return output string OUTSTR
!  o with tabs expanded assuming tabs are set every 8 characters
!  o carriage return and line feed characters are replaced with a space
!  o ILEN holds the position of the last non-blank character in OUTSTR
!
! USES:
!       It is often useful to expand tabs in input files to simplify further processing such as tokenizing an input line.
!       Some FORTRAN compilers hate tabs in input files; some printers; some editors will have problems with tabs.
! AUTHOR:
!       John S. Urban
!
! SEE ALSO:
!       GNU/Unix commands expand(1) and unexpand(1)
!
   CHARACTER(LEN=*),INTENT(IN)   :: instr     ! input line to scan for tab characters
   CHARACTER(LEN=*),INTENT(OUT)  :: outstr    ! tab-expanded version of INSTR produced
   INTEGER,INTENT(OUT)           :: ilen      ! column position of last character put into output string

   INTEGER,PARAMETER             :: tabsize=8 ! assume a tab stop is set every 8th column
   INTEGER                       :: ipos      ! position in OUTSTR to put next character of INSTR
   INTEGER                       :: lenin     ! length of input string trimmed of trailing spaces
   INTEGER                       :: lenout    ! number of characters output string can hold
   INTEGER                       :: istep     ! counter that advances thru input string INSTR one character at a time
   CHARACTER(LEN=1)              :: c         ! character in input line being processed
   INTEGER                       :: iade      ! ADE (ASCII Decimal Equivalent) of character being tested
!===================================================================================================================================
   IPOS=1                                     ! where to put next character in output string OUTSTR
   lenin=LEN(instr)                           ! length of character variable INSTR
   lenin=LEN_TRIM(instr(1:lenin))             ! length of INSTR trimmed of trailing spaces
   lenout=LEN(outstr)                         ! number of characters output string OUTSTR can hold
   OUTSTR=" "                                 ! this SHOULD blank-fill string, a buggy machine required a loop to set all characters
!===================================================================================================================================
      SCAN_LINE: DO istep=1,lenin             ! look through input string one character at a time
         c=instr(istep:istep)                 ! get next character
         iade=ICHAR(c)                        ! get ADE of the character
         expand_tabs : SELECT CASE (iade)     ! take different actions depending on which character was found
         CASE(9)                              ! test if character is a tab and move pointer out to appropriate column
            ipos = ipos + (tabsize - (MOD(ipos-1,tabsize)))
         CASE(10,13)                          ! convert carriage-return and new-line to space ,typically to handle DOS-format files
            ipos=ipos+1
         CASE DEFAULT                         ! c is anything else other than a tab,newline,or return  insert it in output string
            IF(ipos > lenout)THEN
               CALL journal("*nc_notabs* output string overflow")
               EXIT
            ELSE
               outstr(ipos:ipos)=c
               ipos=ipos+1
            ENDIF
         END SELECT expand_tabs
      enddo SCAN_LINE
!===================================================================================================================================
      ipos=MIN(ipos,lenout)                   ! tabs or newline or return characters or last character might have gone too far
      ilen=LEN_TRIM(outstr(:ipos))            ! trim trailing spaces
!===================================================================================================================================
END SUBROUTINE nc_notabs
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
function nc_uniqname(name,ierr) !@(#) append a number to the end of the filename that makes it relatively unique
   ! If necessary, increment the number and try again up to the value 9999.
   ! assumes returned value is 256 characters
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in) :: name
   integer,intent(out)         :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=256)          :: nc_uniqname
   logical                     :: around
   integer,save                :: icount=0      ! counter to generate suffix from
   character(len=256),save     :: lastname=' '  ! name called with last time the routine was called
   integer                     :: ilen
   integer                     :: itimes
   integer                     :: ii
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   itimes=0                                     ! count number of times tried to get a unique name
   nc_uniqname='UNIQ_DID_NOT_WORK'
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lastname.ne.name)then                     ! if a different input name than last time called reset icount
      lastname=name                             ! a new name to keep for subsequent calls
      icount=0                                  ! icount is used to make a suffix to add to make the file unique
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len_trim(name)                          ! find last non-blank character in file name
   if(ilen.gt.(256-4))then                      ! input filename is too long to add suffix to
      call nc_errmessage('*nc_uniqname* ERROR: unable to append number to filename. Filename is too long')
      ierr=-1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   INFINITE: do                                 ! top of loop trying for a unique name
      if(itimes.ge.9999)then                    ! if too many tries to be reasonable give up
         call nc_errmessage('*nc_uniqname* unable to find a unique filename. Too many tries')
         ierr=-2
         return
      endif
      if(icount.gt.9999) icount=0               ! reset ICOUNT when it hits arbitrary maximum value
      ii=index(name,'.')
      if(ii.eq.0)then
         nc_uniqname=name
         write(nc_uniqname(ilen+1:),'(i4.4,a)')icount,'.out'      ! create name by adding a numeric string to end
      else
         nc_uniqname=name(:ii-1)
         write(nc_uniqname(ii:),'(i4.4,a)')icount,trim(name(ii:)) ! create name by adding a numeric string before suffix
      endif
      icount=icount+1                           ! increment counter used to come up with suffix
      inquire(file=trim(nc_uniqname),exist=around) ! see if this filename already exists
      if(.not.around)then                       ! found an unused name
         return                                 ! return successfully
      endif
      itimes=itimes+1                           ! haven't found a unique name, try again
   enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
end function nc_uniqname
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
end module M_fixedform
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
