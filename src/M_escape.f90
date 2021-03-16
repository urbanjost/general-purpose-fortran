!>
!!##NAME
!!    M_escape(3f) - [M_escape] substitute escape sequences for XML-like
!!                   syntax in strings
!!
!!##SYNOPSIS
!!
!!     use M_escape, only : esc, esc_mode, update
!!     use M_escape, only : attr
!!     use M_escape, only : color, color_mode
!!
!!##DESCRIPTION
!!    M_escape is a Fortran module for using XML-like syntax to add
!!    attributes to terminal output such as color.
!!
!!    ANSI escape sequences are not universally supported by all terminal
!!    emulators; and normally should be suppressed when not going to a tty
!!    device. This routine provides the basic structure to support such
!!    behaviors, or to perhaps in the future generate a CSS style sheet
!!    and HTML instead of text to the terminal, ...
!!
!!    Alternatively, direct use of the escape sequences is supported,
!!    as well as a functional interface, and an object-oriented approach.
!!
!!    The original concept was to allow formatting by using an existing
!!    XML library to allow the user to write HTML and to format it on a
!!    terminal like w3m, lynx, and link do. And in some ways this is an
!!    opposite approach in that it is directly formatting the text by using
!!    a similar syntax to directly generate text attributes; but it is a
!!    much simpler approach programmatically.
!!
!!    Typically, you should use M_system::system_istty(3f) or the common
!!    Fortran extension ISATTY() to set the default
!!    to "plain" instead of "color" when the output file is not a terminal.
!!
!!##MAJOR FEATURES
!!    o Add ANSI terminal escape sequences with an XML-like syntax with
!!      ESC(3f).
!!    o suppress the escape sequence output with ESC_MODE(3f).
!!    o add, delete, and replace what strings are produced using UPDATE(3f).
!!
!!##LIMITATIONS
!!      o colors are not nestable, keywords are case-sensitive,
!!      o not all terminals obey the sequences. On Windows, it is best if
!!        you use Windows 10+ and/or the Linux mode; although it has worked
!!        with all CygWin and MinGW and Putty windows and mintty(1).
!!
!!##FUTURE
!!     Full support for alternate output formats like HTML and popular markdown
!!     syntax. For example
!!
!!       ANSI  HTML        Markdown
!!             <h1></h1>   #
!!             <h2></h2>   ##
!!             <b></b>     ** and **
!!             <i></i>     __ and __
!!
!!    Apparently have to make a stack of colors to allow nesting colors
!!
!!    How common are extensions like xterm-256 has to set RGB values for
!!    colors and so on?
!!
!!    Should a call to system_istty(3f) be built in to turn off escape sequences
!!    when a terminal is not present?
!!
!!    For color or pre-defined words a case statement could be used to call
!!    specific functions to support platforms like old Microsoft consoles that
!!    require a function call to assign text attributes instead of in-band ANSI
!!    escape control sequences. See the "Rosetta Code" web site for examples
!!    of generating color in Microsoft consoles.
!!
!!    Attributes are currently ended at the end of each call to esc(3f). Perhaps
!!    allow multi-line formatting?
!!
!!    Ultimately, an object-oriented package with capabilities like ncurses to
!!    define a "pad" and move and resize and format it would be ideal and very
!!    useful. Also see fixedform(3f) in the GPF (General Fortran Package).
!!
!!    It is a shame xterm(1) does not support pixel-oriented abilities to define
!!    a "graphics" area or support canvas(3c)-like in-band graphics, somewhat
!!    like Tektronix terminals, although it does have a Tektronix 4010 mode.
!!
!!    Perhaps overload + to replace //
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_M_escape
!!    use M_escape, only : esc, esc_mode
!!    implicit none
!!    character(len=1024) :: line
!!    real :: value
!!       write(*,'(a)')&
!!       &esc('<r><W>ERROR:</W>This should appear as red text</y>')
!!       write(*,'(a)')&
!!       &esc('<y><B>WARNING:</B></y> This should appear as default text')
!!
!!       value=3.4567
!!       if( (value>0.0) .and. (value<100.0))then
!!          write(line,fmt=&
!!          &'("<w><G>GREAT</G></w>:&
!!          &The new value <Y><b>",f8.4,"</b></Y> is in range")')value
!!       else
!!          write(line,fmt=&
!!          &'("<R><e>ERROR</e></R>:&
!!          &The new value <Y><b>",g0,"</b></Y> is out of range")')value
!!       endif
!!
!!       write(*,'(a)')esc(trim(line))
!!       ! write as plain text
!!       call esc_mode(manner='plain')
!!       write(*,'(a)')esc(trim(line))
!!
!!    end program demo_M_escape
!!
!!##ALTERNATE DIRECT USE
!!
!!   Alternatively, you may use the escape sequences directly
!!
!!    program direct
!!       use M_escape, only : &
!!      ! FOREGROUND COLORS
!!         & fg_red, fg_cyan, fg_magenta, fg_blue, &
!!         & fg_green, fg_yellow, fg_white, fg_ebony, &
!!         & fg_default, &
!!      ! BACKGROUND COLORS
!!         & bg_red, bg_cyan, bg_magenta, bg_blue, &
!!         & bg_green, bg_yellow, bg_white, bg_ebony, &
!!         & bg_default, &
!!      ! ATTRIBUTES
!!         & bold, italic, inverse, underline,  &
!!         & unbold, unitalic, uninverse, ununderline,  &
!!         & reset, &
!!      ! DISPLAY
!!         & clear
!!       implicit none
!!         write(*,'(*(g0))')fg_red,bg_green,bold,'Hello!',reset
!!    end program direct
!!
!!##ALTERNATE FUNCTIONAL INTERFACE
!!
!!  If you prefer a functional interface, use the attr(3f) function with
!!  the same keywords as with the esc(3f) function. Note that esc_mode(3f)
!!  will work with this function.
!!
!!    program functional
!!    use M_escape, only : attr, esc_mode
!!    implicit none
!!         call printme('color')
!!         call printme('plain')
!!         call printme('raw')
!!    contains
!!    subroutine printme(mymode)
!!    character(len=*),intent(in) :: mymode
!!       call esc_mode(mymode)
!!       write(*,'(a)')mymode
!!       write(*,'(*(g0))',advance='no') &
!!        & attr('red:BLUE:bold','Hello!'), &
!!        & 'and everything is back to defaults or ', &
!!        & attr('RED:blue:bold'),'Hello Again!', &
!!        & attr('/BLUE'),' Well, the text color is still on.',attr('reset')
!!       write(*,'(*(g0))',advance='yes')' Back to normal writes.'
!!    end subroutine printme
!!    end program functional
!!
!!##ALTERNATE OBJECT ORIENTED
module M_escape
use M_list, only : insert, locate, replace, remove
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,stdin=>INPUT_UNIT    ! access computing environment
implicit none
private
public esc
public esc_mode
public update
public print_dictionary

!-!public flush_colors, init_colors
public attr

! direct use of constant strings
public color
public color_mode
logical,save :: G_color=.true.

logical,save :: debug=.false.

character(len=:),allocatable,save :: keywords(:)
character(len=:),allocatable,save :: values(:)
integer,allocatable,save :: counts(:)

character(len=:),allocatable,save :: mode

! mnemonics
character(len=*),parameter  :: NL=new_line('a')                     ! New line character.
character(len=*),parameter  :: ESCAPE=achar(27)                     ! "\" character.
! codes
character(len=*),parameter  :: CODE_START=ESCAPE//'['               ! Start ANSI code, "\[".
character(len=*),parameter  :: CODE_END='m'                         ! End ANSI code, "m".
character(len=*),parameter  :: CODE_RESET=CODE_START//'0'//CODE_END ! Clear all styles, "\[0m".

character(len=*),parameter  :: CLEAR_DISPLAY=CODE_START//'2J'
character(len=*),parameter  :: HOME_DISPLAY=CODE_START//'H'
character(len=*),parameter  :: BELL=achar(7)

character(len=*),parameter  :: BOLD_ON='1',   ITALIC_ON='3',   UNDERLINE_ON='4',   INVERSE_ON='7'
character(len=*),parameter  :: BOLD_OFF='22', ITALIC_OFF='23', UNDERLINE_OFF='24', INVERSE_OFF='27'

character(len=*),parameter  :: COLOR_FG_BLACK='30', COLOR_FG_RED='31',     COLOR_FG_GREEN='32', COLOR_FG_YELLOW='33'
character(len=*),parameter  :: COLOR_FG_BLUE='34',  COLOR_FG_MAGENTA='35', COLOR_FG_CYAN='36',  COLOR_FG_WHITE='37'
character(len=*),parameter  :: COLOR_FG_DEFAULT='39'

character(len=*),parameter  :: COLOR_FG_BLACK_INTENSE='90', COLOR_FG_RED_INTENSE='91'
character(len=*),parameter  :: COLOR_FG_GREEN_INTENSE='92', COLOR_FG_YELLOW_INTENSE='93'
character(len=*),parameter  :: COLOR_FG_BLUE_INTENSE='94',  COLOR_FG_MAGENTA_INTENSE='95'
character(len=*),parameter  :: COLOR_FG_CYAN_INTENSE='96',  COLOR_FG_WHITE_INTENSE='97'

character(len=*),parameter  :: COLOR_BG_BLACK='40', COLOR_BG_RED='41',     COLOR_BG_GREEN='42', COLOR_BG_YELLOW='43'
character(len=*),parameter  :: COLOR_BG_BLUE='44',  COLOR_BG_MAGENTA='45', COLOR_BG_CYAN='46',  COLOR_BG_WHITE='47'

character(len=*),parameter  :: COLOR_BG_DEFAULT='49'

character(len=*),parameter  :: COLOR_BG_BLACK_INTENSE='100', COLOR_BG_RED_INTENSE='101'
character(len=*),parameter  :: COLOR_BG_GREEN_INTENSE='102', COLOR_BG_YELLOW_INTENSE='103'
character(len=*),parameter  :: COLOR_BG_BLUE_INTENSE='104',  COLOR_BG_MAGENTA_INTENSE='105'
character(len=*),parameter  :: COLOR_BG_CYAN_INTENSE='106',  COLOR_BG_WHITE_INTENSE='107'

! for direct use of escape sequences

! foreground colors
character(len=*),parameter,public :: fg_red      =  CODE_START//COLOR_FG_RED//CODE_END
character(len=*),parameter,public :: fg_cyan     =  CODE_START//COLOR_FG_CYAN//CODE_END
character(len=*),parameter,public :: fg_magenta  =  CODE_START//COLOR_FG_MAGENTA//CODE_END
character(len=*),parameter,public :: fg_blue     =  CODE_START//COLOR_FG_BLUE//CODE_END
character(len=*),parameter,public :: fg_green    =  CODE_START//COLOR_FG_GREEN//CODE_END
character(len=*),parameter,public :: fg_yellow   =  CODE_START//COLOR_FG_YELLOW//CODE_END
character(len=*),parameter,public :: fg_white    =  CODE_START//COLOR_FG_WHITE//CODE_END
character(len=*),parameter,public :: fg_ebony    =  CODE_START//COLOR_FG_BLACK//CODE_END
character(len=*),parameter,public :: fg_black    =  CODE_START//COLOR_FG_BLACK//CODE_END
character(len=*),parameter,public :: fg_default  =  CODE_START//COLOR_FG_DEFAULT//CODE_END

! background colors
character(len=*),parameter,public :: bg_red      =  CODE_START//COLOR_BG_RED//CODE_END
character(len=*),parameter,public :: bg_cyan     =  CODE_START//COLOR_BG_CYAN//CODE_END
character(len=*),parameter,public :: bg_magenta  =  CODE_START//COLOR_BG_MAGENTA//CODE_END
character(len=*),parameter,public :: bg_blue     =  CODE_START//COLOR_BG_BLUE//CODE_END
character(len=*),parameter,public :: bg_green    =  CODE_START//COLOR_BG_GREEN//CODE_END
character(len=*),parameter,public :: bg_yellow   =  CODE_START//COLOR_BG_YELLOW//CODE_END
character(len=*),parameter,public :: bg_white    =  CODE_START//COLOR_BG_WHITE//CODE_END
character(len=*),parameter,public :: bg_ebony    =  CODE_START//COLOR_BG_BLACK//CODE_END
character(len=*),parameter,public :: bg_black    =  CODE_START//COLOR_BG_BLACK//CODE_END
character(len=*),parameter,public :: bg_default  =  CODE_START//COLOR_BG_DEFAULT//CODE_END

! attributes
character(len=*),parameter,public :: bold        =  CODE_START//BOLD_ON//CODE_END
character(len=*),parameter,public :: italic      =  CODE_START//ITALIC_ON//CODE_END
character(len=*),parameter,public :: inverse     =  CODE_START//INVERSE_ON//CODE_END
character(len=*),parameter,public :: underline   =  CODE_START//UNDERLINE_ON//CODE_END
character(len=*),parameter,public :: unbold      =  CODE_START//BOLD_OFF//CODE_END
character(len=*),parameter,public :: unitalic    =  CODE_START//ITALIC_OFF//CODE_END
character(len=*),parameter,public :: uninverse   =  CODE_START//INVERSE_OFF//CODE_END
character(len=*),parameter,public :: ununderline =  CODE_START//UNDERLINE_OFF//CODE_END

character(len=*),parameter,public :: reset       =  CODE_RESET
character(len=*),parameter,public :: clear       =  HOME_DISPLAY//CLEAR_DISPLAY


contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    esc(3f) - [M_escape] substitute escape sequences for XML-like syntax
!!              in strings
!!
!!##SYNOPSIS
!!
!!     function esc(string,clear_at_end) result (expanded)
!!
!!       character(len=*),intent(in) :: string
!!       logical,intent(in),optional :: clear_at_end
!!       character(len=:),allocatable :: expanded
!!
!!##DESCRIPTION
!!    Use XML-like syntax to add attributes to terminal output such as color.
!!
!!    ANSI escape sequences are not universally supported by all terminal
!!    emulators; and normally should be suppressed when not going to a
!!    tty device. This routine provides the basic structure to support
!!    such behaviors.
!!
!!##OPTIONS
!!    string        input string  of form
!!
!!                    "<attribute_name>string</attribute_name> ...".
!!
!!                   where the current attributes are color names,
!!                   bold, italic, underline, ...
!!
!!    clear_at_end   By default, a sequence to clear all text attributes
!!                   is sent at the end of the returned text if an escape
!!                   character appears in the output string. This can be
!!                   turned off by setting this value to false.
!!##KEYWORDS
!!    current keywords
!!
!!     colors:
!!       r,         red,       R,  RED
!!       g,         green,     G,  GREEN
!!       b,         blue,      B,  BLUE
!!       m,         magenta,   M,  MAGENTA
!!       c,         cyan,      C,  CYAN
!!       y,         yellow,    Y,  YELLOW
!!       e,         ebony,     E,  EBONY
!!       w,         white,     W,  WHITE
!!     attributes:
!!       it,        italic
!!       bo,        bold
!!       un,        underline
!!      other:
!!       clear
!!       esc,       escape
!!       default
!!       gt
!!       lt
!!
!!    By default, if the color mnemonics (ie. the keywords) are uppercase
!!    they change the background color. If lowercase, the foreground color.
!!
!!    The "default" keyword is typically used explicitly when
!!    clear_at_end=.false.
!!
!!    Add, delete, and replace what strings are produced using UPDATE(3f).
!!
!!##LIMITATIONS
!!      o colors are not nestable, keywords are case-sensitive,
!!      o not all terminals obey the sequences. On Windows, it is best if
!!        you use Windows 10+ and/or the Linux mode; although it has worked
!!        with all CygWin and MinGW and Putty windows and mintty.
!!      o you should use "<gt>" and "<lt>" instead of ">" and "<" in a string
!!        processed by esc(3f) instead of in any plain text output so that
!!        the raw mode will create correct input for the esc(3f) function
!!        if read back in.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_esc
!!    use M_escape, only : esc, esc_mode, update
!!       write(*,'(a)') esc('<clear>TEST DEFAULTS:')
!!       call printstuff()
!!
!!       write(*,'(a)') esc('TEST MANNER=PLAIN:')
!!       call esc_mode(manner='plain')
!!       call printstuff()
!!
!!       write(*,'(a)') esc('TEST MANNER=RAW:')
!!       call esc_mode(manner='raw')
!!       call printstuff()
!!
!!       write(*,'(a)') esc('TEST MANNER=color:')
!!       call esc_mode(manner='color')
!!       call printstuff()
!!
!!       write(*,'(a)') esc('TEST ADDING A CUSTOM SEQUENCE:')
!!       call update('blink',char(27)//'[5m')
!!       call update('/blink',char(27)//'[38m')
!!       write(*,'(a)') esc('<blink>Items for Friday</blink>')
!!
!!    contains
!!    subroutine printstuff()
!!
!!      write(*,'(a)') esc('<r>RED</r>,<g>GREEN</g>,<b>BLUE</b>')
!!      write(*,'(a)') esc('<c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y>')
!!      write(*,'(a)') esc('<w>WHITE</w> and <e>EBONY</e>')
!!
!!      write(*,'(a)') esc('Adding <bo>bold</bo>')
!!      write(*,'(a)') esc('<bo><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></bo>')
!!      write(*,'(a)') esc('<bo><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y></bo>')
!!      write(*,'(a)') esc('<bo><w>WHITE</w> and <e>EBONY</e></bo>')
!!
!!      write(*,'(a)') esc('Adding <ul>underline</ul>')
!!      write(*,'(a)') esc(&
!!       &'<bo><ul><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></ul></bo>')
!!      write(*,'(a)') esc(&
!!       &'<bo><ul><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y></ul></bo>')
!!      write(*,'(a)') esc('<bo><ul><w>WHITE</w> and <e>EBONY</e></ul></bo>')
!!
!!      write(*,'(a)') esc('Adding <ul>italic</ul>')
!!      write(*,'(a)') esc(&
!!       &'<bo><ul><it><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></it></ul></bo>')
!!      write(*,'(a)') esc(&
!!       &'<bo><ul><it><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</it></y></ul></bo>')
!!      write(*,'(a)') esc('<bo><ul><it><w>WHITE</w> and <e>EBONY</e></ul></bo>')
!!
!!      write(*,'(a)') esc('Adding <in>inverse</in>')
!!      write(*,'(a)') esc(&
!!       &'<in><bo><ul><it><r>RED</r>,<g>GREEN</g>,&
!!       &<b>BLUE</b></it></ul></bo></in>')
!!      write(*,'(a)') esc(&
!!       &'<in><bo><ul><it><c>CYAN</c>,<m>MAGENTA</g>,&
!!       &<y>YELLOW</it></y></ul></bo></in>')
!!      write(*,'(a)') esc(&
!!       &'<in><bo><ul><it><w>WHITE</w> and <e>EBONY</e></ul></bo></in>')
!!    end subroutine printstuff
!!
!!    end program demo_esc
function esc(string,clear_at_end) result (expanded)
character(len=*),intent(in)  :: string
logical,intent(in),optional  :: clear_at_end
logical                      :: clear_at_end_local
character(len=:),allocatable :: padded
character(len=:),allocatable :: expanded
character(len=:),allocatable :: name
integer                      :: i
integer                      :: ii
integer                      :: maxlen
integer                      :: place
if(present(clear_at_end))then
   clear_at_end_local=clear_at_end
else
   clear_at_end_local=.false.
endif
if(.not.allocated(mode))then  ! set substitution mode
   mode='color' ! 'color'|'raw'|'plain'
   call vt102()
endif

if(mode=='raw')then
   expanded=string
   return
endif

maxlen=len(string)
padded=string//' '
i=1
expanded=''
do
   if(debug)write(*,*)'DEBUG:*esc*: processing',padded(i:i),' from',string(i:),' EXPANDED=',expanded
   select case(padded(i:i))
   case('>')  ! should not get here unless unmatched
      i=i+1
      expanded=expanded//'>'
   case('<')  ! assuming not nested for now
      ii=index(padded(i+1:),'>')
      if(ii.eq.0)then
         expanded=expanded//'<'
         i=i+1
      else
         name=padded(i+1:i+ii-1)
         name=trim(adjustl(name))
         if(debug)write(*,*)'DEBUG:*esc* 1: NAME=',name,get(name)
         call locate(keywords,name,place)
         if(debug)write(*,*)'DEBUG:*esc* 1: LOCATE=',place

         if(mode.eq.'plain')then
         elseif(place.le.0)then     ! unknown name; print what you found
            expanded=expanded//padded(i:i+ii)
         else
            expanded=expanded//get(name)
         endif
         if(name.eq.'debug')debug=.true.   !! developement version
         if(name.eq.'/debug')debug=.false. !! developement version
         i=ii+i+1
      endif
   case default
      expanded=expanded//padded(i:i)
      i=i+1
   end select
   if(i >= maxlen+1)exit
enddo
if( (index(expanded,escape).ne.0).and.(.not.clear_at_end_local))then
   if((mode.ne.'raw').and.(mode.ne.'plain'))then
      expanded=expanded//CODE_RESET                                   ! Clear all styles
   endif
endif
end function esc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    color(3f) - [M_escape] colorize text using a simple function-based approach
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    use M_escape, only : color, color_mode, &
!!
!!     ! FOREGROUND COLORS
!!         & fg_red, fg_cyan, fg_magenta, fg_blue, &
!!         & fg_green, fg_yellow, fg_white, fg_ebony, &
!!         & fg_default, &
!!     ! BACKGROUND COLORS
!!         & bg_red, bg_cyan, bg_magenta, bg_blue, &
!!         & bg_green, bg_yellow, bg_white, bg_ebony, &
!!         & bg_default, &
!!      ! ATTRIBUTES
!!         & bold, italic, inverse, underline,  &
!!         & unbold, unitalic, uninverse, ununderline,  &
!!         & reset, &
!!      ! DISPLAY
!!         & clear
!!
!!     function color(string,fg,bg,style) result (out)
!!
!!      character(len=*),intent(in)          :: string
!!      character(len=*),intent(in),optional :: fg
!!      character(len=*),intent(in),optional :: bg
!!      character(len=*),intent(in),optional :: style
!!
!!##DESCRIPTION
!!     The color constant strings can be used directly but unconditionally
!!     in an output statement. To allow the attributes to be ignored they
!!     can be called with the color(3f) routine, which the color_mode(3f)
!!     procedure can be used to toggle on and off. Note that this routine
!!     does an implicit reset at the end of each use.
!!
!!##OPTIONS
!!    string     string to assign attributes to
!!    fg         foreground color constant
!!    bg         background color constant
!!    style      style keyword or concatenated style keywords
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_color
!!       use M_escape, only : color, color_mode, &
!!      ! FOREGROUND COLORS
!!         & fg_red, fg_cyan, fg_magenta, fg_blue, &
!!         & fg_green, fg_yellow, fg_white, fg_ebony, &
!!         & fg_default, &
!!      ! BACKGROUND COLORS
!!         & bg_red, bg_cyan, bg_magenta, bg_blue, &
!!         & bg_green, bg_yellow, bg_white, bg_ebony, &
!!         & bg_default, &
!!      ! ATTRIBUTES
!!         & bold, italic, inverse, underline,  &
!!         & unbold, unitalic, uninverse, ununderline,  &
!!         & reset, &
!!      ! DISPLAY
!!         & clear
!!       implicit none
!!         write(*,'(*(g0))')fg_red,bg_green,bold,' Hello! ',reset
!!
!!         write(*,'(a)')color(' Hello! ',&
!!          & fg=fg_white,bg=bg_red,style=italic//bold)
!!         call color_mode(.false.)
!!         write(*,'(a)')color(' Hello! ',&
!!          & fg=fg_red,bg=bg_red,style=italic//bold)
!!    end program demo_color
!!
!!##AUTHOR
!!    John S. Urban, 2020
!!##LICENSE
!!    Public Domain
function color(string,fg,bg,style) result (out)

! ident_1="@(#)use the color string constants, optionally ignoring them if G_switch is .false. as set by color_mode(3f)"

character(len=*),intent(in)          :: string
character(len=*),intent(in),optional :: fg
character(len=*),intent(in),optional :: bg
character(len=*),intent(in),optional :: style
character(len=:),allocatable         :: out
out=''
if(G_color)then
   if(present(style))out=out//style
   if(present(bg))out=out//bg
   if(present(fg))out=out//fg
   out=out//string
   out=out//reset
else
   out=string
endif
end function color
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    color_mode(3f) - [M_escape] toggle style effects of color(3f) on and off
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine color_mode(switch)
!!
!!      logical,intent(in) :: switch
!!
!!##DESCRIPTION
!!     The color constant strings can be used directly but unconditionally
!!     in an output statement. To allow the attributes to be ignored they
!!     can be called with the color(3f) routine, which the color_mode(3f)
!!     procedure can be used to toggle on and off. Note that this routine
!!     does an implicit reset at the end of each use.
!!
!!##OPTIONS
!!     switch   turn attributes set by color(3f) on and off
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_color_mode
!!       use M_escape, only : color, color_mode, &
!!      ! FOREGROUND COLORS
!!         & fg_red, fg_cyan, fg_magenta, fg_blue, &
!!         & fg_green, fg_yellow, fg_white, fg_ebony, &
!!         & fg_default, &
!!      ! BACKGROUND COLORS
!!         & bg_red, bg_cyan, bg_magenta, bg_blue, &
!!         & bg_green, bg_yellow, bg_white, bg_ebony, &
!!         & bg_default, &
!!      ! ATTRIBUTES
!!         & bold, italic, inverse, underline,  &
!!         & unbold, unitalic, uninverse, ununderline,  &
!!         & reset, &
!!      ! DISPLAY
!!         & clear
!!       implicit none
!!         write(*,'(*(g0))')fg_red,bg_green,bold,' Hello! ',reset
!!
!!         write(*,'(a)')color(' Hello! ',&
!!          & fg=fg_white,bg=bg_red,style=italic//bold)
!!         call color_mode(.false.)
!!         write(*,'(a)')color(' Hello! ',&
!!          & fg=fg_red,bg=bg_red,style=italic//bold)
!!    end program demo_color_mode
!!
!!##AUTHOR
!!    John S. Urban, 2020
!!##LICENSE
!!    Public Domain
subroutine color_mode(switch)
logical,intent(in) :: switch
   G_color=switch
end subroutine color_mode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine vt102()
! create a dictionary with character keywords, values, and value lengths
! using the routines for maintaining a list

   call wipe_dictionary()
   ! insert and replace entries

   call update('bold',bold)
   call update('/bold',unbold)
   call update('bo',bold)
   call update('/bo',unbold)
   call update('livid',bold)
   call update('/livid',unbold)
   call update('li',bold)
   call update('/li',unbold)

   call update('italic',italic)
   call update('/italic',unitalic)
   call update('it',italic)
   call update('/it',unitalic)

   call update('inverse',inverse)
   call update('/inverse',uninverse)
   call update('in',inverse)
   call update('/in',uninverse)

   call update('underline',underline)
   call update('/underline',ununderline)
   call update('un',underline)
   call update('/un',ununderline)

   call update('esc',ESCAPE)
   call update('escape',ESCAPE)

   call update('clear',clear)
   call update('reset',reset)
   call update('bell',BELL)
   call update('gt','>')
   call update('lt','<')

   ! foreground colors
   call update('r',fg_red)
       call update('/r',fg_default)
       call update('red',fg_red)
       call update('/red',fg_default)
   call update('c',fg_cyan)
       call update('/c',fg_default)
       call update('cyan',fg_cyan)
       call update('/cyan',fg_default)
   call update('m',fg_magenta)
       call update('/m',fg_default)
       call update('magenta',fg_magenta)
       call update('/magenta',fg_default)
   call update('b',fg_blue)
       call update('/b',fg_default)
       call update('blue',fg_blue)
       call update('/blue',fg_default)
   call update('g',fg_green)
       call update('/g',fg_default)
       call update('green',fg_green)
       call update('/green',fg_default)
   call update('y',fg_yellow)
       call update('/y',fg_default)
       call update('yellow',fg_yellow)
       call update('/yellow',fg_default)
   call update('w',fg_white)
       call update('/w',fg_default)
       call update('white',fg_white)
       call update('/white',fg_default)
   call update('e',fg_ebony)
       call update('/e',fg_default)
       call update('ebony',fg_ebony)
       call update('/ebony',fg_default)
   call update('x',fg_ebony)
       call update('/x',fg_default)
       call update('black',fg_ebony)
       call update('/black',fg_default)

   ! background colors
   call update('R',bg_red)
       call update('/R',bg_default)
       call update('RED',bg_red)
       call update('/RED',bg_default)
   call update('C',bg_cyan)
       call update('/C',bg_default)
       call update('CYAN',bg_cyan)
       call update('/CYAN',bg_default)
   call update('M',bg_magenta)
       call update('/M',bg_default)
       call update('MAGENTA',bg_magenta)
       call update('/MAGENTA',bg_default)
   call update('B',bg_blue)
       call update('/B',bg_default)
       call update('BLUE',bg_blue)
       call update('/BLUE',bg_default)
   call update('G',bg_green)
       call update('/G',bg_default)
       call update('GREEN',bg_green)
       call update('/GREEN',bg_default)
   call update('Y',bg_yellow)
       call update('/Y',bg_default)
       call update('YELLOW',bg_yellow)
       call update('/YELLOW',bg_default)
   call update('W',bg_white)
       call update('/W',bg_default)
       call update('WHITE',bg_white)
       call update('/WHITE',bg_default)
   call update('E',bg_ebony)
       call update('/E',bg_default)
       call update('EBONY',bg_ebony)
       call update('/EBONY',bg_default)
   call update('X',bg_ebony)
       call update('/X',bg_default)
       call update('BLACK',bg_ebony)
       call update('/BLACK',bg_default)

end subroutine vt102
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    esc_mode(3f) - [M_escape] select processing mode for output from esc(3f)
!!
!!##SYNOPSIS
!!
!!    subroutine esc_mode(manner)
!!
!!       character(len=*),intent(in) :: manner
!!
!!##DESCRIPTION
!!       Turn off the generation of strings associated with the XML keywords
!!       in the string generated by the esc(3f) function, or display the
!!       text in raw mode as it was passed to esc(3f) or return to ANSI
!!       escape control sequence generation.
!!
!!##OPTIONS
!!    MANNER  The current manners or modes supported via the ESC_MODE(3f)
!!            procedure are
!!
!!        plain          suppress the output associated with keywords
!!        color(default) commonly supported escape sequences
!!        raw            echo the input to ESC(3f) as its output
!!        reload         restore original keyword meanings deleted or
!!                       replaced by calls to update(3f).
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_esc_mode
!!    use M_escape, only : esc, esc_mode
!!    implicit none
!!    character(len=1024) :: line
!!    real :: value
!!
!!      value=3.4567
!!      if( (value>0.0) .and. (value<100.0))then
!!        write(line,fmt='("&
!!       &<w><G>GREAT</G></w>: The value <Y><b>",f8.4,"</b></Y> is in range &
!!       &")')value
!!      else
!!        write(line,fmt='("&
!!       &<R><e>ERROR</e></R>:The new value <Y><b>",g0,"</b></Y> is out of range&
!!       & ")')value
!!      endif
!!
!!      write(*,'(a)')esc(trim(line))
!!
!!      call esc_mode(manner='plain') ! write as plain text
!!      write(*,'(a)')esc(trim(line))
!!      call esc_mode(manner='raw')   ! write as-is
!!      write(*,'(a)')esc(trim(line))
!!      call esc_mode(manner='ansi')  ! return to default mode
!!      write(*,'(a)')esc(trim(line))
!!
!!    end program demo_esc_mode
subroutine esc_mode(manner)
character(len=*),intent(in) :: manner
   if(.not.allocated(mode))then  ! set substitution mode
      mode='color'
      call vt102()
   endif
   select case(manner)
   case('vt102','ANSI','ansi','color','COLOR')
      mode='color'
   case('reload')
      call vt102()
      mode='color'
   case('xterm')
      mode=manner
   case('raw')
      mode=manner
   case('dummy','plain','text')
      mode='plain'
   case default
      write(*,*)'unknown manner. Try color|raw|plain'
      mode='color'
   end select
end subroutine esc_mode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine wipe_dictionary()
   if(allocated(keywords))deallocate(keywords)
   allocate(character(len=0) :: keywords(0))
   if(allocated(values))deallocate(values)
   allocate(character(len=0) :: values(0))
   if(allocated(counts))deallocate(counts)
   allocate(counts(0))
end subroutine wipe_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    update(3f) - [M_escape] update internal dictionary given keyword and value
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine update(key,val)
!!
!!    character(len=*),intent(in)           :: key
!!    character(len=*),intent(in),optional  :: val
!!
!!##DESCRIPTION
!!    Update internal dictionary in M_escape(3fm) module.
!!
!!##OPTIONS
!!    key  name of keyword to add, replace, or delete from dictionary
!!    val  if present add or replace value associated with keyword. If not
!!         present remove keyword entry from dictionary.
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_update
!!    use M_escape, only : esc, update
!!       write(*,'(a)') esc('<clear>TEST CUSTOMIZED:')
!!       ! add custom keywords
!!       call update('blink',char(27)//'[5m')
!!       call update('/blink',char(27)//'[38m')
!!
!!       write(*,'(a)') esc('<blink>Items for Friday</blink>')
!!
!!       write(*,'(a)',advance='no') esc('<r>RED</r>,')
!!       write(*,'(a)',advance='no') esc('<b>BLUE</b>,')
!!       write(*,'(a)',advance='yes') esc('<g>GREEN</g>')
!!
!!       ! delete
!!       call update('r')
!!       call update('/r')
!!       ! replace
!!       call update('b','<<<<')
!!       call update('/b','>>>>')
!!       write(*,'(a)',advance='no') esc('<r>RED</r>,')
!!       write(*,'(a)',advance='no') esc('<b>BLUE</b>,')
!!       write(*,'(a)',advance='yes') esc('<g>GREEN</g>')
!!
!!    end program demo_update
!!
!!##AUTHOR
!!    John S. Urban, 2020
!!
!!##LICENSE
!!    Public Domain
subroutine update(key,valin)
character(len=*),intent(in)           :: key
character(len=*),intent(in),optional  :: valin
integer                               :: place
integer                               :: ilen
character(len=:),allocatable          :: val
if(present(valin))then
   val=valin
   ilen=len_trim(val)
   ! find where string is or should be
   call locate(keywords,key,place)
   ! if string was not found insert it
   if(place.lt.1)then
      call insert(keywords,key,iabs(place))
      call insert(values,val,iabs(place))
      call insert(counts,ilen,iabs(place))
   else
      call replace(values,val,place)
      call replace(counts,ilen,place)
   endif
else
   call locate(keywords,key,place)
   if(place.gt.0)then
      call remove(keywords,place)
      call remove(values,place)
      call remove(counts,place)
   endif
endif
end subroutine update
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function get(key) result(valout)
character(len=*),intent(in)   :: key
character(len=:),allocatable  :: valout
integer                       :: place
   ! find where string is or should be
   call locate(keywords,key,place)
   if(place.lt.1)then
      valout=''
   else
      valout=values(place)(:counts(place))
   endif
end function get
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    print_dictionary(3f) - [ARGUMENTS:M_CLI2] print internal dictionary
!!                           created by calls to update(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    subroutine print_dictionary(header)
!!
!!     character(len=*),intent(in),optional :: header
!!
!!##DESCRIPTION
!!    Print the internal dictionary created by calls to update(3f).
!!    This routine is intended to print the state of the argument list
!!    if an error occurs in using the update(3f) procedure.
!!
!!##OPTIONS
!!     HEADER  label to print before printing the state of the command
!!             argument list.
!!##EXAMPLE
!!
!!
!!   Typical usage:
!!
!!    program demo_print_dictionary
!!    use M_escape, only : esc, update, print_dictionary
!!    implicit none
!!       write(*,'(a)') esc('<clear>TEST CUSTOMIZED:')
!!       ! add custom keywords
!!       call update('blink',char(27)//'[5m')
!!       call update('/blink',char(27)//'[38m')
!!       call print_dictionary('DICTIONARY')
!!       write(*,'(a)') esc('<blink>Items for Friday</blink>')
!!    end program demo_print_dictionary
!!
!!   Sample output
!!
!!    demo_print_dictionary |cat -v -e -t
!!
!!##AUTHOR
!!      John S. Urban, 2020
!!
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine print_dictionary(header)
character(len=*),intent(in),optional :: header
integer          :: i
   if(present(header))then
      if(header.ne.'')then
         write(stderr,'(a)')header
      endif
   endif
   if(allocated(keywords))then
      if(size(keywords).gt.0)then
         write(stderr,'(*(a,t30,a))')'KEYWORD','VALUE'
         write(stderr,'(*(a,t30,"[",a,"]",/))')(trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords))
      endif
   endif
end subroutine print_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine split(input_line,array,delimiters)
!-----------------------------------------------------------------------------------------------------------------------------------

!$@(#) M_escape::split(3f): parse string on delimiter characters and store tokens into an allocatable array

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
integer                       :: ii                     ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: ilen                   ! length of input string with trailing spaces trimmed
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters.ne.'')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   if(allocated(ibegin))deallocate(ibegin)
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   if(allocated(iterm))deallocate(iterm)
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len(input_line)                                           ! ILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ilen)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (:0)                                                      ! command was totally blank
!-----------------------------------------------------------------------------------------------------------------------------------
   case default                                                   ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,ilen,1                                   ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
            iterm(i30)=ilen                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):ilen),dlim(i10:i10))
               IF(ifound.gt.0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol.gt.ilen)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
      ireturn=inotnull
   if(allocated(array))deallocate(array)
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
!-----------------------------------------------------------------------------------------------------------------------------------
   ii=1
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20).lt.ibegin(i20))then
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+1
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   if(allocated(ibegin))deallocate(ibegin)
   if(allocated(iterm))deallocate(iterm)
   end subroutine split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    attr(3f) - [M_escape] colorize text using a simple function-based approach
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function attr(attribute) result (out)
!!
!!    character(len=*),intent(in)  :: attribute
!!    character(len=:),allocatable :: out
!!
!!##DESCRIPTION
!!    attr(3f) uses the same keywords as esc(3f) to send ANSI escape
!!    sequences to the display screen, except instead of using a pseudo-XML
!!    string to select the codes it uses a simple colon-delimited list of
!!    the keywords.
!!
!!##OPTIONS
!!    attribute  colon, space, or comma-delimited list of attribute keywords
!!               as defined in the esc(3f) procedure.
!!    text       if supplied it is printed and then an attribute reset is added
!!
!!##RETURNS
!!    out        output the strings assigned by the keywords (by default
!!    ANSI video
!!               display escape sequences, see update(3f) )
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_attr
!!    use M_escape, only : attr, esc_mode
!!    implicit none
!!         call printme('color')
!!         call printme('plain')
!!         call printme('raw')
!!    contains
!!    subroutine printme(mymode)
!!    character(len=*),intent(in) :: mymode
!!       call esc_mode(mymode)
!!       write(*,'(a)')mymode
!!       write(*,'(*(g0))',advance='no')attr('red:BLUE:bold','Hello!'), &
!!        & 'and everything is back to defaults or ', &
!!        & attr('RED:blue:bold'),'Hello Again!', &
!!        & attr('/RED'),' Well, the text color is still blue.',attr('reset')
!!       write(*,'(*(g0))',advance='yes')' Back to a normal write statement.'
!!    end subroutine printme
!!    end program demo_attr
!!
!!##AUTHOR
!!    John S. Urban, 2020
!!
!!##LICENSE
!!    Public Domain
function attr(attribute,text) result(out)
! colon,space, or comma-delimited string of attributes
character(len=*),intent(in)          :: attribute
character(len=*),intent(in),optional :: text
character(len=:),allocatable         :: out
character(len=:),allocatable         :: array(:)
integer                              :: i
   if(.not.allocated(mode))then  ! set substitution mode
      mode='color'
      call vt102()
   endif
   out=''
   call split(attribute,array,delimiters=' :,')
   do i=1,size(array)
      if(mode=='raw')then
         out=out//'<'//trim(array(i))//'>'
      elseif(mode=='plain')then
         out=''
      else
         out=out//get(trim(array(i)))
      endif
   enddo
   if(present(text))then
   out=out//text//esc('<reset>')
   endif
end function attr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
! one question would be if the strings should not be parameters so you could flush and reset them.
!-!subroutine color_reset()
!-!! for direct use of escape sequences
!-!
!-!! foreground colors
!-! fg_red      =  CODE_START//COLOR_FG_RED//CODE_END
!-! fg_cyan     =  CODE_START//COLOR_FG_CYAN//CODE_END
!-! fg_magenta  =  CODE_START//COLOR_FG_MAGENTA//CODE_END
!-! fg_blue     =  CODE_START//COLOR_FG_BLUE//CODE_END
!-! fg_green    =  CODE_START//COLOR_FG_GREEN//CODE_END
!-! fg_yellow   =  CODE_START//COLOR_FG_YELLOW//CODE_END
!-! fg_white    =  CODE_START//COLOR_FG_WHITE//CODE_END
!-! fg_ebony    =  CODE_START//COLOR_FG_BLACK//CODE_END
!-! fg_default  =  CODE_START//COLOR_FG_DEFAULT//CODE_END
!-!
!-!! background colors
!-! bg_red      =  CODE_START//COLOR_BG_RED//CODE_END
!-! bg_cyan     =  CODE_START//COLOR_BG_CYAN//CODE_END
!-! bg_magenta  =  CODE_START//COLOR_BG_MAGENTA//CODE_END
!-! bg_blue     =  CODE_START//COLOR_BG_BLUE//CODE_END
!-! bg_green    =  CODE_START//COLOR_BG_GREEN//CODE_END
!-! bg_yellow   =  CODE_START//COLOR_BG_YELLOW//CODE_END
!-! bg_white    =  CODE_START//COLOR_BG_WHITE//CODE_END
!-! bg_ebony    =  CODE_START//COLOR_BG_BLACK//CODE_END
!-! bg_default  =  CODE_START//COLOR_BG_DEFAULT//CODE_END
!-!
!-!! attributes
!-! bold        =  CODE_START//BOLD_ON//CODE_END
!-! italic      =  CODE_START//ITALIC_ON//CODE_END
!-! inverse     =  CODE_START//INVERSE_ON//CODE_END
!-! underline   =  CODE_START//UNDERLINE_ON//CODE_END
!-! unbold      =  CODE_START//BOLD_OFF//CODE_END
!-! unitalic    =  CODE_START//ITALIC_OFF//CODE_END
!-! uninverse   =  CODE_START//INVERSE_OFF//CODE_END
!-! ununderline =  CODE_START//UNDERLINE_OFF//CODE_END
!-!
!-! reset       =  CODE_RESET
!-! clear       =  CLEAR_DISPLAY
!-!subroutine color_reset()
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!-!subroutine color_flush()
!-!! for direct use of escape sequences
!-!
!-!! foreground colors
!-! fg_red      =  ''
!-! fg_cyan     =  ''
!-! fg_magenta  =  ''
!-! fg_blue     =  ''
!-! fg_green    =  ''
!-! fg_yellow   =  ''
!-! fg_white    =  ''
!-! fg_ebony    =  ''
!-! fg_default  =  ''
!-!
!-!! background colors
!-! bg_red      =  ''
!-! bg_cyan     =  ''
!-! bg_magenta  =  ''
!-! bg_blue     =  ''
!-! bg_green    =  ''
!-! bg_yellow   =  ''
!-! bg_white    =  ''
!-! bg_ebony    =  ''
!-! bg_default  =  ''
!-!
!-!! attributes
!-! bold        =  ''
!-! italic      =  ''
!-! inverse     =  ''
!-! underline   =  ''
!-! unbold      =  ''
!-! unitalic    =  ''
!-! uninverse   =  ''
!-! ununderline =  ''
!-!
!-! reset       =  CODE_RESET
!-! clear       =  CLEAR_DISPLAY
!-!end subroutine color_flush
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_escape
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
