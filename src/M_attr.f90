!>
!!##NAME
!!    M_attr(3f) - [M_attr::INTRO] control text attributes on terminals
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!
!!      use M_attr, only : attr, attr_mode, attr_update
!!
!!      use M_attr, only : alert ! generate standard messages
!!
!!##DESCRIPTION
!!    M_attr(3f) is a Fortran module that uses common ANSI escape sequences
!!    to control terminal text attributes.
!!
!!         use M_attr
!!         write(*,*)attr('<red>Red Text!</red> <green>Green Text!</green>')
!!         end
!!
!!    It is designed to use three simple procedures to
!!
!!     + Specify attributes using simple HTML-like syntax
!!     + allow the sequences to be suppressed when desired
!!     + permit the  user program to completely customize the keywords.
!!       The user can add, delete and replace the sequences associated with
!!       a keyword without changing the code.
!!
!!    One advantage of the approach of using formatting directives which
!!    are replaced with in-band escape sequences is that it is easy to turn
!!    off when running batch.
!!
!!    Another important capability is that programs can be run in "raw" mode
!!    and create a simple text file with the formatting directives in it
!!    that can then be read back in by a simple filter program that strips
!!    it back to plain text( see app/plain.f90), or displays it to a screen
!!    in color(see app/light.f90) or perhaps converts it to another format.
!!
!!    So this approach makes it trivial to read specially-formatted data
!!    from a file like a message catalog (perhaps with various versions
!!    in different languages) and colorize it or display it as plain text
!!
!!    By making each line self-contained (by default) lines can be filtered
!!    by external utilities and still display correctly.
!!
!!##ACCESS
!!    Via git(1):
!!
!!        git clone https://github.com/urbanjost/M_attr.git
!!        cd M_attr/src
!!        # change Makefile if not using one of the listed compilers
!!        make clean; make gfortran    # for gfortran
!!        make clean; make ifort       # for ifort
!!        make clean; make nvfortran   # for nvfortran
!!
!!    This will compile the M_attr module and example programs.
!!
!!    Alternatively, via fpm ( described at https://github.com/fortran-lang/fpm):
!!
!!         git clone https://github.com/urbanjost/M_attr.git
!!
!!    or just list it as a dependency in your fpm.toml project file.
!!
!!         [dependencies]
!!         M_attr        = { git = "https://github.com/urbanjost/M_attr.git" }
!!
!!##LIMITATIONS
!!   o colors are not nestable.
!!   o keywords are case-sensitive,
!!   o ANSI escape sequences are not universally supported by
!!     all terminal emulators; and normally should be suppressed
!!     when not going to a tty device. Therefore, you should use
!!     M_system::system_istty(3f) or the common Fortran extension
!!     ISATTY() to set the default to "plain" instead of "color"
!!     when the output file is not a conforming terminal. On basic
!!     MSWindows console windows, it is best to use Windows 10+ and/or
!!     the Linux mode; you may have to enable ANSI escape sequence
!!     mode on MSWindows. It does work as-is with CygWin and MinGW and
!!     Putty windows and mintty(1) as tested.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!     program demo_M_attr
!!     use M_attr, only : attr, attr_mode, attr_update, alert
!!     implicit none
!!     character(len=256) :: line
!!     character(len=*),parameter :: f='( &
!!      &"   <bo><w><G> GREAT: </G></w>&
!!      &The new value <Y><b>",f8.4,1x,"</b></Y> is in range"&
!!      &)'
!!     real :: value
!!
!!        write(*,'(a)')&
!!        &attr('   <r><W><bo> ERROR: </W>red text on a white background</y>')
!!
!!        value=3.4567
!!        write(line,fmt=f) value
!!        write(*,'(a)')attr(trim(line))
!!
!!        ! write same string as plain text
!!        write(*,*)
!!        call attr_mode(manner='plain')
!!        write(*,'(a)')attr(trim(line))
!!
!!        call attr_mode(manner='color')
!!        ! use pre-defined or user defined strings
!!        write(*,*)
!!        write(*,'(a)')attr('<ERROR> Woe is nigh.')
!!        write(*,'(a)')attr('<WARNING> The night is young.')
!!        write(*,'(a)')attr('<INFO> It is Monday')
!!
!!        call alert('<ERROR>', 'Woe is nigh.')
!!        call alert('<WARNING>', 'The night is young.')
!!        call alert('<INFO>', 'It is Monday')
!!
!!        ! create a custom mnemonic
!!        call attr_update('MYERROR',attr(&
!!        ' <R><e> E<w>-<e>R<w>-<e>R<w>-<e>O<w>-<e>R: </e></R></bo>'&
!!        ))
!!        write(*,*)
!!        write(*,'(a)')attr('<MYERROR> my custom message style')
!!
!!     end program demo_M_attr
!!
!!##AUTHOR
!!    John S. Urban, 2021
!!
!!##LICENSE
!!    MIT
!!
!!##SEE ALSO
!!    attr(3f), attr_mode(3f), attr_update(3f)
!!
!!    Related information:
!!
!!     terminfo(3c), termlib(3c), tput(1), reset(1), clear(1),
!!     console_codes(4), ECMA-48,
!!     https://en.wikipedia.org/wiki/ANSI_escape_code
module M_attr
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,stdin=>INPUT_UNIT,stdout=>OUTPUT_UNIT
use, intrinsic :: iso_c_binding, only: c_int
implicit none
private
public  :: attr
public  :: attr_mode
public  :: attr_update
public  :: alert, advice

private :: attr_matrix
private :: attr_scalar
private :: attr_scalar_width
private :: get

private :: locate   ! find PLACE in sorted character array where value can be found or should be placed
private :: insert   ! insert entry into a sorted allocatable array at specified position
private :: replace  ! replace entry by index from a sorted allocatable array if it is present
private :: remove   ! delete entry by index from a sorted allocatable array if it is present
private :: wipe_dictionary

private :: vt102

interface attr
   module procedure attr_scalar
   module procedure attr_matrix
   module procedure attr_scalar_width
end interface

interface advice  ! deprecated old name for alert(3f)
   module procedure alert
end interface

! direct use of constant strings

character(len=:),allocatable,save :: keywords(:)
character(len=:),allocatable,save :: values(:)
character(len=:),allocatable,save :: mono_values(:)

character(len=:),allocatable,save :: mode

! mnemonics
character(len=*),parameter  :: NL=new_line('a')                     ! New line character.
! DECIMAL
! *-------*-------*-------*-------*-------*-------*-------*-------*
! | 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|
! | 08 bs | 09 ht | 10 nl | 11 vt | 12 np | 13 cr | 14 so | 15 si |
! | 16 dle| 17 dc1| 18 dc2| 19 dc3| 20 dc4| 21 nak| 22 syn| 23 etb|
! | 24 can| 25 em | 26 sub| 27 esc| 28 fs | 29 gs | 30 rs | 31 us |
! | 32 sp | 33  ! | 34  " | 35  # | 36  $ | 37  % | 38  & | 39  ' |
! | 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |
! | 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |
! | 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |
! | 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |
! | 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |
! | 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |
! | 88  X | 89  Y | 90  Z | 91  [ | 92  \ | 93  ] | 94  ^ | 95  _ |
! | 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |
! |104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |
! |112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |
! |120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 del|
! *-------*-------*-------*-------*-------*-------*-------*-------*
character(len=*),parameter  :: nul=achar(0)
character(len=*),parameter  :: bel =achar(7)   ! ^G beeps;
character(len=*),parameter  :: bs =achar(8)    ! ^H backspaces one column (but not past the beginning of the line);
character(len=*),parameter  :: ht =achar(9)    ! ^I goes to next tab stop or to the end of the line if there is no earlier tab stop
character(len=*),parameter  :: lf =achar(10)   ! ^J
character(len=*),parameter  :: vt =achar(11)   ! ^K
character(len=*),parameter  :: ff =achar(12)   ! ^L all give a linefeed, and if LF/NL (new-line mode) is set also a carriage return
character(len=*),parameter  :: cr =achar(13)   ! ^M gives a carriage return;
character(len=*),parameter  :: so =achar(14)   ! ^N activates the G1 character set;
character(len=*),parameter  :: si =achar(15)   ! ^O activates the G0 character set;
character(len=*),parameter  :: can =achar(24)  ! ^X interrupt escape sequences;
character(len=*),parameter  :: sub=achar(26)   ! ^Z interrupt escape sequences;
character(len=*),parameter  :: esc =achar(27)  ! ^[ starts an escape sequence;
character(len=*),parameter  :: del =achar(127) ! is ignored;
! codes
character(len=*),parameter  :: CODE_START=esc//'['               ! Start ANSI code, "\[".
character(len=*),parameter  :: CODE_END='m'                         ! End ANSI code, "m".
character(len=*),parameter  :: CODE_RESET=CODE_START//'0'//CODE_END ! Clear all styles, "\[0m".

character(len=*),parameter  :: CLEAR_DISPLAY=CODE_START//'2J'
character(len=*),parameter  :: HOME_DISPLAY=CODE_START//'H'
character(len=*),parameter  :: BELL=achar(7)

character(len=*),parameter  :: AT_BOLD='1', AT_ITALIC='3', AT_UNDERLINE='4', AT_INVERSE='7'
character(len=*),parameter  :: BLACK='0', RED='1', GREEN='2', YELLOW='3', BLUE='4', MAGENTA='5', CYAN='6', WHITE='7', DEFAULT='9'
!prefixes
character(len=*),parameter  :: FG='3'
character(len=*),parameter  :: BG='4'
character(len=*),parameter  :: FG_INTENSE='9'
character(len=*),parameter  :: BG_INTENSE='10'
character(len=*),parameter  :: ON=''
character(len=*),parameter  :: OFF='2'


! foreground colors
character(len=*),parameter,public :: fg_red      =  CODE_START//FG//RED//CODE_END
character(len=*),parameter,public :: fg_cyan     =  CODE_START//FG//CYAN//CODE_END
character(len=*),parameter,public :: fg_magenta  =  CODE_START//FG//MAGENTA//CODE_END
character(len=*),parameter,public :: fg_blue     =  CODE_START//FG//BLUE//CODE_END
character(len=*),parameter,public :: fg_green    =  CODE_START//FG//GREEN//CODE_END
character(len=*),parameter,public :: fg_yellow   =  CODE_START//FG//YELLOW//CODE_END
character(len=*),parameter,public :: fg_white    =  CODE_START//FG//WHITE//CODE_END
character(len=*),parameter,public :: fg_ebony    =  CODE_START//FG//BLACK//CODE_END
character(len=*),parameter,public :: fg_black    =  CODE_START//FG//BLACK//CODE_END
character(len=*),parameter,public :: fg_default  =  CODE_START//FG//DEFAULT//CODE_END
! background colors
character(len=*),parameter,public :: bg_red      =  CODE_START//BG//RED//CODE_END
character(len=*),parameter,public :: bg_cyan     =  CODE_START//BG//CYAN//CODE_END
character(len=*),parameter,public :: bg_magenta  =  CODE_START//BG//MAGENTA//CODE_END
character(len=*),parameter,public :: bg_blue     =  CODE_START//BG//BLUE//CODE_END
character(len=*),parameter,public :: bg_green    =  CODE_START//BG//GREEN//CODE_END
character(len=*),parameter,public :: bg_yellow   =  CODE_START//BG//YELLOW//CODE_END
character(len=*),parameter,public :: bg_white    =  CODE_START//BG//WHITE//CODE_END
character(len=*),parameter,public :: bg_ebony    =  CODE_START//BG//BLACK//CODE_END
character(len=*),parameter,public :: bg_black    =  CODE_START//BG//BLACK//CODE_END
character(len=*),parameter,public :: bg_default  =  CODE_START//BG//DEFAULT//CODE_END
! attributes
character(len=*),parameter,public :: bold        =  CODE_START//ON//AT_BOLD//CODE_END
character(len=*),parameter,public :: italic      =  CODE_START//ON//AT_ITALIC//CODE_END
character(len=*),parameter,public :: inverse     =  CODE_START//ON//AT_INVERSE//CODE_END
character(len=*),parameter,public :: underline   =  CODE_START//ON//AT_UNDERLINE//CODE_END
character(len=*),parameter,public :: unbold      =  CODE_START//'22'//CODE_END
character(len=*),parameter,public :: unitalic    =  CODE_START//OFF//AT_ITALIC//CODE_END
character(len=*),parameter,public :: uninverse   =  CODE_START//OFF//AT_INVERSE//CODE_END
character(len=*),parameter,public :: ununderline =  CODE_START//OFF//AT_UNDERLINE//CODE_END

character(len=*),parameter,public :: reset       =  CODE_RESET
character(len=*),parameter,public :: clear       =  HOME_DISPLAY//CLEAR_DISPLAY

!private fmt
private str

integer,save :: alert_unit=stdout
logical,save :: alert_debug=.true.
logical,save :: alert_warn=.true.
logical,save :: alert_info=.true.
logical,save :: alert_error=.true.
logical,save :: alert_other=.true.

interface str
   module procedure msg_scalar, msg_one
end interface str

contains
!>
!!##NAME
!!    attr(3f) - [M_attr] substitute escape sequences for HTML-like syntax
!!               in strings
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!      function attr(string,reset) result (expanded)
!!
!!        ! scalar
!!        character(len=*),intent(in)  :: string
!!        logical,intent(in),optional  :: reset
!!        character(len=:),allocatable :: expanded
!!        ! or array
!!        character(len=*),intent(in)  :: string(:)
!!        logical,intent(in),optional  :: reset
!!        character(len=:),allocatable :: expanded(:)
!!        integer,intent(in),optional  :: chars
!!
!!##DESCRIPTION
!!    Use HTML-like syntax to add attributes to terminal output such as color
!!    on devices that recognize ANSI escape sequences.
!!
!!##OPTIONS
!!    string        input string  of form
!!
!!                    "<attribute_name>string</attribute_name> ...".
!!
!!                   where the current attributes are color names,
!!                   bold, italic, underline, ...
!!
!!    reset          By default, a sequence to clear all text attributes
!!                   is sent at the end of each returned line if an escape
!!                   character appears in the output string. This can be
!!                   turned off by setting RESET to .false. .
!!
!!                   Note if turning off the reset attributes may be
!!                   continued across lines, but if each line is not
!!                   self-contained attributes may not display properly
!!                   when filtered with commands such as grep(1).
!!
!!    chars          For arrays, a reset will be placed after the Nth
!!                   displayable column count in order to make it easier
!!                   to generate consistent right borders for non-default
!!                   background colors for a text block.
!!##KEYWORDS
!!    primary default keywords
!!
!!      colors:
!!        r,         red,       R,  RED
!!        g,         green,     G,  GREEN
!!        b,         blue,      B,  BLUE
!!        m,         magenta,   M,  MAGENTA
!!        c,         cyan,      C,  CYAN
!!        y,         yellow,    Y,  YELLOW
!!        e,         ebony,     E,  EBONY
!!        w,         white,     W,  WHITE
!!
!!      attributes:
!!        it,        italic
!!        bo,        bold
!!        un,        underline
!!
!!      basic control characters:
!!       nul
!!       bel  (0x07, ^G) beeps;
!!       bs   (0x08, ^H) backspaces one column (but not past the beginning of
!!                       the line);
!!       ht   (0x09, ^I) goes to the next tab stop or to the end of the line if
!!                       there is no earlier tab stop;
!!       lf   (0x0A, ^J),
!!       vt   (0x0B, ^K)
!!       ff   (0x0C, ^L) all give a linefeed, and if LF/NL (new-line mode) is
!!                       set also a carriage return
!!       cr   (0x0D, ^M) gives a carriage return;
!!       so   (0x0E, ^N) activates the G1 character set;
!!       si   (0x0F, ^O) activates the G0 character set;
!!       can  (0x18, ^X) and SUB (0x1A, ^Z) interrupt escape sequences;
!!       sub
!!       esc  (0x1B, ^[) starts an escape sequence;
!!       del  (0x7F) is ignored;
!!
!!      other:
!!        clear
!!        default
!!        reset
!!        gt
!!        lt
!!        save,DECSC     Save  current state (cursor coordinates, attributes,
!!                       character sets pointed at by G0, G1).
!!        restore,DECRC  Restore state most recently saved by ESC 7.
!!        CSI            "Control Sequence Introducer"(0x9B) is equivalent to
!!                       "ESC [".
!!
!!      dual-value (one for color, one for mono):
!!
!!        write(*,*)attr('<ERROR>an error message')
!!        write(*,*)attr('<WARNING>a warning message')
!!        write(*,*)attr('<INFO>an informational message')
!!
!!    By default, if the color mnemonics (ie. the keywords) are uppercase
!!    they change the background color. If lowercase, the foreground color.
!!    When preceded by a "/" character the attribute is returned to the default.
!!
!!    The "default" keyword is typically used explicitly when
!!    reset=.false, and sets all text attributes to their initial defaults.
!!
!!##LIMITATIONS
!!    o colors are not nestable, keywords are case-sensitive,
!!    o not all terminals obey the sequences. On Windows, it is best if
!!      you use Windows 10+ and/or the Linux mode; although it has worked
!!      with all CygWin and MinGW and Putty windows and mintty.
!!    o you should use "<gt>" and "<lt>" instead of ">" and "<" in a string
!!      processed by attr(3f) instead of in any plain text output so that
!!      the raw mode will create correct input for the attr(3f) function
!!      if read back in.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!     program demo_attr
!!     use M_attr, only : attr, attr_mode, attr_update
!!        call printstuff('defaults')
!!
!!        call attr_mode(manner='plain')
!!        call printstuff('plain:')
!!
!!        call printstuff('raw')
!!
!!        call attr_mode(manner='color')
!!        call printstuff('')
!!
!!        write(*,'(a)') attr('TEST ADDING A CUSTOM SEQUENCE:')
!!        call attr_update('blink',char(27)//'[5m')
!!        call attr_update('/blink',char(27)//'[25m')
!!        write(*,'(a)') attr('<blink>Items for Friday</blink>')
!!
!!     contains
!!     subroutine printstuff(label)
!!     character(len=*),intent(in)  :: label
!!     character(len=:),allocatable :: array(:)
!!       call attr_mode(manner=label)
!!
!!       array=[character(len=60) ::    &
!!        'TEST MANNER='//label,                      &
!!        '<r>RED</r>,<g>GREEN</g>,<b>BLUE</b>',      &
!!        '<c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y>', &
!!        '<w>WHITE</w> and <e>EBONY</e>']
!!       write(*,'(a)') attr(array)
!!
!!       write(*,'(a)') attr('Adding <bo>bold</bo>')
!!       write(*,'(a)') attr('<bo><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></bo>')
!!       write(*,'(a)') attr('<bo><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y></bo>')
!!       write(*,'(a)') attr('<bo><w>WHITE</w> and <e>EBONY</e></bo>')
!!
!!       write(*,'(a)') attr('Adding <ul>underline</ul>')
!!       write(*,'(a)') attr(&
!!        &'<bo><ul><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></ul></bo>')
!!       write(*,'(a)') attr(&
!!        &'<bo><ul><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y></ul></bo>')
!!       write(*,'(a)') attr('<bo><ul><w>WHITE</w> and <e>EBONY</e></ul></bo>')
!!
!!       write(*,'(a)') attr('Adding <ul>italic</ul>')
!!       write(*,'(a)') attr(&
!!        &'<bo><ul><it><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></it></ul></bo>')
!!       write(*,'(a)') attr(&
!!        &'<bo><ul><it><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</it></y></ul></bo>')
!!       write(*,'(a)') attr('<bo><ul><it><w>WHITE</w> and <e>EBONY</e></ul></bo>')
!!
!!       write(*,'(a)') attr('Adding <in>inverse</in>')
!!       write(*,'(a)') attr('<in><bo><ul><it><r>RED</r>,<g>GREEN</g>,&
!!        &<b>BLUE</b></it></ul></bo></in>')
!!       write(*,'(a)') attr('<in><bo><ul><it><c>CYAN</c>,<m>MAGENTA</g>,&
!!        &<y>YELLOW</it></y></ul></bo></in>')
!!       write(*,'(a)') attr(&
!!        &'<in><bo><ul><it><w>WHITE</w> and <e>EBONY</e></ul></bo></in>')
!!     end subroutine printstuff
!!     end program demo_attr
!!
!!##AUTHOR
!!    John S. Urban, 2021
!!
!!##LICENSE
!!    MIT
!!
!!##SEE ALSO
!!    attr_mode(3f), attr_update(3f)
function attr_scalar(string,reset) result (expanded)
character(len=*),intent(in)  :: string
logical,intent(in),optional  :: reset
logical                      :: clear_at_end
character(len=:),allocatable :: padded
character(len=:),allocatable :: expanded
character(len=:),allocatable :: name
integer                      :: i
integer                      :: ii
integer                      :: maxlen
integer                      :: place
   if(present(reset))then
      clear_at_end=reset
   else
      clear_at_end=.true.
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
            call locate(keywords,name,place)

            if(mode.eq.'plain')then
               expanded=expanded//get(name)
            elseif(place.le.0)then     ! unknown name; print what you found
               expanded=expanded//padded(i:i+ii)
               maxlen=maxlen-ii-1
            else
               expanded=expanded//get(name)
            endif
            i=ii+i+1
         endif
      case default
         expanded=expanded//padded(i:i)
         i=i+1
      end select
      if(i >= maxlen+1)exit
   enddo
   if( (index(expanded,esc).ne.0).and.(clear_at_end))then
      if((mode.ne.'raw').and.(mode.ne.'plain'))then
         expanded=expanded//CODE_RESET                                   ! Clear all styles
      endif
   endif
   expanded=expanded
end function attr_scalar

function attr_matrix(strings,reset,chars) result (expanded)
character(len=*),intent(in)  :: strings(:)
logical,intent(in),optional  :: reset
integer,intent(in),optional  :: chars
character(len=:),allocatable :: expanded(:)
   ! gfortran does not return allocatable array from a function properly, but works with subroutine
   call kludge_bug(strings,reset,chars,expanded)
end function attr_matrix

subroutine kludge_bug(strings,reset,chars,expanded)
character(len=*),intent(in)  :: strings(:)
logical,intent(in),optional  :: reset
integer,intent(in),optional  :: chars
character(len=:),allocatable :: expanded(:)
integer                      :: width
character(len=:),allocatable :: hold
integer                      :: i
integer                      :: right
integer                      :: len_local

allocate(character(len=0) :: expanded(0))
if(present(chars))then
   right=chars
else
   right=len(strings)
endif

if(.not.allocated(mode))then  ! set substitution mode
   mode='color' ! 'color'|'raw'|'plain'
   call vt102()
endif

do i=1,size(strings)

   if(mode.eq.'color')then
      mode='plain'
      len_local=len(attr_scalar(strings(i)))
      hold=trim(strings(i))//repeat(' ',max(0,right-len_local))
      mode='color'
   else
      hold=strings(i)
   endif

   hold=trim(attr_scalar(hold,reset=reset))
   width=max(len(hold),len(expanded))
   expanded=[character(len=width) :: expanded,hold]
enddo

end subroutine kludge_bug

function attr_scalar_width(string,reset,chars) result (expanded)
character(len=*),intent(in)  :: string
logical,intent(in),optional  :: reset
integer,intent(in)           :: chars
character(len=:),allocatable :: expanded_arr(:)
character(len=:),allocatable :: expanded
   expanded_arr=attr_matrix([string],reset,chars)
   expanded=expanded_arr(1)
end function attr_scalar_width

subroutine vt102()
! create a dictionary with character keywords, values, and value lengths
! using the routines for maintaining a list

   call wipe_dictionary()
   ! insert and replace entries
   call attr_update('bold',bold)
   call attr_update('/bold',unbold)
   call attr_update('bo',bold)
   call attr_update('/bo',unbold)
   call attr_update('italic',italic)
   call attr_update('/italic',unitalic)
   call attr_update('it',italic)
   call attr_update('/it',unitalic)
   call attr_update('inverse',inverse)
   call attr_update('/inverse',uninverse)
   call attr_update('in',inverse)
   call attr_update('/in',uninverse)
   call attr_update('underline',underline)
   call attr_update('/underline',ununderline)
   call attr_update('un',underline)
   call attr_update('/un',ununderline)
   call attr_update('ul',underline)
   call attr_update('/ul',ununderline)

   call attr_update('bell',BELL)
   call attr_update('nul', nul )
   call attr_update('bel', bel )
   call attr_update('bs', bs )
   call attr_update('ht', ht )
   call attr_update('lf', lf )
   call attr_update('vt', vt )
   call attr_update('ff', ff )
   call attr_update('cr', cr )
   call attr_update('so', so )
   call attr_update('si', si )
   call attr_update('can', can )
   call attr_update('sub', sub )
   call attr_update('esc', esc )
   call attr_update('escape',esc)
   call attr_update('del', del )

   call attr_update('save',esc//'7')
   call attr_update('DECSC',esc//'7')
   call attr_update('restore',esc//'8')
   call attr_update('DECRC',esc//'8')
   call attr_update('CSI',esc//'[')

   call attr_update('clear',clear)
   call attr_update('reset',reset)

   call attr_update('gt','>','>')
   call attr_update('lt','<','<')

   ! foreground colors
   call attr_update('r',fg_red)
       call attr_update('/r',fg_default)
       call attr_update('red',fg_red)
       call attr_update('/red',fg_default)
       call attr_update('fg_red',fg_red)
       call attr_update('/fg_red',fg_default)
   call attr_update('c',fg_cyan)
       call attr_update('/c',fg_default)
       call attr_update('cyan',fg_cyan)
       call attr_update('/cyan',fg_default)
       call attr_update('fg_cyan',fg_cyan)
       call attr_update('/fg_cyan',fg_default)
   call attr_update('m',fg_magenta)
       call attr_update('/m',fg_default)
       call attr_update('magenta',fg_magenta)
       call attr_update('/magenta',fg_default)
       call attr_update('fg_magenta',fg_magenta)
       call attr_update('/fg_magenta',fg_default)
   call attr_update('b',fg_blue)
       call attr_update('/b',fg_default)
       call attr_update('blue',fg_blue)
       call attr_update('fg_blue',fg_blue)
       call attr_update('/fg_blue',fg_default)
   call attr_update('g',fg_green)
       call attr_update('/g',fg_default)
       call attr_update('green',fg_green)
       call attr_update('/green',fg_default)
       call attr_update('fg_green',fg_green)
       call attr_update('/fg_green',fg_default)
   call attr_update('y',fg_yellow)
       call attr_update('/y',fg_default)
       call attr_update('yellow',fg_yellow)
       call attr_update('/yellow',fg_default)
       call attr_update('fg_yellow',fg_yellow)
       call attr_update('/fg_yellow',fg_default)
   call attr_update('w',fg_white)
       call attr_update('/w',fg_default)
       call attr_update('white',fg_white)
       call attr_update('/white',fg_default)
       call attr_update('fg_white',fg_white)
       call attr_update('/fg_white',fg_default)
   call attr_update('e',fg_ebony)
       call attr_update('/e',fg_default)
       call attr_update('ebony',fg_ebony)
       call attr_update('/ebony',fg_default)
       call attr_update('fg_ebony',fg_ebony)
       call attr_update('/fg_ebony',fg_default)
   call attr_update('x',fg_ebony)
       call attr_update('/x',fg_default)
       call attr_update('black',fg_ebony)
       call attr_update('/black',fg_default)
       call attr_update('fg_black',fg_ebony)
       call attr_update('/fg_black',fg_default)

   ! background colors
   call attr_update('R',bg_red)
       call attr_update('/R',bg_default)
       call attr_update('RED',bg_red)
       call attr_update('/RED',bg_default)
       call attr_update('bg_red',bg_red)
       call attr_update('/bg_red',bg_default)
   call attr_update('C',bg_cyan)
       call attr_update('/C',bg_default)
       call attr_update('CYAN',bg_cyan)
       call attr_update('/CYAN',bg_default)
       call attr_update('bg_cyan',bg_cyan)
       call attr_update('/bg_cyan',bg_default)
   call attr_update('M',bg_magenta)
       call attr_update('/M',bg_default)
       call attr_update('MAGENTA',bg_magenta)
       call attr_update('/MAGENTA',bg_default)
       call attr_update('bg_magenta',bg_magenta)
       call attr_update('/bg_magenta',bg_default)
   call attr_update('B',bg_blue)
       call attr_update('/B',bg_default)
       call attr_update('BLUE',bg_blue)
       call attr_update('/BLUE',bg_default)
       call attr_update('bg_blue',bg_blue)
       call attr_update('/bg_blue',bg_default)
   call attr_update('G',bg_green)
       call attr_update('/G',bg_default)
       call attr_update('GREEN',bg_green)
       call attr_update('/GREEN',bg_default)
       call attr_update('bg_green',bg_green)
       call attr_update('/bg_green',bg_default)
   call attr_update('Y',bg_yellow)
       call attr_update('/Y',bg_default)
       call attr_update('YELLOW',bg_yellow)
       call attr_update('/YELLOW',bg_default)
       call attr_update('bg_yellow',bg_yellow)
       call attr_update('/bg_yellow',bg_default)
   call attr_update('W',bg_white)
       call attr_update('/W',bg_default)
       call attr_update('WHITE',bg_white)
       call attr_update('/WHITE',bg_default)
       call attr_update('bg_white',bg_white)
       call attr_update('/bg_white',bg_default)
   call attr_update('E',bg_ebony)
       call attr_update('/E',bg_default)
       call attr_update('EBONY',bg_ebony)
       call attr_update('/EBONY',bg_default)
       call attr_update('bg_ebony',bg_ebony)
       call attr_update('/bg_ebony',bg_default)
   call attr_update('X',bg_ebony)
       call attr_update('/X',bg_default)
       call attr_update('BLACK',bg_ebony)
       call attr_update('/BLACK',bg_default)
       call attr_update('bg_black',bg_ebony)
       call attr_update('/bg_black',bg_default)

   ! compound
   call attr_update('ERROR',fg_red//bold//bg_ebony     //':error:  '//bg_default//fg_default,':error:')
   call attr_update('WARNING',fg_yellow//bold//bg_ebony//':warning:'//bg_default//fg_default,':warning:')
   call attr_update('INFO',fg_green//bold//bg_ebony    //':info:   '//bg_default//fg_default,':info:')

end subroutine vt102
!>
!! !>
!!##NAME
!!    attr_mode(3f) - [M_attr] select processing mode for output from attr(3f)
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!     subroutine attr_mode(manner)
!!
!!        character(len=*),intent(in) :: manner
!!
!!##DESCRIPTION
!!    Turn off the generation of strings associated with the HTML keywords
!!    in the string generated by the attr(3f) function, or display the
!!    text in raw mode as it was passed to attr(3f) or return to ANSI
!!    escape control sequence generation.
!!
!!##OPTIONS
!!    MANNER  The current manners or modes supported via the attr_mode(3f)
!!             procedure are
!!
!!         plain          suppress the output associated with keywords
!!         color(default) commonly supported escape sequences
!!         raw            echo the input to attr(3f) as its output
!!         reload         restore original keyword meanings deleted or
!!                        replaced by calls to attr_update(3f).
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!     program demo_attr_mode
!!     use M_attr, only : attr, attr_mode
!!     implicit none
!!     character(len=:),allocatable :: lines(:)
!!     character(len=:),allocatable :: outlines(:)
!!     integer :: i
!!        lines=[character(len=110):: &
!!        &'<M><y>',&
!!        &'<M><y>  Suffice it to say that black and white are also colors',&
!!        &'<M><y>  for their simultaneous contrast is as striking as that ',&
!!        &'<M><y>  of green and red, for instance. &
!!        & --- <y><bo>Vincent van Gogh</bo></y>',&
!!        &' ']
!!
!!        outlines=attr(lines,chars=57)
!!        write(*,'(a)')(trim(outlines(i)),i=1,size(outlines))
!!
!!        call attr_mode(manner='plain') ! write as plain text
!!        write(*,'(a)')attr(lines)
!!
!!        call attr_mode(manner='raw')   ! write as-is
!!        write(*,'(a)')attr(lines)
!!
!!        call attr_mode(manner='ansi')  ! return to default mode
!!
!!     end program demo_attr_mode
!!
!!##AUTHOR
!!    John S. Urban, 2021
!!
!!##LICENSE
!!    MIT
subroutine attr_mode(manner)
character(len=*),intent(in) :: manner
integer                     :: i
   if(.not.allocated(mode))then  ! set substitution mode
      mode='color'
      call vt102()
   endif
   select case(manner)
   case('vt102','ANSI','ansi','color','COLOR')
      mode='color'
   case('reload','default','defaults','')
      call vt102()
      mode='color'
   case('raw')
      mode='raw'
   case('dump')  ! dump dictionary for debugging
      if(allocated(keywords))then
         if(size(keywords).gt.0)then
            write(stderr,'(*(a,t30,a))')'KEYWORD','VALUE'
            write(stderr,'(*(a,t30,2("[",a,"]"),/))')(trim(keywords(i)),values(i),mono_values(i),i=1,size(keywords))
         endif
      endif
   case('dummy','plain','text')
      mode='plain'
   case default
      write(*,*)'unknown manner. Try color|raw|plain'
      mode='color'
   end select
end subroutine attr_mode

subroutine wipe_dictionary()
   if(allocated(keywords))deallocate(keywords)
   allocate(character(len=0) :: keywords(0))
   if(allocated(values))deallocate(values)
   allocate(character(len=0) :: values(0))
   if(allocated(mono_values))deallocate(mono_values)
   allocate(character(len=0) :: mono_values(0))
end subroutine wipe_dictionary

!>
!! !>
!!##NAME
!!    attr_update(3f) - [M_attr] update internal dictionary given keyword and value
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine attr_update(key,val)
!!
!!     character(len=*),intent(in)           :: key
!!     character(len=*),intent(in),optional  :: val
!!     character(len=*),intent(in),optional  :: mono_val
!!
!!##DESCRIPTION
!!    Update internal dictionary in M_attr(3fm) module.
!!
!!##OPTIONS
!!    key       name of keyword to add, replace, or delete from dictionary
!!    val       if present add or replace value associated with keyword. If
!!              not present remove keyword entry from dictionary.
!!    mono_val  if present add or replace second value associated with
!!              keyword used for plain text mode.
!!              Must only be specified if VAL is also specified.
!!
!!##KEYWORDS
!!    The following keywords are defined by default
!!
!!    colors:
!!
!!      r,red     c,cyan     w,white
!!      g,green   m,magenta  e,ebony
!!      b,blue    y,yellow
!!
!!    If the color keywords are capitalized they control the text background
!!    instead of the text color.
!!
!!    attributes:
!!
!!      ul,underline
!!      it,italics (often produces inverse colors on many devices
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!      program demo_update
!!      use M_attr, only : attr, attr_update
!!         write(*,'(a)') attr('<clear>TEST CUSTOMIZATIONS:')
!!         ! add custom keywords
!!         call attr_update('blink',char(27)//'[5m')
!!         call attr_update('/blink',char(27)//'[25m')
!!         write(*,*)
!!         write(*,'(a)') attr('<blink>Items for Friday</blink>')
!!         call attr_update('ouch',attr( &
!!         ' <R><bo><w>BIG mistake!</R></w> '))
!!         write(*,*)
!!         write(*,'(a)') attr('<ouch> Did not see that coming.')
!!         write(*,*)
!!         write(*,'(a)') attr( &
!!         'ORIGINALLY: <r>Apple</r>, <b>Sky</b>, <g>Grass</g>')
!!         ! delete
!!         call attr_update('r')
!!         call attr_update('/r')
!!         ! replace (or create)
!!         call attr_update('b','<<<<')
!!         call attr_update('/b','>>>>')
!!         write(*,*)
!!         write(*,'(a)') attr( &
!!         'CUSTOMIZED: <r>Apple</r>, <b>Sky</b>, <g>Grass</g>')
!!      end program demo_update
!!
!!##AUTHOR
!!    John S. Urban, 2021
!!
!!##LICENSE
!!    MIT
subroutine attr_update(key,valin,mono_valin)
character(len=*),intent(in)           :: key
character(len=*),intent(in),optional  :: valin
character(len=*),intent(in),optional  :: mono_valin
integer                               :: place
character(len=:),allocatable          :: val
character(len=:),allocatable          :: mono_val

if(.not.allocated(mode))then  ! set substitution mode
   mode='color' ! 'color'|'raw'|'plain'
   call vt102()
endif

if(present(mono_valin))then
   mono_val=mono_valin
else
   mono_val=''
endif

if(present(valin))then
   val=valin
   ! find where string is or should be
   call locate(keywords,key,place)
   ! if string was not found insert it
   if(place.lt.1)then
      call insert(keywords,key,iabs(place))
      call insert(values,val,iabs(place))
      call insert(mono_values,mono_val,iabs(place))
   else
      call replace(values,val,place)
      call replace(mono_values,mono_val,place)
   endif
else
   call locate(keywords,key,place)
   if(place.gt.0)then
      call remove(keywords,place)
      call remove(values,place)
      call remove(mono_values,place)
   endif
endif
end subroutine attr_update

function get(key) result(valout)
character(len=*),intent(in)   :: key
character(len=:),allocatable  :: valout
integer                       :: place
   ! find where string is or should be
   call locate(keywords,key,place)
   if(place.lt.1)then
      valout=''
   else
      if(mode.eq.'plain')then
         valout=trim(mono_values(place))
      else
         valout=trim(values(place))
      endif
   endif
end function get

subroutine locate(list,value,place,ier,errmsg)
character(len=*),intent(in)             :: value
integer,intent(out)                     :: place
character(len=:),allocatable            :: list(:)
integer,intent(out),optional            :: ier
character(len=*),intent(out),optional   :: errmsg
integer                                 :: i
character(len=:),allocatable            :: message
integer                                 :: arraysize
integer                                 :: maxtry
integer                                 :: imin, imax
integer                                 :: error
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   arraysize=size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry
      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif
      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif
      place=(imax+imin)/2
      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif
   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',trim(value)//' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
end subroutine locate

subroutine remove(list,place)
character(len=:),allocatable :: list(:)
integer,intent(in)           :: place
integer                      :: ii, end
   if(.not.allocated(list))then
      list=[character(len=2) :: ]
   endif
   ii=len(list)
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[character(len=ii) :: list(:place-1) ]
   else
      list=[character(len=ii) :: list(:place-1), list(place+1:) ]
   endif
end subroutine remove

subroutine replace(list,value,place)
character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: tlen
integer                      :: end
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   tlen=len_trim(value)
   end=size(list)
   if(place.lt.0.or.place.gt.end)then
           write(stderr,*)'*replace* error: index out of range. end=',end,' index=',place
   elseif(len_trim(value).le.len(list))then
      list(place)=value
   else  ! increase length of variable
      ii=max(tlen,len(list))
      kludge=[character(len=ii) :: list ]
      list=kludge
      list(place)=value
   endif
end subroutine replace

subroutine insert(list,value,place)
character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: end
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   ii=max(len_trim(value),len(list),2)
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[character(len=ii) :: value ]
   elseif(place.eq.1)then                                    ! put in front of array
      kludge=[character(len=ii) :: value, list]
      list=kludge
   elseif(place.gt.end)then                                  ! put at end of array
      kludge=[character(len=ii) :: list, value ]
      list=kludge
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      kludge=[character(len=ii) :: list(:place-1), value,list(place:) ]
      list=kludge
   else                                                      ! index out of range
      write(stderr,*)'*insert* error: index out of range. end=',end,' index=',place,' value=',value
   endif
end subroutine insert
!>
!! !>
!!##NAME
!!    alert(3f) - [M_attr] print messages using a standard format including time and program name
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!     subroutine alert(message,g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj)
!!
!!        character(len=*),intent(in),optional :: type
!!        character(len=*),intent(in),optional :: message
!!        class(*),intent(in),optional :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9, &
!!                                      & ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!
!!##DESCRIPTION
!!    Display a message prefixed with a timestamp and the name
!!    of the calling program when the TYPE is specified as any
!!    of 'error','warn', or 'info'.
!!
!!    It also allows the keywords
!!    <ARG0>,<TZ>,<YE>,<MO>,<DA>,<HR>,<MI>,<SE>,<MS> to be used in the
!!    message (which is passed to ATTR(3f)).
!!
!!    Note that time stamp keywords will only be updated when using ALERT(3f)
!!    and will only be displayed in color mode!
!!
!!##OPTIONS
!!    TYPE     if present and one of 'warn','message','info', or 'debug'  a predefined
!!             message is written to stderr of the form
!!
!!              : <HR>:<MI>:<SE>.<MS> : (<ARG0>) : TYPE -> message
!!
!!    MESSAGE  the user-supplied message to display via a call to ATTR(3f)
!!
!!    g[0-9a-j]   optional values to print after the message. May
!!                be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION,
!!                COMPLEX, or CHARACTER.
!!
!!    if no parameters are supplied the macros are updated but no output is generated.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!       program demo_alert
!!       use M_attr, only : alert, attr, attr_mode
!!       implicit none
!!       real X
!!          call attr_mode(manner='plain')
!!          call attr_mode(manner='color')
!!          call alert("error", "Say you didn't!")
!!          call alert("warn",  "I wouldn't if I were you, Will Robinson.")
!!          call alert("info",  "I fixed that for you, but it was a bad idea.")
!!          call alert("debug", "Who knows what is happening now?.")
!!          call alert("???    ",  "not today you don't")
!!          ! call to just update the macros
!!          call alert()
!!          ! conventional call to ATTR(3f) using the ALERT(3f)-defined macros
!!          write(*,*)attr('<bo>The year was <g><YE></g>, the month was <g><MO></g>')
!!          ! optional arguments
!!          X=211.3
!!          call alert('error','allowed range of X is 0 <lt> X <lt> 100, X=<r>',X)
!!          ! up to twenty values are allowed of intrinsic type
!!          call alert('info','values are<g>',10,234.567,cmplx(11.0,22.0),123.456d0,'</g>today')
!!       end program demo_alert
!!
!!   Results:
!!
!!     00:38:30.566 : (demo_alert) : error    -> Say you didn't!
!!     00:38:30.567 : (demo_alert) : warning  -> I wouldn't if I were you, Will Robinson.
!!     00:38:30.567 : (demo_alert) : info     -> I fixed that for you, but it was a bad idea.
!!     00:38:30.567 : (demo_alert) : debug    -> Who knows what is happening now?.
!!     00:38:30.567 : (demo_alert) : ???      -> not today you don't
!!     00:38:30.567 : (demo_alert) : error    -> allowed range of X is 0  X  100, X= 211.300003
!!     00:38:30.567 : (demo_alert) : info     -> values are 10 234.567001 (11.0000000,22.0000000) 123.45600000000000 today
!!
!!
!!
!!##AUTHOR
!!    John S. Urban, 2021
!!
!!##LICENSE
!!    MIT
subroutine alert(type,message,g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj)
! TODO: could add a warning level to ignore info, or info|warning, or all
implicit none
character(len=*),intent(in),optional :: type
character(len=*),intent(in),optional :: message
class(*),intent(in),optional  :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9
class(*),intent(in),optional  :: ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
character(len=8)      :: dt
character(len=10)     :: tm
character(len=5)      :: zone
integer,dimension(8)  :: values
character(len=4096)   :: arg0
character(len=:),allocatable :: new_message
character(len=:),allocatable :: other
logical :: printme
   call date_and_time(dt,tm,zone,values)
   call attr_update('YE',dt(1:4),dt(1:4))
   call attr_update('MO',dt(5:6),dt(5:6))
   call attr_update('DA',dt(7:8),dt(7:8))
   call attr_update('HR',tm(1:2),tm(1:2))
   call attr_update('MI',tm(3:4),tm(3:4))
   call attr_update('SE',tm(5:6),tm(5:6))
   call attr_update('MS',tm(8:10),tm(8:10))
   call attr_update('TZ',zone,zone)
   call get_command_argument(0,arg0)
   if(index(arg0,'/').ne.0) arg0=arg0(index(arg0,'/',back=.true.)+1:)
   if(index(arg0,'\').ne.0) arg0=arg0(index(arg0,'\',back=.true.)+1:)
   call attr_update('ARG0',arg0,arg0)
   printme=.true.
   if(present(type))then
      new_message= ' <b>'//tm(1:2)//':'//tm(3:4)//':'//tm(5:6)//'.'//tm(8:10)//'</b> : ('//trim(arg0)//') : '
      other=message//' '//str(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj)
      select case(type)

      case('warn','WARN','warning','WARNING')
       new_message= new_message//'<EBONY><bo><y>warning </y></EBONY> -<gt> '
       printme=alert_warn

      case('info','INFO','information','INFORMATION')
       new_message= new_message//'<EBONY><bo><g>info    </g></EBONY> -<gt> '
       printme=alert_info

      case('error','ERROR')
       new_message= new_message//'<EBONY><bo><r>error   </r></EBONY> -<gt> '
       printme=alert_error

      case('debug','DEBUG')
       new_message= new_message//'<EBONY><white><bo>debug   </white></EBONY> -<gt> '
       printme=alert_debug

      case default
       new_message= new_message//'<EBONY><bo><c>'//type//' </c></EBONY> -<gt> '
       printme=alert_other

      end select
    if(printme)then
       write(alert_unit,'(a)')attr(trim(new_message//other))
    endif

   elseif(present(message))then
    write(alert_unit,'(a)')attr(trim(other))
   endif
end subroutine alert
!>
!!##NAME
!!    str(3f) - [M_attr] converts any standard scalar type to a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    Syntax:
!!
!!      function str(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,&
!!      & ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,sep)
!!      class(*),intent(in),optional  :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9
!!      class(*),intent(in),optional  :: ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!      character(len=*),intent(in),optional :: sep
!!      character,len=(:),allocatable :: str
!!
!!##DESCRIPTION
!!    str(3f) builds a space-separated string from up to twenty scalar values.
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
!!    sep         separator string used between values. Defaults to a space.
!!
!!##RETURNS
!!    str     description to print
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_msg
!!    use M_attr, only : alert
!!    end program demo_msg
!!
!!   Output
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function msg_scalar(generic0, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9, &
                  & generica, genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj, &
                  & sep)
implicit none

! ident_1="@(#)M_attr::msg_scalar(3fp): writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic0, generic1, generic2, generic3, generic4
class(*),intent(in),optional  :: generic5, generic6, generic7, generic8, generic9
class(*),intent(in),optional  :: generica, genericb, genericc, genericd, generice
class(*),intent(in),optional  :: genericf, genericg, generich, generici, genericj
character(len=:),allocatable  :: msg_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
   if(present(sep))then
      increment=len(sep)+1
      sep_local=sep
   else
      increment=2
      sep_local=' '
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
   msg_scalar=trim(line)
contains

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
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic

end function msg_scalar
function msg_one(generic0,generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,&
               & generica,genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj,&
               & sep)
implicit none

! ident_2="@(#)M_attr::msg_one(3fp): writes a message to a string composed of any standard one dimensional types"

class(*),intent(in)           :: generic0(:)
class(*),intent(in),optional  :: generic1(:), generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
class(*),intent(in),optional  :: generica(:), genericb(:), genericc(:), genericd(:), generice(:)
class(*),intent(in),optional  :: genericf(:), genericg(:), generich(:), generici(:), genericj(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_one
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
   msg_one=trim(line)
contains

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
   istart=len_trim(line)+increment+1
   line=trim(line)//']'//sep_local
end subroutine print_generic

end function msg_one
end module M_attr
