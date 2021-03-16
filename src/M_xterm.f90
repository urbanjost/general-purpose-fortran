










!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     M_xterm(3fm) - [M_xterm] send escape sequences to an xterm(1) window that control VT102 emulator attributes
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!     use M_xterm, only : xterm_font
!!     use M_xterm, only : xterm_colors, xterm_pencolor
!!     use M_xterm, only : xterm_geometry, xterm_width, xterm_position
!!     use M_xterm, only : xterm_clear, xterm_keywords, xterm_labels
!!
!!     use M_xterm, only : xterm_get_geometry, xterm_get_font, xterm_get_position
!!     use M_xterm, only : xterm_get_iconstate, xterm_get_colors, xterm_get_pencolor
!!     use M_xterm, only : xterm_xrdb, xterm_occupancy
!!##DESCRIPTION
!!    The xterm(1) terminal emulator emulates an extended VT102 terminal on
!!    X11 Windows platforms. This means a number of strings beginning with
!!    an escape character can be used to clear the screen, toggle between
!!    80 and 132 column mode, position the cursor, and perform other command
!!    sequences that allow screen-based utilities to operate.
!!
!!    Since basic terminal control libraries such as ncurses(3c) already
!!    exist for basic terminal screen formatting, The M_xterm(3fm) module
!!    specifically supports the xterm(1) extensions that control the X11
!!    window attributes such as window size, font, and colors:
!!
!!      xterm_colors(type,color)     set xterm colors
!!      xterm_font(fontname)         set xterm fonts
!!      xterm_geometry(rows,cols)    set terminal size
!!      xterm_position(right,downs)  set terminal position
!!      xterm_width(iwidth)          set width to 80 or 132 characters
!!      xterm_clear()                clear screen
!!      xterm_keywords(keyword)      X11 Window Manager hints (lower,raise,
!!                                   maximize,restore,...) and modes
!!      xterm_labels(keyword)        X11 Window Manager label hints
!!
!!    An additional set of routines sends escape sequences to stdout that
!!    query the current state of the xterm(1). Then, with the terminal set
!!    to raw mode, they read back the response. In this implementation that
!!    means the command may not work in a basic pipe or have I/O redirected
!!    on some systems.
!!
!!       xterm_get_iconstate         Query whether window is iconified
!!       xterm_get_geometry          Get size as number of rows and columns
!!                                   or characters.
!!       xterm_get_position          Get position of upper left corner of
!!                                   window relative to upper left corner
!!                                   of display in pixels
!!       xterm_get_font              Get name of current font
!!       xterm_get_colors            Get colors of various terminal window
!!                                   attributes.
!!       xterm_get_pencolor          Get description of a terminal color number
!!       xterm_xrdb                  Write out current terminal settings as X11
!!                                   Windows resources
!!       xterm_occupancy             Move window to specified virtual display
!!                                   where supported.
!!
!!    For all these routines to work with some xterm versions, you may have to
!!    set the following X11 resources in your ~.Xresources file **before**
!!    starting the xterm(1) window. You may also use the xrdb(1) command.
!!    For example
!!
!!       xrdb --merge <<\end_of_file
!!       XTerm*VT100.allowWindowOps: true
!!       XTerm*VT100.allowTitleOps: true
!!       XTerm*VT100.allowFontOps: true
!!       end_of_file
!!
!!   For a running xterm(1), you may use the "VT Fonts" menu to enable "Window Ops","Title Ops","Font Ops",
!!   and "Color Ops" if they are no enabled. If the "VT Fonts" menu is not visible in the upper left corner
!!   of the xterm(1) display, ctrl-Mouse3 in the main terminal display will make the menu visible.
!!
!!   An example program that can easily be called from scripts and shell aliases called setxt(1) is included
!!   in the GPF (General Purpose Fortran) distribution that this module is a part of.
!!
!!##EXAMPLE
!!
!!   Sample usage:
!!
!!     program demo_M_xterm
!!     use M_xterm, only : xterm_colors, xterm_font
!!     use M_xterm, only : xterm_geometry, xterm_width, xterm_position
!!     use M_xterm, only : xterm_clear, xterm_keywords, xterm_labels
!!     call xterm_colors('background','black')
!!     call xterm_colors('foreground','white')
!!     call xterm_colors('cursor','red')
!!     call xterm_colors('mouse_foreground','red')
!!     call xterm_colors('mouse_background','white')
!!     call xterm_font('5')
!!     call xterm_geometry(cols=132,rows=36)
!!     call xterm_position(right=100,down=200)
!!     call xterm_keywords('raise')
!!     end program demo_M_xterm
!!
!!##SEE ALSO
!!    xlsfonts(1x),
!!    clear(1), ncurses(3c), stty (1), tabs (1),
!!    terminfo(5), tput (1), tset (1), tty (1)
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
module M_xterm
use,intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
use           :: M_system,        only : system_getenv
implicit none
private

! ident_1="@(#)M_xterm(3fm): send xterm control sequences. John S. Urban, 19910719"

character(len=1),parameter :: esc=char(27)
character(len=1),parameter :: bel=char(7)
character(len=1),parameter :: etx=char(3)
character(len=2),parameter :: CSI=char(27)//'['

public xterm_colors
public xterm_font
public xterm_labels
public xterm_geometry
public xterm_position
public xterm_keywords
public xterm_width
public xterm_clear
public xterm_pencolor

public xterm_get_geometry
public xterm_get_position
public xterm_get_font
public xterm_get_iconstate
public xterm_get_colors
public xterm_get_pencolor
public xterm_xrdb
public xterm_occupancy

public test_suite_M_xterm

integer :: G_io=0

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_xrdb(3f) - [M_xterm] write current xterm(1) window attributes as X11 Windows resources
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine xterm_xrdb(name)
!!    character(len=*),intent(in):: color
!!
!!##DESCRIPTION
!!
!!    Writes out current xterm(1) terminal settings as X11 Windows resources with the
!!    specified name so that subsequent xterm(1) window can easily be created with the
!!    same attributes.
!!
!!    Append the output to ~/.Xresources to make the named set of attributes permanently
!!    available from nodes that share the file, or use the xrdb(1) command to make the
!!    named resources easily available until the X11 Windows client is restarted.
!!
!!##OPTIONS
!!    name     name to give the X11 resources. To make this the default for any xterm(1)
!!             command use the name "XTerm".
!!
!!##EXAMPLE
!!
!!
!!   A sample program:
!!
!!     program demo_xterm_xrdb
!!     use M_xterm, only : xterm_xrdb
!!     character(len=:),allocatable :: cache
!!        call xterm_xrdb('FAVORITES')
!!     end program demo_xterm_xrdb
!!
!!   Sample output:
!!
!!     FAVORITES*VT100.allowWindowOps:        true
!!     FAVORITES*VT100.allowTitleOps:         true
!!     FAVORITES*VT100.allowFontOps:          true
!!     FAVORITES*saveLines:                   10000
!!     FAVORITES*c132:                        on
!!     FAVORITES*scrollBar:                   true
!!     FAVORITES*RightScrollBar:              true
!!     FAVORITES*scrollbar*background:        black
!!     FAVORITES*scrollbar*foreground:        red
!!     FAVORITES.VT100.scrollbar.foreground:  red
!!     FAVORITES*scrollbar*thickness:         13
!!     FAVORITES*XTerm*internalBorder:         2
!!     FAVORITES*VT100.background:  rgb:0000/8b8b/0000
!!     FAVORITES*VT100.foreground:  rgb:ffff/ffff/ffff
!!     FAVORITES*VT100.cursorColor: rgb:ffff/0000/0000
!!     FAVORITES*VT100.geometry: 80x24+0+55
!!     FAVORITES*windowName:    FAVORITES
!!     FAVORITES*iconName:      FAVORITES
!!     FAVORITES*VT100*font: -misc-fixed-medium-r-normal--20-200-75-75-c-100-iso10646-1
!!
!!   Sample usage:
!!
!!     # load resources into X11 client memory
!!     ./demo_xterm_xrdb|xrdb -merge
!!     # or put them permanently into you X11 resource file
!!     ./demo_xterm_xrdb >> ~/.Xresources
!!     # then
!!     # launch an xterm(1) using the resource specifications
!!     xterm -name FAVORITES
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine xterm_xrdb(name)
implicit none
character(len=*),intent(in)  :: name
integer                      :: irows, icols, iright, idown
integer                      :: i
character(len=80)            :: geometry

   write(*,'(a)')NAME//'*VT100.allowWindowOps:        true'
   write(*,'(a)')NAME//'*VT100.allowTitleOps:         true'
   write(*,'(a)')NAME//'*VT100.allowFontOps:          true'
   write(*,'(a)')NAME//'*saveLines:                   10000'
   write(*,'(a)')NAME//'*c132:                        on'
   write(*,'(a)')NAME//'*scrollBar:                   true'
   write(*,'(a)')NAME//'*RightScrollBar:              true'
   write(*,'(a)')NAME//'*scrollbar*background:        black'
   write(*,'(a)')NAME//'*scrollbar*foreground:        red'
   write(*,'(a)')NAME//'.VT100.scrollbar.foreground:  red'
   write(*,'(a)')NAME//'*scrollbar*thickness:         13'
   write(*,'(a)')NAME//'XTerm*internalBorder:         2'

   write(*,'(a)')NAME//'*VT100.background:  '//xterm_get_colors('background')
   write(*,'(a)')NAME//'*VT100.foreground:  '//xterm_get_colors('foreground')
   write(*,'(a)')NAME//'*VT100.cursorColor: '//xterm_get_colors('cursor')

   call xterm_get_geometry(irows,icols)
   call xterm_get_position(iright,idown)
   write(geometry,'(i0,"x",i0,"+",i0,"+",i0)')icols,irows,iright,idown
   write(*,'(a)')NAME//'*VT100.geometry: '//trim(geometry)

   write(*,'(a)')NAME//'*windowName: '//NAME
   write(*,'(a)')NAME//'*iconName:   '//NAME
   write(*,'(a)')NAME//'*VT100.font: '//xterm_get_font()

   do i=0,15
      write(*,'(a,i0,": ",a)')NAME//'*VT100.color:',i,xterm_get_pencolor(i)
   enddo

end subroutine xterm_xrdb
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_occupancy(3f) - [M_xterm] try to move xterm(1) to specified virtual display
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine xterm_occupancy(windowname)
!!    character(len=*)    :: windowname
!!
!!##DESCRIPTION
!!
!!    Move an xterm(1) window to the specified virtual display, if the
!!    X11 Windows window manager supports the property WM_OCCUPATION.
!!
!!    If your window manager supports multiple virtual displays by defining
!!    the property WM_OCCUPANCY (use xprop -id $WINDOWID to see) then you
!!    can move your windows to specific virtual displays; usually by number
!!    or by name. The name "all" is typically special and makes the window
!!    visible in all the virtual displays.
!!
!!    This works for the ctwm(1) window manager.
!!
!!    If your window manager has assigned the property WS_OCCUPATION as
!!    seen by entering
!!
!!       xprop -id $WINDOWID
!!
!!          WM_OCCUPATION(STRING) = "2"
!!          WM_OCCUPATION(STRING) = "Two"
!!
!!##OPTIONS
!!    windowname   The name and/or numeric name of the virtual display.
!!                 The name "all" is usually special and means all
!!                 virtual displays.
!!
!!##EXAMPLE
!!
!!
!!   A sample program:
!!
!!    program demo_xterm_occupancy
!!    use M_xterm, only : xterm_occupancy
!!    call xterm_occupancy("all")
!!    call xterm_occupancy("1")
!!    call xterm_occupancy("Project A")
!!    end program demo_xterm_occupancy
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine xterm_occupancy(windowname)
implicit none

! ident_2="@(#)M_xterm::xterm_occupancy(3f): move xterm(1) to specified virtual display, if supported"

character(len=*),intent(in)   :: windowname

   call set_G_io()
   write(G_io,'(a)')esc//']3;WM_OCCUPATION='//trim(windowname)//bel
   write(G_io,'(a)')esc//']3;WM_CURRENTWORKSPACE='//trim(windowname)//bel

end subroutine xterm_occupancy
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_get_pencolor(3f) - [M_xterm] query xterm(1) color by number
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function xterm_get_pencolor(pennum) result(color)
!!    integer,intent(in)  :: pennum
!!    character(len=*)    :: color
!!
!!##DESCRIPTION
!!
!!    Get the color of an xterm(1) color number.
!!
!!##OPTIONS
!!    pennum   which pen number to describe the color  of
!!
!!##RETURNS
!!    color  A string specifying the pen color in the
!!           form "rgb:RR/GG/BB"
!!
!!##EXAMPLE
!!
!!
!!   A sample program:
!!
!!    program demo_xterm_get_pencolor
!!    use M_xterm, only : xterm_get_pencolor
!!    character(len=:),allocatable :: cache
!!    do i=0,15
!!       cache=xterm_get_pencolor(i)
!!       write(*,'(i4.4,1x,a)')i,cache
!!    enddo
!!    end program demo_xterm_get_pencolor
!!
!!   Sample output:
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function xterm_get_pencolor(pennum) result(color)
use M_strings, only : v2s, split
implicit none

! ident_3="@(#)M_xterm::xterm_get_pencolor(3f): get description of color assigned to an xterm(1) color number"

integer,intent(in)            :: pennum
character(len=:),allocatable  :: color

character(len=:),allocatable  :: string
character(len=:),allocatable  :: array(:)

   string=RAWGET(esc//']4;'//v2s(pennum)//';?'//bel)
   CALL split(string,array,delimiters=' ;'//bel)
   if(size(array).ge.3)then
      color=trim(array(3))
   else
      color=''
   endif

end function xterm_get_pencolor
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_get_colors(3f) - [M_xterm] query xterm(1) colors
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function xterm_get_colors(type) result(color)
!!    character(len=*),intent(in) :: type
!!    character(len=*)            :: color
!!
!!##DESCRIPTION
!!
!!    Get the color of various xterm(1) window attributes.
!!
!!##OPTIONS
!!    type   which type of color to get
!!
!!       o "bg" , "background"
!!       o "fg" , "foreground"
!!       o "cr" , "cursor"
!!       o "mfg", "mouse_foreground"
!!       o "mbg", "mouse_background"
!!       o "hc" , "highlight_color"
!!       o "tfg", "tektronix_foreground"
!!       o "tbg", "textronix_background"
!!       o "tcr", "textronix_cursor"
!!
!!##RETURNS
!!    color  A string specifying a color. Value is
!!           of form "rgb:RR/GG/BB"
!!
!!##SEE ALSO
!!    showrgb(1) shows known named X11 Windows colors
!!
!!##EXAMPLE
!!
!!   A sample program:
!!
!!    program demo_xterm_get_colors
!!    use M_xterm, only : xterm_get_colors
!!    character(len=:),allocatable :: cache
!!
!!    cache=xterm_get_colors('background')
!!    write(*,'("BACKGROUND:",a)')cache
!!
!!    cache=xterm_get_colors('foreground')
!!    write(*,'("FOREGROUND:",a)')cache
!!
!!    cache=xterm_get_colors('cursor')
!!    write(*,'("CURSOR    :",a)')cache
!!
!!    cache=xterm_get_colors('highlight')
!!    write(*,'("HIGHLIGHT :",a)')cache
!!
!!    end program demo_xterm_get_colors
!!
!!   Sample output:
!!
!!    BACKGROUND:rgb:0000/0000/0000
!!    FOREGROUND:rgb:ffff/ffff/ffff
!!    CURSOR    :rgb:ffff/0000/0000
!!    HIGHLIGHT :rgb:ffff/0000/0000
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function xterm_get_colors(type) result(color)
use M_strings, only : isdigit, isspace, switch
use M_strings, only : split

! ident_4="@(#)M_xterm::xterm_get_colors(3f): set various xterm(1) window colors using escape sequences"

!   Esc]Ps;ColorBel
!   Ps = 1 0  -> Change VT100 text foreground color to Pt
!   Ps = 1 1  -> Change VT100 text background color to Pt
!   Ps = 1 2  -> Change text cursor color to Pt
!   Ps = 1 3  -> Change mouse foreground color to Pt
!   Ps = 1 4  -> Change mouse background color to Pt
!   Ps = 1 5  -> Change Tektronix foreground color to Pt
!   Ps = 1 6  -> Change Tektronix background color to Pt
!   Ps = 1 7  -> Change highlight color to Pt
!   Ps = 1 8  -> Change Tektronix cursor color to Pt
! If a "?" is given rather than a name or RGB specification,
! xterm replies with a control sequence of the same
! form which can be used to set the corresponding dynamic
! color.  Because more than one pair of color number and
! specification can be given in one control sequence, xterm
! can make more than one reply.
!
character(len=*),intent(in)    :: type
character(len=:),allocatable   :: color
character(len=2)               :: code
character(len=:),allocatable   :: string
character(len=:),allocatable   :: array(:)

   select case(type)
   case('bg','background')            ;  code='11'
   case('fg','foreground')            ;  code='10'
   case('cr','cursor')                ;  code='12'
   case('mfg','mouse_foreground')     ;  code='13'
   case('mbg','mouse_background')     ;  code='14'
   case('hc','highlight')             ;  code='17'
   case('tfg','tektronix_foreground') ;  code='15'
   case('tbg','textronix_background') ;  code='16'
   case('tcr','textronix_cursor')     ;  code='18'
   case default
      write(*,*)'*xterm_color* unknown type ',trim(type)
      color='unknown'
      return
   end select
   string=RAWGET(esc//']'//code//';?'//bel)
   CALL split(string,array,delimiters=' ;'//bel)
   if(size(array).ge.2)then
      color=trim(array(2))
   else
      color=''
   endif
end function xterm_get_colors
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_pencolor(3f) - [M_xterm] set xterm(1) color by number using escape sequences
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine xterm_pencolor(pennum,color)
!!    integer,intent(in)          :: pennum
!!    character(len=*),intent(in) :: color
!!
!!##DESCRIPTION
!!    Set the color of a pen of an xterm(1) window.
!!
!!##OPTIONS
!!    pennum   which pen color to set. Typically, allowed
!!             values are from 0 to at least 15.
!!
!!    color    A string specifying a color. Value may
!!             be a name or a HEX value string of the form
!!             #RRGGBB, or of the form rgb:RR/GG/BB
!!
!!##SEE ALSO
!!    showrgb(1) shows known named X11 Windows colors
!!
!!##EXAMPLE
!!
!!   A sample program:
!!
!!    program demo_xterm_pencolor
!!    use M_xterm, only : xterm_pencolor
!!    call xterm_pencolor(0,'gray')
!!    call xterm_pencolor(1,'rgb:000/fff/000')
!!    call xterm_pencolor(2,'#FF00FF')
!!    end program demo_xterm_pencolor
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine xterm_pencolor(pennum,color)
implicit none

! ident_5="@(#)M_xterm::xterm_pencolor(3f): set xterm(1) color by number using escape sequences"

integer,intent(in)          :: pennum
character(len=*),intent(in) :: color
   call set_G_io()
   ! multiple colors can be done at once
   ! ]4;0;red;1;green;2;blue;3;cyan
   write(G_io,'(a,i0,a)',advance='no') esc//']4;', pennum, ';'//trim(color)//bel
end subroutine xterm_pencolor
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_colors(3f) - [M_xterm] set xterm(1) colors
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine xterm_colors(type,color)
!!    character(len=*),intent(in) :: type
!!    character(len=*),intent(in) :: color
!!
!!##DESCRIPTION
!!
!!    Set the color of various xterm(1) window attributes.
!!
!!##OPTIONS
!!    type   which type of color to set
!!
!!       o "bg" , "background"
!!       o "fg" , "foreground"
!!       o "cr" , "cursor"
!!       o "mfg", "mouse_foreground"
!!       o "mbg", "mouse_background"
!!       o "hc" , "highlight"
!!       o "tfg", "tektronix_foreground"
!!       o "tbg", "textronix_background"
!!       o "tcr", "textronix_cursor"
!!
!!    color  A string specifying a color. Value may
!!           be a name or a HEX value of the form
!!           #RRGGBB, or of the form rgb:RR/GG/BB
!!
!!##SEE ALSO
!!    showrgb(1) shows known named X11 Windows colors
!!
!!##EXAMPLE
!!
!!   A sample program:
!!
!!    program demo_xterm_colors
!!    use M_xterm, only : xterm_colors
!!    call xterm_colors('background','gray')
!!    call xterm_colors('foreground','black')
!!    call xterm_colors('cursor','red')
!!    call xterm_colors('highlight','blue')
!!    end program demo_xterm_colors
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine xterm_colors(type,color)
use M_strings, only : isdigit, isspace, switch

! ident_6="@(#)M_xterm::xterm_colors(3f): set various xterm(1) window colors using escape sequences"

!   esc]Ps;bel
!   Ps = 1 0  -> Change VT100 text foreground color to Pt
!   Ps = 1 1  -> Change VT100 text background color to Pt
!   Ps = 1 2  -> Change text cursor color to Pt
!   Ps = 1 3  -> Change mouse foreground color to Pt
!   Ps = 1 4  -> Change mouse background color to Pt
!   Ps = 1 5  -> Change Tektronix foreground color to Pt
!   Ps = 1 6  -> Change Tektronix background color to Pt
!   Ps = 1 7  -> Change highlight color to Pt
!   Ps = 1 8  -> Change Tektronix cursor color to Pt
! If a "?" is given rather than a name or RGB specification,
! xterm replies with a control sequence of the same
! form which can be used to set the corresponding dynamic
! color.  Because more than one pair of color number and
! specification can be given in one control sequence, xterm
! can make more than one reply.
!
character(len=*),intent(in)  :: type
character(len=*),intent(in)  :: color
character(len=1),allocatable :: kolr(:)
integer                      :: code

   select case(type)
   case('bg','background')            ; code=11
   case('fg','foreground')            ; code=10
   case('cr','cursor')                ; code=12
   case('mfg','mouse_foreground')     ; code=13
   case('mbg','mouse_background')     ; code=14
   case('hc','highlight')             ; code=17
   case('tfg','tektronix_foreground') ; code=15
   case('tbg','textronix_background') ; code=16
   case('tcr','textronix_cursor')     ; code=18
   case default
      write(*,*)'*xterm_color* unknown type ',trim(type)
      return
   end select
   call set_G_io()
   kolr=switch(color)
   if(all(isdigit(kolr).or.isspace(kolr)))then ! a number, not a string
      write(G_io,'(a)',advance='no')esc//'[48;5;'//trim(color)//'m'
   else
      write(G_io,'(a,i0,a)',advance='no')esc//']',code,';'//trim(color)//bel
   endif
end subroutine xterm_colors
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_font(3f) - [M_xterm] set xterm(1) font
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine xterm_font(fontname)
!!    character(len=*),intent(in) :: fontname
!!
!!##DESCRIPTION
!!
!!    Sets the font for an xterm(1) window.
!!
!!##OPTIONS
!!    fontname   X11 windows font name. Note you should always pick a
!!               fixed-space font. Allowed formats are
!!
!!           fontname       a fixed-space font name. Wildcards
!!                          are allowed (eg. "*-15-*-c-*" or
!!                          "*-14-*-m-*"), as well as X11
!!                          aliases (eg. "fixed","9x15","8x13").
!!           mapped         Map the default names used in the
!!                          font menu to numbers (0 to 8).
!!                          The names are "default","unreadable",
!!                          "tiny","small","medium","large",
!!                          "huge","escape sequence","selection".
!!           "+","up"       go up thru default font list.
!!           "-","down"     go down thru default font list.
!!           "0" thru "9"   pick a font by number from the
!!                          font defaults (default, unreadable,
!!                          tiny,small,medium,large,huge,
!!                          escape sequence,selection).
!!
!!##SEE ALSO
!!
!!    To list all the fixed-space fonts on your system, try
!!
!!     xlsfonts '*-*-c-*'
!!     xlsfonts '*-*-m-*'
!!
!!
!!##EXAMPLE
!!
!!   A sample program:
!!
!!    program demo_xterm_font
!!    use M_xterm, only : xterm_font
!!    implicit none
!!    character(len=256) :: string
!!    character(len=1)   :: paws
!!    integer            :: i,count,ios
!!    ! get number of arguments on command line
!!    count = command_argument_count()
!!    ! if at least one name on command line select fonts,
!!    ! pausing between names until last font name
!!    !is reached
!!    do i=1,count
!!       call get_command_argument(number=i,value=string)
!!       write(*,'(2a)',advance='no')'font=',trim(string)
!!       call xterm_font(string)
!!       if(i.eq.count)exit
!!       read(*,'(a)',iostat=ios)paws
!!    enddo
!!    end program demo_xterm_font
!!
!!    ./demo_xterm_font 0  # set to default font
!!    ./demo_xterm_font '*--15-*-c-*'  # fixed a 15-point font
!!    ./demo_xterm_font 0 1 2 3 4 5 6 7 8 # step thru defaults
!!    ./demo_xterm_font fixed # common alias for a font
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine xterm_font(fontname)
character(len=*),intent(in) :: fontname

! ident_7="@(#)M_xterm::xterm_font(3f): set a font by name in an xterm window"

   ! NOTE: list of fonts in font menu (ctrl-Mouse3) can be set via xterm(1) X11 resources
   call set_G_io()
   select case(fontname)

   case('-','+','0':'9') ; write(G_io,'(a)',advance='no')esc//"]50;#"//trim(fontname)//bel   ! try the string that was found
   case('up')            ; write(G_io,'(a)',advance='no')esc//"]50;#+"//bel                  ! up list of predefined fonts
   case('down')          ; write(G_io,'(a)',advance='no')esc//"]50;#-"//bel                  ! down list of predefined fonts

   case('default')       ; write(G_io,'(a)',advance='no')esc//"]50;#0"//bel
   case('unreadable')    ; write(G_io,'(a)',advance='no')esc//"]50;#1"//bel
   case('tiny')          ; write(G_io,'(a)',advance='no')esc//"]50;#2"//bel
   case('small')         ; write(G_io,'(a)',advance='no')esc//"]50;#3"//bel
   case('medium')        ; write(G_io,'(a)',advance='no')esc//"]50;#4"//bel
   case('large')         ; write(G_io,'(a)',advance='no')esc//"]50;#5"//bel
   case('huge')          ; write(G_io,'(a)',advance='no')esc//"]50;#6"//bel
   case('escape')        ; write(G_io,'(a)',advance='no')esc//"]50;#7"//bel
   case('selection')     ; write(G_io,'(a)',advance='no')esc//"]50;#8"//bel

   case('')              ; write(G_io,'(a)',advance='no')esc//"]50;fixed"//bel  ! the standard default X11 Windows font name

   case default
      write(G_io,'(a)',advance='no')esc//"]50;"//trim(fontname)//bel   ! try the string that was found
   end select

end subroutine xterm_font
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! xterm_keywords(3f) - [M_xterm] sent hints to X11 Window manager and select modes
!! (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine xterm_keywords(keyword)
!!    character(len=*),intent(in) :: keyword
!!
!!##DESCRIPTION
!!    Send basic hints to the X11 Window Manager, such as to raise
!!    or lower or iconify the window. Also, set major modes such as
!!    Tektronix 4010 emulator mode or Digital VT102 emulator mode.
!!
!!##OPTIONS
!!    keyword   Recognized keyword values are
!!
!!              raise      raise to top of window stack
!!              lower      lower to back of window stack
!!              iconify    iconify window
!!              uniconify  uniconify window
!!              maximize   make xterm(1) window size of display
!!              restore    restore window size to before the last "maximize"
!!              vt102      switch to VT102 terminal emulator mode
!!              tek        switch to Tektronix 4010 terminal emulator mode
!!
!!##EXAMPLE
!!
!!    A sample program:
!!
!!     program demo_xterm_keywords
!!     use M_xterm, only : xterm_keywords
!!     implicit none
!!        call xterm_keywords('iconify')
!!        write(*,*)'do some stuff'
!!        call xterm_keywords('uniconify')
!!        call xterm_keywords('raise')
!!     end program demo_xterm_keywords
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine xterm_keywords(keyword)
character(len=*),intent(in)  :: keyword
character(len=:),allocatable :: code

! ident_8="@(#)M_xterm::xterm_keywords(3f): send keywords to the X11 Window Manager to change xterm(1) window and select modes"

   select case(keyword)
   ! WINDOW STACK
   case('raise');     code=CSI//'5'//'t'
   case('lower');     code=CSI//'6'//'t'
   ! MAXIMIZE
   case('maximize');  code=CSI//'9;1'//'t'    ! Maximize window (i.e., resize to screen size).
   case('restore');   code=CSI//'9;0'//'t'    ! Restore maximized window.
   ! ICONIFY/UNICONIFY
   case('iconify');   code=CSI//'2'//'t'      ! crack the shroud and unveil
   case('uniconify'); code=CSI//'1'//'t'
   case('toggle');    code=CSI//'2'//'t'
   ! THE TERMINAL EMULATION TYPE
   case('vt102','vt100');   code=esc//etx//'T'   ! vt102 emulation mode
   case('tek','tektronix'); code=esc//'[?38h'    ! tektronix
   case default                                  ! unknown keyword
      write(stderr,'(a,a)')  '*xterm_keywords* : unknown keyword:',trim(keyword)
      write(stderr,'(a,a)')  '*xterm_keywords* : raise,lower,maximize,restore,iconify,uniconify,toggle,vt102|vt100,tek|tektronix'
      return
   end select

   call set_G_io()
   write(G_io,'(a)',advance='no') code

end subroutine xterm_keywords
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_position(3f) - [M_xterm] set xterm(1) window position using escape sequences
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine xterm_position(right,down)
!!    integer,intent(in),optional :: right
!!    integer,intent(in),optional :: down
!!
!!##DESCRIPTION
!!    Sets the position of an xterm(1) window by specifying the
!!    position of the upper left corner.
!!
!!##OPTIONS
!!    right  how far in pixels to place upper left corner of window from
!!           left edge of display
!!    down   how far down in pixels to place upper left corner of window
!!           from upper edge of display
!!
!!##EXAMPLE
!!
!!   A sample program:
!!
!!    program demo_xterm_position
!!    use M_xterm, only : xterm_position
!!    implicit none
!!    integer :: right, down
!!    call xterm_position(down=200,right=100)
!!    end program demo_xterm_position
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine xterm_position(right,down)
integer,intent(in),optional :: right,down
integer                     :: current_right,current_down

! ident_9="@(#)M_xterm::xterm_position(3f): set xterm(1) window position using escape sequences"

   call set_G_io()
   if(present(right).and.present(down))then
      write(G_io,'(a,i0,a,i0,a)',advance='no') CSI//'3;',right,';',down,'t'
   else
      ! unfortunately, a blank value is taken as 0 instead of as "leave alone", so have to query current position
      call xterm_get_position(current_right,current_down)
      if(present(right))then
         write(G_io,'(a,i0,a,i0,a)',advance='no') CSI//'3;',right,        ';',current_down,'t'
      elseif(present(down))then
         write(G_io,'(a,i0,a,i0,a)',advance='no') CSI//'3;',current_right,';',down,        't'
      endif
   endif

end subroutine xterm_position
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_geometry(3f) - [M_xterm] set xterm(1) size using escape sequences
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine xterm_geometry(rows,cols)
!!    integer,intent(in),optional :: rows
!!    integer,intent(in),optional :: cols
!!
!!##DESCRIPTION
!!    Sets the size of an xterm(1) window.
!!
!!##OPTIONS
!!    rows  number of text rows
!!    cols  number of text columns
!!
!!##EXAMPLE
!!
!!   A sample program:
!!
!!    program demo_xterm_geometry
!!    use M_xterm, only : xterm_geometry
!!    implicit none
!!    integer :: ios
!!    integer :: rows, cols
!!    write(*,'(a)',advance='no')'Enter rows and columns: '
!!    read(*,*,iostat=ios)rows,cols
!!    if(ios.eq.0)then
!!       call xterm_geometry(rows,cols)
!!    endif
!!    end program demo_xterm_geometry
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine xterm_geometry(rows,cols)
integer,intent(in),optional :: rows,cols

! ident_10="@(#)M_xterm::xterm_geometry(3f): set size of xterm(1) window using escape sequences"

   call set_G_io()
   if(present(rows).and.present(cols))then
      write(G_io,'(a,i0,a,i0,a)',advance='no') CSI//'8;',rows,';',cols,'t'
   elseif(present(rows))then
      write(G_io,'(a,i0,a,i0,a)',advance='no') CSI//'8;',rows,';t'
   elseif(present(cols))then
      write(G_io,'(a,i0,a,i0,a)',advance='no') CSI//'8;;',cols,'t'
   endif

end subroutine xterm_geometry
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_width(3f) - [M_xterm] set xterm(1) width to 80 or 132 characters
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine xterm_width(iwidth)
!!    integer,intent(in) :: iwidth
!!
!!##DESCRIPTION
!!    Sets the width of an xterm(1) window to 80 or 132 characters.
!!    Unlike xterm_geometry(3f), this routine does not require X11
!!    Windows communication to be established.
!!
!!##OPTIONS
!!    iwidth  80 or 132
!!
!!##EXAMPLE
!!
!!   A sample program:
!!
!!    program demo_xterm_width
!!    use M_xterm, only : xterm_width
!!    implicit none
!!    call xterm_width(80)
!!    end program demo_xterm_width
!!
!!##NOTE:
!!    Newer versions of xterm(1) require enabling of column switching.
!!    one way to turn on 132-column switching for NEW xterm windows is:
!!
!!       echo'XTerm*c132: on'|xrdb -merge
!!
!!    You may also use ctrl-Mouse2 to produce the Options menu and select
!!    "Allow 80/132 column switching" to enable the mode in running xterm
!!    windows.
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine xterm_width(iwidth)
integer,intent(in) :: iwidth

! ident_11="@(#)M_xterm::xterm_width(3f): set size of xterm(1) window to standard sizes using escape sequences"

   call set_G_io()
   write(G_io,'(a)',advance='no') CSI//'?40h' ! enable 80/132 modes
   select case(iwidth)
   case(80)
      write(G_io,'(a)',advance='no') CSI//'?3l' ! 80 was specified
   case(132)
      write(G_io,'(a)',advance='no') CSI//'?3h' ! 132 was specified
   case default
      return
   end select

end subroutine xterm_width
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_labels(3f) - [M_xterm] set xterm(1) X11 WIndow labels using escape sequences
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine xterm_labels(type,label)
!!
!!    character(len=*),intent(in) :: type
!!    character(len=*),intent(in) :: label
!!
!!##DESCRIPTION
!!    Set icon and decoration title labels for xterm(1) windows.
!!##OPTIONS
!!    type
!!           'title'      set title bar label hint in X11 Windows decoration
!!           'name'       set icon name hint for X11 Windows manager
!!           'nt'         set title and icon name
!!
!!    label  string used to set label associated with TYPE.
!!
!!##EXAMPLE
!!
!!   A sample program:
!!
!!    program demo_xterm_labels
!!    use M_xterm, only : xterm_labels
!!    implicit none
!!    call xterm_labels('title','CLUSTER LOGIN')
!!    end program demo_xterm_labels
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine xterm_labels(type,label)
character(len=*),intent(in) :: type
character(len=*),intent(in) :: label

! ident_12="@(#)M_xterm::xterm_labels(3f): set various strings and labels associated with an xterm(1) such as title and icon name"

   call set_G_io()
   select case(type)
   case('title');     write(G_io,'(a)',advance='no') esc//']2;'//label//bel
   case('name');      write(G_io,'(a)',advance='no') esc//']1;'//label//bel
   case('nt');        write(G_io,'(a)',advance='no') esc//']0;'//label//bel
   case default
      write(stderr,'(a,a)')  '*xterm_labels* : unknown label type:',trim(type)
      return
   end select

end subroutine xterm_labels
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_clear(3f) - [M_xterm] clear xterm(1) screen using escape sequences
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine xterm_clear()
!!
!!##DESCRIPTION
!!    clears the screen of an xterm(1) window using escape sequences
!!
!!##EXAMPLE
!!
!!   A sample program:
!!
!!    program demo_xterm_clear
!!    use M_xterm, only : xterm_clear
!!    implicit none
!!    call xterm_clear()
!!    end program demo_xterm_clear
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine xterm_clear()

! ident_13="@(#)M_xterm::xterm_clear(3f): clear the screen of an xterm(1) window using escape sequences"

   call set_G_io()
   write(G_io,'(a)',advance='no') CSI//'2J'

end subroutine xterm_clear
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function RAWGET(string) result (readback)
! write xterm(1) query string and read back reply until a timeout occurs, which is assumed to mean the end of the reply was reached
use M_getkey, only : system_timeout_getkey
implicit none
character(len=*),intent(in)     :: string
character(len=:),allocatable    :: readback

   character(len=1)             :: letter
   character(len=4096)          :: path
   integer                      :: icount
   integer                      :: ios
   integer                      :: io

   ! when using applications like screen(1) you need
   ! to use the tty of the original connection. If
   ! OTHERTTY is set to the original tty before screen
   ! was started/restarted this will still work
   ! Otherwise, because screen(1) is capturing escape
   ! sequences to convert them to correct VT102 sequences
   ! it ignores unrecognized (as far as it is concerned,
   ! as the sequences are not know to termcaps.) sequences
   ! like Tektronix commands and xterm(1) X11 WIndows
   ! attribute extensions
   path=''
   call get_environment_variable('OTHERTTY',path)
   if(path.eq.'')path='/dev/tty'

   open(newunit=io,file=path,iostat=ios)
   if(ios.ne.0)then
      io=6
   endif

   write(io,'(a)',advance='no')trim(string)
   flush(io,iostat=ios)

   icount=0
   readback=''
   do
      if(icount.gt.2048)exit            ! arbitrary indication taking too long
      letter=system_timeout_getkey(1)   ! get next key or timeout after 1/10 second
      if(letter.eq.char(0))exit         ! assume if a null is returned it is a timeout and end of current input buffer was reached
      icount=icount+1
      readback=readback//letter
   enddo

   if(io.ne.6)then
      close(io,iostat=ios)
   endif

end function RAWGET
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_get_font(3f) - [M_xterm] obtain xterm(1) font name
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function xterm_get_font() result(fontname)
!!    character(len=:),allocatable :: fontname
!!
!!##DESCRIPTION
!!    Sends an escape sequence to stdout to query the fontname of an xterm(1) window.
!!
!!    For this to work stdout must be your terminal device; so this may not be used
!!    effectively in a pipe or when redirection is used, depending on your system.
!!
!!##EXAMPLE
!!
!!   A sample program:
!!
!!    program demo_xterm_get_font
!!    use M_xterm, only : xterm_get_font
!!    implicit none
!!       write(*,*) xterm_get_font()
!!    end program demo_xterm_get_font
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function xterm_get_font() result(fontname)
! Obtain current screen fontname
use M_strings, only : split
implicit none

! ident_14="@(#)M_xterm::xterm_get_font(3f): Obtain current xterm(1) window font name using escape sequences"

character(len=:),allocatable  :: fontname
character(len=:),allocatable  :: string
character(len=:),allocatable  :: array(:)
   string=rawget(esc//']50;?'//bel)
   ! string=rawget(esc//']50;?$'//bel)
   ! STRING=]50;#0 *-cronyx-courier-medium-r-normal--17-120-100-100-m-90-koi8-r
   ! #0 shows up when font matches a VT fonts menu number
   CALL split(string,array,delimiters=' ;'//bel)
   if(size(array).ge.3)then
      fontname=trim(array(3))
   elseif(size(array).ge.2)then
      fontname=trim(array(2))
   else
      fontname=''
   endif
end function xterm_get_font
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_get_iconstate(3f) - [M_xterm] obtain xterm(1) icon state using escape sequences
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function xterm_get_iconstate() result(state)
!!    integer :: state
!!
!!##DESCRIPTION
!!    Sends an escape sequence to stdout to query the icon state of an xterm(1) window.
!!
!!    For this to work stdout must be your terminal device; so this may not work
!!    effectively in a pipe or when redirection is used, depending on your system.
!!
!!##RESULT
!!    state  returns  'opened' if window is not iconified; 'closed' if iconified.
!!
!!##EXAMPLE
!!
!!   A sample program:
!!
!!    program demo_xterm_get_iconstate
!!    use M_xterm, only : xterm_get_iconstate
!!    implicit none
!!       write(*,*) xterm_get_iconstate()
!!    end program demo_xterm_get_iconstate
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function xterm_get_iconstate() result(state)
use M_strings, only : split
use M_strings, only : visible
implicit none

! ident_15="@(#)M_xterm::xterm_get_iconstate(3f): Obtain current xterm(1) window icon state using escape sequences"

character(len=:),allocatable   :: string
character(len=:),allocatable   :: state
   ! Ps = 1 1  -> Report xterm window state.  If the xterm
   !              window is open (non-iconified), it returns CSI 1 t .  If
   !              the xterm window is iconified, it returns CSI 2 t .
   string=rawget(esc//'[11t')
   select case(string)
   case(CSI//'1t')
      state='opened'
   case(CSI//'2t')
      state='closed'
   case default
      state='unknown'
   end select
end function xterm_get_iconstate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_get_geometry(3f) - [M_xterm] obtain xterm(1) screen size in character units
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine xterm_get_geometry(rows,cols)
!!    integer,intent(out) :: rows
!!    integer,intent(out) :: cols
!!
!!##DESCRIPTION
!!    Sends an escape sequence to stdout to query the size of an xterm(1) window.
!!    The number of character rows and character columns is returned.
!!
!!    For this to work stdout must be your terminal device; so this may not work
!!    effectively in a pipe or when redirection is used, depending on your system.
!!
!!##EXAMPLE
!!
!!   A sample program:
!!
!!    program demo_xterm_get_geometry
!!    use M_xterm, only : xterm_get_geometry
!!    implicit none
!!    integer :: irows, icols
!!       call xterm_get_geometry(irows,icols)
!!       write(*,*)'rows=',irows,' cols=',icols
!!    end program demo_xterm_get_geometry
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine xterm_get_geometry(rows,cols)
! Obtain current screen size and place in wide and high
! Ps = 1 8  -> Report the size of the text area in characters as CSI  8  ;  height ;  width t
! also see stty -a, xwininfo -id $WINDOWID, env ROWS= LINES= COLUMNS=, xresize(1)
use M_strings, only : split, s2v
implicit none

! ident_16="@(#)M_xterm::xterm_get_geometry(3f): Obtain current xterm(1) window size in character rows and columns"

integer,intent(out)           :: rows
integer,intent(out)           :: cols
character(len=:),allocatable  :: string
character(len=:),allocatable  :: array(:)
integer                       :: isize
   string=rawget(esc//'[18t')
   CALL split(string,array,delimiters=';t')
   isize=size(array)
   if(isize.ge.2) rows=int(s2v(array(2)))
   if(isize.ge.3) cols=int(s2v(array(3)))
end subroutine xterm_get_geometry
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    xterm_get_position(3f) - [M_xterm] obtain xterm(1) screen position
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine xterm_get_position(right,down)
!!    integer,intent(out) :: right
!!    integer,intent(out) :: down
!!
!!##DESCRIPTION
!!    Sends an escape sequence to stdout to query the position of an xterm(1) window.
!!    The position of the upper left corner of the xterm window is returned relative
!!    to the upper left corner of the display
!!
!!    For this to work stdout must be your terminal device; so this may not work
!!    effectively in a pipe or when redirection is used.
!!
!!##EXAMPLE
!!
!!   A sample program:
!!
!!    program demo_xterm_get_position
!!    use M_xterm, only : xterm_get_position
!!    implicit none
!!    integer :: iright, idown
!!       call xterm_get_position(iright,idown)
!!       write(*,*)'right=',iright,' down=',idown
!!    end program demo_xterm_get_position
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine xterm_get_position(right,down)
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use M_strings, only : split, s2v
implicit none

! ident_17="@(#)M_xterm::xterm_get_position(3f): Obtain current xterm(1) window position"

! Ps = 1 3  -> Report xterm window position as CSI 3 ; x; yt
! Obtain current screen size and place in wide and high
! also see stty -a, xwininfo -id $WINDOWID, env ROWS= LINES= COLUMNS=, xresize(1)
integer,intent(out)           :: right
integer,intent(out)           :: down
integer(kind=int64)           :: r,d
character(len=:),allocatable  :: string
character(len=:),allocatable  :: array(:)
integer                       :: isize
   string=rawget(esc//'[13t')
   CALL split(string,array,delimiters=';t')
   isize=size(array)
   if(isize.ge.2) r=int(s2v(array(2)),kind=int64)
   if(isize.ge.3) d=int(s2v(array(3)),kind=int64)
   if(d.gt.2147483648_int64) d=(4294967296_int64-d)
   if(r.gt.2147483648_int64) r=(4294967296_int64-r)
   right=int(r)
   down=int(d)
end subroutine xterm_get_position
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine set_g_io()

! ident_18="@(#)M_xterm::set_g_io(3f): send output to alternative file for use with programs like screen(1) and tmux(1)"

character(len=:),allocatable :: altout
integer :: IOS
integer :: IO
character(len=256) :: MESSAGE
   if(G_io.eq.0)then
      altout=system_getenv('OTHERTTY')
      if(altout.ne.'')then
         open (newunit=io, file=altout, action='write', iostat=ios,iomsg=message)
         if (ios < 0) then
            ! Perform end-of-file processing on the file connected to unit
            G_io=stdout
         elseif (ios > 0) then
            ! Perform error processing
            G_io=stdout
         else
            G_io=io
         endif
      else
         G_io=stdout
      endif
   endif
end subroutine set_g_io
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_xterm()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   call test_xterm_clear()
   call test_xterm_colors()
   call test_xterm_font()
   call test_xterm_geometry()
   call test_xterm_get_colors()
   call test_xterm_get_font()
   call test_xterm_get_geometry()
   call test_xterm_get_iconstate()
   call test_xterm_get_pencolor()
   call test_xterm_get_position()
   call test_xterm_keywords()
   call test_xterm_labels()
   call test_xterm_occupancy()
   call test_xterm_pencolor()
   call test_xterm_position()
   call test_xterm_width()
   call test_xterm_xrdb()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_clear()
   call unit_check_start('xterm_clear',msg='')
   !!call unit_check('xterm_clear', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_clear',msg='')
end subroutine test_xterm_clear
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_colors()
   call unit_check_start('xterm_colors',msg='')
   !!call unit_check('xterm_colors', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_colors',msg='')
end subroutine test_xterm_colors
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_font()
   call unit_check_start('xterm_font',msg='')
   !!call unit_check('xterm_font', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_font',msg='')
end subroutine test_xterm_font
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_geometry()
   call unit_check_start('xterm_geometry',msg='')
   !!call unit_check('xterm_geometry', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_geometry',msg='')
end subroutine test_xterm_geometry
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_get_colors()
   call unit_check_start('xterm_get_colors',msg='')
   !!call unit_check('xterm_get_colors', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_get_colors',msg='')
end subroutine test_xterm_get_colors
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_get_font()
   call unit_check_start('xterm_get_font',msg='')
   !!call unit_check('xterm_get_font', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_get_font',msg='')
end subroutine test_xterm_get_font
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_get_geometry()
   call unit_check_start('xterm_get_geometry',msg='')
   !!call unit_check('xterm_get_geometry', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_get_geometry',msg='')
end subroutine test_xterm_get_geometry
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_get_iconstate()
   call unit_check_start('xterm_get_iconstate',msg='')
   !!call unit_check('xterm_get_iconstate', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_get_iconstate',msg='')
end subroutine test_xterm_get_iconstate
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_get_pencolor()
   call unit_check_start('xterm_get_pencolor',msg='')
   !!call unit_check('xterm_get_pencolor', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_get_pencolor',msg='')
end subroutine test_xterm_get_pencolor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_get_position()
   call unit_check_start('xterm_get_position',msg='')
   !!call unit_check('xterm_get_position', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_get_position',msg='')
end subroutine test_xterm_get_position
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_keywords()
   call unit_check_start('xterm_keywords',msg='')
   !!call unit_check('xterm_keywords', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_keywords',msg='')
end subroutine test_xterm_keywords
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_labels()
   call unit_check_start('xterm_labels',msg='')
   !!call unit_check('xterm_labels', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_labels',msg='')
end subroutine test_xterm_labels
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_occupancy()
   call unit_check_start('xterm_occupancy',msg='')
   !!call unit_check('xterm_occupancy', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_occupancy',msg='')
end subroutine test_xterm_occupancy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_pencolor()
   call unit_check_start('xterm_pencolor',msg='')
   !!call unit_check('xterm_pencolor', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_pencolor',msg='')
end subroutine test_xterm_pencolor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_position()
   call unit_check_start('xterm_position',msg='')
   !!call unit_check('xterm_position', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_position',msg='')
end subroutine test_xterm_position
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_width()
   call unit_check_start('xterm_width',msg='')
   !!call unit_check('xterm_width', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_width',msg='')
end subroutine test_xterm_width
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_xterm_xrdb()
   call unit_check_start('xterm_xrdb',msg='')
   !!call unit_check('xterm_xrdb', 0.eq.0, 'checking',100)
   call unit_check_done('xterm_xrdb',msg='')
end subroutine test_xterm_xrdb
!===================================================================================================================================
end subroutine test_suite_M_xterm
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_xterm
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
