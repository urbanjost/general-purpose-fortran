!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
program prg_esc
use M_kracken, only : kracken, iget
use M_kracken, only : kracken, lget, sget, iget, sgets           ! add command-line parser module
use M_strings, only : split, s2v, isdigit, switch, nospace, isspace, s2vs
use M_ncurses
use M_fixedform
use M_xterm,   only : xterm_colors, xterm_pencolor
use M_xterm,   only : xterm_font, xterm_clear,xterm_keywords, xterm_labels
use M_xterm,   only : xterm_geometry, xterm_position, xterm_width
use M_xterm,   only : xterm_xrdb, xterm_occupancy
use M_xterm,   only : xterm_get_geometry, xterm_get_position
use M_xterm,   only : xterm_get_font, xterm_get_iconstate, xterm_get_colors, xterm_get_pencolor
use M_system,  only : system_getenv
use,intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT

implicit none

   call esc()
contains
subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                                                                            ',&
'   esc(1) - [NCURSES] set xterm(1) attributes using a screen or line mode                                                       ',&
'   (LICENSE:PD)                                                                                                                 ',&
'SYNOPSIS                                                                                                                        ',&
'   esc   [keywords]                                                                                                             ',&
'         -rows NN                                                                                                               ',&
'         -cols NN                                                                                                               ',&
'         -right NN                                                                                                              ',&
'         -down NN                                                                                                               ',&
'         -fn fontname                                                                                                           ',&
'         -fsize fontsize                                                                                                        ',&
'         -bg background_color                                                                                                   ',&
'         -fg foreground_color                                                                                                   ',&
'         -cr cursor_color                                                                                                       ',&
'         -cn [0 COLOR_VALUE 1 COLOR_VALUE 2 COLOR_VALUE ...|]                                                                   ',&
'         -o windowname                                                                                                          ',&
'         -title TITLE window title                                                                                              ',&
'         -name TITLE icon name                                                                                                  ',&
'         -nt TITLE icon name and title                                                                                          ',&
'         -xrdb [NAME]                                                                                                           ',&
'         -iconstate                                                                                                             ',&
'         -alias                                                                                                                 ',&
'         --help                                                                                                                 ',&
'         --version                                                                                                              ',&
'DESCRIPTION                                                                                                                     ',&
'   If no values are specified on the command line a screen interface is                                                         ',&
'   displayed that allows setting the background, foreground, cursor color,                                                      ',&
'   font size, and window size of an xterm(1) terminal emulator window                                                           ',&
'   using ncurses(3f)/fixedform(3f).                                                                                             ',&
'                                                                                                                                ',&
'   If options are specified on the command line the values are set and the                                                      ',&
'   screen mode is not launched.                                                                                                 ',&
'                                                                                                                                ',&
'   If the positioning and resize options do not work, run the following                                                         ',&
'   before starting a new xterm:                                                                                                 ',&
'                                                                                                                                ',&
'      esc -xrdb |xrdb -merge                                                                                                    ',&
'                                                                                                                                ',&
'   If you use something that filters stdout such as tmux(1), screen(1) assign                                                   ',&
'   esc(1) output to the initial stdout before starting the program, such as                                                     ',&
'                                                                                                                                ',&
'      export OTHERTTY=`tty`                                                                                                     ',&
'      tmux                                                                                                                      ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'   VALUES     The allowed keywords are:                                                                                         ',&
'                                                                                                                                ',&
'              o 80........ set terminal width to eighty characters                                                              ',&
'              o 132....... set terminal width to 132 characters                                                                 ',&
'              o raise..... raise to top of window stack                                                                         ',&
'              o lower..... lower to back of window stack                                                                        ',&
'              o iconify... iconify window                                                                                       ',&
'              o uniconify. uniconify window                                                                                     ',&
'              o toggle.... toggle icon state                                                                                    ',&
'              o maximize.. make xterm(1) window size of display                                                                 ',&
'              o restore... restore window size to before the last "maximize"                                                    ',&
'              o vt102..... switch to VT102 terminal emulator mode                                                               ',&
'              o tek....... switch to Tektronix 4010 terminal emulator mode                                                      ',&
'                                                                                                                                ',&
'   -rows NN         change number of rows of window to specified value                                                          ',&
'   -cols NN         change number of columns of window to specified value                                                       ',&
'   -right NN        distance in rasters from left display edge to place                                                         ',&
'                    upper left corner                                                                                           ',&
'   -down NN         distance in rasters from upper display edge to place                                                        ',&
'                    upper left corner                                                                                           ',&
'                                                                                                                                ',&
'   -fn FONTNAME     change to specified font name. You can list several                                                         ',&
'                    available fixed-space font names by using:                                                                  ',&
'                                                                                                                                ',&
'                      (xlsfonts -fn ''*-c-*'' ; xlsfonts -fn ''*-m-*'')                                                         ',&
'                                                                                                                                ',&
'                     Example names: (font names may vary from server to server):                                                ',&
'                                                                                                                                ',&
'                          fixed     8x13       9x15        ''*24*-c-*''                                                         ',&
'                                                                                                                                ',&
'   -fsize NN        guess at a fixed font of specified size                                                                     ',&
'                                                                                                                                ',&
'   -bg CNAME        specify background color by name or hex value "#RRGGBB"                                                     ',&
'   -fg CNAME        specify foreground color by name or hex value "#RRGGBB"                                                     ',&
'   -cr CNAME        specify cursor color by name or hex value "#RRGGBB"                                                         ',&
'   -cn NN CNAME...  define terminal pen colors by number "NN $RRGGBB" or                                                        ',&
'                    name. Typically, at least 0 thru 15 are supported.                                                          ',&
'                                                                                                                                ',&
'   -title TITLE     window title                                                                                                ',&
'   -name TITLE      icon name                                                                                                   ',&
'   -nt TITLE        icon name and title                                                                                         ',&
'                                                                                                                                ',&
'   --alias          list some common bash shell aliases that use esc(1).                                                        ',&
'                    These are typically added to the ~/.bashrc file                                                             ',&
'                    or sourced:                                                                                                 ',&
'                                                                                                                                ',&
'                       esc -alias >./junk;source ./junk;rm ./junk;alias                                                         ',&
'                                                                                                                                ',&
'   -o WINDOWNAME    If your window manager supports multiple virtual                                                            ',&
'                    displays by defining the property WM_OCCUPANCY                                                              ',&
'                    (use xprop -id $WINDOWID to see) then you can                                                               ',&
'                    move your windows to specific virtual displays;                                                             ',&
'                    usually by number or by name. The name "all" is                                                             ',&
'                    typically special and makes the window visible                                                              ',&
'                    in all the virtual displays.                                                                                ',&
'                                                                                                                                ',&
'                    This works for the ctwm(1) window manager.                                                                  ',&
'                                                                                                                                ',&
'                    If your window manager has assigned the property                                                            ',&
'                    WS_OCCUPATION as seen by entering                                                                           ',&
'                                                                                                                                ',&
'                       xprop -id $WINDOWID                                                                                      ',&
'                             :                                                                                                  ',&
'                          WM_OCCUPATION(STRING) = "2"                                                                           ',&
'                          WM_OCCUPATION(STRING) = "Two"                                                                         ',&
'                            :                                                                                                   ',&
'                                                                                                                                ',&
'                    you can specify which virtual display to display on.                                                        ',&
'                                                                                                                                ',&
'  --iconstate       return current icon state of X11 xterm window (opened or                                                    ',&
'                    closed).                                                                                                    ',&
'                                                                                                                                ',&
'   --xrdb NAME      print out current terminal settings as a set of                                                             ',&
'                    X11 resources that can be added to your ~/.Xresources                                                       ',&
'                    file so that you can launch terminals with those                                                            ',&
'                    attributes with                                                                                             ',&
'                                                                                                                                ',&
'                       "xterm -name NAME"                                                                                       ',&
'                                                                                                                                ',&
'                    For example:                                                                                                ',&
'                                                                                                                                ',&
'                       esc -xrdb EDIT|xrdb -merge                                                                               ',&
'                       xterm -name EDIT                                                                                         ',&
'                                                                                                                                ',&
'                    All other options will be ignored. This must be run                                                         ',&
'                    in an xterm(1) window.                                                                                      ',&
'                                                                                                                                ',&
'   --help           display help and exit                                                                                       ',&
'   --version        display version information and exit                                                                        ',&
'                                                                                                                                ',&
'   The following options will return the current value if given a null                                                          ',&
'   value:                                                                                                                       ',&
'                                                                                                                                ',&
'   -fn                                                                                                                          ',&
'   -bg -fg -cr                                                                                                                  ',&
'   -rows -cols                                                                                                                  ',&
'   -down -right                                                                                                                 ',&
'   -iconstate                                                                                                                   ',&
'   -cn ''''|0 1 2 ...                                                                                                           ',&
'                                                                                                                                ',&
'   By itself, -cn will list the first 16 pen colors. Given a list of                                                            ',&
'   numbers, it will query those specific pen numbers.                                                                           ',&
'                                                                                                                                ',&
'VARIABLES                                                                                                                       ',&
'   By default esc(1) writes output to the current stdout file. The                                                              ',&
'   environment variable OTHERTTY can be used to change the default                                                              ',&
'   file. This is commonly required before starting programs that                                                                ',&
'   filter stdout, such as tmux(1) and screen(1).                                                                                ',&
'                                                                                                                                ',&
'      export OTHERTTY=/dev/pts/1                                                                                                ',&
'                                                                                                                                ',&
'EXAMPLE                                                                                                                         ',&
'  Sample usage:                                                                                                                 ',&
'                                                                                                                                ',&
'   # if do not have xterm settings set might need to enter                                                                      ',&
'   esc -xrdb|xrdb -merge                                                                                                        ',&
'   # and then try the commands in xterm(1) windows launched after this                                                          ',&
'                                                                                                                                ',&
'   esc # bring up screen interface                                                                                              ',&
'   esc -fn ''*--14*-c-*''  # find a font of specified size                                                                      ',&
'   esc -fn 5  # set to fifth font in font menu (ctrl-Mouse2)                                                                    ',&
'   esc -fsize 20 # set to first 20-point font found (if any)                                                                    ',&
'   esc -rows 40 -cols 132 # set terminal size                                                                                   ',&
'   esc -bg brown -fg white -cr red                                                                                              ',&
'   esc -bg ''#ff00ff''                                                                                                          ',&
'   esc -cn 0 red 1 green 2 cyan                                                                                                 ',&
'   esc -alias >x;source x;rm x # set up a lot of bash shell aliases                                                             ',&
'                                                                                                                                ',&
'   # set up X11 resources so name BROWN sets some favorite defaults                                                             ',&
'   esc -bg brown -fg black -fn 5 -xrdb BROWN >>$HOME/.Xdefaults                                                                 ',&
'   xrdb -merge $HOME/.Xdefaults                                                                                                 ',&
'   xterm -name BROWN                                                                                                            ',&
'                                                                                                                                ',&
'   C-shell                                                                                                                      ',&
'                                                                                                                                ',&
'      alias cd ''cd \!*; esc -nt''                                                                                              ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public Domain                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!    esc(1) - [NCURSES] set xterm(1) attributes using a screen or line mode
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    esc   [keywords]
!!          -rows NN
!!          -cols NN
!!          -right NN
!!          -down NN
!!          -fn fontname
!!          -fsize fontsize
!!          -bg background_color
!!          -fg foreground_color
!!          -cr cursor_color
!!          -cn [0 COLOR_VALUE 1 COLOR_VALUE 2 COLOR_VALUE ...|]
!!          -o windowname
!!          -title TITLE window title
!!          -name TITLE icon name
!!          -nt TITLE icon name and title
!!          -xrdb [NAME]
!!          -iconstate
!!          -alias
!!          --help
!!          --version
!!##DESCRIPTION
!!    If no values are specified on the command line a screen interface is
!!    displayed that allows setting the background, foreground, cursor color,
!!    font size, and window size of an xterm(1) terminal emulator window
!!    using ncurses(3f)/fixedform(3f).
!!
!!    If options are specified on the command line the values are set and the
!!    screen mode is not launched.
!!
!!    If the positioning and resize options do not work, run the following
!!    before starting a new xterm:
!!
!!       esc -xrdb |xrdb -merge
!!
!!    If you use something that filters stdout such as tmux(1), screen(1) assign
!!    esc(1) output to the initial stdout before starting the program, such as
!!
!!       export OTHERTTY=`tty`
!!       tmux
!!
!!##OPTIONS
!!    VALUES     The allowed keywords are:
!!
!!               o 80........ set terminal width to eighty characters
!!               o 132....... set terminal width to 132 characters
!!               o raise..... raise to top of window stack
!!               o lower..... lower to back of window stack
!!               o iconify... iconify window
!!               o uniconify. uniconify window
!!               o toggle.... toggle icon state
!!               o maximize.. make xterm(1) window size of display
!!               o restore... restore window size to before the last "maximize"
!!               o vt102..... switch to VT102 terminal emulator mode
!!               o tek....... switch to Tektronix 4010 terminal emulator mode
!!
!!    -rows NN         change number of rows of window to specified value
!!    -cols NN         change number of columns of window to specified value
!!    -right NN        distance in rasters from left display edge to place
!!                     upper left corner
!!    -down NN         distance in rasters from upper display edge to place
!!                     upper left corner
!!
!!    -fn FONTNAME     change to specified font name. You can list several
!!                     available fixed-space font names by using:
!!
!!                       (xlsfonts -fn '*-c-*' ; xlsfonts -fn '*-m-*')
!!
!!                      Example names: (font names may vary from server to server):
!!
!!                           fixed     8x13       9x15        '*24*-c-*'
!!
!!    -fsize NN        guess at a fixed font of specified size
!!
!!    -bg CNAME        specify background color by name or hex value "#RRGGBB"
!!    -fg CNAME        specify foreground color by name or hex value "#RRGGBB"
!!    -cr CNAME        specify cursor color by name or hex value "#RRGGBB"
!!    -cn NN CNAME...  define terminal pen colors by number "NN $RRGGBB" or
!!                     name. Typically, at least 0 thru 15 are supported.
!!
!!    -title TITLE     window title
!!    -name TITLE      icon name
!!    -nt TITLE        icon name and title
!!
!!    --alias          list some common bash shell aliases that use esc(1).
!!                     These are typically added to the ~/.bashrc file
!!                     or sourced:
!!
!!                        esc -alias >./junk;source ./junk;rm ./junk;alias
!!
!!    -o WINDOWNAME    If your window manager supports multiple virtual
!!                     displays by defining the property WM_OCCUPANCY
!!                     (use xprop -id $WINDOWID to see) then you can
!!                     move your windows to specific virtual displays;
!!                     usually by number or by name. The name "all" is
!!                     typically special and makes the window visible
!!                     in all the virtual displays.
!!
!!                     This works for the ctwm(1) window manager.
!!
!!                     If your window manager has assigned the property
!!                     WS_OCCUPATION as seen by entering
!!
!!                        xprop -id $WINDOWID
!!                              :
!!                           WM_OCCUPATION(STRING) = "2"
!!                           WM_OCCUPATION(STRING) = "Two"
!!                             :
!!
!!                     you can specify which virtual display to display on.
!!
!!   --iconstate       return current icon state of X11 xterm window (opened or
!!                     closed).
!!
!!    --xrdb NAME      print out current terminal settings as a set of
!!                     X11 resources that can be added to your ~/.Xresources
!!                     file so that you can launch terminals with those
!!                     attributes with
!!
!!                        "xterm -name NAME"
!!
!!                     For example:
!!
!!                        esc -xrdb EDIT|xrdb -merge
!!                        xterm -name EDIT
!!
!!                     All other options will be ignored. This must be run
!!                     in an xterm(1) window.
!!
!!    --help           display help and exit
!!    --version        display version information and exit
!!
!!    The following options will return the current value if given a null
!!    value:
!!
!!    -fn
!!    -bg -fg -cr
!!    -rows -cols
!!    -down -right
!!    -iconstate
!!    -cn ''|0 1 2 ...
!!
!!    By itself, -cn will list the first 16 pen colors. Given a list of
!!    numbers, it will query those specific pen numbers.
!!
!!##VARIABLES
!!    By default esc(1) writes output to the current stdout file. The
!!    environment variable OTHERTTY can be used to change the default
!!    file. This is commonly required before starting programs that
!!    filter stdout, such as tmux(1) and screen(1).
!!
!!       export OTHERTTY=/dev/pts/1
!!
!!##EXAMPLE
!!
!!   Sample usage:
!!
!!    # if do not have xterm settings set might need to enter
!!    esc -xrdb|xrdb -merge
!!    # and then try the commands in xterm(1) windows launched after this
!!
!!    esc # bring up screen interface
!!    esc -fn '*--14*-c-*'  # find a font of specified size
!!    esc -fn 5  # set to fifth font in font menu (ctrl-Mouse2)
!!    esc -fsize 20 # set to first 20-point font found (if any)
!!    esc -rows 40 -cols 132 # set terminal size
!!    esc -bg brown -fg white -cr red
!!    esc -bg '#ff00ff'
!!    esc -cn 0 red 1 green 2 cyan
!!    esc -alias >x;source x;rm x # set up a lot of bash shell aliases
!!
!!    # set up X11 resources so name BROWN sets some favorite defaults
!!    esc -bg brown -fg black -fn 5 -xrdb BROWN >>$HOME/.Xdefaults
!!    xrdb -merge $HOME/.Xdefaults
!!    xterm -name BROWN
!!
!!    C-shell
!!
!!       alias cd 'cd \!*; esc -nt'
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        esc(1)>',&
'@(#)DESCRIPTION:    set xterm(1) window attributes using a screen or line mode.>',&
'@(#)VERSION:        1.0, 20180408>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2022-11-11 14:31:26 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine esc
implicit none
character(len=:),allocatable   :: answers
character(len=:),allocatable   :: fn
character(len=:),allocatable   :: array(:) ! output array of tokens
character(len=30)              :: fontname
character(len=:),allocatable   :: background
character(len=:),allocatable   :: foreground
character(len=:),allocatable   :: cursor
character(len=:),allocatable   :: keyword
character(len=:),allocatable   :: label
character(len=10)              :: menu_colors(7)=[character(len=10) ::'red','green','gray','white','black','yellow','brown']
character(len=1)               :: paws
integer                        :: i,j,k
integer                        :: fontsize
integer                        :: num_rows,num_cols
integer                        :: num_down,num_right
integer                        :: cnums(4)=[72,80,128,132]
integer                        :: rnums(4)=[24,32,40,60]
integer                        :: ios
logical                        :: debug
logical                        :: noscreen

integer,allocatable            :: icache(:)
character(len=:),allocatable   :: cache

   noscreen=.false.

   call kracken('esc','-rows -1 -cols -1 -down -999999 -right -999999 &
        & -fn "#N#" -fsize -1 &
        & -bg "#N#" -fg "#N#" -cr "#N#" -cn "#N#"&
        & -iconstate "#N#" &
        & -title "#N#" -name "#N#" -nt "#N#" &
        & -help .f. -version .f. -debug F -alias F -xrdb "#N#" -o ')
   call help_usage(lget('esc_help'))                             ! if -help option is present, display help text and exit
   call help_version(lget('esc_version'))                        ! if -version option is present, display version text and exit
!===================================================================================================================================
   if(lget('esc_alias'))then                                     ! display bash shell aliases to source
      call alias()
      stop
   endif
!===================================================================================================================================
   cache=trim(sget('esc_xrdb'))                                  ! display X11 resources for current xterm for xrdb to read
   if(cache.ne.'#N#')then
      if(cache.eq.'')cache='XTerm'
      call xterm_xrdb(cache)
      noscreen=.true.
      stop
   endif
!===================================================================================================================================
   debug=lget('esc_debug')
   background = trim(sget('esc_bg'))
   foreground = trim(sget('esc_fg'))
   cursor = trim(sget('esc_cr'))
!===================================================================================================================================
   array=sgets('esc_oo')
   do i=1,size(array)                                            ! process keywords
      keyword=trim(array(i))
      select case(keyword)
      case('80')   ; call xterm_width(80); noscreen=.true.
      case('132')  ; call xterm_width(132); noscreen=.true.
      case('')
      case default
         call xterm_keywords(keyword); noscreen=.true.
      end select
   enddo
!===================================================================================================================================
   label=trim(sget('esc_title'))
   if(label.ne.'#N#')then
      call xterm_labels('title',label)                           ! set window title
      noscreen=.true.
   endif
!===================================================================================================================================
   label=trim(sget('esc_name'))
   if(label.ne.'#N#')then
      call xterm_labels('name',label)                            ! set window icon name
      noscreen=.true.
   endif
!===================================================================================================================================
   label=trim(sget('esc_nt'))
   if(label.ne.'#N#')then
      call xterm_labels('nt',label)                              ! set window icon name and title
      noscreen=.true.
   endif
!===================================================================================================================================
   cache=trim(sget('esc_cn'))
   if(cache.eq.'')then                                           ! if -cn with no parameters list first sixteen pen values
      do i=0,15
         cache=xterm_get_pencolor(i)
         write(stdout,'("pen",i0,"=",a)')i,cache
      enddo
      noscreen=.true.
   endif
!===================================================================================================================================
   !elseif(all(isdigit(switch(nospace(cache)))))then
   if(all(isdigit(switch(cache)).or.isspace(switch(cache))))then ! if all non-space characters are digits list those pen numbers
      icache=int(s2vs(cache))
      do i=1,size(icache)
         cache=xterm_get_pencolor(icache(i))
         write(stdout,'("pen",i0,"=",a)')icache(i),cache
      enddo
      noscreen=.true.
   elseif(cache.ne.'#N#')then                             ! assume this is a string of form NNNN #RRGGBB|name|rgb:/RR/GG/BB ...
           write(*,*)'CACHE=',cache
      call split(cache,array,' ')                         ! parse string into an array using specified delimiters
      write(*,*)'SIZE=',size(array),size(array)/2
      do i=1,(size(array)/2)*2,2
         call xterm_pencolor(int(s2v(array(i))),array(i+1))
      enddo
      noscreen=.true.
   endif
!===================================================================================================================================
   cache=trim(sget('esc_o'))
   if(cache.ne.' ')then
      call xterm_occupancy(trim(cache))
      noscreen=.true.
   endif
!===================================================================================================================================
   if(background.eq.'#N#')then
   elseif(background.eq.'')then
      cache=xterm_get_colors('background')
      write(stdout,'("background=",a)')cache
      noscreen=.true.
   elseif(background.ne.'')then
      call xterm_colors('background',background)
      noscreen=.true.
   endif
!===================================================================================================================================
   if(foreground.eq.'#N#')then
   elseif(foreground.eq.'')then
      cache=xterm_get_colors('foreground')
      write(stdout,'("foreground=",a)')cache
      noscreen=.true.
   elseif(foreground.ne.'')then
      call xterm_colors('foreground',foreground)
      noscreen=.true.
   endif
!===================================================================================================================================
   if(cursor.eq.'#N#')then
   elseif(cursor.eq.'')then
      cache=xterm_get_colors('cursor')
      write(stdout,'("cursor=",a)')cache
      noscreen=.true.
   elseif(cursor.ne.'')then
      call xterm_colors('cursor',cursor)
      noscreen=.true.
   endif
!===================================================================================================================================
POSITION: block
   num_right= iget('esc_right')
   num_down= iget('esc_down')

   if(sget('esc_right').eq.'')then
      call xterm_get_position(num_right,num_down)
      write(stdout,'("right=",i0)')num_right
      noscreen=.true.
   elseif(num_right.ge.-99999)then
      call xterm_position(right=num_right)
      noscreen=.true.
   endif


   if(sget('esc_down').eq.'')then
      call xterm_get_position(num_right,num_down)
      write(stdout,'("down=",i0)')num_down
      noscreen=.true.
   elseif(num_down.ge.-99999)then
      call xterm_position(down=num_down)
      noscreen=.true.
   endif

endblock POSITION
!===================================================================================================================================
   num_rows= iget('esc_rows')
   if(num_rows.gt.0)then
      call xterm_geometry(rows=num_rows)
      noscreen=.true.
   elseif(num_rows.eq.0)then
      call xterm_get_geometry(num_rows,num_cols)
      write(stdout,'("rows=",i0)')num_rows
      noscreen=.true.
   endif
!===================================================================================================================================
   num_cols= iget('esc_cols')
   if(num_cols.gt.0)then
      call xterm_geometry(cols=num_cols)
      noscreen=.true.
   elseif(num_cols.eq.0)then
      call xterm_get_geometry(rows=num_rows,cols=num_cols)
      write(stdout,'("cols=",i0)')num_cols
      noscreen=.true.
   endif
!===================================================================================================================================
   fn = trim(sget('esc_fn'))
   if(fn.eq.'#N#')then   ! nothing specified
   elseif(fn.eq.'')then  ! query
      fn=xterm_get_font()
      write(stdout,'("font=",a)')fn
      noscreen=.true.
   elseif(fn.ne.'')then  ! name given
      call xterm_font(fn)
      noscreen=.true.
   endif
!===================================================================================================================================
   fontsize= iget('esc_fsize')
   if(fontsize.ge.0)then
      write(fontname,'(a,i0,a)')'*--',fontsize,'-*-c-*'
      call xterm_font(fontname)
      noscreen=.true.
   endif
!===================================================================================================================================
   cache=sget('esc_iconstate')
   if(cache.eq.'#N#')then
   elseif(cache.eq.'')then
      cache=xterm_get_iconstate()
      write(stdout,'("iconstate=",a)')cache
      noscreen=.true.
   endif
!===================================================================================================================================
if(noscreen)then                                                 ! if no options were specified display screen panel else stop
      stop
   endif
!===================================================================================================================================
   page_ptr=>page_pd
   icount_ptr=>icount_pd
   page_ptr(:)=[ character(len=132) :: &
"################################################################################", &
"#  @BACKGROUND COLOR:@________________                                         #", &
"#  @FOREGROUND COLOR:@________________     ~##Set xterm(1) options:##########~ #", &
"#  @CURSOR COLOR:@    ________________     ~# This program allows you to    #~ #", &
"#   BG FG CS                               ~# select common xterm(1)        #~ #", &
"#   #^ #^ #^ `aRED   `                     ~# options. You may select       #~ #", &
"#   #^ #^ #^ `bGREEN `                     ~# window size, colors, and font #~ #", &
"#   #^ #^ #^ `cGRAY  `                     ~# size. Colors may be selected  #~ #", &
"#   #^ #^ #^ `dWHITE `                     ~# by name or from a list of     #~ #", &
"#   #^ #^ #^ `eBLACK `                     ~# common colors.                #~ #", &
"#   #^ #^ #^ `fYELLOW`                     ~#################################~ #", &
"#   #^ #^ #^ `gBROWN `                                                         #", &
"#              ##############################################################  #", &
"#  @FONT SIZE:@^6 ^7 ^8 ^9^10^11^12^13^14^15^16^17^18^19^20^21^22^23^24^25^26  #", &
"#                                                                              #", &
"#                     #################                        ##############  #", &
"#  @WIDTH: @_____ or  #  ^  ^  ^   ^  #      @HEIGHT:@_____ or # ^  ^  ^  ^ #  #", &
"#                     # 72 80 128 132 #                        # 24 32 40 60#  #", &
"#                     #################                        ##############  #", &
"#                                                                              #", &
"################################################################################", &
"@COLOR NAMES FOR USE IN ABOVE ENTRIES@                                          ", &
"snow               ghostwhite            whitesmoke       gainsboro             ", &
"floralwhite        oldlace               linen            antiquewhite          ", &
"papayawhip         blanchedalmond        bisque           peachpuff             ", &
"navajowhite        moccasin              cornsilk         ivory                 ", &
"lemonchiffon       seashell              honeydew         mintcream             ", &
"azure              aliceblue             lavender         lavenderblush         ", &
"mistyrose          white                 black            darkslategray         ", &
"dimgray            slategray             lightslategray   gray                  ", &
"lightgray          midnightblue          navy             navyblue              ", &
"cornflowerblue     darkslateblue         slateblue        mediumslateblue       ", &
"lightslateblue     mediumblue            royalblue        blue                  ", &
"dodgerblue         deepskyblue           skyblue          lightskyblue          ", &
"steelblue          lightsteelblue        lightblue        powderblue            ", &
"paleturquoise      darkturquoise         mediumturquoise  turquoise             ", &
"cyan               lightcyan             cadetblue        mediumaquamarine      ", &
"aquamarine         darkgreen             darkolivegreen   darkseagreen          ", &
"seagreen           mediumseagreen        lightseagreen    palegreen             ", &
"springgreen        lawngreen             green            chartreuse            ", &
"mediumspringgreen  greenyellow           limegreen        yellowgreen           ", &
"forestgreen        olivedrab             darkkhaki        khaki                 ", &
"palegoldenrod      lightgoldenrodyellow  lightyellow      yellow                ", &
"gold               lightgoldenrod        goldenrod        darkgoldenrod         ", &
"rosybrown          indianred             saddlebrown      sienna                ", &
"peru               burlywood             beige            wheat                 ", &
"sandybrown         tan                   chocolate        firebrick             ", &
"brown              darksalmon            salmon           lightsalmon           ", &
"orange             darkorange            coral            lightcoral            ", &
"tomato             orangered             red              hotpink               ", &
"deeppink           pink                  lightpink        palevioletred         ", &
"maroon             mediumvioletred       violetred        magenta               ", &
"violet             plum                  orchid           mediumorchid          ", &
"darkorchid         darkviolet            blueviolet       purple                ", &
"mediumpurple       thistle               snow1            snow2                 ", &
"snow3              snow4                 seashell1        seashell2             ", &
"seashell3          seashell4             antiquewhite1    antiquewhite2         ", &
"antiquewhite3      antiquewhite4         bisque1          bisque2               ", &
"bisque3            bisque4               peachpuff1       peachpuff2            ", &
"peachpuff3         peachpuff4            navajowhite1     navajowhite2          ", &
"navajowhite3       navajowhite4          lemonchiffon1    lemonchiffon2         ", &
"lemonchiffon3      lemonchiffon4         cornsilk1        cornsilk2             ", &
"cornsilk3          cornsilk4             ivory1           ivory2                ", &
"ivory3             ivory4                honeydew1        honeydew2             ", &
"honeydew3          honeydew4             lavenderblush1   lavenderblush2        ", &
"lavenderblush3     lavenderblush4        mistyrose1       mistyrose2            ", &
"mistyrose3         mistyrose4            azure1           azure2                ", &
"azure3             azure4                slateblue1       slateblue2            ", &
"slateblue3         slateblue4            royalblue1       royalblue2            ", &
"royalblue3         royalblue4            blue1            blue2                 ", &
"blue3              blue4                 dodgerblue1      dodgerblue2           ", &
"dodgerblue3        dodgerblue4           steelblue1       steelblue2            ", &
"steelblue3         steelblue4            deepskyblue1     deepskyblue2          ", &
"deepskyblue3       deepskyblue4          skyblue1         skyblue2              ", &
"skyblue3           skyblue4              lightskyblue1    lightskyblue2         ", &
"lightskyblue3      lightskyblue4         slategray1       slategray2            ", &
"slategray3         slategray4            lightsteelblue1  lightsteelblue2       ", &
"lightsteelblue3    lightsteelblue4       lightblue1       lightblue2            ", &
"lightblue3         lightblue4            lightcyan1       lightcyan2            ", &
"lightcyan3         lightcyan4            paleturquoise1   paleturquoise2        ", &
"paleturquoise3     paleturquoise4        cadetblue1       cadetblue2            ", &
"cadetblue3         cadetblue4            turquoise1       turquoise2            ", &
"turquoise3         turquoise4            cyan1            cyan2                 ", &
"cyan3              cyan4                 darkslategray1   darkslategray2        ", &
"darkslategray3     darkslategray4        aquamarine1      aquamarine2           ", &
"aquamarine3        aquamarine4           darkseagreen1    darkseagreen2         ", &
"darkseagreen3      darkseagreen4         seagreen1        seagreen2             ", &
"seagreen3          seagreen4             palegreen1       palegreen2            ", &
"palegreen3         palegreen4            springgreen1     springgreen2          ", &
"springgreen3       springgreen4          green1           green2                ", &
"green3             green4                chartreuse1      chartreuse2           ", &
"chartreuse3        chartreuse4           olivedrab1       olivedrab2            ", &
"olivedrab3         olivedrab4            darkolivegreen1  darkolivegreen2       ", &
"darkolivegreen3    darkolivegreen4       khaki1           khaki2                ", &
"khaki3             khaki4                lightgoldenrod1  lightgoldenrod2       ", &
"lightgoldenrod3    lightgoldenrod4       lightyellow1     lightyellow2          ", &
"lightyellow3       lightyellow4          yellow1          yellow2               ", &
"yellow3            yellow4               gold1            gold2                 ", &
"gold3              gold4                 goldenrod1       goldenrod2            ", &
"goldenrod3         goldenrod4            darkgoldenrod1   darkgoldenrod2        ", &
"darkgoldenrod3     darkgoldenrod4        rosybrown1       rosybrown2            ", &
"rosybrown3         rosybrown4            indianred1       indianred2            ", &
"indianred3         indianred4            sienna1          sienna2               ", &
"sienna3            sienna4               burlywood1       burlywood2            ", &
"burlywood3         burlywood4            wheat1           wheat2                ", &
"wheat3             wheat4                tan1             tan2                  ", &
"tan3               tan4                  chocolate1       chocolate2            ", &
"chocolate3         chocolate4            firebrick1       firebrick2            ", &
"firebrick3         firebrick4            brown1           brown2                ", &
"brown3             brown4                salmon1          salmon2               ", &
"salmon3            salmon4               lightsalmon1     lightsalmon2          ", &
"lightsalmon3       lightsalmon4          orange1          orange2               ", &
"orange3            orange4               darkorange1      darkorange2           ", &
"darkorange3        darkorange4           coral1           coral2                ", &
"coral3             coral4                tomato1          tomato2               ", &
"tomato3            tomato4               orangered1       orangered2            ", &
"orangered3         orangered4            red1             red2                  ", &
"red3               red4                  deeppink1        deeppink2             ", &
"deeppink3          deeppink4             hotpink1         hotpink2              ", &
"hotpink3           hotpink4              pink1            pink2                 ", &
"pink3              pink4                 lightpink1       lightpink2            ", &
"lightpink3         lightpink4            palevioletred1   palevioletred2        ", &
"palevioletred3     palevioletred4        maroon1          maroon2               ", &
"maroon3            maroon4               violetred1       violetred2            ", &
"violetred3         violetred4            magenta1         magenta2              ", &
"magenta3           magenta4              orchid1          orchid2               ", &
"orchid3            orchid4               plum1            plum2                 ", &
"plum3              plum4                 mediumorchid1    mediumorchid2         ", &
"mediumorchid3      mediumorchid4         darkorchid1      darkorchid2           ", &
"darkorchid3        darkorchid4           purple1          purple2               ", &
"purple3            purple4               mediumpurple1    mediumpurple2         ", &
"mediumpurple3      mediumpurple4         thistle1         thistle2              ", &
"thistle3           thistle4              gray0            gray1                 ", &
"gray2              gray3                 gray4            gray5                 ", &
"gray6              gray7                 gray8            gray9                 ", &
"gray10             gray11                gray12           gray13                ", &
"gray14             gray15                gray16           gray17                ", &
"gray18             gray19                gray20           gray21                ", &
"gray22             gray23                gray24           gray25                ", &
"gray26             gray27                gray28           gray29                ", &
"gray30             gray31                gray32           gray33                ", &
"gray34             gray35                gray36           gray37                ", &
"gray38             gray39                gray40           gray41                ", &
"gray42             gray43                gray44           gray45                ", &
"gray46             gray47                gray48           gray49                ", &
"gray50             gray51                gray52           gray53                ", &
"gray54             gray55                gray56           gray57                ", &
"gray58             gray59                gray60           gray61                ", &
"gray62             gray63                gray64           gray65                ", &
"gray66             gray67                gray68           gray69                ", &
"gray70             gray71                gray72           gray73                ", &
"gray74             gray75                gray76           gray77                ", &
"gray78             gray79                gray80           gray81                ", &
"gray82             gray83                gray84           gray85                ", &
"gray86             gray87                gray88           gray89                ", &
"gray90             gray91                gray92           gray93                ", &
"gray94             gray95                gray96           gray97                ", &
"gray98             gray99                gray100          darkgray              ", &
"darkblue           darkcyan              darkmagenta      darkred               ", &
"lightgreen         silver                teal             olive                 ", &
"lime               aqua                  fuchsia                                ", &
"                                                                                ", &
" -xrdb         # For esc(1) to work with some xterm versions, use this once     ", &
"               # after you start your X11 Windows client or add the             ", &
"               # following X11 Windows resources to ~.Xresources:               ", &
"   *VT100.allowWindowOps: true                                                  ", &
"   *VT100.allowTitleOps:  true                                                  ", &
"   *VT100.allowFontOps:   true                                                  ", &
"################################################################################"]
   icount_ptr=180                       ! rows in form
   longline_pd=80                       ! width of form
   answers=''
   do
      call fixedform(tabs=answers)      ! display form for input
      call split(answers,array,char(9)) ! parse string into an array using specified delimiters

     ! COLORS
      background=adjustl(trim(array(1)))
      foreground=adjustl(trim(array(2)))
      cursor=adjustl(trim(array(3)))
      if(debug)write(*,*)':',background,':',foreground,':',cursor,':'
      k=4
      if(debug)write(*,'(a)',advance='no')'BFC='
      do j=1,7
         do i=1,3
            if(debug)write(*,'(a)',advance='no')trim(array(k))
            if(trim(array(k)).eq.'T')then
               if(i.eq.1.and.background.eq.'')background=menu_colors(j)
               if(i.eq.2.and.foreground.eq.'')foreground=menu_colors(j)
               if(i.eq.3.and.cursor.eq.'')cursor=menu_colors(j)
            endif
            k=k+1
         enddo
      enddo
      if(debug)write(*,*)
      if(debug)write(*,*)':',background,':',foreground,':',cursor,':'
      if(background.ne.'')call xterm_colors('background',background)
      if(foreground.ne.'')call xterm_colors('foreground',foreground)
      if(cursor.ne.'')call xterm_colors('cursor',cursor)

      ! FONTS
      if(debug)write(*,'(a)',advance='no')'FONT='
      do i=k,k+20
          if(debug)write(*,'(a)',advance='no')trim(array(k))
          if(trim(array(i)).eq.'T')then
             fontsize=i-k+6
             if(debug)write(*,*)'FONTSIZE=',fontsize
             if(debug)write(fontname,'(a,i0,a)')'*--',fontsize,'-*-c-*'
             if(debug)write(*,*)'FONTNAME=',fontname
             call xterm_font(fontname)
          endif
      enddo
      if(debug)write(*,*)

     ! GEOMETRY
      if(debug)write(*,*)'COLS=',trim(array(46))
      num_cols=int(s2v(array(46)))
      if(num_cols.gt.0)then
         call xterm_geometry(cols=num_cols)
      else
         do i=47,50
            if(trim(array(i)).eq.'T')then
               num_cols=cnums(i-47+1)
               call xterm_geometry(cols=num_cols)
            endif
         enddo
      endif

      if(debug)write(*,*)'ROWS=',trim(array(51))
      num_rows=int(s2v(array(51)))
      if(num_rows.gt.0)then
         call xterm_geometry(rows=num_rows)
      else
         do i=52,55
            if(trim(array(i)).eq.'T')then
               num_rows=rnums(i-52+1)
               call xterm_geometry(rows=num_rows)
            endif
         enddo
      endif

   write(*,'(a)',advance='no') 'Enter return to continue ...'
   read(*,'(a)',iostat=ios) paws

   enddo
end subroutine esc
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine alias()
implicit none
integer                      :: i
character(len=:),allocatable :: text(:)
text=[ CHARACTER(LEN=128) :: &
'alias 80=''esc 80''',&
'alias 132=''esc 132''',&
'alias lower=''esc lower''',&
'alias raise=''esc raise''',&
'alias icon=''esc iconify''',&
'alias unicon=''esc uniconify''',&
'alias 0=''esc -fn 0''',&
'alias 1=''esc -fn 1''',&
'alias 2=''esc -fn 2''',&
'alias 3=''esc -fn 3''',&
'alias 4=''esc -fn 4''',&
'alias 5=''esc -fn 5''',&
'alias 6=''esc -fn 6''',&
'alias 7=''esc -fn 7''',&
'alias 8=''esc -fn 8''',&
'alias small=''esc -rows 24 -cols 80''',&
'alias full=''esc maximize''',&
'alias fullback=''esc restore''',&
'################################################################################',&
'# some favorite terminal configurations as examples',&
'alias default="esc \',&
'   -fn ''*-cronyx-courier-medium-r-normal--17-120-100-100-m-90-koi8-r'' \',&
'   -rows 36 -cols 132 \',&
'   -down 0 -right 0 \',&
'   -bg black -fg white -cr red \',&
'   -cn \',&
'      0  rgb:0000/0000/0000 \',&
'      1  rgb:cdcd/0000/0000 \',&
'      2  rgb:0000/cdcd/cdcd \',&
'      3  rgb:cdcd/cdcd/0000 \',&
'      4  rgb:0000/0000/eeee \',&
'      5  rgb:cdcd/0000/cdcd \',&
'      6  rgb:0000/cdcd/0000 \',&
'      7  rgb:e5e5/e5e5/e5e5 \',&
'      8  rgb:7f7f/7f7f/7f7f \',&
'      9  rgb:ffff/0000/0000 \',&
'      10 rgb:0000/ffff/0000 \',&
'      11 rgb:ffff/ffff/0000 \',&
'      12 rgb:5c5c/5c5c/ffff \',&
'      13 rgb:ffff/0000/ffff \',&
'      14 rgb:0000/ffff/ffff \',&
'      15 rgb:ffff/ffff/ffff',&
'"',&
'alias green=''esc -rows 24 -cols 80 -fn huge -bg green4 -fg yellow -cr red''',&
'alias brown=''esc -fn huge -bg brown4 -fg white -cr red''',&
'################################################################################',&
'function ID(){',&
'# color terminal according to which cluster logged onto',&
'case $(hostname) in',&
'b15*) set -bg yellow -fg black;;',&
'b16*) set -bg brown  -fg white;;',&
'b17*) set -bg white  -fg black;;',&
'*) default;;',&
'esac',&
'}',&
'################################################################################',&
'function kolor(){',&
'   # set foreground and background color',&
'   case $# in',&
'   0) showrgb ;;',&
'   1) esc -bg $1        ;;',&
'   2) esc -bg $1 -fg $2 ;;',&
'   3) esc -bg $1 -fg $2 -cr $3;;',&
'   *) esc -bg $1 -fg $2 -cr $3;;',&
'   esac',&
'}',&
'################################################################################',&
'function trykolor(){',&
'# try all named background colors',&
'TTY=$(tty)',&
'showrgb|while read R G B COLOR_NAME',&
'do',&
'   [ "$COLOR_NAME" = '''' ] && continue',&
'   esc -bg $COLOR_NAME',&
'   printf ''\nColor %s Next ...'', "$COLOR_NAME"',&
'   read PAWS < $TTY',&
'done',&
'}',&
'################################################################################',&
'tryfont(){',&
'list and select all fixed-space fonts until prompted to stop',&
'export TTY="`tty`"',&
'(xlsfonts "*-${1}-*-c-*"; xlsfonts "*-${1}-*-m-*") 2>/dev/null| while read FONT',&
'do',&
'   echo " $FONT"',&
'   esc -fn ''*''"$FONT"',&
'   printf ''Keep? (y or n):''',&
'   read PAUSE < $TTY',&
'   case "$PAUSE" in',&
'   y*|Y*) break;;',&
'   esac',&
'done',&
'}',&
'################################################################################',&
'']
!!write(*,'(a)')text
write(*,'(a)')(trim(text(i)),i=1,size(text))
end subroutine alias
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
end program prg_esc
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
! black
! white
! red
! T F F
! F T F
! F F T
! F F F
! F F F
! F F F
! F F F
! F F F F F F F F F T F F F F F F F F F F F
! 111
! F F F T
! 2222
! F F T F
