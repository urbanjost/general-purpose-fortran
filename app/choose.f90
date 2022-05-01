! @(#) draw a simple menu using ncurses(3c) from Fortran
! differences between Fortran and C usage
! o  printw(3c) is not implemented; do internal WRITE(3f) into a character variable and call addstr(3c)
! o  add C_NULL_CHAR to the end of strings when calling addstr(3c)
! o  C arrays start at 0, Fortran at 1 by default; so defined "choices(0:4)" instead of "choices(5)"
!
! NOTE: Assuming at least an 80x24 character window
!-----------------------------------------------------------------------------------------------------------------------------------
module m_simple_key
use iso_fortran_env, only : ERROR_UNIT        ! access computing environment
use M_strings, only : upper
use M_ncurses
   character(len=:),allocatable :: choices(:)
   integer           :: n_choices                                                                  ! number of choices in menu
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine print_menu(menu_win, highlight)                        ! draw a menu using the list in choices(), highlighting one choice
   implicit none
   type(C_PTR)         :: menu_win                                ! this is the subwindow to draw in
   integer             :: highlight                               ! this says which choice description to highlight
   integer,save        :: x=2                                     ! x position relative to left edge of subwindow to print choices
   integer             :: y                                       ! y position where to display next choice description
   integer             :: i                                       ! loop counter
   integer             :: ierr                                    ! used to hold the return value of many curses(3f) functions
   y=2
   ierr=box(menu_win, 0_C_LONG, 0_C_LONG)                             ! outline the subwindow with a box
   do i=0,n_choices-1,1                                               ! draw the menu
      if(highlight == i + 1)then                                      ! Highlight the present choice
         ierr=wattron(menu_win, A_REVERSE)                            ! turn on highlighting
         ierr=mvwaddstr(menu_win,y, x, choices(i)//C_NULL_CHAR)       ! print choice description
         ierr=wattroff(menu_win, A_REVERSE)                           ! turn off highlighting
      else
         ierr=mvwaddstr(menu_win, y, x, choices(i)//C_NULL_CHAR)      ! just print choice description without highlighting
      endif
      y=y+1                                                           ! move down to location to display next menu choice
   enddo
   ierr=wrefresh(menu_win)                                            ! post everything to the real screen
end subroutine print_menu
!-----------------------------------------------------------------------------------------------------------------------------------
end module m_simple_key
!-----------------------------------------------------------------------------------------------------------------------------------
program simple_key
use M_kracken, only : kracken, sgets, lget, sget, iget
use m_simple_key
use m_time, only : system_sleep
implicit none
type (C_PTR)                 :: menu_win
integer                      :: highlight = 1
integer                      :: choice = 0
integer                      :: c
integer                      :: cursor_state
integer                      :: startx, starty
integer                      :: WIDTH=60,HEIGHT=20               ! size of subwindow to create menu in
integer                      :: ierr                             ! used to hold the return value of many curses(3f) functions
integer                      :: i,j
integer                      :: ii
integer                      :: downtime
integer(C_INT)               :: icount=0                         ! number of calls to read an input character and none was found
logical                      :: debug=.false.
type(MEVENT)                 :: mort
character(len=80)            :: string
character(len=:),allocatable :: choices_f(:)
character(len=:),allocatable :: message
character(len=:),allocatable :: index_list
character(len=:),allocatable :: default
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('choose',' -m Choose one ... -t -1 -help .F. -i -version .F. -debug .F. -d')
   call help_usage(lget('choose_help'))                          ! if -help option is present, display help text and exit
   call help_version(lget('choose_version'))                     ! if -version option is present, display version text and exit
   message=trim(sget('choose_m'))
   index_list=upper(trim(sget('choose_i')))                      ! Other keys should just display their keycode values
   downtime=iget('choose_t')                                     ! How long before taking exit
   default=sget('choose_d')                                      ! How long before taking exit
   ! could not find a simple way to define the array with a lower subscript not one
   !!choices_f=[character(len=60) :: sgets('choose_oo',delim=':') ] ! menu choices to display
   choices_f=sgets('choose_oo',delim=':')                           ! menu choices to display
   choices_f=[character(len=60) :: choices_f ]
   if(size(choices_f).eq.0)then                               !! NOT TRUE IF BLANK, EXPECTED IT TO BE
      choices_f=[character(len=8) :: 'Yes','No','Cancel']
   elseif(len(trim(choices_f(1))).eq.0)then
      choices_f=[character(len=8) :: 'Yes','No','Cancel']
   endif
   n_choices = size(choices_f)                             ! get the number of items in the menu
   allocate(character(len=len(choices_f)) :: choices(0:n_choices-1))
   choices(:)=choices_f
   do i=0,size(choices)-1
      do j=1,len(choices)
         if(choices(i)(j:j).eq.char(0))then
            choices(i)(j:j)=' '
         endif
      enddo
   enddo
   deallocate(choices_f)
   if(size(choices).eq.0)then
           choices=['Exit']
   endif
   if(default.eq.'')default=choices(0)
   if(index_list.eq.'')then                                ! if no index list, use first letter of choice strings
      index_list=''
      do i=0,size(choices)-1
         if(len(choices(i)).gt.0)then
            index_list=index_list//upper(choices(i)(1:1))
         else
            index_list=index_list//char(0)
         endif
      enddo
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   stdscr=initscr()                                        ! initialize curses(3c)
   ierr=start_color()
   ierr=init_pair(1_C_SHORT,COLOR_WHITE,COLOR_BLUE)
   ierr=clear()                                            ! clear the screen in case initscr(3c) did not
                                                           ! most curses(3c) programs always call cbreak(3c) and noecho(3c)
                                                           ! to ensure only what is posted by curses to the screen appears
                                                           ! and that the program can read almost any keystroke immediately
   ierr=cbreak()                                           ! Line buffering disabled. pass on most everything immediately
   ierr=noecho()                                           ! tell the system to not echo the user keystrokes so you have control
                                                           ! of what appears
   ierr=keypad(stdscr,TRUE)                                ! set up for mouse
   ierr=mousemask(ALL_MOUSE_EVENTS,C_NULL_PTR)
   cursor_state=curs_set(0)                                ! The curs_set routine sets the cursor state to invisible, normal,
                                                           ! or very visible for visibility equal to 0, 1, or 2 respectively. If
                                                           ! the terminal supports the visibility requested, the previous cursor
                                                           ! state is returned; otherwise, ERR is returned
   if(downtime.ge.0)then
      ierr=halfdelay(1)                                    ! Don't wait for more than 1/10 seconds for a keypress
   endif
   startx = (80-WIDTH)/2                                   ! define the top left corner for a subwindow
   starty = (24-HEIGHT)/2
   if(debug)then
      call numbers()
   endif
   menu_win = newwin(HEIGHT, WIDTH, starty, startx)        ! start a subwindow for the menu to display in
   ierr=wbkgd(menu_win,COLOR_PAIR(1))
   ierr=keypad(menu_win, TRUE)
                                                           ! enables the keypad of the user's terminal.  If enabled
                                                           ! the user can press a function  key (such  as an arrow key)
                                                           ! and wgetch returns a single value representing the function key,
                                                           ! as in KEY_LEFT.
                                                           ! print usage instructions along the top of the main window
   ierr=mvaddstr(0, 5, "Use arrow keys to go up and down, Press enter to select a choice"//C_NULL_CHAR)
   ierr=mvaddstr(1, 5, message//C_NULL_CHAR)
   ierr=refresh()                                          ! make sure everything is posted to the real screen
   call print_menu(menu_win, highlight)                    ! draw the menu with the top choice highlighted
   INFINITE: do                                            ! look for an up or down arrow or return key
      c = wgetch(menu_win)                                 ! get next keypress
      select case(c)
      case(ERR)                                            ! wgetch(3c) did not find that a keyboard event was found
         icount=icount+1                                   ! count number of times nothing happened
         if(downtime.ge.0.and.(icount.gt.downtime*10))then ! idle time exceeded so take default
            choices(0)=default                             ! overwrite first choice with default string . A little kludge
            choice=1                                       ! set reply to first element of choices
            exit INFINITE
         endif
         message='                                                          '
         write(message,101)icount                          ! create a message that tells what happened in this pass thru the loop
         101 format("Nothing happened ",i0," times.")
      case(KEY_UP)                                         ! if the up-arrow was pressed change which item to highlight
         if(highlight == 1)then                            ! already at top of menu and going up
            highlight = n_choices                          ! to wrap down to the bottom choice
         else                                              ! move hightlight up an option
            highlight=highlight-1
         endif
         icount=0
      case(KEY_DOWN)                                       ! move highlight choice according to down-arrow being pressed
         if(highlight == n_choices)then
            highlight = 1
         else
            highlight=highlight+1
         endif
         icount=0
      case(27)                                             ! escape
            choices(0)='EXIT'                              ! overwrite first choice with default string . A little kludge
            choice=1                                       ! set reply to first element of choices
      case(10)                                             ! entered a RETURN so record which choice is highlighted as the selection
         choice = highlight
         icount=0
      case(KEY_MOUSE)
         ierr=getmouse(mort)
         highlight=mort%y-3
         highlight=max(highlight,1)
         highlight=min(highlight,size(choices))
         icount=0
         !--------------------------------------
         ! make it so if mouse click, exit. If you want it to wait for RETURN remove this
         call print_menu(menu_win, highlight)              ! redraw the menu with the highlighting possibly moved
         call system_sleep(0.3)
         choice = highlight
         exit INFINITE
         !--------------------------------------
      case default                                         ! for any other key show the key value and (maybe) the character
         ii=index(index_list,upper(char(c)))
         if(ii.gt.0.and.ii.le.size(choices))then
            choice=ii
            exit INFINITE
         else
            write(string,'("Character pressed is = ",i3," Hopefully it can be printed as ",a)') c,char(c)
            ierr=mvaddstr(23, 5, trim(string)//C_NULL_CHAR)
            ierr=clrtoeol()    ! erase the tail of any old message (erases from current position to the end of the current line).
         endif
         ierr=refresh()                                    ! make sure real screen is up-to-date
         icount=0
      end select
      call print_menu(menu_win, highlight)                 ! redraw the menu with the highlighting possibly moved
      if(choice /= 0)then                                  ! User did a choice so come out of the infinite loop
         exit INFINITE
      endif
   enddo INFINITE
   if(cursor_state >= 0)then                            ! restore cursor state so cursor (probably) shows at end of following output
      ierr=curs_set(cursor_state)
   endif
   ! print information on selection made
   if(debug)then
      write(string,'("You chose choice ",i1," with choice string ",a)') choice, trim(choices(choice-1))
      ierr=mvaddstr(23, 5, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"//C_NULL_CHAR)
      ierr=mvaddstr(23, 5, trim(string)//C_NULL_CHAR)
      ierr=clrtoeol()    ! erase the tail of any old message (erases from current position to the end of the current line).
      ierr=refresh()     ! make sure everything is posted to the real screen
      ierr=getch()       ! Wait for a user keystroke. Some terminal types will restore or clear the screen so a pause is a good idea
   endif
   ierr=endwin()      ! exit curses mode
   write(ERROR_UNIT,'(a)',advance='no') trim(choices(choice-1))
end program simple_key
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine numbers()
   use M_ncurses
   character(len=3)   :: label
   do i=0,999   ! label up to 1000 rows of the main screen with a number on the left edge
      write(label,'(i3.3)')i
      ierr=mvaddstr(i, 0, trim(label)//C_NULL_CHAR)
      if(ierr.lt.0) exit ! if printed off-screen then stop
   enddo
   ierr=refresh()   ! make sure everything is posted to the real screen
end subroutine numbers
!-----------------------------------------------------------------------------------------------------------------------------------
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
'   choose(1f) - [NCURSES] select one item from a menu using a screen interface                                                  ',&
'   (LICENSE:PD)                                                                                                                 ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'   choose  [choices] [ -i index] [ -t timeout ] [ -m text] [ -d default]|[ -help|--version]                                     ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'    choose(1f) allows users to select one item from a list of choices                                                           ',&
'    and returns the prompt of the selected choice on stderr. If the escape                                                      ',&
'    key is struck the return string is ''EXIT'' and the program terminates.                                                     ',&
'    Otherwise, the highlighted string is returned when a RETURN is entered.                                                     ',&
'    Cursor keys may be used to scroll thru the choices. The current                                                             ',&
'    selection is highlighted.                                                                                                   ',&
'                                                                                                                                ',&
'    Hot keys are generated by using the first character of each choice                                                          ',&
'    (the default) or from the string provided for the -i option.                                                                ',&
'                                                                                                                                ',&
'    If the mouse is supported by the terminal type being used, clicking on                                                      ',&
'    a menu item selects it and the program then exits.                                                                          ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'   choices     Specifies the list of choices to be created,                                                                     ',&
'               delimited by :. The default list is "Yes:No:Cancel".                                                             ',&
'                                                                                                                                ',&
'               Currently limited to 16 60-character prompts.                                                                    ',&
'                                                                                                                                ',&
'   -i index    optional list of letters to act as case-insensitive                                                              ',&
'               shortcuts (ie. "hot-keys") for menu items. Striking the                                                          ',&
'               key will select the matching string from the choices                                                             ',&
'               and exit. The default is the first letter of each of                                                             ',&
'               the choices.                                                                                                     ',&
'                                                                                                                                ',&
'   -t timeout  The number of 1/10 seconds to pause before a default                                                             ',&
'               choice is made. Acceptable values are from 0 to 9999.                                                            ',&
'   -d default  If a timeout occurs the default is to return the first                                                           ',&
'               string in the choices. This allows you to specify a                                                              ',&
'               string specifically returned by a timeout. It is ignored                                                         ',&
'               otherwise.                                                                                                       ',&
'                                                                                                                                ',&
'   -m text     Specifies the message to be displayed before                                                                     ',&
'               the prompt.                                                                                                      ',&
'                                                                                                                                ',&
'   --help      Displays this help message and exit                                                                              ',&
'   --version   Displays version info and exit                                                                                   ',&
'                                                                                                                                ',&
'   NOTE:                                                                                                                        ',&
'                                                                                                                                ',&
'EXAMPLES                                                                                                                        ',&
'  Sample commands                                                                                                               ',&
'                                                                                                                                ',&
'   choose --help                                                                                                                ',&
'                                                                                                                                ',&
'   choose Yes:No:Cancel -i ync -m Select Y for Yes, N for No or C for Cancel                                                    ',&
'                                                                                                                                ',&
'   # time out and return "Yes" if no activity for ten seconds                                                                   ',&
'   choose Yes:No:Cancel -t 10                                                                                                   ',&
'                                                                                                                                ',&
'   # hot-keys will be "12E"                                                                                                     ',&
'   choose ''1) Tomorrow:2) Next Week:Exit'' -m Select when to start ...                                                         ',&
'                                                                                                                                ',&
'  The program writes the selection to stderr. A bash(1) example showing                                                         ',&
'  how to read the output into a variable follows:                                                                               ',&
'                                                                                                                                ',&
'   #!/bin/bash                                                                                                                  ',&
'   # The program writes to stderr. To read the output into a                                                                    ',&
'   # variable but let ncurses(3c) control the screen we need                                                                    ',&
'   # to first perform some redirection to perform a command substitution                                                        ',&
'   # and assign the output of a the command to the variable RESULT.                                                             ',&
'   #                                                                                                                            ',&
'   exec 3>&1 # Duplicate file descriptor 1 on descriptor 3                                                                      ',&
'             # as backup of descriptor 1                                                                                        ',&
'   RESULT=$(choose 2>&1 1>&3)  # run program                                                                                    ',&
'   exec 3>&- # Close file descriptor 3                                                                                          ',&
'   case "$RESULT" in                                                                                                            ',&
'   Yes)    echo "Do stuff when answer is $RESULT" ;;                                                                            ',&
'   No)     echo "Do stuff when answer is $RESULT" ;;                                                                            ',&
'   Cancel) echo "Do stuff when answer is $RESULT" ;;                                                                            ',&
'   EXIT)   echo "Do stuff when answer is escape key ;;                                                                          ',&
'   *)      echo "$0 : ERROR: unknown answer $RESULT" ;;                                                                         ',&
'   esac                                                                                                                         ',&
'   # redirection of descriptor 2 (stderr) to be the duplicate                                                                   ',&
'   # of descriptor 1 and descriptor 1 is restored to its original                                                               ',&
'   # value by duplicating descriptor 3 which contains the backup copy.                                                          ',&
'   exit                                                                                                                         ',&
'                                                                                                                                ',&
'  You can also use scratch files. For example                                                                                   ',&
'                                                                                                                                ',&
'   #!/bin/bash                                                                                                                  ',&
'   # The program writes the chosen string to stderr.                                                                            ',&
'   # Create a temporary file and make sure it goes away when dome                                                               ',&
'   tmp_file=$(tempfile 2>/dev/null) || tmp_file=/tmp/test$$                                                                     ',&
'   trap "rm -f $tmp_file" 0 1 2 5 15                                                                                            ',&
'   choose 2> $tmp_file                                                                                                          ',&
'   RESULT=`cat $tmp_file`                                                                                                       ',&
'   exit                                                                                                                         ',&
'                                                                                                                                ',&
'SEE ALSO                                                                                                                        ',&
'   dialog                                                                                                                       ',&
'                                                                                                                                ',&
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
!!    choose(1f) - [NCURSES] select one item from a menu using a screen interface
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    choose  [choices] [ -i index] [ -t timeout ] [ -m text] [ -d default]|[ -help|--version]
!!
!!##DESCRIPTION
!!     choose(1f) allows users to select one item from a list of choices
!!     and returns the prompt of the selected choice on stderr. If the escape
!!     key is struck the return string is 'EXIT' and the program terminates.
!!     Otherwise, the highlighted string is returned when a RETURN is entered.
!!     Cursor keys may be used to scroll thru the choices. The current
!!     selection is highlighted.
!!
!!     Hot keys are generated by using the first character of each choice
!!     (the default) or from the string provided for the -i option.
!!
!!     If the mouse is supported by the terminal type being used, clicking on
!!     a menu item selects it and the program then exits.
!!
!!##OPTIONS
!!    choices     Specifies the list of choices to be created,
!!                delimited by :. The default list is "Yes:No:Cancel".
!!
!!                Currently limited to 16 60-character prompts.
!!
!!    -i index    optional list of letters to act as case-insensitive
!!                shortcuts (ie. "hot-keys") for menu items. Striking the
!!                key will select the matching string from the choices
!!                and exit. The default is the first letter of each of
!!                the choices.
!!
!!    -t timeout  The number of 1/10 seconds to pause before a default
!!                choice is made. Acceptable values are from 0 to 9999.
!!    -d default  If a timeout occurs the default is to return the first
!!                string in the choices. This allows you to specify a
!!                string specifically returned by a timeout. It is ignored
!!                otherwise.
!!
!!    -m text     Specifies the message to be displayed before
!!                the prompt.
!!
!!    --help      Displays this help message and exit
!!    --version   Displays version info and exit
!!
!!    NOTE:
!!
!!##EXAMPLES
!!
!!   Sample commands
!!
!!    choose --help
!!
!!    choose Yes:No:Cancel -i ync -m Select Y for Yes, N for No or C for Cancel
!!
!!    # time out and return "Yes" if no activity for ten seconds
!!    choose Yes:No:Cancel -t 10
!!
!!    # hot-keys will be "12E"
!!    choose '1) Tomorrow:2) Next Week:Exit' -m Select when to start ...
!!
!!   The program writes the selection to stderr. A bash(1) example showing
!!   how to read the output into a variable follows:
!!
!!    #!/bin/bash
!!    # The program writes to stderr. To read the output into a
!!    # variable but let ncurses(3c) control the screen we need
!!    # to first perform some redirection to perform a command substitution
!!    # and assign the output of a the command to the variable RESULT.
!!    #
!!    exec 3>&1 # Duplicate file descriptor 1 on descriptor 3
!!              # as backup of descriptor 1
!!    RESULT=$(choose 2>&1 1>&3)  # run program
!!    exec 3>&- # Close file descriptor 3
!!    case "$RESULT" in
!!    Yes)    echo "Do stuff when answer is $RESULT" ;;
!!    No)     echo "Do stuff when answer is $RESULT" ;;
!!    Cancel) echo "Do stuff when answer is $RESULT" ;;
!!    EXIT)   echo "Do stuff when answer is escape key ;;
!!    *)      echo "$0 : ERROR: unknown answer $RESULT" ;;
!!    esac
!!    # redirection of descriptor 2 (stderr) to be the duplicate
!!    # of descriptor 1 and descriptor 1 is restored to its original
!!    # value by duplicating descriptor 3 which contains the backup copy.
!!    exit
!!
!!   You can also use scratch files. For example
!!
!!    #!/bin/bash
!!    # The program writes the chosen string to stderr.
!!    # Create a temporary file and make sure it goes away when dome
!!    tmp_file=$(tempfile 2>/dev/null) || tmp_file=/tmp/test$$
!!    trap "rm -f $tmp_file" 0 1 2 5 15
!!    choose 2> $tmp_file
!!    RESULT=`cat $tmp_file`
!!    exit
!!
!!##SEE ALSO
!!    dialog
!!
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
'@(#)PROGRAM:        choose(1)>',&
'@(#)DESCRIPTION:    select an item from a screen menu>',&
'@(#)VERSION:        1.0, 20180331>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2022-04-29 11:56:21 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
