!(LICENSE:PD)
! @(#) draw a simple menu using ncurses(3c) from Fortran
! differences between Fortran and C usage
! printw(3c) is not implimented; do internal WRITE(3f) into a character variable and call addstr(3c)
! add C_NULL_CHAR to the end of strings when calling addstr(3c)
! C arrays start at 0, Fortran at 1 by default; so defined "choices(0:4)" instead of "choices(5)"
!
! NOTE: Assuming at least an 80x24 character window

module m_simple_key
   use M_ncurses
   character(len=8)  :: choices(0:4)=[ "Choice 1", "Choice 2", "Choice 3", "Choice 4", "Exit    "] ! menu choices to display
   integer           :: n_choices                                                                  ! number of choices in menu
contains
!-------------------------------------------------------------------------------
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
         ierr=mvwaddstr(menu_win,y, x, trim(choices(i))//C_NULL_CHAR) ! print choice description
         ierr=wattroff(menu_win, A_REVERSE)                           ! turn off highlighting
      else
         ierr=mvwaddstr(menu_win, y, x, choices(i)//C_NULL_CHAR)      ! just print choice description without highlighting
      endif
      y=y+1                                                           ! move down to location to display next menu choice
   enddo
   ierr=wrefresh(menu_win)                                            ! post everything to the real screen
end subroutine print_menu
!-------------------------------------------------------------------------------
end module m_simple_key
!-------------------------------------------------------------------------------
program simple_key
   use m_simple_key
   implicit none
   type (C_PTR)        :: menu_win
   integer             :: highlight = 1
   integer             :: choice = 0
   integer             :: c
   character(len=80)   :: string
   integer             :: cursor_state
   integer             :: startx, starty
   integer,parameter   :: WIDTH=30,HEIGHT=10               ! size of subwindow to create menu in
   integer             :: ierr                             ! used to hold the return value of many curses(3f) functions

   n_choices = size(choices)                               ! get the number of items in the menu
   stdscr=initscr()                                        ! initialize curses(3c)
   ierr=clear()                                            ! clear the screen in case initscr(3c) did not
                                                           ! most curses(3c) programs always call cbreak(3c) and noecho(3c)
                                                           ! to ensure only what is posted by curses to the screen appears
                                                           ! and that the program can read almost any keystroke immediately
   ierr=cbreak()                                           ! Line buffering disabled. pass on most everything immediately
   ierr=noecho()                                           ! tell the system to not echo the user keystrokes so you have control
                                                           ! of what appears
   cursor_state=curs_set(0)                                ! The curs_set routine sets the cursor state to invisible, normal,
                                                           ! or very visible for visibility equal to 0, 1, or 2 respectively. If
                                                           ! the terminal supports the visibility requested, the previous cursor
                                                           ! state is returned; otherwise, ERR is returned
   startx = (80-WIDTH)/2                                   ! define the top left corner for a subwindow
   starty = (24-HEIGHT)/2
   call numbers()
   menu_win = newwin(HEIGHT, WIDTH, starty, startx)        ! start a subwindow for the menu to display in
   ierr=keypad(menu_win, TRUE)
                                                           ! enables the keypad of the user's terminal.  If enabled
                                                           ! the user can press a function  key (such  as an arrow key)
                                                           ! and wgetch returns a single value representing the function key,
                                                           ! as in KEY_LEFT.
                                                           ! print usage instructions along the top of the main window
   ierr=mvaddstr(0, 5, "Use arrow keys to go up and down, Press enter to select a choice"//C_NULL_CHAR)
   ierr=mvaddstr(1, 5, "Other keys should just display their keycode values"//C_NULL_CHAR)
   ierr=refresh()                                          ! make sure everything is posted to the real screen
   call print_menu(menu_win, highlight)                    ! draw the menu with the top choice highlighted
   INFINITE: do                                            ! look for an up or down arrow or return key
      c = wgetch(menu_win)                                 ! get next keypress
      select case(c)
      case(KEY_UP)                                         ! if the up-arrow was pressed change which item to highlight
         if(highlight == 1)then                            ! already at top of menu and going up
            highlight = n_choices                          ! to wrap down to the bottom choice
         else                                              ! move hightlight up an option
            highlight=highlight-1
         endif
      case(KEY_DOWN)                                       ! move highlight choice according to down-arrow being pressed
         if(highlight == n_choices)then
            highlight = 1
         else
            highlight=highlight+1
         endif
      case(10)                                             ! entered a RETURN so record which choice is highlighted as the selection
         choice = highlight
      case default                                         ! for any other key show the key value and (maybe) the character
         write(string,'("Character pressed is = ",i3," Hopefully it can be printed as ",a)') c,char(c)
         ierr=mvaddstr(23, 5, trim(string)//C_NULL_CHAR)
         ierr=refresh()                                    ! make sure real screen is up-to-date
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
   write(string,'("You chose choice ",i1," with choice string ",a)') choice, trim(choices(choice-1))
   ierr=mvaddstr(23, 5, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"//C_NULL_CHAR)
   ierr=mvaddstr(23, 5, trim(string)//C_NULL_CHAR)
   ierr=clrtoeol()       ! erase the tail of any old message (erases from current position to the end of the current line).
   ierr=refresh()        ! make sure everything is posted to the real screen
   ierr=getch()          ! Wait for a user keystroke. Some terminal types will restore or clear the screen so a pause is a good idea
   ierr=endwin()         ! exit curses mode
end program simple_key
!-------------------------------------------------------------------------------
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
!-------------------------------------------------------------------------------
