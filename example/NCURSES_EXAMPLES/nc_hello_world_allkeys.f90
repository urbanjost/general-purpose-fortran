program hello_word_ncurses 
!(LICENSE:PD)
! @(#) simple "hello world!" example of calling ncurses(3f) from fortran
   use M_ncurses
   implicit none
   integer(C_INT) :: ierr
   integer(C_INT) :: ic
   stdscr=initscr()
   ierr=noecho()
   ierr=raw()
   ierr=cbreak()             ! Line buffering disabled, Pass on all keyboard events
   ierr=keypad(stdscr, TRUE) ! include function keys and mouse as single events
   if (.not.c_associated(stdscr)) then
      ierr=endwin()
      write(*,*)'could not open standard screen. input not a tty? ERRNO=',ierr
      stop
   endif
   ierr=box(stdscr,0_C_LONG,0_C_LONG)
   call getmaxyx(stdscr,LINES,COLS) !! should not have to set these
   ierr=addstr("Hello World!!!"//C_NEW_LINE//C_NULL_CHAR)
   ierr=printw("SCREEN SIZE:LINES %d COLS %d"//C_NEW_LINE//C_NULL_CHAR,LINES,COLS)
   ierr=refresh()
   ic=getch()
   ierr=endwin()
end program hello_word_ncurses
!-------------------------------------------------------------------------------
