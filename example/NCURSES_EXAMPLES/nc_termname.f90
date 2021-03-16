program demo_termname
!(LICENSE:PD)
! @(#) query terminal type set using termname(3f)
use M_ncurses
implicit none
integer  :: ierr
   stdscr=initscr()
   ierr=cbreak()
   ierr=noecho()
   ierr=printw("The term name is "//termname()//C_NEW_LINE//C_NULL_CHAR)
   ierr=refresh()
   ierr=getch()
   ierr=endwin()
end program demo_termname
