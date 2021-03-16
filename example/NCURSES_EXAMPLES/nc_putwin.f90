program nc_putwin ! @(#) use putwin(3f) to create a window dump to a file
!(LICENSE:PD)
   use M_ncurses
   implicit none
   type(C_PTR)          :: win
   integer              :: ierr
   stdscr=initscr()
   ierr=start_color()
   ierr=init_pair(1_C_SHORT,COLOR_WHITE,COLOR_BLUE)
   ierr=addstr("Creating new window"//C_NEW_LINE//C_NULL_CHAR)
   ierr=refresh()
   win = newwin(5,20,7,30) ! Create the window
   if (.not.c_associated(win)) then
      ierr=endwin()
      write(*,*)'could not create window'
      stop
   endif
   ierr=wbkgd(win,COLOR_PAIR(1))
   ierr=mvwaddstr(win,1,2,"This program was"//C_NEW_LINE//C_NULL_CHAR)
   ierr=mvwaddstr(win,2,5,"created by"//C_NEW_LINE//C_NULL_CHAR)
   ierr=mvwaddstr(win,3,3,"John S. Urban"//C_NEW_LINE//C_NULL_CHAR) ! put your name here
   ierr=wrefresh(win)
   ierr=getch()
   ierr = putwin(win,"window.dat"//C_NULL_CHAR) ! write the window's data 
   if( ierr == ERR)then
      ierr=addstr("Error putting window to disk"//C_NEW_LINE//C_NULL_CHAR)
   else
      ierr=addstr("Window put to disk"//C_NEW_LINE//C_NULL_CHAR)
   endif
   ierr=refresh()
   ierr=getch()
   ierr=endwin()
end program nc_putwin
