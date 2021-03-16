program nc_getwin ! @(#) read a window dump from putwin(3c) using getwin(3c)
!(LICENSE:PD)
   use M_ncurses
   type(C_PTR) :: win
   integer     :: ierr
   stdscr=initscr()
   ierr=start_color()
   ierr=init_pair(1_C_SHORT,COLOR_WHITE,COLOR_RED)
   ierr=addstr("Press Enter to read the window from disk:"//C_NEW_LINE//C_NULL_CHAR)
   ierr=refresh()
   ierr=getch()
   win = getwin("window.dat") ! read the window's data
   if (.not.c_associated(win)) then
      ierr=endwin()
      write(*,*)"Unable to read/create window"
      stop
   endif
   ierr=wrefresh(win)
   ierr=getch()
   ierr=endwin()
end program nc_getwin

