program nc_use_default_colors
!(LICENSE:PD)
   use M_ncurses
   implicit none
   integer(C_INT) :: ierr
   stdscr=initscr()
   ierr=start_color() 
   ierr=use_default_colors() 
   ierr=addstr("The default colors have been set to"//C_NEW_LINE//C_NULL_CHAR) 
   ierr=addstr("the same as the terminal's colors."//C_NEW_LINE//C_NULL_CHAR) 
   ierr=refresh() 
   ierr=getch() 
   ierr=endwin() 
end program nc_use_default_colors
