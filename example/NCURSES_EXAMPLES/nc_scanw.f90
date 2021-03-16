program nc_scanw
!(LICENSE:PD)
   use M_ncurses
   real,parameter ::  UNI=4.5
   integer        ::  pieces
   
   stdscr=initscr()
   
   ierr=addstr("SUSHI BAR MENU"//C_NEW_LINE//C_NEW_LINE//C_NULL_CHAR)
   ierr=printw("We have Uni today for $%.2f."//C_NEW_LINE//C_NULL_CHAR,UNI)
   ierr=addstr("How many pieces would you like? "//C_NULL_CHAR)
   ierr=refresh()
   
   ierr=scanw("%d"//C_NULL_CHAR,pieces)
   ierr=printw("You want %d pieces?"//C_NEW_LINE//C_NULL_CHAR,pieces)
   ierr=printw("That will be $%.2f!"//C_NEW_LINE//C_NULL_CHAR,UNI*pieces)
   ierr=refresh()
   ierr=getch()
   
   ierr=endwin()
end program nc_scanw
