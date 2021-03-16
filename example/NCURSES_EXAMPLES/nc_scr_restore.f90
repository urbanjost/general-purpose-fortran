program nc_scr_restore
!(LICENSE:PD)
   use M_ncurses
   character(len=7),parameter :: FILENAME="windump"
   integer :: r
   integer :: ierr
   stdscr=initscr()
   ierr=addstr("Press Enter to restore the screen"//C_NEW_LINE//C_NULL_CHAR)
   ierr=refresh()
   ierr=getch()
   r = scr_init(FILENAME) ! restore the window from disk
   if( r .ne. ERR)then
      ierr=scr_restore(FILENAME)
      ierr=wrefresh(curscr)
   else
      ierr=addstr("Error reading window file: press Enter"//C_NEW_LINE//C_NULL_CHAR)
      ierr=refresh()
   endif
   ierr=getch()
   ierr=endwin()
end program nc_scr_restore
