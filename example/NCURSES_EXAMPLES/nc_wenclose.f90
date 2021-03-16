program wenclose_test ! @(#) detect when click mouse in a window using wenclose(3c) 
!(LICENSE:PD)
   use M_ncurses
   implicit none
   type(C_PTR)          :: target
   type(MEVENT)         :: eek
   integer              :: ch
   integer              :: ierr
   integer(C_LONG)      :: mouse_mask
   stdscr=initscr()
   ierr=start_color()
   ierr=init_pair(1_C_SHORT,COLOR_WHITE,COLOR_CYAN)
   ierr=noecho()
   ierr=keypad(stdscr,TRUE)
   target = newwin(5,3,9,39)
   ierr=wbkgd(target,COLOR_PAIR(1))
   ierr=wnoutrefresh(stdscr)
   ierr=wnoutrefresh(target)
   ierr=doupdate()
   mouse_mask=mousemask(BUTTON1_CLICKED,C_NULL_PTR)
   ierr=printw("Click the left mouse around the screen. Asterisks should appear where you click"//C_NEW_LINE//C_NULL_CHAR)
   ierr=printw("unless you click in the blue bar, which should cause a beep instead."//C_NEW_LINE//C_NULL_CHAR)
   ierr=printw("The ENTER key exits."//C_NEW_LINE//C_NULL_CHAR)
   do
      ch = getch()
      if( ch == KEY_MOUSE )then
         ierr=getmouse(eek)
         if(wenclose(target,eek%y,eek%x)) then
            ierr=beep()
            ierr=touchwin(target)
            ierr=wnoutrefresh(target)
         else
            ierr=mvaddch(eek%y,eek%x,int(ichar('*'),C_LONG))
            ierr=wnoutrefresh(stdscr)
         endif
         ierr=doupdate()
         cycle
      endif
      if( ch == ichar(C_NEW_LINE) ) exit
   enddo
   ierr=endwin()
end program wenclose_test
