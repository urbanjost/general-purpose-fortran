subroutine attryn(a, c)
!(LICENSE:PD)
! @(#) test termattrs(3f) and query which attributes this terminal type takes
   use M_ncurses
   implicit none
   integer(C_LONG)       :: a     !chtype a
   integer(C_LONG)       :: c     !chtype c
   integer               :: ierr
   !if( ior(a,c)  .eq. a) then
   !if( iand(a,c)  .ne. 0) then  !! this would get A_NORMAL wrong, because it is zero
   if( iand(a,c) .eq. c) then
      ierr=addstr("Yes "//C_NULL_CHAR)
      ierr=attron(c)
      ierr=addstr("abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"//C_NULL_CHAR)
      ierr=attroff(c)
   else
      ierr=addstr("No  "//C_NULL_CHAR)
      ierr=attron(c)
      ierr=addstr("abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"//C_NULL_CHAR)
      ierr=attroff(c)
   endif
   ierr=addch(int(ichar(C_NEW_LINE),C_LONG))
end subroutine attryn

program seeattr
   use M_ncurses
   use ISO_FORTRAN_ENV
   integer(C_LONG)       :: attributes !chtype attributes
   integer(C_INT)        :: y, x
   integer               :: ierr
   integer(C_INT)        :: i
   integer(kind=mmask_t) :: mmask
   stdscr=initscr()
   !if(stdscr == C_NULL_PTR)then
   if (.not.c_associated(stdscr)) then
      ierr=endwin()
      write(*,*)'could not open standard screen. input not a tty? ERRNO=',ierr
      stop
   endif
   ierr=raw()
   ierr=cbreak()    ! Line buffering disabled, Pass on everthing to me
   ierr=noecho()    ! only put on screen what is explicitly placed there, do not echo input
   attributes = termattrs()
   ierr=addstr("This terminal is capable of the following attributes:"//C_NEW_LINE//C_NULL_CHAR)
   ierr=printw("%13s"//C_NULL_CHAR, "AltCharSet: "//C_NULL_CHAR); call attryn(attributes, A_ALTCHARSET)
   ierr=printw("%13s"//C_NULL_CHAR, "Blink: "//C_NULL_CHAR);      call attryn(attributes, A_BLINK)
   ierr=printw("%13s"//C_NULL_CHAR, "Bold: "//C_NULL_CHAR);       call attryn(attributes, A_BOLD)
   ierr=printw("%13s"//C_NULL_CHAR, "Dim: "//C_NULL_CHAR);        call attryn(attributes, A_DIM)
   ierr=printw("%13s"//C_NULL_CHAR, "Invis(ible):"//C_NULL_CHAR); call attryn(attributes, A_INVIS)
   ierr=printw("%13s"//C_NULL_CHAR, "Normal: "//C_NULL_CHAR);     call attryn(attributes, A_NORMAL)
   ierr=printw("%13s"//C_NULL_CHAR, "Reverse: "//C_NULL_CHAR);    call attryn(attributes, A_REVERSE)
   ierr=printw("%13s"//C_NULL_CHAR, "Standout: "//C_NULL_CHAR);   call attryn(attributes, A_STANDOUT)
   ierr=printw("%13s"//C_NULL_CHAR, "Underline: "//C_NULL_CHAR);  call attryn(attributes, A_UNDERLINE)
   ierr=printw("%13s"//C_NULL_CHAR, "Protect: "//C_NULL_CHAR);    call attryn(attributes, A_PROTECT)
   ierr=printw("%13s"//C_NULL_CHAR, "Horizontal: "//C_NULL_CHAR); call attryn(attributes, A_HORIZONTAL)
   ierr=printw("%13s"//C_NULL_CHAR, "Left: "//C_NULL_CHAR);       call attryn(attributes, A_LEFT)
   ierr=printw("%13s"//C_NULL_CHAR, "Low: "//C_NULL_CHAR);        call attryn(attributes, A_LOW)
   ierr=printw("%13s"//C_NULL_CHAR, "Right: "//C_NULL_CHAR);      call attryn(attributes, A_RIGHT)
   ierr=printw("%13s"//C_NULL_CHAR, "Top: "//C_NULL_CHAR);        call attryn(attributes, A_TOP)
   ierr=printw("%13s"//C_NULL_CHAR, "Vertical: "//C_NULL_CHAR);   call attryn(attributes, A_VERTICAL)
   ierr=printw("%13s"//C_NULL_CHAR, "Italic:   "//C_NULL_CHAR);   call attryn(attributes, A_ITALIC)
   !ierr=printw("%13s"//C_NULL_CHAR, "Color:    "//C_NULL_CHAR);   call attryn(attributes, A_COLOR)
   !ierr=printw("%13s"//C_NULL_CHAR, "Attributes:"//C_NULL_CHAR);  call attryn(attributes, A_ATTRIBUTES)
   !ierr=printw("%13s"//C_NULL_CHAR, "CharText:  "//C_NULL_CHAR);  call attryn(attributes, A_CHARTEXT)
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=clear()               ! clear stdscr
   ierr=box(stdscr,0_C_LONG,0_C_LONG)
   ierr=move(0_C_INT,0_C_INT) ! go to top of page
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   call getmaxyx(stdscr,y,x)
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=0,y-1
      ierr=move(i,0)
      ierr=hline(0_C_LONG,x)
   enddo
   ierr=move(0,0)
   ierr=printw("Window size is %d rows, %d columns."//C_NEW_LINE//C_NULL_CHAR,y,x)
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=addstr("This terminal "//C_NULL_CHAR)
   if(has_ic())then
      ierr=addstr("has "//C_NULL_CHAR)
   else
      ierr=addstr("does not have "//C_NULL_CHAR)
   endif
   ierr=addstr("insert/delete character abilities"//C_NEW_LINE//C_NULL_CHAR)
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=addstr("This terminal "//C_NULL_CHAR)
   if(has_il())then
      ierr=addstr("has "//C_NULL_CHAR)
   else
      ierr=addstr("does not have "//C_NULL_CHAR)
   endif
   ierr=addstr("insert/delete line abilities"//C_NEW_LINE//C_NULL_CHAR)
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   if(has_colors())then
      ierr=addstr("This terminal can do colors."//C_NEW_LINE//C_NULL_CHAR)
      if(start_color() .ne. OK) then ! next attempt to initialize curses colors
         ierr=printw("Unable to start colors."//C_NEW_LINE//C_NULL_CHAR)
      else  ! colors are okay; continue
         ierr=printw("Colors have been properly initialized."//C_NEW_LINE//C_NULL_CHAR)
         call getcolor(COLORS,COLOR_PAIRS) !! extension to get these set
         ierr=printw("NCurses reports that you can use %d colors, "//C_NULL_CHAR,COLORS)
         ierr=printw("and %d color pairs."//C_NEW_LINE//C_NULL_CHAR,COLOR_PAIRS)
         ierr=refresh()
         ierr=getch()
         ierr=init_pair(1_C_SHORT, COLOR_BLACK,     COLOR_RED);
         ierr=init_pair(2_C_SHORT, COLOR_RED,       COLOR_GREEN);
         ierr=init_pair(3_C_SHORT, COLOR_GREEN,     COLOR_BLACK);
         ierr=attron(COLOR_PAIR(1))
         ierr=addstr("COLOR_PAIR(1)"//C_NULL_CHAR)
         ierr=attroff(COLOR_PAIR(1))
         ierr=attron(COLOR_PAIR(2))
         ierr=addstr("COLOR_PAIR(2)"//C_NULL_CHAR)
         ierr=attroff(COLOR_PAIR(2))
         ierr=attron(COLOR_PAIR(3))
         ierr=addstr("COLOR_PAIR(3)"//C_NEW_LINE//C_NULL_CHAR)
         ierr=attroff(COLOR_PAIR(3))
      endif
   else
      ierr=addstr("This terminal cannot do colors."//C_NEW_LINE//C_NULL_CHAR)
   endif
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   if(can_change_color())then
      ierr=addstr("This terminal can change the standard colors."//C_NEW_LINE//C_NULL_CHAR)
   else
      ierr=addstr("This terminal cannot change the colors."//C_NEW_LINE//C_NULL_CHAR)
   endif
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=printw("This terminal's baud rate is %d."//C_NEW_LINE//C_NULL_CHAR,baudrate())
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=keypad(stdscr,TRUE)
   mmask = mousemask(ALL_MOUSE_EVENTS,C_NULL_PTR)
   if(mmask == 0)then
      ierr=addstr("Unable to access the mouse on this terminal."//C_NEW_LINE//C_NULL_CHAR)
   else
      ierr=addstr("Mouse events can be captured."//C_NEW_LINE//C_NULL_CHAR)
   endif
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=addstr("VERSION:"//trim(curses_version())//C_NEW_LINE//C_NULL_CHAR)
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=addstr("TERMINAL TYPE:"//trim(termname())//C_NEW_LINE//C_NULL_CHAR)
   ierr=addstr("LONG TERMINAL TYPE:"//trim(longname())//C_NEW_LINE//C_NULL_CHAR)
   ierr=refresh()
   ierr=getch()
!-------------------------------------------------------------------------------
   ierr=addstr("COMPILER VERSION:   "//compiler_version()//C_NEW_LINE//C_NULL_CHAR)
   ierr=addstr("COMPILER OPTIONS:   "//compiler_options()//C_NEW_LINE//C_NULL_CHAR)
!-------------------------------------------------------------------------------
   ierr=printw("ERASECHAR:          %s"//C_NEW_LINE//C_NULL_CHAR,keyname(ichar(erasechar())))
   ierr=printw("KILLCHAR:           %s"//C_NEW_LINE//C_NULL_CHAR,keyname(ichar(killchar())))
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,3
      ierr=getch()
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=endwin()
!-------------------------------------------------------------------------------
! CDEF: char *keybound ( int keycode, int count );
! CDEF: char *slk_label ( int labnum );
! CDEF: char *unctrl ( chtype c );
!-----------------------------------------------------------------------------------------------------------------------------------
end program seeattr
