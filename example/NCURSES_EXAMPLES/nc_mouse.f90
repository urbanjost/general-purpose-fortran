program nc_mouse  ! @(#) how to get position and click information from mouse presses
!(LICENSE:PD)
! A press is processed and then you are back in your program and then a release gets its own KEY_MOUSE return
   use M_ncurses
   implicit none
   integer :: ierr
   type(MEVENT) :: mort
   integer :: ch
   stdscr=initscr()
   ierr=noecho()
   ierr=keypad(stdscr,TRUE)
   ierr=mousemask(ALL_MOUSE_EVENTS,C_NULL_PTR)
   ierr=addstr("Move mouse around and click, click and hold then release, and double-click different mouse buttons"//C_NULL_CHAR)
   INFINITE: do
      ch = getch()
      if( ch == KEY_MOUSE ) then
         !!ierr=clear()
         ierr=getmouse(mort)
         ierr=move(mort%y,mort%x)
         select case(mort%bstate)
            case (BUTTON1_PRESSED)          ;ierr=addstr("Button 1 Pressed"     //C_NULL_CHAR)
            case (BUTTON1_RELEASED)         ;ierr=addstr("Button 1 Released"    //C_NULL_CHAR)
            case (BUTTON1_CLICKED)          ;ierr=addstr("Button 1 Clicked"     //C_NULL_CHAR)
            case (BUTTON1_DOUBLE_CLICKED)   ;ierr=addstr("Button 1 Dbl-Clicked" //C_NULL_CHAR)
            case (BUTTON2_PRESSED)          ;ierr=addstr("Button 2 Pressed"     //C_NULL_CHAR)
            case (BUTTON2_RELEASED)         ;ierr=addstr("Button 2 Released"    //C_NULL_CHAR)
            case (BUTTON2_CLICKED)          ;ierr=addstr("Button 2 Clicked"     //C_NULL_CHAR)
            case (BUTTON2_DOUBLE_CLICKED)   ;ierr=addstr("Button 2 Dbl-Clicked" //C_NULL_CHAR)
            case (BUTTON3_PRESSED)          ;ierr=addstr("Button 3 Pressed"     //C_NULL_CHAR)
            case (BUTTON3_RELEASED)         ;ierr=addstr("Button 3 Released"    //C_NULL_CHAR)
            case (BUTTON3_CLICKED)          ;ierr=addstr("Button 3 Clicked"     //C_NULL_CHAR)
            case (BUTTON3_DOUBLE_CLICKED)   ;ierr=addstr("Button 3 Dbl-Clicked" //C_NULL_CHAR)
            case default
         end select
         ierr=refresh()
         continue
      endif
      if( char(ch) == C_NEW_LINE ) exit
   enddo INFINITE
   ierr=endwin()
end program nc_mouse
