program nc_events                                ! @(#) event driven loop that IDs key presses and mouse clicks using keyname(3c)
!(LICENSE:PD)
   use M_ncurses                                   ! use to Fortran interface module to the ncurses(3c) library
   implicit none
   character(len=1024) :: buffer                 ! buffer to write messages into using Fortran to pass to ncurses(3c)
   integer(C_INT)      :: c                      ! integer representing last key pressed read by wgetch(3c)
   integer(C_INT)      :: icount=0               ! number of calls to read an input character and none was found
   integer(C_INT)      :: xx, yy                 ! where to write message on screen
   integer             :: ierr                   ! status returned by most ncurses(3c) functions
   type(MEVENT)        :: event                  ! mouse event
   integer(C_LONG)     :: mouse_mask

   stdscr=initscr()                                    ! initialize ncurses(3c)
   ierr=cbreak()                                       ! pass all keypresses to ncurses(3c); even ctrl-C, ctrl-S, ctrl-Q.
   ierr=noecho()                                       ! nothing echos to screen unless it is explicity put there with a call
   ierr=curs_set(0)                                    ! Invisible cursor
   ierr=halfdelay(1)                                   ! Don't wait for more than 1/10 seconds for a keypress
   ierr=keypad(stdscr, TRUE)                           ! Enable keypad mode
   mouse_mask=mousemask(ALL_MOUSE_EVENTS, C_NULL_PTR)  ! Report all mouse events
   INFINITE: do                                        ! create a loop to read key presses and mouse clicks
      yy=0                                             ! default position for where to write next message
      xx=0
      c = wgetch(stdscr)                               ! read next keyboard event
      if(c == ichar(C_CARRIAGE_RETURN) .or. c == ichar(C_NEW_LINE) )then ! Enter or newline exits the program.
         exit INFINITE                                 ! begin exiting program
      else if(c == ERR)then                            ! wgetch(3c) did not find that a keyboard event was found
         icount=icount+1                               ! count number of times nothing happened
         write(buffer,101)icount                       ! create a message that tells what happened in this pass thru the loop
         101 format("Nothing happened ",i0," times. Click the mouse somewhere or hold down a key (ENTER to exit)")
      else if(KEY_MOUSE == c) then                     ! Mouse event.
          if(OK == getmouse(event))then
             ierr=move(event%y,event%x)
             write(buffer,202)event%y,event%x,event%bstate
             202 format("mouse at row ",i0,", column ",i0,": 0x",Z0)
             yy=event%y
             xx=event%x
          else
             yy=1
             write(buffer,'(a)')"Bad mouse event."
          endif
      else                                             ! a key was pressed 
        yy=1
        !write(20,*)c,keyname(c)
        write(buffer,303)trim(keyname(c)), c, c
        303 format("Key ", a, " (0x", Z0, " = ", I0, ") pressed.")
      endif
      ierr=move(yy, xx)                                ! move to location to print message
      ierr=addstr(trim(buffer)//C_NULL_CHAR)           ! place new string on screen
      ierr=clrtoeol()                                  ! clear from end of string just printed to end of line on screen
      ierr=wrefresh(stdscr)                            ! flush current data to hardware screen
   enddo INFINITE
   ierr=endwin()                                       ! exit ncurses(3c) mode
end program nc_events
