program key_code
!(LICENSE:PD)
! @(#) basic example of using ncurses(3c) from Fortran that shows identifying what keys were pressed
   use M_ncurses
   implicit none
   integer ::  ch
   integer ::  ierr

   stdscr=initscr()                                 ! initialize screen mode
   ierr=cbreak()                                    ! make sure essentially all key presses are passed to the program
   ierr=noecho()                                    ! turn off echoing so only characters drawn by a refresh appear
   ierr=keypad(stdscr, TRUE)                        ! allow for function keys to be identified as well as standard alphanumeric keys

   ierr=addstr("Press any key to have it be identified ..."//C_NULL_CHAR)   ! Print message to current position (upper left corner)
   ierr=refresh()                                   ! update the real screen
   ch = getch()                                     ! wait for a key to be pressed
   ierr=endwin()                                    ! exit screen mode
   write(*,*) "The key pressed is ", ch             ! regular Fortran READ(3f) and WRITE(3f) statements can be used again
end program key_code
