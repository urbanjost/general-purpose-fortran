program temp_leave ! @(#) temporarily leave screen mode in ncurses from Fortran
!(LICENSE:PD)
   use M_ncurses
   implicit none
   integer :: ierr
   stdscr=initscr()                            ! Start curses mode
   ierr=addstr("Hello World!"//C_NULL_CHAR)    ! Print Hello World!
   ierr=mvaddstr(1,0,"Now enter 'RETURN' to temporarily leave screen mode"//C_NULL_CHAR)
   ierr=refresh()                              ! post everything onto the real screen
   !--------------------------------------------
   ierr=def_prog_mode()                        ! Save the tty modes
   ierr=getch()                                ! Wait for a user keystroke
   ierr=endwin()                               ! End curses mode temporarily
   !--------------------------------------------
   write(*,*)'You have now entered a Bourne shell'
   write(*,*)'You can enter regular commands. When done,'
   write(*,*)'enter "exit" to resume your program.'
   call execute_command_line("/bin/sh -i")     ! Do whatever you like in cooked mode
   !--------------------------------------------
   ierr=reset_prog_mode()                      ! Return to the previous tty mode stored by def_prog_mode()
   ierr=refresh()                              ! Do refresh() to restore the screen contents
   !--------------------------------------------
   ! Back to curses. You can once again use the full capabilities of curses
   ierr=mvaddstr(5,0,"You have now resumed screen mode."//C_NULL_CHAR)
   ierr=mvaddstr(6,0,"Note that everything that was on the screen was retained!"//C_NULL_CHAR)
   ierr=mvaddstr(7,0,"(press any key to exit ...)"//C_NULL_CHAR)
   ierr=refresh()                              ! post to real screen
   ierr=getch()                                ! Wait for a user keystroke
   ierr=endwin()                               ! End curses mode
end program temp_leave
