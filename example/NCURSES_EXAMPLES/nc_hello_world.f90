program nc_hello_world
!(LICENSE:PD)
! @(#) HELLO WORLD program using ncurses(3c) from Fortran
!
!! C PROGRAMMERS: NOTE THAT FORTRAN FUNCTIONS REQUIRE THEIR RETURNED VALUE BE ASSIGNED TO A VARIABLE
!
   use M_ncurses                                  ! load the Fortran module to interface to the ncurses(3c) C library
   implicit none
   integer :: ierr, ii
   stdscr=initscr()                             ! Start curses mode to pre-defined standard screen
                                                !! NOTE THAT
                                                !!    THE SCREEN WILL CLEAR
                                                !!    NORMAL READ()s and WRITE()s TO THE SCREEN WILL NOT WORK CORRECTLY NOW

   ierr=addstr("Hello World!!!"//C_NULL_CHAR)   ! Print Hello World
                                                !! NOTE THAT
                                                !!    A NULL IS APPENDED TO THE STRING FOR C
                                                !!    THE INITIAL POSITION IS AT THE TOP LEFT CORNER (Y=0,X=0)
                                                !!    THE CURRENT POSITION IS LEFT AT THE END OF THE STRING

   ierr=refresh()                               ! update the real screen
   ii=getch()                                   ! Wait for a user keystroke

   ierr=endwin()                                ! End curses mode
                                                !! NOTE THAT
                                                !!    THE SCREEN WILL CLEAR OR RETURN TO THE STATE BEFORE INITSCR() WAS
                                                !!    CALLED, OR STAY AS-IS (DEPENDS ON TERMINAL TYPE AND DEFINITIONS).
                                                !!    NORMAL READ()s and WRITE()s TO THE SCREEN WILL NOW WORK CORRECTLY

   write(*,*)'Your keypress returned a ',ii     ! Normal Fortran I/O will work again AFTER endwin() is called

end program nc_hello_world
