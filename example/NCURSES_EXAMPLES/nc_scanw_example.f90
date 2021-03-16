program scanw_example ! @(#) read a string from the screen using ncurses(3c) from Fortran
!(LICENSE:PD)
 use M_ncurses
 implicit none
 integer           :: ierr
 character(len=80) :: mesg="Enter a string:"    ! message to appear on the screen
 character(len=20) :: str=""                    ! holds the string read from the screen
 integer           :: max_rows,max_cols         ! to store the number of rows and the number of columns of the screen
 stdscr=initscr()                               ! start the curses mode
 call getmaxyx(stdscr,max_rows,max_cols)        ! get the number of rows and columns
                                                ! print the message at the center of the screen
 ierr=mvprintw(max_rows/2,(max_cols-len_trim(mesg))/2,"%s"//C_NULL_CHAR,trim(mesg)//C_NULL_CHAR)
 ierr=refresh()
 ierr=getnstr(str,len(str)-1)                   ! read the string
 ierr=mvprintw(max_rows - 2, 0, "You Entered: %s"//C_NULL_CHAR, str//C_NULL_CHAR)
 ierr=clrtoeol()       ! erase the tail of any old message (erases from current position to the end of the current line).
 ierr=refresh()
 ierr=getch()
 ierr=endwin()
end program scanw_example
