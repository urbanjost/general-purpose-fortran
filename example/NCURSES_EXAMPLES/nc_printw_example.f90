program printw_example
!(LICENSE:PD)
! @(#) simple printw-family calls to place text on the screen
 ! NOTE:
 !    mvprintw is currently very limited in that it can only be called using the forms
 !       mvprintw(X,Y,FMT,INT|FLOAT|STRING||[INT1,INT2])
 !    and does not process escaped characters such as \t, \n ....
 use M_ncurses
 character(len=80) :: mesg                           ! message to appear on the screen
 integer(C_INT)    :: row,col                        ! to store the number of rows and number of colums of the screen
 mesg="Just a string in the middle"                  ! set message to appear on the screen
 stdscr=initscr()                                    ! start curses mode
 call getmaxyx(stdscr,row,col)                       ! get the number of rows and columns
 ierr=mvprintw(row/2,(col-len_trim(mesg))/2,"%s"//C_NULL_CHAR,mesg//C_NULL_CHAR) ! print the message at center of screen

 ! Note that \n did not work like it does in C
 !ierr=mvprintw(row-2,0,"This screen has %d rows and %d columns\n"//C_NULL_CHAR,row,col)
 ierr=mvprintw(row-2,0,"This screen has %d rows and %d columns"//C_NEW_LINE//C_NULL_CHAR,row,col)

 ierr=printw("Try resizing your window(if possible) and then run this program again"//C_NULL_CHAR)
 ierr=refresh()
 ierr=getch()
 ierr=endwin()
end program printw_example
