program hline_test
!(LICENSE:PD)
! @(#) draw horizonal lines using mvhline(3f)
   use M_ncurses                                    ! enable the Fortran module interface to ncurses(3c)
   implicit none
   integer        :: ierr
   integer(C_INT) :: maxy,maxx,halfx,y,len        ! values passed to the routines should be interoperable with C
   stdscr=initscr()                               ! initialize ncurses(3c) and "stdscr"
   call getmaxyx(stdscr,maxy,maxx)                ! get dimensions of display size
   halfx = maxx/2                                  
   len = 1
   do y=0,maxy-1                                  ! draw horizonal lines of various length
      ierr=mvhline(y,halfx-len,0_C_LONG,len+len)  ! draws a horizonal line on stdscr
      len=len+1                                   ! change length of string
   enddo
   ierr=refresh()                                 ! post your data to the screen
   ierr=getch()                                   ! pause for a key stroke
   ierr=endwin()                                  ! end ncurses(3c) mode
end program hline_test
