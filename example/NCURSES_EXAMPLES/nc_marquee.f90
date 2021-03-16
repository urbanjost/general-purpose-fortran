program nc_marquee ! @(#) example of inserting characters with insch(3c)
!(LICENSE:PD)
use M_ncurses
implicit none
character(len=80) :: text = "NEWS: *** Congressman indicted ***"
integer           :: i,j
integer           :: ierr
integer(C_LONG)   :: ich
!-------------------------------------------------------------------------------
   stdscr=initscr()
!-------------------------------------------------------------------------------
! set up some color pairs
   ierr=start_color()
   ierr=init_pair(1_C_SHORT,COLOR_WHITE,COLOR_BLUE)
   ierr=init_pair(2_C_SHORT,COLOR_RED,COLOR_YELLOW)
   ierr=init_pair(3_C_SHORT,COLOR_GREEN,COLOR_YELLOW)
   ierr=init_pair(4_C_SHORT,COLOR_BLUE,COLOR_YELLOW)
   ierr=init_pair(5_C_SHORT,COLOR_BLACK,COLOR_YELLOW)
!-------------------------------------------------------------------------------
! change background to field of "+" chars. so the insert change is seen clearly
   ierr=wbkgd(stdscr,ior(ichar("+",C_LONG),COLOR_PAIR(2)))
   ierr=refresh()                  ! post background change to the display
!-------------------------------------------------------------------------------
! insert a string onto the screen one character at a time from end to beginning
   do i=len_trim(text),1,-1               ! work through string backwards
      do j=1,5
         ierr=move(5+j,5)                 ! always insert at the same spot
         ich=int(ichar(text(i:i)),C_LONG) ! convert char to INTEGER(C_LONG)
         ich=ior(ich,COLOR_PAIR(j))
         ierr=insch(ich)
         ierr=refresh()                   ! post change to the display
      enddo
      ierr=napms(100)                     ! 0.1 sec. delay
   enddo
   ierr=getch()
   ierr=endwin()
end program nc_marquee
!-------------------------------------------------------------------------------
