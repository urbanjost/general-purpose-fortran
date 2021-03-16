program exp1     ! @(#) draw a panel and fill it and show a subsection of it
!(LICENSE:PD)
   use M_ncurses
   implicit none
   type(C_PTR)    :: pad_ptr
   integer(C_INT) :: x,y
   integer(C_INT) :: pad_lines
   integer(C_INT) :: pad_cols
   integer        :: ierr
   integer(C_LONG):: ilet,iend
   stdscr=initscr()                             ! start ncurses
   call getmaxyx(stdscr,LINES,COLS)             ! get screen size
   pad_lines=LINES + 50                         ! define a pad bigger than the window
   pad_cols=COLS + 50
   pad_ptr=newpad(pad_lines,pad_cols)           ! create the pad
   iend=ichar('Z')                              ! define the end of the alphabet as a number
   do x=0,pad_lines-1                           ! fill the pad with the alphabet
      ilet=ichar('A')                           ! start filling the line by repeating A-Z
      do y=0,pad_cols-1                         
       ierr=mvwaddch(pad_ptr,x,y,ilet)
       ilet=ilet+1
       if(ilet.gt.iend)ilet=ichar('A')
      enddo
   enddo
   do x=0,20                                    ! loop thru a subsection of the pad
      ierr=prefresh(pad_ptr,0,x,3,3,9+3,9+3)    ! display the subsection (must fit on screen)
      ierr=wgetch(pad_ptr);                     ! pause to read a keyboard key 
   enddo
   ierr=delwin(pad_ptr)                         ! delete the pad
   ierr=endwin()                                ! end ncurses
end program exp1
