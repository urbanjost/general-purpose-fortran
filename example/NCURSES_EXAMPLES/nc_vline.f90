program nc_vline !@(#) draw vertical lines
!(LICENSE:PD)
   use M_ncurses
   integer :: maxy,maxx,halfy,x,len
   integer :: ierr
   stdscr=initscr()
   call getmaxyx(stdscr,maxy,maxx)
   halfy = maxy/2
   len = 1
   do x=0,maxx-1
      ierr=mvvline(halfy-len,x,0_C_LONG,len+len)
      if(mod(x,7).eq.0)len=len+1
   enddo
   ierr=refresh()
   ierr=getch()
   ierr=endwin()
end program nc_vline
