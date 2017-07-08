!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine nc_errmessage(msg) ! exit screen mode to use WRITE(3f) to print a message and then read a line to create a pause
   use M_ncurses
   implicit none
   character(len=*),intent(in) :: msg
   integer                     :: ierr    ! return value for most ncurses(3c) functions
   integer                     :: ios     ! status return of READ(3f)
   ierr=def_prog_mode()                   ! Save the tty modes
   ierr=refresh()
   ierr=endwin()                          ! End curses mode temporarily
   write(*,*)msg
   read(*,'(a)',iostat=ios)
   ierr=refresh() ! Back to curses. You can once again use the full capabilities of curses; and the screen resumes where it was
   ierr=flash()
end subroutine nc_errmessage
