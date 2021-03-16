!-------------------------------------------------------------------------------
program simple_color
!(LICENSE:PD)
! @(#) Simple color text program using ncurses(3c) from Fortran
   use M_ncurses
   implicit none
   integer :: ierr
   stdscr=initscr() ! Start curses mode

   ! KLUDGE: LINES and COLS are not being set properly yet
   call getmaxyx(stdscr,LINES,COLS)                            ! get the number of rows and columns

   if(has_colors() .eqv. FALSE)then
      ierr=endwin()
      write(*,*)"Your terminal does not support color"
      stop 1
   endif

   ierr=start_color()       ! Start color
   !! NOTE
   !!    NAME IS COLOR_RED, NOT COLOR_RED
   !!    HAD TO CAST EVERYTHING TO TYPE C_SHORT
   ierr=init_pair(1_C_SHORT, COLOR_RED, COLOR_BLACK)

   ierr=attron(COLOR_PAIR(1))
   call print_in_middle(stdscr, LINES / 2, 0, 0, "Viola !!! In color ...")
   ierr=attroff(COLOR_PAIR(1))
   ierr=getch()
   ierr=endwin()
end program simple_color
!-------------------------------------------------------------------------------
subroutine print_in_middle(win, starty, startx, width0, string)
   use M_ncurses
   use ncurses_types
   implicit none
   integer(C_INT)                          :: ierr
   type(C_PTR)                             :: win
   integer,intent(in)                      :: starty, startx
   integer(C_INT)                          :: width0
   integer(C_INT)                          :: width
   character(len=*,KIND=C_CHAR),intent(in) :: string
   integer(C_INT)                          :: length, x=30, y=20
   integer(C_INT)                          :: temp

   width=width0  ! make mutable copy

  !if(win == C_NULL_PTR)then
  if (.not.c_associated(win)) then
      win = stdscr
   endif

   call getyx(win, y, x)

   if(startx /= 0)then
      x = startx
   endif

   if(starty /= 0)then
      y = starty
   endif
   if(width == 0)then
      width = 80
   endif

   length = len_trim(string)
   temp = (width - length)/ 2
   x = startx + temp
   ierr=mvwaddstr(win, y, x, string//C_NULL_CHAR)
   ierr=refresh()

end subroutine print_in_middle
!-------------------------------------------------------------------------------
