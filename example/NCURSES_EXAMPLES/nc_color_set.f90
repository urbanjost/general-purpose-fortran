!-------------------------------------------------------------------------------
!@(#) setting color pairs using ncurses(3c) from Fortran
!(LICENSE:PD)
program color_set_example
    use M_ncurses
    implicit none
    integer(C_SHORT) :: f,b
    integer          :: i,ierr
    stdscr=initscr()
    ierr=cbreak()
    ierr=noecho()
    if (has_colors()) then
        ierr=start_color()
        ierr=pair_content(0_C_SHORT,f,b)
        ierr=printw("pair 0 contains (%d,%d)"//C_NEW_LINE//C_NULL_CHAR,int(f),int(b))
        ierr=getch()
        ierr=printw("Initializing pair 1 to red/black"//C_NEW_LINE//C_NULL_CHAR)
        ierr=init_pair(1_C_SHORT, int(COLOR_RED,C_SHORT), int(COLOR_BLACK,C_SHORT))
        i = color_set(1_C_SHORT, C_NULL_PTR)
        ierr=printw("RED/BLACK (%s)"//C_NEW_LINE//C_NULL_CHAR, SHOW(i)//C_NULL_CHAR)
        ierr=getch()
        ierr=printw("Initializing pair 2 to white/blue"//C_NEW_LINE//C_NULL_CHAR)
        ierr=init_pair(2_C_SHORT, int(COLOR_WHITE,C_SHORT), int(COLOR_BLUE,C_SHORT))
        i = color_set(2_C_SHORT, C_NULL_PTR)
        ierr=printw("WHITE/BLUE (%s)"//C_NEW_LINE//C_NULL_CHAR, SHOW(i)//C_NULL_CHAR)
        ierr=getch()
        ierr=printw("Resetting colors to pair 0"//C_NEW_LINE//C_NULL_CHAR)
        i = color_set(0_C_SHORT, C_NULL_PTR)
        ierr=printw("Default Colors (%s)"//C_NEW_LINE//C_NULL_CHAR, SHOW(i)//C_NULL_CHAR)
        ierr=getch()
        ierr=printw("Resetting colors to pair 1"//C_NEW_LINE//C_NULL_CHAR)
        i = color_set(1_C_SHORT, C_NULL_PTR)
        ierr=printw("RED/BLACK (%s)"//C_NEW_LINE//C_NULL_CHAR, SHOW(i)//C_NULL_CHAR)
        !ierr=getch()
    else
        ierr=printw("This demo requires a color terminal"//C_NEW_LINE//C_NULL_CHAR)
        ierr=getch()
    endif
    ierr=endwin()
    write(*,*)'GOT TO END'
contains
!-------------------------------------------------------------------------------
character(len=3) function SHOW(n)
   integer :: n
   if(n.eq.0)then
      SHOW="OK"
   else
      SHOW="ERR"
   endif
end function SHOW
!-------------------------------------------------------------------------------
end program color_set_example
!-------------------------------------------------------------------------------
