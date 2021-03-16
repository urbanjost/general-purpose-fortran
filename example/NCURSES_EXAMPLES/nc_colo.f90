!-------------------------------------------------------------------------------
program colo
! @(#) Simple color text program using ncurses(3c) from Fortran with more than 8 colors
!(LICENSE:PD)
   use M_ncurses
   implicit none
   integer :: num=0
   integer :: ierr
   integer(C_LONG) :: c
    stdscr=initscr()           ! initialize the curses library
    ! KLUDGE: LINES and COLS are not being set properly yet
    call getmaxyx(stdscr,LINES,COLS)                            ! get the number of rows and columns
    !if(stdscr == C_NULL_PTR)then
    if (.not.c_associated(stdscr)) then
       ierr=endwin()
       write(*,*)'could not open standard screen. input not a tty? ERRNO=',ierr
       stop
    endif

    ierr=keypad(stdscr, TRUE)  ! enable keyboard mapping
    ierr=nonl()                ! tell curses not to do NL->CR/NL on output
    ierr=cbreak()              ! take input chars one at a time, no wait for \n
    ierr=echo()                ! echo input - in color

    !if(has_colors() .eqv. TRUE)then
    if (has_colors())then
        ierr=start_color()
        !! NOTE: HAD TO CAST EVERYTHING TO TYPE C_SHORT
        ierr=init_pair(1_C_SHORT, COLOR_BLACK,     COLOR_RED)
        ierr=init_pair(2_C_SHORT, COLOR_GREEN,     COLOR_WHITE)
        ierr=init_pair(3_C_SHORT, COLOR_WHITE,     COLOR_GREEN)

        ierr=attron(COLOR_PAIR(1))
        ierr=mvprintw(0,0,"This terminal has colors"//C_NEW_LINE//C_NULL_CHAR)
        ierr=refresh()
        ierr=attroff(COLOR_PAIR(1))

        call getcolor(COLORS,COLOR_PAIRS) !! extension to get these set
        ierr=  printw(    "The maximum number of colors is %d"//C_NEW_LINE//C_NULL_CHAR,COLORS)
        call setcolors(COLORS)

        ierr=attron(COLOR_PAIR(2))
        ierr=  printw(    "The maximum number of color pairs supported is %d"//C_NEW_LINE//C_NULL_CHAR,COLOR_PAIRS)
        ierr=attroff(COLOR_PAIR(2))

        ierr=init_pair(1_C_SHORT, COLOR_BLUE,     COLOR_WHITE)  ! change one and see what cells using it do
        ierr=refresh()

        if(can_change_color())then
           ierr=attron(COLOR_PAIR(3))
           ierr=  printw(    "colors can be changed"//C_NEW_LINE//C_NULL_CHAR)
           ierr=attroff(COLOR_PAIR(3))
        else
           ierr=  printw(    "colors can not be changed"//C_NEW_LINE//C_NULL_CHAR)
        endif
        ierr=  printw(    C_NEW_LINE//"TYPE SOME TEXT(esc to quit):"//C_NULL_CHAR)
         ! Simple color assignment, often all we need.  Color pair 0 cannot
         ! be redefined.  This example uses the same value for the color
         ! pair as for the foreground color, though of course that is not
         ! necessary:
        ierr=init_pair(1_C_SHORT, COLOR_RED,     COLOR_BLACK)
        ierr=init_pair(2_C_SHORT, COLOR_GREEN,   COLOR_BLACK)
        ierr=init_pair(3_C_SHORT, COLOR_YELLOW,  COLOR_BLACK)
        ierr=init_pair(4_C_SHORT, COLOR_BLUE,    COLOR_BLACK)
        ierr=init_pair(5_C_SHORT, COLOR_CYAN,    COLOR_BLACK)
        ierr=init_pair(6_C_SHORT, COLOR_MAGENTA, COLOR_BLACK)
        ierr=init_pair(7_C_SHORT, COLOR_WHITE,   COLOR_BLACK)
    else
        ierr=mvprintw(0,0,"This terminal does not support color"//C_NULL_CHAR)
        ierr=getch()
        ierr=endwin() ! wrapup curses
        write(*,*)"Your terminal does not support color"
        stop
    endif


    INFINITE: do
        c = getch()    ! refresh, accept single keystroke of input
        if(c.eq.13)then
           ierr=printw(C_NEW_LINE//C_NULL_CHAR)
        elseif(c.eq.27) then
           exit INFINITE
        endif
        !ierr=attrset(COLOR_PAIR(mod(num,8)))
        ierr=attrset(COLOR_PAIR(mod(num,COLORS)))
        num=num+1
    enddo INFINITE

    ierr=endwin() ! wrapup curses
end program colo
!-------------------------------------------------------------------------------
subroutine setcolors(number_of_colors)
use M_ncurses
implicit none
! silly routine to fill in color table. Do something better sometime
integer(C_SHORT)   :: r,g,b
integer(C_INT)     :: number_of_colors
integer(C_SHORT)   :: icount
integer            :: ierr
if(number_of_colors.gt.8)then
   icount=9
   RL: do r=1,1000,150
      GL: do g=1,1000,150
         BL: do b=1,1000,150
            if(icount.gt.number_of_colors)exit RL
            ierr=init_color(icount,r,g,b)
            ierr=init_pair(icount,icount,int(mod(icount,9_C_SHORT),C_SHORT))
            icount=icount+1_C_SHORT
         enddo BL
      enddo GL
   enddo RL
endif
end subroutine setcolors
!-------------------------------------------------------------------------------
