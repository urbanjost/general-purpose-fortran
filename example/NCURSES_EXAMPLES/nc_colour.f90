program nc_colors
! @(#) Print out some ncurses colors.
!(LICENSE:PD)
!---------------------------------------------
! John S. Urban, 20141214
!---------------------------------------------
! based on a C program example by
!  Yoran Heling <projects@yorhel.nl>
!  Date: 2011-06-11
!  License: MIT
!  Web: http://dev.yorhel.nl/dump/nccolour
!---------------------------------------------
use M_ncurses
integer :: i

type color 
  integer(C_SHORT)  :: code
  character(len=20) :: desc
end type color

type(color)         :: colours(0:8) 
   colours(0) = color(-1_C_SHORT,    "default" )
   colours(1) = color(COLOR_BLACK,   "BLACK"   )
   colours(2) = color(COLOR_RED,     "RED"     )
   colours(3) = color(COLOR_GREEN,   "GREEN"   )
   colours(4) = color(COLOR_YELLOW,  "YELLOW"  )
   colours(5) = color(COLOR_BLUE,    "BLUE"    )
   colours(6) = color(COLOR_MAGENTA, "MAGENTA" )
   colours(7) = color(COLOR_CYAN,    "CYAN"    )
   colours(8) = color(COLOR_WHITE,   "WHITE"   ) 

  stdscr=initscr()
  if (.not.c_associated(stdscr)) then
     ierr=endwin()
     write(*,*)'could not open standard screen. input not a tty? ERRNO=',ierr
     write(*,'("Error initializing screen.")')
     stop 1
  endif

  if(.not.has_colors()) then
    write(*,'(a)')"This terminal does not support colors"
    stop 1
  endif
  ierr=start_color()

  ! this prevents ncurses from forcing a white-on-black color scheme,
  ! regardless of what scheme is used by the terminal.
  ierr=use_default_colors()

  ierr=attron(A_BOLD)
  ierr=mvaddstr(0, 0, " Color      FD  FB  FW  BG  BD    BFD BFB BFW BBG BBD"//C_NULL_CHAR)
  ierr=attroff(A_BOLD)

  do   i=0, size(colours)-1
    ierr=init_pair(int(i*6+1,C_SHORT), colours(i)%code, -1_C_SHORT)
    ierr=init_pair(int(i*6+2,C_SHORT), colours(i)%code, COLOR_BLACK)
    ierr=init_pair(int(i*6+3,C_SHORT), colours(i)%code, COLOR_WHITE)
    ierr=init_pair(int(i*6+4,C_SHORT), colours(i)%code, colours(i)%code)
    ierr=init_pair(int(i*6+5,C_SHORT), -1_C_SHORT, colours(i)%code)
    ierr=mvaddstr(i+2, 1, colours(i)%desc//C_NULL_CHAR)
    ierr=move(i+2, 8)
    call print_colorpart(0_C_LONG, i)
    call print_colorpart(A_BOLD, i)
  enddo

  ierr=mvaddstr(i+3, 2, "FD  = Front color on default background"//C_NULL_CHAR)
  ierr=mvaddstr(i+4, 2, "FB  = Front color on black background"//C_NULL_CHAR)
  ierr=mvaddstr(i+5, 2, "FW  = Front color on white background"//C_NULL_CHAR)
  ierr=mvaddstr(i+6, 2, "BG  = Front and background color"//C_NULL_CHAR)
  ierr=mvaddstr(i+7, 2, "BD  = Background color with default front color"//C_NULL_CHAR)
  ierr=mvaddstr(i+8, 2, "B?? = As above, with A_BOLD enabled"//C_NULL_CHAR)
  ierr=mvaddstr(i+10, 0, "Hit any key to exit."//C_NULL_CHAR)
  ierr=refresh()
  ierr=getch()
  ierr=endwin()
contains
subroutine print_colorpart(attr, i)
integer(C_LONG) :: attr
integer(C_INT)  :: i
!---------- WORKS TOO
!   ierr=  attron(ior(attr,COLOR_PAIR(i*6+2)))
!   ierr=  addstr("FB"//C_NULL_CHAR)
!   ierr=  attroff(ior(attr,COLOR_PAIR(i*6+2)))
!   ierr=  addstr("  "//C_NULL_CHAR)
!----------
!---------- JUST GET COLOR, NOT BOLD ???
!   ierr=  attron(COLOR_PAIR(i*6+2))
!   ierr=  addstr("FB"//C_NULL_CHAR)
!   ierr=  attroff(COLOR_PAIR(i*6+2))
!   ierr=  addstr("  "//C_NULL_CHAR)
!----------
!----------
   ierr=addstr("    "//C_NULL_CHAR)
   ierr=  attrset(ior(attr,COLOR_PAIR(i*6+1)))
   ierr=  addstr("FD"//C_NULL_CHAR)
   ierr=  attrset(attr)
   ierr=  addstr("  "//C_NULL_CHAR)
!----------
   ierr=  attrset(ior(attr,COLOR_PAIR(i*6+2)))
   ierr=  addstr("FB"//C_NULL_CHAR)
   ierr=  attrset(attr)
   ierr=  addstr("  "//C_NULL_CHAR)

   ierr=  attrset(ior(attr,COLOR_PAIR(i*6+3)))
   ierr=  addstr("FW"//C_NULL_CHAR)
   ierr=  attrset(attr)
   ierr=  addstr("  "//C_NULL_CHAR)

   ierr=  attrset(ior(attr,COLOR_PAIR(i*6+4)))
   ierr=  addstr("BG"//C_NULL_CHAR)
   ierr=  attrset(attr)
   ierr=  addstr("  "//C_NULL_CHAR)

   ierr=  attrset(ior(attr,COLOR_PAIR(i*6+5)))
   ierr=  addstr("BD"//C_NULL_CHAR)
   ierr=  attrset(attr)

end subroutine print_colorpart
end program nc_colors
