program acs_vars
! @(#) test Alternate Character Set (ACS) variable names (line boxes characters, ...)
!(LICENSE:PD)
   use M_ncurses
   implicit none
   integer :: ierr
   integer :: cursor_state
   stdscr=initscr()
   cursor_state=curs_set(0) ! sets the cursor state to invisible
   ierr=move(0,0)           ! the upper left corner is the default starting position
   ierr=printw("ACS_ULCORNER:Upper left corner    "//C_NULL_CHAR); ierr=addch(ACS_ULCORNER) ; call newl(stdscr)
   ierr=printw("ACS_LLCORNER:Lower left corner      "//C_NULL_CHAR); ierr=addch(ACS_LLCORNER) ; call newl(stdscr)
   ierr=printw("ACS_LRCORNER:Lower right corner   "//C_NULL_CHAR); ierr=addch(ACS_LRCORNER) ; call newl(stdscr)
   ierr=printw("ACS_URCORNER:Upper right corner     "//C_NULL_CHAR); ierr=addch(ACS_URCORNER) ; call newl(stdscr)
   ierr=printw("ACS_LTEE    :Tee pointing right   "//C_NULL_CHAR); ierr=addch(ACS_LTEE)     ; call newl(stdscr)
   ierr=printw("ACS_RTEE    :Tee pointing left      "//C_NULL_CHAR); ierr=addch(ACS_RTEE)     ; call newl(stdscr)
   ierr=printw("ACS_BTEE    :Tee pointing up      "//C_NULL_CHAR); ierr=addch(ACS_BTEE)     ; call newl(stdscr)
   ierr=printw("ACS_TTEE    :Tee pointing down      "//C_NULL_CHAR); ierr=addch(ACS_TTEE)     ; call newl(stdscr)
   ierr=printw("ACS_HLINE   :Horizontal line      "//C_NULL_CHAR); ierr=addch(ACS_HLINE)    ; call newl(stdscr)
   ierr=printw("ACS_VLINE   :Vertical line          "//C_NULL_CHAR); ierr=addch(ACS_VLINE)    ; call newl(stdscr)
   ierr=printw("ACS_PLUS    :Crossover            "//C_NULL_CHAR); ierr=addch(ACS_PLUS)     ; call newl(stdscr)
   ierr=printw("ACS_S1      :Scan Line 1            "//C_NULL_CHAR); ierr=addch(ACS_S1)       ; call newl(stdscr)
   ierr=printw("ACS_S3      :Scan Line 3          "//C_NULL_CHAR); ierr=addch(ACS_S3)       ; call newl(stdscr)
   ierr=printw("ACS_S7      :Scan Line 7            "//C_NULL_CHAR); ierr=addch(ACS_S7)       ; call newl(stdscr)
   ierr=printw("ACS_S9      :Scan Line 9          "//C_NULL_CHAR); ierr=addch(ACS_S9)       ; call newl(stdscr)
   ierr=printw("ACS_DIAMOND :Diamond                "//C_NULL_CHAR); ierr=addch(ACS_DIAMOND)  ; call newl(stdscr)
   ierr=printw("ACS_CKBOARD :Stipple              "//C_NULL_CHAR); ierr=addch(ACS_CKBOARD)  ; call newl(stdscr)
   ierr=printw("ACS_DEGREE  :Degree Symbol          "//C_NULL_CHAR); ierr=addch(ACS_DEGREE)   ; call newl(stdscr)
   ierr=printw("ACS_PLMINUS :Plus/Minus Symbol    "//C_NULL_CHAR); ierr=addch(ACS_PLMINUS)  ; call newl(stdscr)
   ierr=printw("ACS_BULLET  :Bullet                 "//C_NULL_CHAR); ierr=addch(ACS_BULLET)   ; call newl(stdscr)
   ierr=printw("ACS_LARROW  :Arrow Pointing Left  "//C_NULL_CHAR); ierr=addch(ACS_LARROW)   ; call newl(stdscr)
   ierr=printw("ACS_RARROW  :Arrow Pointing Right   "//C_NULL_CHAR); ierr=addch(ACS_RARROW)   ; call newl(stdscr)
   ierr=printw("ACS_DARROW  :Arrow Pointing Down  "//C_NULL_CHAR); ierr=addch(ACS_DARROW)   ; call newl(stdscr)
   ierr=printw("ACS_UARROW  :Arrow Pointing Up      "//C_NULL_CHAR); ierr=addch(ACS_UARROW)   ; call newl(stdscr)
   ierr=printw("ACS_BOARD   :Board of squares     "//C_NULL_CHAR); ierr=addch(ACS_BOARD)    ; call newl(stdscr)
   ierr=printw("ACS_LANTERN :Lantern Symbol         "//C_NULL_CHAR); ierr=addch(ACS_LANTERN)  ; call newl(stdscr)
   ierr=printw("ACS_BLOCK   :Solid Square Block   "//C_NULL_CHAR); ierr=addch(ACS_BLOCK)    ; call newl(stdscr)
   ierr=printw("ACS_LEQUAL  :Less/Equal sign        "//C_NULL_CHAR); ierr=addch(ACS_LEQUAL)   ; call newl(stdscr)
   ierr=printw("ACS_GEQUAL  :Greater/Equal sign   "//C_NULL_CHAR); ierr=addch(ACS_GEQUAL)   ; call newl(stdscr)
   ierr=printw("ACS_PI      :Pi                     "//C_NULL_CHAR); ierr=addch(ACS_PI)       ; call newl(stdscr)
   ierr=printw("ACS_NEQUAL  :Not equal            "//C_NULL_CHAR); ierr=addch(ACS_NEQUAL)   ; call newl(stdscr)
   ierr=printw("ACS_STERLING:UK pound sign          "//C_NULL_CHAR); ierr=addch(ACS_STERLING) ; call newl(stdscr)
   ierr=refresh()
   ierr=getch()
   ierr=endwin()
contains
subroutine newl(win)  ! do the equivalent of a newline, when hit bottom make left edge 39 instead of zero
   use M_ncurses
   implicit none
   !type(WINDOW), value  :: win
   type(C_PTR), value    :: win
   integer               :: ierr
   integer               :: current_y,current_x
   integer,save          :: left=0
   integer               :: maxy, maxx
   call getyx(win,current_y,current_x)  ! get current position at end of last string printed
   call getmaxyx(stdscr,maxy,maxx)      ! get screen size
   if(current_y.ge.maxy-1)then          ! if hit bottom of screen change left margin and go back to top
      current_y=-1                      ! line number to advance down from before printing
      left=39                           ! new left margin
   endif
   ierr=move(current_y+1,left)          ! move to new beginning point for next string
end subroutine newl
end program acs_vars
