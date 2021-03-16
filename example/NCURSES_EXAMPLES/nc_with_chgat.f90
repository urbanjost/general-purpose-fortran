program with_chgat
!(LICENSE:PD)
! @(#) change attributes of text on the screen using ncurses(3c) from Fortran
   use M_ncurses,only:  n=>C_NULL_CHAR, s=>C_SHORT , null=>C_NULL_PTR
   use M_ncurses
   !local-name => use-name

   implicit none
   integer :: ierr
   integer :: keepy, keepx
   stdscr=initscr()                                                                                ! Start curses mode
   ierr=start_color()                                                                              ! Start color functionality
   ierr=init_pair(1_S, int(COLOR_BLUE   ,S), int(COLOR_BLACK  ,S)) ! define some color combinations
   ierr=init_pair(2_S, int(COLOR_RED    ,S), int(COLOR_BLACK  ,S))
   ierr=init_pair(3_S, int(COLOR_GREEN  ,S), int(COLOR_BLACK  ,S))
   ierr=init_pair(4_S, int(COLOR_YELLOW ,S), int(COLOR_BLACK  ,S))
   ierr=init_pair(5_S, int(COLOR_BLACK  ,S), int(COLOR_WHITE  ,S))
   ierr=init_pair(6_S, int(COLOR_MAGENTA,S), int(COLOR_BLACK  ,S))
   ierr=init_pair(7_S, int(COLOR_CYAN   ,S), int(COLOR_YELLOW ,S))
   ierr=printw("A Big Blinking Blue String."//n)                                         ! print a string to make blink
   ierr=printw(" Made Longer and Longer."//n)
   call getyx(stdscr,keepy,keepx)                              ! store current cursor position (at end of text) for re-use later
   ierr=mvchgat( &    ! change the attributes of the line that was just printed on
   & 0, 0, &          ! First two parameters specify the (Y,X) position at which to start
   & -1, &            ! Third parameter is the number of characters to update. -1 means till end of line
   & A_BLINK,&        ! Fourth parameter is the normal attribute you wanted to give to the character
   & 1_S, &           ! Fifth is the color index. It is the index defined using init_pair() .Use 0 if you didn't want color
   & null)            ! Sixth one is always NULL

   ierr=mvprintw(23,0," Enter RETURN ..."//n)
   ierr=refresh()
   ierr=getch()

   ierr=mvchgat( 0,  0, -1, A_NORMAL, 1_S, null)
   ierr=mvprintw(keepy,keepx,"Now it quit blinking."//n)  ! note that the new text is not blue

   ierr=mvprintw( 4,0," This is line 4. It is Normal."//n)              ! this is all normal text initially
   ierr=mvprintw( 8,0," This is line 8. Let's underline it"//n)
   ierr=mvprintw(12,0," This is line 12. It will be Reserve video"//n)
   ierr=mvprintw(16,0," This is line 16. It will become bold"//n)
   ierr=mvprintw(20,0," This is line 20. Make it blink "//n)

   ierr=refresh()
   ierr=getch()                                               ! pause so the user can see the standard text

   ierr=mvchgat( 4,  0, -1, A_NORMAL   , 2_S, null)           ! change the text attributes on some lines
   ierr=mvchgat( 8,  0, -1, A_UNDERLINE, 3_S, null)
   ierr=mvchgat( 12, 0, -1, A_REVERSE  , 4_S, null)
   ierr=mvchgat( 16, 0, -1, A_BOLD     , 5_S, null)
   ierr=mvchgat( 20, 0, -1, A_BLINK    , 6_S, null)
! The available non-color attributes are bold, underline, invisible,
! right-line, left-line, protect, reverse and blink. 256 color pairs (8
! bits), 8 bits for other attributes, and 16 bits for character data.
   ierr=getch()                                                           ! pause to see the altered attributes
   ierr=endwin()                                                          ! end curses mode
end program with_chgat
