!-----------------------------------------------------------------------------------------
program win_border ! @(#) draw box that can be moved around screen with ncurses(3c) from fortran
!(LICENSE:PD)
   use M_ncurses
   implicit none
   type(C_PTR) :: my_win
   integer :: starty, startx, width, height
   integer :: sy, sx, wy, wx
   integer :: ch
   integer :: ierr
   stdscr=initscr()                                            ! Start curses mode
   ! KLUDGE: LINES and COLS are not being set properly yet
   call getmaxyx(stdscr,LINES,COLS)                            ! get the number of rows and columns
   ierr=noecho()
   ierr=raw()
   ierr=cbreak()                                               ! Line buffering disabled, Pass on everthing to me
   ierr=keypad(stdscr, TRUE)                            ! I need that nifty F1
   height = 3                                                  ! box size
   width = 10
   starty = (LINES - height) / 2                               ! Calculating for a center placement
   startx = (COLS - width) / 2                                 ! of the window
   ierr=printw("Press F1 or 'q' to exit"//C_NULL_CHAR)
   ierr=move(1,0)
   ierr=printw("Move box with arrow keys or 'ljkh' or 'UDRL'"//C_NULL_CHAR)
   ierr=refresh()
   my_win = create_newwin(height, width, starty, startx)
   INFINITE: DO
      ch=getch()
      if(ch.eq.KEY_F(1))then
         exit INFINITE
      endif
       !CASE (:‐1)
       !CASE (0)
       !CASE (1, 3:5, 8)
       !CASE (1:)
      select case (ch)
         case (ichar('q'))
            exit INFINITE
         case (ichar('H'))
            ierr=mvprintw(0,0,"Press F1 or 'q' to exit"//C_NULL_CHAR)
            ierr=mvprintw(1,0,"Move box with arrow keys or 'ljkh' or 'UDRL'"//C_NULL_CHAR)
            ierr=mvprintw(2,0,"H - display Help and move to middle"//C_NULL_CHAR)
            starty = (LINES - height) / 2                               ! Calculating for a center placement
            startx = (COLS - width) / 2                                 ! of the window
            call destroy_win(my_win)
            my_win = create_newwin(height, width, starty,startx)
            ierr=refresh()
         case (KEY_LEFT,ichar('h'),ichar('L'))
            call destroy_win(my_win)
            startx=startx-1
            my_win = create_newwin(height, width, starty,startx)
         case (KEY_RIGHT,ichar('l'),ichar('R'))
            call destroy_win(my_win)
            startx=startx+1
            my_win = create_newwin(height, width, starty,startx)
         case (KEY_UP,ichar('k'),ichar('U'))
            call destroy_win(my_win)
            starty=starty-1
            my_win = create_newwin(height, width, starty,startx)
         case (KEY_DOWN,ichar('j'),ichar('D'))
            call destroy_win(my_win)
            starty=starty+1
            my_win = create_newwin(height, width, starty,startx)
        CASE DEFAULT
      end select
      call getyx(stdscr,sy,sx)
      call getyx(my_win,wy,wx)
   ENDDO INFINITE
   ierr=endwin()        ! End curses mode
contains
!-----------------------------------------------------------------------------------------
function create_newwin(height,width,starty,startx) result(local_win)
   integer,intent(in) :: height, width,starty,startx
   type (C_PTR)       :: local_win
   local_win = newwin(height, width, starty, startx)
   ierr=box(local_win, 0_C_LONG , 0_C_LONG)      ! 0, 0 gives default characters for the vertical and horizontal lines
   ierr=wrefresh(local_win)   ! Show that box
end function create_newwin
!-----------------------------------------------------------------------------------------
subroutine destroy_win(local_win)
   type (C_PTR), intent(inout) :: local_win
   integer(C_LONG) :: char
    ! box(local_win, ' ', ' '); : This won't produce the desired
    ! result of erasing the window. It will leave its four corners
    ! and so an ugly remnant of window.
    !
   char=ichar(" ")
   ierr=wborder(local_win,char,char,char,char,char,char,char,char)
    ! The parameters taken are
    ! 1. win: the window on which to operate
    ! 2. ls: character to be used for the left side of the window
    ! 3. rs: character to be used for the right side of the window
    ! 4. ts: character to be used for the top side of the window
    ! 5. bs: character to be used for the bottom side of the window
    ! 6. tl: character to be used for the top left corner of the window
    ! 7. tr: character to be used for the top right corner of the window
    ! 8. bl: character to be used for the bottom left corner of the window
    ! 9. br: character to be used for the bottom right corner of the window
   ierr=wrefresh(local_win)
   ierr=delwin(local_win)
end subroutine destroy_win
!-----------------------------------------------------------------------------------------
end program win_border
!-----------------------------------------------------------------------------------------
