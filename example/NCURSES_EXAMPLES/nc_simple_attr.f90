!-----------------------------------------------------------------------------------------------------------------------------------
! @(#) ncurses example program that pages a C code and highlights comments
! C version of pager functionality by Joseph Spainhour <spainhou@bellsouth.net>
! Fortran translation by John S. Urban <urbanjost@comcast.net>, 20141129
!-----------------------------------------------------------------------------------------------------------------------------------
PROGRAM simple_attr
!(LICENSE:PD)
   USE M_ncurses
   IMPLICIT NONE
   INTEGER             :: maxrow, maxcol
   CHARACTER(LEN=1)    :: ch,prev
   CHARACTER(LEN=1024) :: cmd
   CHARACTER(LEN=1024) :: filename
   INTEGER(C_INT)      :: y, x
   integer             :: ios
   integer             :: ilet
   integer             :: iflag
   integer(C_LONG)     :: ATTR=A_BOLD
   integer             :: ierr
   IF(COMMAND_ARGUMENT_COUNT() /=1)THEN                                         ! number of command arguments
      CALL GET_COMMAND_ARGUMENT(0, cmd)
      WRITE(*,'("Usage: ",a," C_filename")')TRIM(cmd)
      STOP 1
  ENDIF
  call GET_COMMAND_ARGUMENT(1,filename)                                         ! get filename from command argument
  open(unit=15,file=trim(filename),status="old",access="stream",iostat=ios)
  IF(ios /= 0)THEN
    CALL stderr("Cannot open input file:["//TRIM(filename)//"]")
    STOP 1
  ENDIf
  stdscr=initscr()                                                              ! Start curses mode
  CALL getmaxyx(stdscr, maxrow, maxcol)                                         ! find the boundaries of the screeen
  prev = CHAR(0)
  INFINITE: DO                                                                  ! read each character and display it
     ios=readchar(ch)                                                           ! read next character from input
     IF( ios /= 0 )THEN                                                         ! check for EOF or read error
        exit INFINITE
     ENDIF
     CALL getyx(stdscr, y, x)                                                   ! get the current curser position
     IF(y == (maxrow - 1))THEN                                                  ! are we are at the end of the screen
        ierr=addstr("<-Press Any Key{biglnopruRGB}->"//C_NULL_CHAR)             ! tell the user to press a key
        ilet=getch()
        call on(ilet,attr,iflag)
        ierr=clear()                                                            ! clear the screen
        ierr=move(0, 0)                                                         ! start at the beginning of the screen
      ENDIF
      IF(prev =='/' .AND. ch=='*')THEN                                          ! If it is / and * then only switch bold on
         ierr=attron(ATTR)                                                      ! cut attribute on
         CALL getyx(stdscr, y, x)                                               ! get the current curser position
         ierr=move(y, x - 1)                                                    ! back up one space
         ierr=addstr("/"//ch//C_NULL_CHAR)                                      ! The actual printing is done here
      ELSE
         ierr=addstr(ch//C_NULL_CHAR)                                           ! The actual printing is done here
      ENDIF
      ierr=refresh()
      IF(prev=='*' .AND. ch=='/')THEN
         ierr=attroff(ATTR)                                                     ! Switch it off once we got * and then /
      ENDIF
      prev = ch
  ENDDO infinite
  CALL getyx(stdscr, y, x)                                                      ! get the current cursor position
  ierr=move(maxrow-1,0)                                                         ! move to bottom left corner
  ierr=addstr("<-EOF: Press Any Key->"//C_NULL_CHAR)                            ! tell the user to press a key
  ierr=getch()
  ierr=endwin()                                                                 ! End curses mode
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ON(ilet,attr,iflag)
   use,intrinsic :: iso_c_binding
   use           :: M_ncurses
   IMPLICIT NONE
   integer(C_INT) :: ilet
   integer        :: iflag
   character       :: let
   integer(C_LONG) :: attr
   iflag=0
   let=char(ilet)
   SELECT CASE (LET)
   CASE('b'); ATTR=A_BLINK
   CASE('i'); ATTR=A_INVIS
   CASE('g'); ATTR=A_RIGHT
   CASE('l'); ATTR=A_LEFT
   CASE('n'); ATTR=A_NORMAL
   CASE('o'); ATTR=A_BOLD
   CASE('p'); ATTR=A_PROTECT
   CASE('r'); ATTR=A_REVERSE
   CASE('u'); ATTR=A_UNDERLINE
   CASE DEFAULT
   iflag=-1
   END SELECT
!-----------------------------------------------------------------------
END SUBROUTINE ON
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE stderr(message) ! "@(#) stderr writes a message to standard error using a standard f2003 method"
   USE ISO_FORTRAN_ENV, ONLY : ERROR_UNIT                                       ! access computing environment
   IMPLICIT NONE
   CHARACTER(LEN=*) :: message
   WRITE(ERROR_UNIT,'(a)')message                                               ! write message to standard error
END SUBROUTINE stderr
!-----------------------------------------------------------------------------------------------------------------------------------
INTEGER FUNCTION readchar(char1)                                                ! read a single character from the stream
   IMPLICIT NONE
   CHARACTER(LEN=1) :: char1
   READ(15,IOSTAT=readchar)char1
END FUNCTION readchar
!-----------------------------------------------------------------------------------------------------------------------------------
END PROGRAM simple_attr
!-----------------------------------------------------------------------------------------------------------------------------------
