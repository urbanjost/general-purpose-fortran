program nc_print
use M_kracken, only : kracken, lget, sget
use M_ncurses
use M_fixedform, only : nc_printhtml
implicit none

! ident_1="@(#) print an ncurses(3c) window dump as HTML"

integer :: ierr
type(C_PTR) :: win
character(len=:),allocatable :: filename_in
character(len=:),allocatable :: filename_out
logical :: paws
logical :: verbose
!-----------------------------------------------------------------------------------------------------------------------------------
!  define command arguments, default values and crack command line
   call kracken('nc2html','-i  -o paper.html -pg .false. -help .false. -version .false. -verbose .false.')
   call help_usage(lget('nc2html_help'))                   ! if -help option is present, display help text and exit
   call help_version(lget('nc2html_version'))              ! if -version option is present, display version text and exit
   filename_in=trim(sget('nc2html_i'))                     ! get -i option
   if(filename_in.eq.'')then                               ! if -i option is empty try anything before an option
      filename_in=sget('nc2html_oo')
   endif
   filename_out=trim(sget('nc2html_o'))
   paws=lget('nc2html_pg')
   verbose=lget('nc2html_verbose')
   if(verbose)then
      write(6,*)'INPUT:    ',trim(filename_in)
      write(6,*)'OUTPUT:   ',trim(filename_out)
      flush(unit=6)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   stdscr=initscr()
   ierr=start_color()
   ! unfortunately, a putwin(3c) file does not appear to contain the color definitions from the originating program?
   ! define common default color pairs
   ierr=init_pair(1_C_SHORT, COLOR_RED,     COLOR_BLACK)
   ierr=init_pair(2_C_SHORT, COLOR_GREEN,   COLOR_BLACK)
   ierr=init_pair(3_C_SHORT, COLOR_YELLOW,  COLOR_BLACK)
   ierr=init_pair(4_C_SHORT, COLOR_BLUE,    COLOR_BLACK)
   ierr=init_pair(5_C_SHORT, COLOR_CYAN,    COLOR_BLACK)
   ierr=init_pair(6_C_SHORT, COLOR_MAGENTA, COLOR_BLACK)
   ierr=init_pair(7_C_SHORT, COLOR_WHITE,   COLOR_BLACK)
   !! seems to be OK with reading windows larger than current display. Not clear to me that would be the case. Maybe use pads.
   win = getwin(trim(filename_in)//C_NULL_CHAR) ! read the window data
   if (.not.c_associated(win)) then
      ierr=endwin()
      write(*,'(a)')"*nc2html* Unable to read/create window from file["//trim(filename_in)//"]"
      stop
   endif
   if(paws)then
      ierr=refresh()
      ierr=wrefresh(win)
      ierr=getch()
   endif
   call nc_printhtml(win,filename_out)
   ierr=delwin(win)
   ierr=endwin()
contains
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'      nc2html(1) - [NCURSES] print an ncurses(3c) window dump as HTML           ',&
'      (LICENSE:PD)                                                              ',&
'DESCRIPTION                                                                     ',&
'      Given a file generated with the ncurses(3c) putwin(3c) procedure          ',&
'      read the file and write it out an an HTML file for printing or            ',&
'      for inclusion much like a IMG file into an HTML document.                 ',&
'SYNOPSIS                                                                        ',&
'          nc2html -i INPUT_FILE -o OUTPUT_FILE [ -pg]''                         ',&
'OPTIONS                                                                         ',&
'         -i INPUT_FILE    Name of ncurses(3c) window dump file generated        ',&
'                          by putwin(3c).                                        ',&
'         -o OUTPUT_FILE   Name of HTML file to generate.                        ',&
'         -pg              Optionally display the ncurses(3c) window dump file   ',&
'                          and pause                                             ',&
'EXAMPLE                                                                         ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!       nc2html(1) - [NCURSES] print an ncurses(3c) window dump as HTML
!!       (LICENSE:PD)
!!##DESCRIPTION
!!       Given a file generated with the ncurses(3c) putwin(3c) procedure
!!       read the file and write it out an an HTML file for printing or
!!       for inclusion much like a IMG file into an HTML document.
!!##SYNOPSIS
!!
!!           nc2html -i INPUT_FILE -o OUTPUT_FILE [ -pg]'
!!##OPTIONS
!!          -i INPUT_FILE    Name of ncurses(3c) window dump file generated
!!                           by putwin(3c).
!!          -o OUTPUT_FILE   Name of HTML file to generate.
!!          -pg              Optionally display the ncurses(3c) window dump file
!!                           and pause
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        nc2html>',&
'@(#)DESCRIPTION:    print an ncurses(3c) window dump as HTML>',&
'@(#)VERSION:        1.0, 20150312>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2024-06-29 21:54:12 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program nc_print
!-----------------------------------------------------------------------------------------------------------------------------------
