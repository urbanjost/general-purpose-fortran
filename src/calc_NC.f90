!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcccccccc
subroutine calc_NC(name,args,iargs_type,n,fval,i,ier) ! look this name up to see if it is a ncurses function
!
!@(#) user-added calculator call function for ncurses routines
!
!-----------------------------------------------------------------------------------------------------------------------------------
use M_journal, only : journal
use M_calculator, only : x, y, values_len, values, iclen_calc, icname_calc, stuff, getvalue, stuffa
use M_ncurses                                           ! load the Fortran module to interface to the ncurses(3c) C library
implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)          :: name      ! function name to try and match
      real(kind=kind(0.0d0)),intent(in)    :: args(100) ! array of 100 elements containing procedure arguments.
      integer,intent(in)                   :: iargs_type(100) ! 
      integer,intent(in)                   :: n         ! integer number of parameters
      real(kind=kind(0.0d0)),intent(inout) :: fval      ! value to replace function call
      integer,intent(inout)                :: i         ! number to select which function to call in other routines
      integer,intent(out)                  :: ier       ! returned error flag value.
                                                        !     set to -1 if an error occurs, otherwise, user should leave it alone
!-----------------------------------------------------------------------------------------------------------------------------------
      integer                              :: iargs(n)  ! integer copy of args(:n)
      integer(C_LONG)                      :: iargs8(n) ! long integer copy of args(:n)
      integer                              :: ierr
      integer                              :: i1,i2,len1,len2
      integer,parameter                    :: dp=kind(0.0d0)
      integer                              :: nn        ! expected number of parameters
      character(len=:),allocatable         :: string
!-----------------------------------------------------------------------------------------------------------------------------------
      ier=0            ! no known errors occurred
      nn=-1            ! expected number of parameters
      i=0              ! flag if function name not found
      fval=0.0d0       ! value to be returned
      iargs=int(args(:n))
      iargs8=int(args(:n))
!-----------------------------------------------------------------------------------------------------------------------------------
      FIND_NAME: SELECT CASE (NAME)
!-----------------------------------------------------------------------------------------------------------------------------------
      case ("initscr","endwin","refresh","getch","echo","noecho","clear","cbreak","nonl")
         nn=0     ! number of parameters that should be used
         if(n.eq.0)then
            BASEP: SELECT CASE (NAME)
               CASE ("initscr");
                  stdscr=initscr() ! Start curses mode to pre-defined standard screen
                                   ! NOTE THAT THE SCREEN WILL CLEAR
                                   ! NORMAL READ()s and WRITE()s TO THE SCREEN WILL NOT WORK CORRECTLY NOW
                  FVAL=0
               CASE ("endwin")
                  ierr=endwin()    ! End curses mode
                                   ! NOTE THAT
                                   !    THE SCREEN WILL CLEAR OR RETURN TO THE STATE BEFORE INITSCR() WAS
                                   !    CALLED, OR STAY AS-IS (DEPENDS ON TERMINAL TYPE AND DEFINITIONS).
                                   !    NORMAL READ()s and WRITE()s TO THE SCREEN WILL NOW WORK CORRECTLY
                  FVAL=ierr
               CASE ("refresh") ; fval=refresh() ;  ! update the real screen
               CASE ("noecho")  ; fval=noecho()  ;
               CASE ("echo")    ; fval=echo()    ;
               CASE ("getch")   ; fval=getch()   ;  ! Wait for a user keystroke
               CASE ("clear")   ; fval=clear()   ;  ! clear window
               CASE ("cbreak")  ; fval=cbreak()  ;  ! read character one at a time without waiting for NL
               CASE ("nonl")    ; fval=nonl()    ;  ! 
            END SELECT BASEP
         endif

!-----------------------------------------------------------------------------------------------------------------------------------
      case ("addstr")
         nn=1     ! number of parameters that should be used
         if(n.eq.1)then
            BASE1: SELECT CASE (NAME)
               CASE ("addstr")
                  if(iargs(1).lt.0.or.iargs(1).gt.size(values))then
                     call journal('sc','error in addstr, index out of range=',iargs(1))
                  else
                  string=values(iargs(1))(:values_len(iargs(1)))
                  ierr=addstr(string//C_NULL_CHAR)   ! Print string
                                                               ! NOTE THAT
                                                               !    A NULL IS APPENDED TO THE STRING FOR C
                                                               !    THE INITIAL POSITION IS AT THE TOP LEFT CORNER (Y=0,X=0)
                                                               !    THE CURRENT POSITION IS LEFT AT THE END OF THE STRING
                  FVAL=ierr
                  endif
            END SELECT BASE1
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
      case ("move","getyx","getmaxyx","vline","hline")
         nn=2     ! number of parameters that should be used
         if(n.eq.2)then
            BASE2: SELECT CASE (NAME)
               CASE ("move");  fval=move(iargs(1),iargs(2) )      ! move the current position
               CASE ("vline"); fval=vline(iargs8(1),iargs(2) )    ! vertical line
               CASE ("hline"); fval=hline(iargs8(1),iargs(2) )    ! horizontal line
               CASE ("getyx")                                     ! getyx(STDSCR,"name1","name2")
                  call getyx(stdscr,i1,i2)
                  len1=values_len(iargs(1))
                  call stuff(values(iargs(1))(:len1),dble(i1),'')
                  len2=values_len(iargs(2))
                  call stuff(values(iargs(2))(:len2),dble(i2),'')
               CASE ("getmaxyx")                                  ! getmaxyx(STDSCR,"name1","name2")
                  call getmaxyx(stdscr,i1,i2)
                  len1=values_len(iargs(1))
                  call stuff(values(iargs(1))(:len1),dble(i1),'')
                  len2=values_len(iargs(2))
                  call stuff(values(iargs(2))(:len2),dble(i2),'')
            END SELECT BASE2
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
      CASE ("nc")
         nn=-1
         call journal("nc routines:")
call journal('!==============================================================================#')
call journal('! initscr()        start curses mode to pre-defined standard screen            !')
call journal('! noecho()         nothing echoed to screen unless put there with a call       !')
call journal('! cbreak()         take input chars one at a time, no wait for \n              !')
call journal('! nonl()           do not do NL->CR/NL on output                               !')
call journal('! clear()          clear window                                                !')
call journal('! refresh()        update the real screen                                      !')
call journal('! echo()           turn off noecho mode                                        !')
call journal('! endwin()         end curses mode                                             !')
call journal('!                                                                              !')
call journal('! getyx(STDSCR,"name1","name2")   get current position                         !')
call journal('! ii=getch()       wait for a user keystroke                                   !')
call journal('! addstr("string") print string at current position, leave CP at end of string !')
call journal('! move(Y,X)        move current position                                       !')
call journal('! getmaxyx(STDSCR,"name1","name2")   get maximum coordinates for screen        !')
call journal('!==============================================================================#')
call journal('! int vline(chtype ch, int n)  draw vertical line N characters long. Set ch==0 !')
call journal('! int hline(chtype ch, int n)  draw horiz. line N characters long. Set ch==0   !')
call journal('!==============================================================================#')
!-----------------------------------------------------------------------------------------------------------------------------------
      CASE DEFAULT ! name not matched
         ! no calls for speed of sound? WPTI(PI,TI,IREG) not called
         i=0       ! flag no match is found
         return
      END SELECT FIND_NAME
!-----------------------------------------------------------------------------------------------------------------------------------
      i=1                 ! flag that name was matched by this routine
!-----------------------------------------------------------------------------------------------------------------------------------
      ! nn=desired number of parameters where -1 means do not check; and n is actual number of parameters passed to function
      if(((n-nn).ne.0).and.(nn.ne.-1))then  ! check number of parameters
         call journal("*calc_NC* incorrect number of parameters for function "//name)
         IER=-1
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      end subroutine calc_NC
