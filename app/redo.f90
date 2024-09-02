program redoit
!(LICENSE:PD)
use M_history,    only : redo
use M_calculator, only : calculator,iclen_calc
use M_framework,  only : journal
use M_io,         only : read_line
implicit none

character(len=*),parameter :: ident="@(#) redo(1f): simple history shell allows using redo(3f) for command history"

character(len=1024)        :: line
integer                    :: ios
integer                    :: cstat
character(len=256)         :: sstat

integer,parameter            :: dp=selected_real_kind(15,300)
character(len=iclen_calc)    :: outlin
character(len=iclen_calc)    :: event
real(kind=dp)                :: rvalue
integer                      :: ierr=0
integer                      :: in=5
character(len=:),allocatable :: trailfile
logical                      :: verbose=.false.

   call calculator('ownmode(1)',outlin,event,rvalue,ierr)     ! specify user-supplied routine contains extra routines for calculator
   !trailfile=sget('compute_trail')
   !if(trailfile.ne.' ')then
   !   call journal('O',trailfile)
   !endif
   call instructions()
   do
      write(*,'(a)',advance='no')'>->'    ! write prompt
      read(*,'(a)',iostat=ios) line       ! read new input line

      ! if "r", edit and return a line from the history editor
      call redo(line) ! store into history if not "r".
      select case(line)
      case('q','quit')
          stop ! exit program if user enters "quit"
      case('$')
         call calcit()
      case default
         ! call the system shell 
         call execute_command_line(trim(line),cmdstat=cstat,cmdmsg=sstat) 
      end select
   enddo
contains
   subroutine instructions
   write(*,'(a)')'_______________________________________________________________________________'
   write(*,'(a)')'| REDO(3f) COMMAND HISTORY INPUT EDITOR                                        |'
   write(*,'(a)')'|                                                                              |'
   write(*,'(a)')'| Enter commands to be passed to the shell normally.                           |'
   write(*,'(a)')'|                                                                              |'
   write(*,'(a)')'| Enter "r" or "r_command" at the prompt to enter command history edit mode.   |'
   write(*,'(a)')'|                          in history edit mode, enter "?" for help.           |'
   write(*,'(a)')'| Enter "$" at the command prompt to enter calculator mode. Enter "." to exit. |'
   write(*,'(a)')'| Enter "q" at the command prompt to exit the program.                         |'
   write(*,'(a)')'|______________________________________________________________________________|'
   end subroutine instructions
subroutine processit
   call calculator(line,outlin,event,rvalue,ierr)
   select case(ierr)                                                             ! several different meanings to the error flag
    case(-1);     call journal('sc','error===>',event)                           ! an error has occurred
    case(0)
      if(verbose.or.(in.ne.5))then
         call journal('sc',line,"=>",outlin)                                     ! a numeric value was returned without error
      else
         call journal('sc',outlin)                                               ! a numeric value was returned without error
      endif
    case(1);      call journal('sc','message===>',event)                         ! request for a message (from DUMP or FUNC)
    case(2);      call journal('sc',event(:int(rvalue)))                         ! a string value was returned without error
    case default; call journal('sc','*compute* unexpected ierr value ',ierr)     ! this should not occur
   end select
end subroutine processit
subroutine calcit()
character(len=:),allocatable :: readin
character(len=256)           :: csys
integer                      :: isys
integer                      :: esys
character(len=256)           :: mssge
integer                      :: ios
      READ: do
         INFINITE: do while (read_line(readin,in)==0)  ! no expressions on command line to read expressions from stdin
            readin=adjustl(readin)//'  '               ! make at least two characters long and left-adjusted to avoid overindexing
            if(readin.eq.'.')then                      ! exit command is "."
               if(in.eq.5)then
                  exit INFINITE                        ! quit if user enters command "." on stdin
               else
                  close(in,iostat=ios)
                  in=5
                  cycle INFINITE
               endif
            elseif(readin(1:1).eq.'<')then             ! read from alternate input file assuming have "<filename"
               open(newunit=in,iostat=ios,file=readin(2:),iomsg=mssge)
               if(ios.ne.0)then                        ! file open failed, revert back to stdin
                  close(in,iostat=ios)
                  in=5
                  call journal('sc','error:',mssge)
               endif
               cycle INFINITE
            endif
            line=readin
            call redo(line,r='!')                         ! command history
            call journal('t',line)
            if(line(1:1).eq.'!')then                      ! system command
               call execute_command_line(line(2:),exitstat=esys,cmdstat=isys,cmdmsg=csys)   ! try as system command
            elseif(adjustl(line(1:1)).eq.'#')then         ! comment
            else                                          ! expression
               call processit()
            endif
         enddo INFINITE
         if(in.eq.5)exit READ
         close(in,iostat=ios)
         call journal('sc','exiting altername input file')
         in=5
      enddo READ
end subroutine calcit
!$!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
end program redoit
!$!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
