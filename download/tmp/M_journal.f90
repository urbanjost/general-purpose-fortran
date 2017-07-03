!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_journal
use iso_fortran_env, only : ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT     ! access computing environment
implicit none
private

!>
!!##NAME
!!      journal(3f) - [M_journal]provides public message routine, no paging or graphic mode change"
!!##SYNOPSIS
!!
!!
!!    subroutine journal([where,]message,[VALUE])
!!    character(len=*),intent(in)  :: where
!!    character(len=*),intent(in)  :: msg
!!
!!   write messages
!!
!!    call journal(where,message,[VALUE])
!!    call journal(message) !  shortcut for "call journal('sc',message)"
!!
!!   open or close trail file
!!
!!    call journal('O',[trailfile_name|'']) ! open a trail file, or close trail if filename is blank
!!
!!   set output time prefix
!!
!!    call journal('%',time_stamp_prefix_specification) ! see the NOW(3f) function
!!
!!   modes
!!
!!    call journal([.true.|.false.],'debug')   ! Turn on/off writing DEBUG messages to trail file
!!
!!   assign stdout to an alternate file
!!
!!    call journal(iunit,filename)  ! change stdout to iunit and open filename; or close unit and go back to stdout if filename=''
!!    call journal(iunit)           ! change stdout to iunit to use a file already open
!!
!!##DESCRIPTION
!!##OPTIONS
!!   WHERE  indicates where messages are written. A combination of the following characters can be used
!!
!!      Usually one of these to write to the standard output file ...
!!
!!      S   write to stdout or iounit set with journal(unit) or journal(unit,filename)
!!      E   write to stderr
!!
!!      And one of these to write to trail file (ignore if no trail file defined) ...
!!
!!      C   write to trail file as a comment (if file is open)
!!          Writing output "as a comment" means it is preceded by a pound(#) character.
!!      T   write to trail file (if file is open)
!!
!!      Usually used by itself
!!
!!      D   write to trail file as a comment with DEBUG: prefix in front of message (if file is open) if debug mode is on
!!
!!      Modifier for SECTD
!!
!!      +   subsequent writes for this call are written with advance='no'
!!
!!      Mode changing options used by themselves:
!!
!!      >   turn off debug messages
!!      <   turn on debug messages
!!      O   open trail file "msg" or close trail file if blank filename
!!      %   set prefix to run thru now(3f) to generate time prefix strings
!!
!!   MESSAGE   message to write to stdout, stderr, and the trail file when writing message.
!!   FILENAME  when WHERE="O" to turn the trail file on or off, the message is the filename of a trail to open.
!!   TFORMAT   when WHERE="%" the message is treated as a time format specification as described under now(3f).
!!
!!   VALUE  a numeric value to optionally be appended to the message
!!
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
public journal
interface journal
   module procedure write_msg, wm_i, wm_r, wm_l, wm_d, wm, set_stdout, change_model
end interface journal

character(len=*),parameter :: ident="@(#)M_journal::journal(3fg): provides public message routine, no paging or graphic mode change"

! global variables

!integer,save,private       :: stdin=INPUT_UNIT
integer,save,private       :: stdout=OUTPUT_UNIT
logical,save               :: debug=.false.
integer,save               :: last_int=0

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_msg(where,msg)
character(len=*),parameter :: ident="@(#)M_journal::write_msg(3fp): basic message routine used for journal files"
character(len=*),intent(in)  :: where
character(len=*),intent(in)  :: msg
!
!  writes error messages and general information text to stdout and the trace file
!     where=*C* write to trail file as a comment (if file is open)
!     where=*D* write to trail file as a comment with DEBUG: prefix in front of message (if file is open and debug mode on)
!     where=*E* write to stderr
!     where=*S* write to stdout or iounit set with journal(unit) or journal(unit,filename)
!     where=*T* write to trail file (if file is open)
!     where=*+* subsequent writes for this call are written with advance='no'

!     where=> turn off debug messages (change mode), which are ones with WHERE='D'
!     where=< turn on debug messages  (change mode), which are ones with WHERE='D'

!     where=O open trail file "msg" or close trail file if blank filename is given
!     where=% set prefix to run thru now(3f) to generate time prefix strings, blank turns off time prefix
!     where=N open new file and assign stdout to the file unless file name is blank; then revert to stdout being original stdout.
!
!  the trail file messages are preceded by a pound character (#) by default so they can easily be interpreted as comments
!  if the trace file is subsequently used as input data for a program
!
   logical,save                       :: trailopen=.false.
   integer,save                       :: itrail
   character,save                     :: comment='#'
   integer                            :: i
   integer                            :: ios
   integer                            :: times             ! number of times written to stdout
   character(len=3)                   :: adv               ! whether remaining writes from this call use advancing I/O

   character(len=:),allocatable,save  :: prefix_template   ! string to run thru now_ex(3f) to make prefix
   character(len=:),allocatable       :: prefix            ! the prefix string to add to output
   logical,save                       :: prefix_it=.false. ! flag whether time prefix mode is on or not
!-----------------------------------------------------------------------------------------------------------------------------------
   interface
      function now_ex(format)
         character(len=*),intent(in),optional :: format
         character(len=:),allocatable         :: now_ex
      end function now_ex
   end interface
!-----------------------------------------------------------------------------------------------------------------------------------
   adv='yes'
!-----------------------------------------------------------------------------------------------------------------------------------
   if(prefix_it)then
      prefix=now_ex(prefix_template)
   else
      prefix=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   times=0
   do i=1,len_trim(where)
      select case(where(i:i))
      case('T','t')
         if(trailopen) then
            write(itrail,'(a)',advance=adv)prefix//msg
         elseif(times.eq.0)then
            write(stdout,'(a)',advance=adv)prefix//msg
            times=times+1
         endif
      case('S','s')
         write(stdout,'(a)',advance=adv)prefix//msg
         times=times+1
      case('+'); adv='no'
      !-----------------------------------------------------------------------------------------------------------------------------
      case('%')                       ! setting timestamp prefix
         if(msg.eq.'')then            ! if message is blank turn off prefix
            prefix_it=.false.
         else                         ! store message as string to pass to now_ex() on subsequent calls to make prefix
            prefix_template=msg
            prefix_it=.true.
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('N')                                                   ! new name for stdout
         if(msg.ne.' '.and.msg.ne.'#N#'.and.msg.ne.'"#N#"')then   ! if filename not special or blank open new file
            close(unit=last_int,iostat=ios)
            open(unit=last_int,file=msg(:len_trim(msg)),iostat=ios)
            if(ios.eq.0)then
               stdout=last_int
            else
               write(*,*)'*journal* error opening redirected output file, ioerr=',ios
               write(*,*)'*journal* msg='//msg(:len_trim(msg))
            endif
         elseif(msg.eq.' ')then
            close(unit=last_int,iostat=ios)
            stdout=6
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('C','c')
         if(trailopen)then
            write(itrail,'(2a)',advance=adv)comment,prefix//msg
         elseif(times.eq.0)then
            write(stdout,'(a)',advance=adv)prefix//msg
            times=times+1
         endif
      case('D','d')
         if(debug)then
            if(trailopen)then
               write(itrail,'(3a)',advance=adv)comment,'DEBUG: ',prefix//msg
            elseif(times.eq.0)then
               write(stdout,'(2a)',advance=adv)'DEBUG:',prefix//msg
               times=times+1
            endif
         endif
      case('O','o')
         if(msg.ne.'')then
            open(newunit=itrail,status='unknown',access='sequential', file=trim(msg),form='formatted',iostat=ios)
            trailopen=.true.
         else
            if(trailopen)then
               write(itrail,'(3a)',advance=adv)comment,'closing trail file:',prefix//msg
            endif
            close(unit=itrail,iostat=ios)
            trailopen=.false.
         endif
      case default
         write(stdout,'(a)',advance=adv)'*journal* bad WHERE value '//trim(where)//' when msg=['//trim(msg)//']'
      end select
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine write_msg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set_stdout(iounit)
character(len=*),parameter :: ident="@(#)M_journal::set_stdout(3fp): change I/O logical unit value for standard writes"
integer,intent(in)                   :: iounit
   stdout=iounit
end subroutine set_stdout
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine change_model(value,mode)
character(len=*),parameter :: ident="@(#)M_journal::change_model(3fp): change integer journal(3f) modes"
logical,intent(in)          :: value
character(len=*),intent(in) :: mode

select case(mode)
case('debug','DEBUG')
   debug=value
case default
   call write_msg('sc','*journal* unknown logical mode '//trim(mode))
end select

end subroutine change_model
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine wm(message)
character(len=*),parameter :: ident="@(#)M_journal::wm(3fp): calls JOURNAL('sc',message)"
character(len=*),intent(in)          :: message
!-----------------------------------------------------------------------------------------------------------------------------------
   call write_msg('sc',trim(message))
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine wm
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine wm_r(where,message,value)
character(len=*),parameter :: ident="@(#)M_journal::wm_r(3fp): append real to message and pass to write_msg()"
character(len=*),intent(in)          :: where
character(len=*),intent(in)          :: message
real,intent(in)                      :: value
   character(len=len(message)+21) :: temp1
!-----------------------------------------------------------------------------------------------------------------------------------
   write(temp1,'(a,1x,g0)')message,value
   call write_msg(where,trim(temp1))
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine wm_r
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine wm_l(where,message,truefalse)
character(len=*),parameter :: ident="@(#)M_journal::wm_l(3fp): append logical to message and pass to write_msg()"
character(len=*),intent(in)          :: where
character(len=*),intent(in)          :: message
logical,intent(in)                   :: truefalse
   character(len=len(message)+21)    :: temp1
!-----------------------------------------------------------------------------------------------------------------------------------
   write(temp1,'(a,L2)')message,truefalse
   call write_msg(where,trim(temp1))
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine wm_l
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine wm_d(where,message,dvalue)
character(len=*),parameter :: ident="@(#)M_journal::wm_d(3fp): append doubleprecision to message and pass to write_msg()"
character(len=*),intent(in)       :: where
character(len=*),intent(in)       :: message
doubleprecision,intent(in)        :: dvalue
   character(len=len(message)+41) :: temp1
!-----------------------------------------------------------------------------------------------------------------------------------
   write(temp1,'(a,1x,g0)')message,dvalue
   call write_msg(where,trim(temp1))
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine wm_d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine wm_i(where,message,ival)
character(len=*),parameter :: ident="@(#)M_journal::wm_i(3fp): append integer to string and pass to write_msg()"
character(len=*),intent(in)       :: where     ! flag to specify to write_msg(3f) where to write
character(len=*),intent(in)       :: message   ! base message to print
integer,intent(in)                :: ival      ! integer value to add to message
   integer                        :: ios       ! error status returned by internal WRITE
   character(len=len(message)+21) :: temp1     ! working message to create that has the integer appended to the base message
!-----------------------------------------------------------------------------------------------------------------------------------
   write(temp1,'(a,1x,i0)',iostat=ios)message,ival ! write integer into message
   last_int=ival
   call write_msg(where,trim(temp1))
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine wm_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

end module M_journal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
