module M_test_suite_M_logic
use M_framework__msg
use M_logic, only      : nest_level, cond, write
implicit none
private
public test_suite_m_logic
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_logic()

!! setup
   call test_cond()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cond()

use M_framework__verify, only      : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_strings, only    : lower, delim, v2s  ! convert character case and split a string
use M_calculator, only : inum0 
use M_io, only         : gulp
! WRITE         flag whether current data lines should be written
! NEST_level    nesting level for if/elseif/else/endif
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=1024)              :: line                ! input line
integer,parameter                :: max_words=2         ! maximum number of words allowed on a line, really only need two
character(len=1024)              :: array(max_words)    ! working copy of input line
integer                          :: ibegin(max_words), iterm(max_words) ! location where words start and end
integer                          :: icount
integer                          :: io
integer                          :: ioout
integer                          :: icheck
integer                          :: ilen
integer                          :: ios
integer                          :: value
integer                          :: i
integer                          :: ierr_logic
character(len=:),allocatable     :: text(:)     ! array to hold output file in memory
character(len=:),allocatable     :: expected(:) ! array to hold expected output in memory
character(len=:),allocatable     :: filename
character(len=256)               :: message
!-----------------------------------------------------------------------------------------------------------------------------------
!  Create input file for QA test program
io=10
open(unit=io,status='scratch')
write(io,'(a)')'A=10'
write(io,'(a)')'B=1234.0'
write(io,'(a)')'check_count=0'
write(io,'(a)')'if eq(A,20)'
write(io,'(a)')'   check_count=check_count+2'
write(io,'(a)')'   A=1000'
write(io,'(a)')'   ! SHOULD NOT BE'
write(io,'(a)')'elseif eq(A,10)'
write(io,'(a)')'   ! CORRECT BRANCH'
write(io,'(a)')'   if gt(B,A)'
write(io,'(a)')'      ! CORRECT AGAIN'
write(io,'(a)')'   else'
write(io,'(a)')'      ! SHOULD NOT BE'
write(io,'(a)')'      check_count=check_count+5'
write(io,'(a)')'   endif'
write(io,'(a)')'   ! CORRECT BRANCH END'
write(io,'(a)')'else'
write(io,'(a)')'   check_count=check_count+3'
write(io,'(a)')'   ! SHOULD NOT BE'
write(io,'(a)')'endif'
write(io,'(a)')'! GOT TO END'
!-------------------------------------------------------------------------------
ioout=20
filename='_scratch_M_logic'
open(unit=ioout, file=filename,iomsg=message,iostat=ios)
expected=[ CHARACTER(LEN=128) :: &
'00002 A=10===>10',&
'00002 B=1234.0===>1234',&
'00002 check_count=0===>0',&
'SKIP    check_count=check_count+2',&
'SKIP    A=1000',&
'SKIP    ! SHOULD NOT BE',&
'COMMENT    ! CORRECT BRANCH',&
'COMMENT       ! CORRECT AGAIN',&
'SKIP       ! SHOULD NOT BE',&
'SKIP       check_count=check_count+5',&
'COMMENT    ! CORRECT BRANCH END',&
'SKIP    check_count=check_count+3',&
'SKIP    ! SHOULD NOT BE',&
'COMMENT ! GOT TO END',&
'']
   rewind(io)
   call unit_check_start('cond')                           ! Change database entry to indicate changes have begun
   READLINE: do                                            ! read loop to read input file
      read(io,'(a)',iostat=ios) line
      if(ios.ne.0)then
         if (nest_level.ne.0) then                         ! check to make sure all if blocks are closed
            write(*,*)'*logic* error - IF BLOCK NOT CLOSED WHEN READiNG FILE WAS FINISHED.'
         endif
         exit READLINE
      endif
      ! just parsing the first word out and finding where second word starts although delim(3f) can do more
      array=' ' ! make sure array is initialized for when icount(number of words on line) is zero
      call delim(lower(line),array,max_words,icount,ibegin,iterm,ilen,' ')
      select case(array(1))
      case('if','else','elseif','endif')                            ! find conditional lines
         call cond(trim(array(1)),line(iterm(1)+1:),ierr_logic)     ! process conditional directive
      case default                                                  ! find input lines you want to use, skip others
         if (write) then                                            ! for example, if last conditional was true then write line
            if(index('!#',array(1)(1:1)).ne.0)then
               write(ioout,'(a)')'COMMENT '//trim(line)
            else
               icount=icount+1
               value=inum0(trim(line))
               write(ioout,'(i0.5,1x,a)')icount, trim(line)//'===>'//v2s(value)           ! write data line
            endif
         else
            write(ioout,'(a)')'SKIP '//trim(line)
         endif
      end select
   enddo READLINE
!-----------------------------------------------------------------------------------------------------------------------------------
!  assuming check_count is set to zero initially and  incremented only in the sections it should not an error
!  if variable is not zero.
   icheck=inum0('check_count')
   call unit_check('cond',icheck.eq.0,'CALCULATOR VARIABLE check_count should be zero',icheck)
   call unit_check('cond',nest_level.eq.0,'nesting level should be zero at end of file')
!-----------------------------------------------------------------------------------------------------------------------------------
   ! reopen ioout as a stream for use with gulp(3f)
   close(unit=ioout)
   open(unit=ioout, file=filename, action="read", iomsg=message,form="unformatted", access="stream",status='old',iostat=ios)
   call gulp(ioout,text)  ! read file into an array
   close(unit=ioout,iostat=ios,status='delete')

   if(.not.allocated(text))then
      call unit_check_bad('cond','failed to load file of expected results') ! flag that got unexpected ending
   else
      do i=1,size(text)
         call unit_check('cond',text(i).eq.expected(i),'EXPECTED:',expected(i),'GOT:',text(i))
      enddo
      call unit_check_done('cond') ! flag that got unexpected ending
   endif

   if(allocated(text))then
      deallocate(text)  ! release memory
   endif
   if(allocated(expected))then
      deallocate(expected)  ! release memory
   endif
   close(unit=io,iostat=ios)

end subroutine test_cond
!===================================================================================================================================
end subroutine test_suite_M_logic
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end module M_test_suite_M_logic
!==================================================================================================================================!
program runtest
use M_framework__msg
use M_framework__verify, only : unit_check_stop
use M_test_suite_M_logic
implicit none
   call test_suite_M_logic()
   call unit_check_stop()
end program runtest
!==================================================================================================================================!
