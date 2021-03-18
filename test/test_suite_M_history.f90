program test_suite_M_history
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
use :: M_verify,   only : unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start, unit_check_level
use :: M_verify,   only : unit_check_command, unit_check_keep_going, unit_check_level
use M_history,     only : redo
implicit none
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
!! setup
   call test_redo()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_redo()
character(len=256)           :: read_from_file
character(len=256)           :: inl
integer                      :: ios
integer                      :: io
   write(*,*)'UNIT_CHECK_LEVEL=',UNIT_CHECK_LEVEL
   call unit_check_start('redo',msg='')
   open(newunit=io,file='r_directives.tmp')
   write(io,'(a)')'echo first line'
   write(io,'(a)')'echo abcdefghijklmnopqrstuvwxyz'
   write(io,'(a)')'r'
   write(io,'(a)')'c/klmnopqrst/KLMNOPQRST/'
   write(io,'(a)')'m     ABCDEFGHIJ          UVWXYZ'
   write(io,'(a)')''
   write(io,'(a)')'r'
   write(io,'(a)')'m    ^ The alphabet is:#'
   write(io,'(a)')''
   write(io,'(a)')'r c/alpha/Alpha/'
   write(io,'(a)')''
   write(io,'(a)')'r /XYZ'
   write(io,'(a)')'c@XYZ@XYZ > tmp/_outtest@'
   write(io,'(a)')''
   write(io,'(a)')'r l 1'
   write(io,'(a)')'.'
   write(io,'(a)')'r /ABCD'
   write(io,'(a)')''
   rewind(io)
   do
      read(io,'(a)',iostat=ios)read_from_file
      if(unit_check_level.ne.0) write(*,'(2a," IOS=",i0)')'READ:',trim(read_from_file),ios
      if(ios.ne.0)exit
      inl=read_from_file
      if(unit_check_level.ne.0) write(*,'(2a)')'IN:   ',trim(inl)
      call redo(inl,'r',lun=io)
      if(unit_check_level.ne.0) write(*,'(2a)')'SOFAR:',trim(inl)
   enddo
   close(unit=io,iostat=ios,status='delete')
   if(unit_check_level.ne.0) write(*,*)'LAST: ',trim(inl)
   call unit_check('redo',inl.eq.'echo The Alphabet is: ABCDEFGHIJKLMNOPQRSTUVWXYZ > tmp/_outtest','checking',trim(inl))
   call unit_check_done('redo',msg='')
end subroutine test_redo
end program test_suite_M_history
