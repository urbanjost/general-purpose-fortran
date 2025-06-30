program test_suite_M_history
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
use :: M_framework__verify, only : unit_test, unit_test_good, unit_test_bad, unit_test_done, unit_test_start, unit_test_level
use :: M_framework__verify, only : unit_test_level
use :: M_framework__verify, only : unit_test_stop
use M_history,     only : redo
implicit none
unit_test_level=0
!! setup
   call test_redo()
   call unit_test_stop()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_redo()
character(len=256)           :: read_from_file
character(len=256)           :: inl
integer                      :: ios
integer                      :: io
   write(*,*)'UNIT_TEST_LEVEL=',unit_test_level
   call unit_test_start('redo',msg='')
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
      if(unit_test_level.ne.0) write(*,'(2a," IOS=",i0)')'READ:',trim(read_from_file),ios
      if(ios.ne.0)exit
      inl=read_from_file
      if(unit_test_level.ne.0) write(*,'(2a)')'IN:   ',trim(inl)
      call redo(inl,'r',lun=io)
      if(unit_test_level.ne.0) write(*,'(2a)')'SOFAR:',trim(inl)
   enddo
   close(unit=io,iostat=ios,status='delete')
   if(unit_test_level.ne.0) write(*,*)'LAST: ',trim(inl)
   call unit_test('redo',inl.eq.'echo The Alphabet is: ABCDEFGHIJKLMNOPQRSTUVWXYZ > tmp/_outtest','checking',inl)
   call unit_test_done('redo',msg='')
end subroutine test_redo
end program test_suite_M_history
