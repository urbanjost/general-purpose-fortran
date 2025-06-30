!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
program runtest
use M_msg
use M_verify
use M_journal
implicit none
!! setup
   call test_flush_trail()
   call test_set_stdout_lun()
   call test_where_write_message_all()
   call test_write_message_only()
   call unit_test_stop()
!! teardown
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_flush_trail()
implicit none
   call unit_test_start('flush_trail',msg='')
   call journal()
   !!call unit_test('flush_trail', 0.eq.0, 'checking',100)
   call unit_test_done('flush_trail',msg='')
end subroutine test_flush_trail
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_set_stdout_lun()
implicit none
   call unit_test_start('set_stdout_lun',msg='')
   !!call unit_test('set_stdout_lun', 0.eq.0, 'checking',100)
   call unit_test_done('set_stdout_lun',msg='')
end subroutine test_set_stdout_lun
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_where_write_message_all()
implicit none
   call unit_test_start('where_write_message_all',msg='')
   !!call unit_test('where_write_message_all', 0.eq.0, 'checking',100)
   call unit_test_done('where_write_message_all',msg='')
end subroutine test_where_write_message_all
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_write_message_only()
implicit none
   call unit_test_start('write_message_only',msg='')
   !!call unit_test('write_message_only', 0.eq.0, 'checking',100)
   call unit_test_done('write_message_only',msg='')
end subroutine test_write_message_only
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program runtest
