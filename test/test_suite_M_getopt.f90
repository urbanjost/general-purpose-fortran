program test_suite_M_getopt
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
use :: M_framework__verify, only : unit_test, unit_test_good, unit_test_bad, unit_test_done, unit_test_start
use :: M_framework__verify, only : unit_test_stop
use :: M_args
implicit none
integer,parameter :: HT=9
call unit_test_start('M_args')
!! setup
   call test_getopt()
   call test_process_long()
   call test_process_short()
   call test_substr()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt()
   call unit_test_start('getopt',msg='')
   !!call unit_test('getopt', 0.eq.0, 'checking',100)
   call unit_test_done('getopt',msg='')
end subroutine test_getopt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_process_long()
   call unit_test_start('process_long',msg='')
   !!call unit_test('process_long', 0.eq.0, 'checking',100)
   call unit_test_done('process_long',msg='')
end subroutine test_process_long
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_process_short()
   call unit_test_start('process_short',msg='')
   !!call unit_test('process_short', 0.eq.0, 'checking',100)
   call unit_test_done('process_short',msg='')
end subroutine test_process_short
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_substr()
   call unit_test_start('substr',msg='')
   !!call unit_test('substr', 0.eq.0, 'checking',100)
   call unit_test_done('substr',msg='')
end subroutine test_substr
!===================================================================================================================================
end program test_suite_M_getopt
