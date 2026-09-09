!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
program test_suite_M_getopt_long
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
use :: M_framework__verify,   only : unit_test_msg
use :: M_framework__verify,   only : unit_test, unit_test_good, unit_test_bad, unit_test_done, unit_test_start
use :: M_framework__verify,   only : unit_test_stop
implicit none
!! setup
   call test_getopt()
   call test_getopt_argv()
   call test_getopt_argv_len()
   call test_getopt_new()
!! teardown
   call unit_test_stop()
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt()

   call unit_test_start('getopt',msg='')
   !!call unit_test('getopt', 0.eq.0, 'checking',100)
   call unit_test_done('getopt',msg='')
end subroutine test_getopt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt_argv()

   call unit_test_start('getopt_argv',msg='')
   !!call unit_test('getopt_argv', 0.eq.0, 'checking',100)
   call unit_test_done('getopt_argv',msg='')
end subroutine test_getopt_argv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt_argv_len()

   call unit_test_start('getopt_argv_len',msg='')
   !!call unit_test('getopt_argv_len', 0.eq.0, 'checking',100)
   call unit_test_done('getopt_argv_len',msg='')
end subroutine test_getopt_argv_len
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt_new()

   call unit_test_start('getopt_new',msg='')
   !!call unit_test('getopt_new', 0.eq.0, 'checking',100)
   call unit_test_done('getopt_new',msg='')
end subroutine test_getopt_new
!===================================================================================================================================
end program test_suite_M_getopt_long
