!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
program test_suite_M_getopt_long
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
use :: M_verify,   only : unit_check_msg
use :: M_verify,   only : unit_check_level
use :: M_verify,   only : unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start
use :: M_verify,   only : unit_check_command, unit_check_keep_going, unit_check_level
use :: M_verify,   only : unit_check_stop
implicit none
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
!! setup
   call test_getopt()
   call test_getopt_argv()
   call test_getopt_argv_len()
   call test_getopt_new()
!! teardown
   call unit_check_stop()
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt()

   call unit_check_start('getopt',msg='')
   !!call unit_check('getopt', 0.eq.0, 'checking',100)
   call unit_check_done('getopt',msg='')
end subroutine test_getopt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt_argv()

   call unit_check_start('getopt_argv',msg='')
   !!call unit_check('getopt_argv', 0.eq.0, 'checking',100)
   call unit_check_done('getopt_argv',msg='')
end subroutine test_getopt_argv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt_argv_len()

   call unit_check_start('getopt_argv_len',msg='')
   !!call unit_check('getopt_argv_len', 0.eq.0, 'checking',100)
   call unit_check_done('getopt_argv_len',msg='')
end subroutine test_getopt_argv_len
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt_new()

   call unit_check_start('getopt_new',msg='')
   !!call unit_check('getopt_new', 0.eq.0, 'checking',100)
   call unit_check_done('getopt_new',msg='')
end subroutine test_getopt_new
!===================================================================================================================================
end program test_suite_M_getopt_long
