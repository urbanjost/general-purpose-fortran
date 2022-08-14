program test_suite_M_getopt
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
use :: M_verify,   only : unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start, unit_check_level
use :: M_verify,   only : unit_check_command, unit_check_keep_going, unit_check_level
use :: M_verify,   only : unit_check_stop
use :: M_args
implicit none
integer,parameter :: HT=9
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
call unit_check_start('M_args')
!! setup
   call test_getopt()
   call test_process_long()
   call test_process_short()
   call test_substr()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt()
   call unit_check_start('getopt',msg='')
   !!call unit_check('getopt', 0.eq.0, 'checking',100)
   call unit_check_done('getopt',msg='')
end subroutine test_getopt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_process_long()
   call unit_check_start('process_long',msg='')
   !!call unit_check('process_long', 0.eq.0, 'checking',100)
   call unit_check_done('process_long',msg='')
end subroutine test_process_long
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_process_short()
   call unit_check_start('process_short',msg='')
   !!call unit_check('process_short', 0.eq.0, 'checking',100)
   call unit_check_done('process_short',msg='')
end subroutine test_process_short
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_substr()
   call unit_check_start('substr',msg='')
   !!call unit_check('substr', 0.eq.0, 'checking',100)
   call unit_check_done('substr',msg='')
end subroutine test_substr
!===================================================================================================================================
end program test_suite_M_getopt
