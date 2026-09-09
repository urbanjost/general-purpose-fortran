!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program test_suite_M_namelist
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
use :: M_framework__verify, only : unit_test, unit_test_good, unit_test_bad, unit_test_done, unit_test_start
use :: M_framework__verify, only : unit_test_stop
use :: M_namelist
implicit none
integer,parameter :: HT=9
call unit_test_start('M_namelist')
!! setup
   call test_get_command_arguments_as_raw_namelist()
   call test_get_command_arguments_stack()
   call test_get_command_arguments_string()
   call test_get_namelist()
   call test_longest_command_argument()
   call test_print_dictionary()
   call unit_test_stop()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_command_arguments_as_raw_namelist()
implicit none
   call unit_test_start('get_command_arguments_as_raw_namelist',msg='')
   !!call unit_test('get_command_arguments_as_raw_namelist', 0.eq.0, 'checking', 100)
   call unit_test_done('get_command_arguments_as_raw_namelist',msg='')
end subroutine test_get_command_arguments_as_raw_namelist
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_command_arguments_stack()
implicit none
   call unit_test_start('get_command_arguments_stack',msg='')
   !!call unit_test('get_command_arguments_stack', 0.eq.0, 'checking', 100)
   call unit_test_done('get_command_arguments_stack',msg='')
end subroutine test_get_command_arguments_stack
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_command_arguments_string()
implicit none
   call unit_test_start('get_command_arguments_string',msg='')
   !!call unit_test('get_command_arguments_string', 0.eq.0, 'checking', 100)
   call unit_test_done('get_command_arguments_string',msg='')
end subroutine test_get_command_arguments_string
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_namelist()
implicit none
   call unit_test_start('get_namelist',msg='')
   !!call unit_test('get_namelist', 0.eq.0, 'checking', 100)
   call unit_test_done('get_namelist',msg='')
end subroutine test_get_namelist
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_longest_command_argument()
implicit none
   call unit_test_start('longest_command_argument',msg='')
   !!call unit_test('longest_command_argument', 0.eq.0, 'checking', 100)
   call unit_test_done('longest_command_argument',msg='')
end subroutine test_longest_command_argument
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_dictionary()
implicit none
   call unit_test_start('print_dictionary',msg='')
   !!call unit_test('print_dictionary', 0.eq.0, 'checking', 100)
   call unit_test_done('print_dictionary',msg='')
end subroutine test_print_dictionary
!===================================================================================================================================
end program test_suite_M_namelist
