
program test_suite_M_fixedform
use M_framework__verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_framework__verify, only : unit_test_stop
!use M_anything
!use M_framework__msg
implicit none

!*! setup
   call test_fixedform()
   call test_loaddata()
!*! teardown
   call unit_test_stop()
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fixedform()

   call unit_test_start('fixedform',msg='')
   !*!call unit_test('fixedform', 0.eq.0, 'checking',100)
   call unit_test_done('fixedform',msg='')
end subroutine test_fixedform
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_loaddata()

   call unit_test_start('loaddata',msg='')
   !*!call unit_test('loaddata', 0.eq.0, 'checking',100)
   call unit_test_done('loaddata',msg='')
end subroutine test_loaddata
!===================================================================================================================================
end program test_suite_M_fixedform
