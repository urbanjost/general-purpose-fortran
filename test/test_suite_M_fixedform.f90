
program test_suite_M_fixedform
use M_framework__verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_framework__verify, only : unit_check_stop
!use M_anything
!use M_framework__msg
implicit none

!*! setup
   call test_fixedform()
   call test_loaddata()
!*! teardown
   call unit_check_stop()
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fixedform()

   call unit_check_start('fixedform',msg='')
   !*!call unit_check('fixedform', 0.eq.0, 'checking',100)
   call unit_check_done('fixedform',msg='')
end subroutine test_fixedform
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_loaddata()

   call unit_check_start('loaddata',msg='')
   !*!call unit_check('loaddata', 0.eq.0, 'checking',100)
   call unit_check_done('loaddata',msg='')
end subroutine test_loaddata
!===================================================================================================================================
end program test_suite_M_fixedform
