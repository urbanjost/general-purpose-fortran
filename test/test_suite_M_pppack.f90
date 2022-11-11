module M_test_suite_M_pppack
use M_msg
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level
use M_pppack !, only : 
private
public test_suite_m_pppack
contains
subroutine test_suite_m_pppack()
! this should contains tests for all public procedures in the module
   call test_pppack()
end subroutine test_suite_m_pppack
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine test_pppack()
use M_verify, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done

!   call unit_check_start('M_pppack') ! start tests
!
!   call unit_check('pppack',check_pppack(pppack).and.(pppack =='00000000-0000-0000-0000-000000000000'),msg='Version 0 '//pppack)
!
!   call unit_check('pppack',check_pppack(pppack),msg='Version 1 '//pppack)
!
!   call unit_check('pppack',pppack=='',msg='Version 2 (NOT IMPLEMENTED)')
!
!   call unit_check('pppack',pppack=='',msg='Version 3 (NOT IMPLEMENTED)')
!
!   call unit_check('pppack',check_pppack(pppack),msg='Version 4 '//pppack)
!
!   call unit_check('pppack',pppack=='',msg='Version 5 (NOT IMPLEMENTED)')
!
!   call unit_check('compare',exercise(1000000),msg='test for duplicates in 1000000 values')
   call unit_check_done('M_pppack')
end subroutine test_pppack
!==================================================================================================================================!
end module M_test_suite_M_pppack
!==================================================================================================================================!
program runtest
use M_msg
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level, unit_check_stop
use M_test_suite_M_pppack
implicit none
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_suite_M_pppack()
   call unit_check_stop()
end program runtest
!==================================================================================================================================!
