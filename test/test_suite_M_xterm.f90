module M_test_suite_M_xterm
use M_framework__msg
private
public test_suite_m_xterm
contains
subroutine test_suite_m_xterm()
! this should contains tests for all public procedures in the module
   call test_generate_xterm()
end subroutine test_suite_m_xterm
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine test_generate_xterm()
use M_framework__verify, only : unit_test, unit_test_start, unit_test_good, unit_test_bad, unit_test_done

!   This just checks that we can generate the various types of xterm
!   (without crashing) and checks that they have the correct syntax. We
!   could also check that the xterm changes for each call and I think there
!   is an additional check we could make within the xterm itself. But for
!   now this is enough.
!
!   character(len=36) :: xterm
!   call unit_test_start('generate_xterm') ! start tests
!
!   xterm = generate_xterm(0)
!   call unit_test('generate_xterm',check_xterm(xterm).and.(xterm =='00000000-0000-0000-0000-000000000000'),msg='Version 0 '//xterm)
!
!   xterm = generate_xterm(1)
!   call unit_test('generate_xterm',check_xterm(xterm),msg='Version 1 '//xterm)
!
!   xterm = generate_xterm(2)
!   call unit_test('generate_xterm',xterm=='',msg='Version 2 (NOT IMPLEMENTED)')
!
!   xterm = generate_xterm(3)
!   call unit_test('generate_xterm',xterm=='',msg='Version 3 (NOT IMPLEMENTED)')
!
!   xterm = generate_xterm(4)
!   call unit_test('generate_xterm',check_xterm(xterm),msg='Version 4 '//xterm)
!
!   xterm = generate_xterm(5)
!   call unit_test('generate_xterm',xterm=='',msg='Version 5 (NOT IMPLEMENTED)')
!
!   call unit_test('compare',exercise(1000000),msg='test for duplicates in 1000000 values')
!   call unit_test_done('generate_xterm')
!==================================================================================================================================!
end subroutine test_generate_xterm
!==================================================================================================================================!
end module M_test_suite_M_xterm
!==================================================================================================================================!
program runtest
use M_framework__msg
use M_framework__verify, only : unit_test_stop
use M_test_suite_M_xterm
implicit none
   call test_suite_M_xterm()
   call unit_test_stop()
end program runtest
!==================================================================================================================================!
