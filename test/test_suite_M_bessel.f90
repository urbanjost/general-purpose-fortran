module M_test_suite_M_bessel
use M_framework__msg
use M_framework__verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_bessel, only : bes, besi, besj, besj0, besj1, besk, besy, besy0
private
public test_suite_m_bessel
contains
subroutine test_suite_m_bessel()
! this should contains tests for all public procedures in the module
   call test_bes()
   call test_besi()
   call test_besj()
   call test_besj0()
   call test_besj1()
   call test_besk()
   call test_besy()
   call test_besy0()
end subroutine test_suite_m_bessel
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_bes()

   call unit_test_start('bes',msg='')
   !*!call unit_test('bes', 0.eq.0, 'checking',100)
   call unit_test_done('bes',msg='')
end subroutine test_bes
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besi()

   call unit_test_start('besi',msg='')
   !*!call unit_test('besi', 0.eq.0, 'checking',100)
   call unit_test_done('besi',msg='')
end subroutine test_besi
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besj()

   call unit_test_start('besj',msg='')
   !*!call unit_test('besj', 0.eq.0, 'checking',100)
   call unit_test_done('besj',msg='')
end subroutine test_besj
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besj0()

   call unit_test_start('besj0',msg='')
   !*!call unit_test('besj0', 0.eq.0, 'checking',100)
   call unit_test_done('besj0',msg='')
end subroutine test_besj0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besj1()

   call unit_test_start('besj1',msg='')
   !*!call unit_test('besj1', 0.eq.0, 'checking',100)
   call unit_test_done('besj1',msg='')
end subroutine test_besj1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besk()

   call unit_test_start('besk',msg='')
   !*!call unit_test('besk', 0.eq.0, 'checking',100)
   call unit_test_done('besk',msg='')
end subroutine test_besk
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besy()

   call unit_test_start('besy',msg='')
   !*!call unit_test('besy', 0.eq.0, 'checking',100)
   call unit_test_done('besy',msg='')
end subroutine test_besy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besy0()

   call unit_test_start('besy0',msg='')
   !*!call unit_test('besy0', 0.eq.0, 'checking',100)
   call unit_test_done('besy0',msg='')
end subroutine test_besy0
!==================================================================================================================================!
end module M_test_suite_M_bessel
!==================================================================================================================================!
program runtest
use M_framework__msg
use M_framework__verify, only : unit_test_stop
use M_test_suite_M_bessel
implicit none
   call test_suite_M_bessel()
   call unit_test_stop()
end program runtest
!==================================================================================================================================!
