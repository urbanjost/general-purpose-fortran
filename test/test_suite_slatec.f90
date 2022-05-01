module M_test_suite_slatec
use M_msg
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level
use slatec, only : generate_uuid
private
public test_suite_slatec
contains
subroutine test_suite_slatec()
! this should contains tests for all public procedures in the module
   call test_generate_uuid()
end subroutine test_suite_slatec
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine test_generate_uuid()
use M_verify, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done

!   This just checks that we can generate the various types of UUID
!   (without crashing) and checks that they have the correct syntax. We
!   could also check that the UUID changes for each call and I think there
!   is an additional check we could make within the UUID itself. But for
!   now this is enough.

character(len=36) :: uuid
   call unit_check_start('slatec') ! start tests

!   uuid = generate_uuid(0)
!   call unit_check('generate_uuid',check_uuid(uuid).and.(uuid =='00000000-0000-0000-0000-000000000000'),msg='Version 0 '//uuid)
!
!   uuid = generate_uuid(1)
!   call unit_check('generate_uuid',check_uuid(uuid),msg='Version 1 '//uuid)
!
!   uuid = generate_uuid(2)
!   call unit_check('generate_uuid',uuid=='',msg='Version 2 (NOT IMPLEMENTED)')
!
!   uuid = generate_uuid(3)
!   call unit_check('generate_uuid',uuid=='',msg='Version 3 (NOT IMPLEMENTED)')
!
!   uuid = generate_uuid(4)
!   call unit_check('generate_uuid',check_uuid(uuid),msg='Version 4 '//uuid)
!
!   uuid = generate_uuid(5)
!   call unit_check('generate_uuid',uuid=='',msg='Version 5 (NOT IMPLEMENTED)')
!
!   call unit_check('compare',exercise(1000000),msg='test for duplicates in 1000000 values')
   call unit_check_done('slatec')
!==================================================================================================================================!
contains
!==================================================================================================================================!
end module M_test_suite_slatec
!==================================================================================================================================!
program runtest
use M_msg
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level, unit_check_stop
use M_test_suite_slatec
implicit none
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_suite_slatec()
   call unit_check_stop()
end program runtest
!==================================================================================================================================!
