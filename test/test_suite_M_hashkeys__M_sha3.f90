!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
program runtest
use M_hashkeys__sha3__public
use M_framework, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_framework, only : unit_test_stop
use M_framework, only : unit_test_level
   write(*,*)'STARTED test_suite_M_hashkeys__sha3'
   call test_suite_M_hashkeys__sha3()
   write(*,*)'COMPLETED test_suite_M_hashkeys__sha3'
   call unit_test_stop()
contains
subroutine test_suite_M_hashkeys__sha3()

!! setup
   call test_sha3()
   call test_sha3_auto_test()
   call test_sha3_digest()
   call test_sha3_file()
   call test_sha3_hexdigest()
   call test_sha3_update()
!! teardown
end subroutine test_suite_M_hashkeys__sha3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sha3()

   call unit_test_start('sha3',msg='')
   !!call unit_test('sha3', 0.eq.0, 'checking',100)
   call unit_test_done('sha3',msg='')
end subroutine test_sha3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sha3_auto_test()

   call unit_test_start('sha3_auto_test',msg='')
   !!call unit_test('sha3_auto_test', 0.eq.0, 'checking',100)
   call unit_test_done('sha3_auto_test',msg='')
end subroutine test_sha3_auto_test
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sha3_digest()

   call unit_test_start('sha3_digest',msg='')
   !!call unit_test('sha3_digest', 0.eq.0, 'checking',100)
   call unit_test_done('sha3_digest',msg='')
end subroutine test_sha3_digest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sha3_file()

   call unit_test_start('sha3_file',msg='')
   !!call unit_test('sha3_file', 0.eq.0, 'checking',100)
   call unit_test_done('sha3_file',msg='')
end subroutine test_sha3_file
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sha3_hexdigest()

   call unit_test_start('sha3_hexdigest',msg='')
   !!call unit_test('sha3_hexdigest', 0.eq.0, 'checking',100)
   call unit_test_done('sha3_hexdigest',msg='')
end subroutine test_sha3_hexdigest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sha3_update()

   call unit_test_start('sha3_update',msg='')
   !!call unit_test('sha3_update', 0.eq.0, 'checking',100)
   call unit_test_done('sha3_update',msg='')
end subroutine test_sha3_update
!===================================================================================================================================
end program runtest
