program test_suite_M_html
use M_framework, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_framework, only : unit_test_level

!! setup
   call test_h_array()
   call test_h_close()
   call test_h_open()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_h_array()

   call unit_test_start('h_array',msg='')
   !!call unit_test('h_array', 0.eq.0, 'checking',100)
   call unit_test_done('h_array',msg='')
end subroutine test_h_array
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_h_close()

   call unit_test_start('h_close',msg='')
   !!call unit_test('h_close', 0.eq.0, 'checking',100)
   call unit_test_done('h_close',msg='')
end subroutine test_h_close
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_h_open()

   call unit_test_start('h_open',msg='')
   !!call unit_test('h_open', 0.eq.0, 'checking',100)
   call unit_test_done('h_open',msg='')
end subroutine test_h_open
!===================================================================================================================================
end program test_suite_M_html
