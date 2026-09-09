!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
program runtest
use M_framework__msg
use M_framework__verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
implicit none
   call test_suite_M_pixel_slices()
contains

subroutine test_suite_M_pixel_slices()
!! setup
   call test_dl_init()
   call test_dl_slices()
   call test_dl_symbol()
!! teardown
end subroutine test_suite_M_pixel_slices
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dl_init()

   call unit_test_start('dl_init',msg='')
   !!call unit_test('dl_init', 0.eq.0, 'checking',100)
   call unit_test_done('dl_init',msg='')
end subroutine test_dl_init
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dl_slices()

   call unit_test_start('dl_slices',msg='')
   !!call unit_test('dl_slices', 0.eq.0, 'checking',100)
   call unit_test_done('dl_slices',msg='')
end subroutine test_dl_slices
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dl_symbol()

   call unit_test_start('dl_symbol',msg='')
   !!call unit_test('dl_symbol', 0.eq.0, 'checking',100)
   call unit_test_done('dl_symbol',msg='')
end subroutine test_dl_symbol
!===================================================================================================================================
end program runtest
