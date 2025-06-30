program test_suite_M_generic_list
use M_framework, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg,unit_test_level

!! setup
   call test_list_free()
   call test_list_get()
   call test_list_init()
   call test_list_insert()
   call test_list_next()
   call test_list_put()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_list_free()

   call unit_test_start('list_free',msg='')
   !!call unit_test('list_free', 0.eq.0, 'checking',100)
   call unit_test_done('list_free',msg='')
end subroutine test_list_free
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_list_get()

   call unit_test_start('list_get',msg='')
   !!call unit_test('list_get', 0.eq.0, 'checking',100)
   call unit_test_done('list_get',msg='')
end subroutine test_list_get
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_list_init()

   call unit_test_start('list_init',msg='')
   !!call unit_test('list_init', 0.eq.0, 'checking',100)
   call unit_test_done('list_init',msg='')
end subroutine test_list_init
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_list_insert()

   call unit_test_start('list_insert',msg='')
   !!call unit_test('list_insert', 0.eq.0, 'checking',100)
   call unit_test_done('list_insert',msg='')
end subroutine test_list_insert
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_list_next()

   call unit_test_start('list_next',msg='')
   !!call unit_test('list_next', 0.eq.0, 'checking',100)
   call unit_test_done('list_next',msg='')
end subroutine test_list_next
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_list_put()

   call unit_test_start('list_put',msg='')
   !!call unit_test('list_put', 0.eq.0, 'checking',100)
   call unit_test_done('list_put',msg='')
end subroutine test_list_put
!===================================================================================================================================
end program test_suite_M_generic_list
