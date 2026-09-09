program runtest
use M_framework__verify, only : unit_test, unit_test_bad, unit_test_end, unit_test_good, unit_test_start, unit_test_stop
   call test_unit_test_start()
   call test_unit_test()
   call test_unit_test_end()
   call test_unit_test_bad()
   call test_unit_test_good()
   call test_unit_test_stop()
   call unit_test_stop()
   contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_test_start()
   call unit_test_start('unit_test_start',msg='')
!      call unit_test('unit_test_start', .true.,'expression is true')
!      call unit_test('unit_test_start', .false.,'expression is false')
   call unit_test_end('unit_test_start',msg='')
end subroutine test_unit_test_start
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_test()
   call unit_test_start('unit_test',msg='')
!      call unit_test('unit_test', .true.,'expression is true')
!      call unit_test('unit_test', .false.,'expression is false')
   call unit_test_end('unit_test',msg='')
end subroutine test_unit_test
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_test_end()
   call unit_test_start('unit_test_end',msg='')
!      call unit_test('unit_test_end', .true.,'expression is true')
!      call unit_test('unit_test_end', .false.,'expression is false')
   call unit_test_end('unit_test_end',msg='')
end subroutine test_unit_test_end
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_test_bad()
   call unit_test_start('unit_test_bad',msg='')
!      call unit_test('unit_test_bad', .true.,'expression is true')
!      call unit_test('unit_test_bad', .false.,'expression is false')
   call unit_test_end('unit_test_bad',msg='')
end subroutine test_unit_test_bad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_test_good()
   call unit_test_start('unit_test_good',msg='')
!      call unit_test('unit_test_good', .true.,'expression is true')
!      call unit_test('unit_test_good', .false.,'expression is false')
   call unit_test_end('unit_test_good',msg='')
end subroutine test_unit_test_good
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_test_stop()
   call unit_test_start('unit_test_stop',msg='')
!      call unit_test_stop('unit_test_stop', .true.,'expression is true')
!      call unit_test_stop('unit_test_stop', .false.,'expression is false')
   call unit_test_end('unit_test_stop',msg='')
end subroutine test_unit_test_stop
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
