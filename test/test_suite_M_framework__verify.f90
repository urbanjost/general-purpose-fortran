program runtest
use M_framework__verify, only : unit_check, unit_check_bad, unit_check_done, unit_check_good, unit_check_start, unit_check_stop
   call test_unit_check_start()
   call test_unit_check()
   call test_unit_check_done()
   call test_unit_check_bad()
   call test_unit_check_good()
   call test_unit_check_stop()
   call unit_check_stop()
   contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_check_start()
   call unit_check_start('unit_check_start',msg='')
!      call unit_check('unit_check_start', .true.,'expression is true')
!      call unit_check('unit_check_start', .false.,'expression is false')
   call unit_check_done('unit_check_start',msg='')
end subroutine test_unit_check_start
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_check()
   call unit_check_start('unit_check',msg='')
!      call unit_check('unit_check', .true.,'expression is true')
!      call unit_check('unit_check', .false.,'expression is false')
   call unit_check_done('unit_check',msg='')
end subroutine test_unit_check
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_check_done()
   call unit_check_start('unit_check_done',msg='')
!      call unit_check('unit_check_done', .true.,'expression is true')
!      call unit_check('unit_check_done', .false.,'expression is false')
   call unit_check_done('unit_check_done',msg='')
end subroutine test_unit_check_done
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_check_bad()
   call unit_check_start('unit_check_bad',msg='')
!      call unit_check('unit_check_bad', .true.,'expression is true')
!      call unit_check('unit_check_bad', .false.,'expression is false')
   call unit_check_done('unit_check_bad',msg='')
end subroutine test_unit_check_bad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_check_good()
   call unit_check_start('unit_check_good',msg='')
!      call unit_check('unit_check_good', .true.,'expression is true')
!      call unit_check('unit_check_good', .false.,'expression is false')
   call unit_check_done('unit_check_good',msg='')
end subroutine test_unit_check_good
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_check_stop()
   call unit_check_start('unit_check_stop',msg='')
!      call unit_check_stop('unit_check_stop', .true.,'expression is true')
!      call unit_check_stop('unit_check_stop', .false.,'expression is false')
   call unit_check_done('unit_check_stop',msg='')
end subroutine test_unit_check_stop
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
