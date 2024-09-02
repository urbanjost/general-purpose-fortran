      program demo_unit_test_start
      use M_framework, only: unit_test_start, unit_test, &
       & unit_test_end, unit_test_mode, unit_test_stop
      implicit none
      integer :: ival
      logical :: matched
      call unit_test_mode()
      call test_mysub1()
      call test_mysub2()
      call unit_test_stop()
      contains
      subroutine test_mysub1() ! first test
      call unit_test_start('mysub1')
      ! the example goodbad(1) command called here takes many options
      ! used to build an SQLite3 entry
      ival=10
      call unit_test('mysub1', ival > 3 ,   msg=' if big enough')
      call unit_test('mysub1', ival < 100 , msg=' if small enough')
      call unit_test_end('mysub1',msg='completed checks of "mysub1"')
      end subroutine test_mysub1

      subroutine test_mysub2() ! second test
      call unit_test_start('mysub1','',matched=matched)
      ival=200
         if(.not.matched)return ! makes it skippable
         call unit_test('mysub1', ival > 3 ,   msg=' if big enough')
         call unit_test('mysub1', ival < 100 , msg=' if small enough')
         call unit_test_end('mysub1',msg='completed checks of "mysub2"')
      end subroutine test_mysub2

      end program demo_unit_test_start
