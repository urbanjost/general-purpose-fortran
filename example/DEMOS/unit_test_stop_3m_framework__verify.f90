      program demo_unit_test_stop
      use M_framework, only: unit_test_start, unit_test_end, &
      & unit_test, unit_test_stop, unit_test_mode
      use,intrinsic :: iso_fortran_env, stdout=>OUTPUT_UNIT
      implicit none
      integer :: x
      x=10
      call unit_test_mode(luns=[stdout])
      ! do a test
      call unit_test_start('proc1')
      call unit_test('proc1', x > 3 , 'if big enough')
      call unit_test('proc1', x < 100 , 'if small enough')
      call unit_test_end  ('proc1',msg='checks all done' )
      ! do another test
      call unit_test_start('proc2')
      call unit_test('proc2', x > 3 , 'if big enough')
      call unit_test('proc2', x < 100 , 'if small enough')
      call unit_test_end  ('proc2',msg='checks all done' )

      ! tally up test results and stop program
      call unit_test_stop()

      end program demo_unit_test_stop
