      program demo_unit_test_good
      use M_verify, only: unit_test_start, unit_test_done
      use M_verify, only: unit_test
      use M_verify, only: unit_test_good, unit_test_bad

      implicit none
      integer :: x
      x=10
      call unit_test_start('myroutine')

      call unit_test('myroutine', x > 3 ,'test if big enough')
      call unit_test('myroutine', x < 100 ,'test if small enough')

      call unit_test_good('myroutine',msg='checks on "myroutine" ')

      end program demo_unit_test_good
