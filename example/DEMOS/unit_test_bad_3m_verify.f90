      program demo_unit_test_bad
      use M_verify, only: unit_test_start
      use M_verify, only: unit_test
      use M_verify, only: unit_test_good, unit_test_bad

      implicit none
      integer :: x
      x=10
      call unit_test_start('myroutine')

      call unit_test('myroutine', x > 3 ,'test if big enough')
      call unit_test('myroutine', x < 100 ,'test if small enough')

      if(x /= 0)then
         call unit_test_bad ('myroutine',msg='checks on "myroutine" failed') ! program execution stopped
      endif

      end program demo_unit_test_bad
