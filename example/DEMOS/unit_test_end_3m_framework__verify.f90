      program demo_unit_test_end
      use M_framework, only: unit_test_start
      use M_framework, only: unit_test
      use M_framework, only: unit_test_end
      implicit none
      integer :: x
      x=10
      call unit_test_start('myroutine')

      call unit_test('myroutine', x > 3 ,'if big enough')
      call unit_test('myroutine', x < 100 ,'if small enough')

      ! program execution stopped
      call unit_test_end ('myroutine',msg='checks on "myroutine"' )

      end program demo_unit_test_end
