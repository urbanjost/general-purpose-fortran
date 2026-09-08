      program demo_unit_test_stop
      use M_verify, only: unit_test_start, unit_test_done
      use M_verify, only: unit_test
      use M_verify, only: unit_test_good, unit_test_stop, unit_test_bad
      use M_verify, only: unit_test_command, unit_test_keep_going, unit_test_level

      implicit none
      integer :: x

      unit_test_command=''
      unit_test_keep_going=.true.
      unit_test_level=0

      x=10
      call unit_test_start('myroutine')

      call unit_test('myroutine', x > 3 ,'test if big enough')
      call unit_test('myroutine', x < 100 ,'test if small enough')

      if(x /= 0)then
         call unit_test_bad  ('myroutine',msg='x /= 0' )
      endif
      call unit_test_done  ('myroutine',msg='checks on "myroutine"' )

      call unit_test_stop()
      end program demo_unit_test_stop
