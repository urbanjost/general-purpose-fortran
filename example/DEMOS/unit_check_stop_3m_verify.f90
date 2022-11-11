      program demo_unit_check_stop
      use M_verify, only: unit_check_start, unit_check_done
      use M_verify, only: unit_check
      use M_verify, only: unit_check_good, unit_check_stop, unit_check_bad
      use M_verify, only: unit_check_command, unit_check_keep_going, unit_check_level

      implicit none
      integer :: x

      unit_check_command=''
      unit_check_keep_going=.true.
      unit_check_level=0

      x=10
      call unit_check_start('myroutine')

      call unit_check('myroutine', x > 3 ,'test if big enough')
      call unit_check('myroutine', x < 100 ,'test if small enough')

      if(x /= 0)then
         call unit_check_bad  ('myroutine',msg='x /= 0' )
      endif
      call unit_check_done  ('myroutine',msg='checks on "myroutine"' )

      call unit_check_stop()
      end program demo_unit_check_stop
