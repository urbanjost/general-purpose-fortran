           program demo_unit_check_done
           use M_verify, only: unit_check_start
           use M_verify, only: unit_check
           use M_verify, only: unit_check_good, unit_check_done, unit_check_bad

           implicit none
           integer :: x
           x=10
           call unit_check_start('myroutine')

           call unit_check('myroutine', x.gt.3 ,'test if big enough')
           call unit_check('myroutine', x.lt.100 ,'test if small enough')

           if(x.ne.0)then
              call unit_check_done ('myroutine',msg='checks on "myroutine"' ) ! program execution stopped
           endif

           end program demo_unit_check_done
