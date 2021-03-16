           program demo_unit_check_good
           use M_verify, only: unit_check_start, unit_check_done
           use M_verify, only: unit_check
           use M_verify, only: unit_check_good, unit_check_bad

           implicit none
           integer :: x
           x=10
           call unit_check_start('myroutine')

           call unit_check('myroutine', x.gt.3 ,'test if big enough')
           call unit_check('myroutine', x.lt.100 ,'test if small enough')

           call unit_check_good('myroutine',msg='checks on "myroutine" ')

           end program demo_unit_check_good
