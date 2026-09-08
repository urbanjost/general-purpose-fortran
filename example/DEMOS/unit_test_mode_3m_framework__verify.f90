     program demo_unit_test_mode
     use M_framework
     implicit none

     call unit_test_mode(keep_going=.false.,luns=[6], &
             & brief=.true.)

     end program demo_unit_test_mode
