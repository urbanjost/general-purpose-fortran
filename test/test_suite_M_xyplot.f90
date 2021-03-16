program program_test_suite_M_xyplot
use M_xyplot
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_xyplot'
   call test_suite_M_xyplot()
   write(*,*)'COMPLETED M_xyplot'
end program program_test_suite_M_xyplot
