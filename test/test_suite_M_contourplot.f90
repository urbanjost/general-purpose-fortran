program program_test_suite_M_contourplot
use M_contourplot
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_contourplot'
   call test_suite_M_contourplot()
   write(*,*)'COMPLETED M_contourplot'
end program program_test_suite_M_contourplot
