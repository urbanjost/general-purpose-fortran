program program_test_suite_M_geography
use M_geography
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_geography'
   call test_suite_M_geography()
   write(*,*)'COMPLETED M_geography'
end program program_test_suite_M_geography
