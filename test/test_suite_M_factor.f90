program program_test_suite_M_factor
use M_factor
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_factor'
   call test_suite_M_factor()
   write(*,*)'COMPLETED M_factor'
end program program_test_suite_M_factor
