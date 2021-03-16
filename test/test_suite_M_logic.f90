program program_test_suite_M_logic
use M_logic
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_logic'
   call test_suite_M_logic()
   write(*,*)'COMPLETED M_logic'
end program program_test_suite_M_logic
