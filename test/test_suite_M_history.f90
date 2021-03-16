program program_test_suite_M_history
use M_history
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_history'
   call test_suite_M_history()
   write(*,*)'COMPLETED M_history'
end program program_test_suite_M_history
