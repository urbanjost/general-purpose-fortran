program program_test_suite_M_random
use M_random
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_random'
   call test_suite_M_random()
   write(*,*)'COMPLETED M_random'
end program program_test_suite_M_random
