program program_test_suite_M_math
use M_math
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_math'
   call test_suite_M_math()
   write(*,*)'COMPLETED M_math'
end program program_test_suite_M_math
