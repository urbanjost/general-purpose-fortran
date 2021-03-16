program program_test_suite_M_args
use M_args
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_args'
   call test_suite_M_args()
   write(*,*)'COMPLETED M_args'
end program program_test_suite_M_args
